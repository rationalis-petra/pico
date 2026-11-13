#include "platform/machine_info.h"

#ifdef WINDOW_SYSTEM
#if (OS_FAMILY == WINDOWS)

#include <limits.h>
#include <string.h>
#include "platform/signals.h"
#include "platform/window/window.h"
#include "platform/window/winkey_translate.h"
#include "platform/window/internal.h"
#include <windows.h>

// TODO: Check the 'text services framework' (TSF) for more
//   info/help.
// https://learn.microsoft.com/en-us/windows/win32/tsf/text-services-framework

static Allocator* wsa = NULL;
static HINSTANCE app_handle = 0;
static HMODULE gdi32_handle = NULL;
typedef int (WINAPI *StretchDIBitsFn)(HDC, int, int, int, int, int, int, int, int,
                                      const VOID*, const BITMAPINFO*, UINT, DWORD);
static StretchDIBitsFn stretch_dibits = NULL;
static WNDCLASS wind_class;
static const char* wind_class_name = "Relic Window Class";

static void draw_framebuffer(HDC dc, RECT client_area, FrameBuffer frame) {
    BITMAPINFO bitmap_info = {0};
    bitmap_info.bmiHeader.biSize = sizeof(bitmap_info.bmiHeader);
    bitmap_info.bmiHeader.biWidth = (LONG)frame.stride;
    bitmap_info.bmiHeader.biHeight = -(LONG)frame.height;
    bitmap_info.bmiHeader.biPlanes = 1;
    bitmap_info.bmiHeader.biBitCount = 32;
    bitmap_info.bmiHeader.biCompression = BI_RGB;

#ifdef DEBUG_ASSERT
    if (!stretch_dibits) {
        panic(mv_string("Window system was not initialized before presenting a framebuffer"));
    }
#endif
    stretch_dibits(dc,
                   0, 0,
                   client_area.right - client_area.left,
                   client_area.bottom - client_area.top,
                   0, 0,
                   (int)frame.width,
                   (int)frame.height,
                   frame.pixels,
                   &bitmap_info,
                   DIB_RGB_COLORS,
                   SRCCOPY);
}

LRESULT CALLBACK WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam) {
    PlWindow* window;
    if (uMsg == WM_NCCREATE) {
        CREATESTRUCT* pCreate = (CREATESTRUCT*)lParam;
        window = pCreate->lpCreateParams;
        SetWindowLongPtr(hwnd, GWLP_USERDATA, (LONG_PTR)window);

        RECT windowArea;
        GetClientRect(hwnd, &windowArea);
        *window = (PlWindow) {
            .should_close = false,
            .impl = hwnd,
            .messages = mk_wm_array(8, wsa),

            .width = windowArea.right - windowArea.left,
            .height = windowArea.bottom - windowArea.top,
        };

        WinMessage message = (WinMessage) {
            .type = KeymapChanged,
            .keymap = NULL,
        };
        push_wm(message, &window->messages);
    } else {
        window = (PlWindow*) GetWindowLongPtr(hwnd, GWLP_USERDATA);
    }

    switch (uMsg) {
    case WM_SIZE: {
        uint32_t width = LOWORD(lParam);
        uint32_t height = HIWORD(lParam);
        WinMessage message = (WinMessage) {
            .type = WindowResized,
            .dims.width = width,
            .dims.height = height,
        };
        window->width = width;
        window->height = height;
        push_wm(message, &window->messages);
        break;
    }
    case WM_SIZING: {
        RECT* winRect = (RECT*)lParam;
        uint32_t width = winRect->right - winRect->left;
        uint32_t height = winRect->bottom - winRect->top;
        WinMessage message = (WinMessage) {
            .type = WindowResized,
            .dims.width = height,
            .dims.height = height,
        };
        window->width = width;
        window->height = height;
        push_wm(message, &window->messages);
        break;
    }
    case WM_KEYDOWN:
    case WM_KEYUP: {
        RawKey key = keycode_to_rawkey(wParam, lParam);
        WinMessage message = (WinMessage){
            .type = KeyEvent,
            .key_event.key_id = key,
            .key_event.modifier_key_mask = 0,
            .key_event.key_pressed = uMsg == WM_KEYDOWN,
        };
        push_wm(message, &window->messages);
        break;
    }
    case WM_CLOSE:
        window->should_close = true;
        break;
    default:
        return DefWindowProc(hwnd, uMsg, wParam, lParam);
    }
    return 0;
}

int pl_init_window_system(Allocator* a) {
    wsa = a;
    app_handle = GetModuleHandle(NULL);
    gdi32_handle = LoadLibraryA("gdi32.dll");
    if (!gdi32_handle) {
        return 1;
    }
    FARPROC procedure = GetProcAddress(gdi32_handle, "StretchDIBits");
    memcpy(&stretch_dibits, &procedure, sizeof(stretch_dibits));
    if (!stretch_dibits) {
        FreeLibrary(gdi32_handle);
        gdi32_handle = NULL;
        return 1;
    }

    wind_class.lpfnWndProc = WindowProc;
    wind_class.hInstance = app_handle;
    wind_class.lpszClassName = wind_class_name;
    RegisterClass(&wind_class);
    return app_handle ? 0 : 1;
}

void pl_teardown_window_system() {
    if (gdi32_handle) {
        FreeLibrary(gdi32_handle);
        gdi32_handle = NULL;
        stretch_dibits = NULL;
    }
}

PlWindow* pl_create_window(String name, int width, int height) {
    PlWindow *win = mem_alloc(sizeof(PlWindow), wsa);
    char* c_name = to_c_string(name, wsa);
    HWND window = CreateWindowEx(0, // styles (optional)
                                 wind_class_name,
                                 c_name,
                                 WS_OVERLAPPEDWINDOW, // window style/type
                                 CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,  // position/size
                                 NULL, // parent
                                 NULL, // Menu
                                 app_handle,
                                 win // This will get passed into the WindowProc function as LPARAM
                                 );
    mem_free(c_name, wsa);
    if (window) {
        ShowWindow(window, SW_SHOWDEFAULT);
        return win;
    } else {
        return NULL;
    }
}

void pl_destroy_window(PlWindow *window) {
    sdelete_wm_array(window->messages);
    DestroyWindow(window->impl);
    mem_free(window, wsa);
}

bool pl_window_should_close(PlWindow *window) {
    return window->should_close;
}

WinMessageSlice pl_poll_events(PlWindow* window, Allocator* a) {
    MSG msg;
        while (PeekMessage(&msg, window->impl,  0, 0, PM_REMOVE))  {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }
    WinMessageArray out = scopy_wm_array(window->messages, a);
    window->messages.len = 0;
    return (WinMessageSlice){.data = out.data, .len = out.len};
}

FrameBufferOption acquire_framebuffer(PlWindow* window) {
    if (!window || window->width == 0 || window->height == 0 ||
        window->render.claimed_by_hedron || window->render.claimed_by_cpu) {
        return (FrameBufferOption){.type = None};
    }

    size_t width = window->width;
    size_t height = window->height;
    if (width > (size_t)-1 / sizeof(uint32_t) / height) {
        return (FrameBufferOption){.type = None};
    }

    size_t required_bytes = width * height * sizeof(uint32_t);
    if (window->render.memsize < required_bytes) {
        uint32_t* pixels = mem_realloc(window->render.pixels, required_bytes, wsa);
        if (!pixels) {
            return (FrameBufferOption){.type = None};
        }
        window->render.pixels = pixels;
        window->render.memsize = required_bytes;
    }

    window->render.claimed_by_cpu = true;
    return (FrameBufferOption) {
        .type = Some,
        .val = {
            .pixels = window->render.pixels,
            .memsize = required_bytes,
            .width = window->width,
            .height = window->height,
            .stride = window->width,
        },
    };
}

void present_framebuffer(FrameBuffer frame, PlWindow* window) {
    if (!window) {
        return;
    }

    if (!frame.pixels || frame.width == 0 || frame.height == 0 ||
        frame.stride < frame.width || frame.width > LONG_MAX ||
        frame.height > LONG_MAX || frame.stride > LONG_MAX ||
        frame.height > (size_t)-1 / sizeof(uint32_t) / frame.stride ||
        frame.memsize < (size_t)frame.stride * frame.height * sizeof(uint32_t)) {
        window->render.claimed_by_cpu = false;
        return;
    }

    HDC dc = GetDC(window->impl);
    if (dc) {
        RECT client_area;
        GetClientRect(window->impl, &client_area);
        draw_framebuffer(dc, client_area, frame);
        ReleaseDC(window->impl, dc);
    }
    window->render.claimed_by_cpu = false;
}

// Key handling: 
struct KeyboardState {
    uint8_t keys[256];
};

KeyboardState* create_keyboard_state(KeyMap *keymap) {
    KeyboardState* keystate = mem_alloc(sizeof(KeyboardState), wsa);
    for (size_t i = 0; i < 256; i++) {
        keystate->keys[i] = 0;
    }
    return keystate;
}

void destroy_keyboard_state(KeyboardState* state) {
    mem_free(state, wsa);
}

void update_keystate_key(RawKey raw, uint32_t modifier_mask, bool is_pressed, KeyboardState *state) {
    state->keys[raw] = is_pressed << 7;
}
void update_keystate_modifiers(uint32_t depressed, uint32_t latched, uint32_t locked, uint32_t group, KeyboardState *state) {
    // TODO: implement me!
}

Key get_key(RawKey raw, KeyboardState *state) {
    uint16_t unicode_out[1];
    int numchar = ToUnicode(rawkey_to_keycode(raw),
                            //[in]           UINT       wVirtKey,
                            0,
                            //[in]           UINT       wScanCode,
                            state->keys,
                            //[in, optional] const BYTE *lpKeyState,
                            unicode_out,
                            //[out]          LPWSTR     pwszBuff,
                            1,
                            //[in]           int        cchBuff,
                            0
                            //[in]           UINT       wFlags
                            );
    return translate_win_keycode(raw, unicode_out[0], numchar == 1);
}

#endif
#endif
