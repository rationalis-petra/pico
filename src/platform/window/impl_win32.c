#include "platform/machine_info.h"

#ifdef WINDOW_SYSTEM
#if (OS_FAMILY == WINDOWS)

#include "platform/window/window.h"
#include "platform/window/winkey_translate.h"
#include "platform/window/internal.h"
#include <windows.h>

// TODO: Check the 'text services framework' (TSF) for more
//   info/help.
// https://learn.microsoft.com/en-us/windows/win32/tsf/text-services-framework

static Allocator* wsa = NULL;
static HINSTANCE app_handle = 0;
static WNDCLASS wind_class;
static const char* wind_class_name = "Relic Window Class";

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
        Key key;
        if (translate_key_id(&key, wParam)) {
            WinMessage message = (WinMessage) {
                .type = KeyEvent,
                .key_event.key_id = key,
                .key_event.modifier_key_mask = 0,
                .key_event.key_pressed = uMsg == WM_KEYDOWN,
            };
            push_wm(message, &window->messages);
        }
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

    wind_class.lpfnWndProc = WindowProc;
    wind_class.hInstance = app_handle;
    wind_class.lpszClassName = wind_class_name;
    RegisterClass(&wind_class);
    return app_handle ? 0 : 1;
}

void pl_teardown_window_system() {
    // Dummy method
}

PlWindow* pl_create_window(String name, int width, int height) {
    PlWindow *win = mem_alloc(sizeof(PlWindow), wsa);
    HWND window = CreateWindowEx(0, // styles (optional)
                                 wind_class_name,
                                 (char*)name.bytes,
                                 WS_OVERLAPPEDWINDOW, // window style/type
                                 CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,  // position/size
                                 NULL, // parent
                                 NULL, // Menu
                                 app_handle,
                                 win // This will get passed into the WindowProc function as LPARAM
                                );
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

WinMessageArray pl_poll_events(PlWindow* window, Allocator* a) {
    MSG msg;
        while (PeekMessage(&msg, window->impl,  0, 0, PM_REMOVE))  {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }
    WinMessageArray out = scopy_wm_array(window->messages, a);
    window->messages.len = 0;
    return out;
}

// Key handling: 
struct KeyboardState {
    uint8_t[256] keys;
}

KeyboardState* create_keystate(KeyMap *keymap) {
    KeyboardState* keystate = mem_alloc(sizeof(KeyboardState), wsa);
    for (size_t i = 0; i < 256; i++) {
        keys[i] = 0;
    }
    return keys;
}

void destroy_keystate(KeyboardState* state) {
    mem_free(state, wsa);
}

void update_keystate_key(RawKey raw, uint32_t modifier_mask, bool is_pressed, KeyState *state) {
    WPARAM keycode = rawkey_to_keycode(RawKey raw);
    keystate[keycode] = is_pressed << 7;
}
void update_keystate_modifiers(uint32_t depressed, uint32_t latched, uint32_t locked, uint32_t group, KeyState *state) {
    // TODO: implement me!
}

Key get_key(RawKey raw, KeyState *state) {
    uint16_t unicode_out[1];
    int numchar = ToUnicode(rawkey_to_keycode(RawKey rawkey),
                            //[in]           UINT       wVirtKey,
                            0,
                            //[in]           UINT       wScanCode,
                            state,
                            //[in, optional] const BYTE *lpKeyState,
                            unicode_out
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
