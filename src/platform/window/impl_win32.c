#include "platform/machine_info.h"

#ifdef WINDOW_SYSTEM
#if (OS_FAMILY == WINDOWS)

#include "platform/window/window.h"
#include "platform/window/internal.h"
#include <windows.h>

// TODO: Check the 'text services framework' (TSF) for more
//   info/help.
// https://learn.microsoft.com/en-us/windows/win32/tsf/text-services-framework

static Allocator* wsa = NULL;
static HINSTANCE app_handle = 0;
static WNDCLASS wind_class;
static const char* wind_class_name = "Relic Window Class";

bool translate_key_id(Key* key, WPARAM windows_keycode) {
    switch (windows_keycode) {
    case VK_BACK:
        *key = WKEY_BACKSPACE;
        break;
    case VK_RETURN:
        *key = WKEY_ENTER;
        break;
    case VK_SPACE:
        *key = WKEY_SPACE;
        break;
    case 0x30:
        *key = WKEY_0;
        break;
    case 0x31:
        *key = WKEY_1;
        break;
    case 0x32:
        *key = WKEY_2;
        break;
    case 0x33:
        *key = WKEY_3;
        break;
    case 0x34:
        *key = WKEY_4;
        break;
    case 0x35:
        *key = WKEY_5;
        break;
    case 0x36:
        *key = WKEY_6;
        break;
    case 0x37:
        *key = WKEY_7;
        break;
    case 0x38:
        *key = WKEY_8;
        break;
    case 0x39:
        *key = WKEY_9;
        break;

    case 0x41:
        *key = WKEY_A;
        break;
    case 0x42:
        *key = WKEY_B;
        break;
    case 0x43:
        *key = WKEY_C;
        break;
    case 0x44:
        *key = WKEY_D;
        break;
    case 0x45:
        *key = WKEY_E;
        break;
    case 0x46:
        *key = WKEY_F;
        break;
    case 0x47:
        *key = WKEY_G;
        break;
    case 0x48:
        *key = WKEY_H;
        break;
    case 0x49:
        *key = WKEY_I;
        break;
    case 0x4A:
        *key = WKEY_J;
        break;
    case 0x4B:
        *key = WKEY_K;
        break;
    case 0x4C:
        *key = WKEY_L;
        break;
    case 0x4D:
        *key = WKEY_M;
        break;
    case 0x4E:
        *key = WKEY_N;
        break;
    case 0x4F:
        *key = WKEY_O;
        break;
    case 0x50:
        *key = WKEY_P;
        break;
    case 0x51:
        *key = WKEY_Q;
        break;
    case 0x52:
        *key = WKEY_R;
        break;
    case 0x53:
        *key = WKEY_S;
        break;
    case 0x54:
        *key = WKEY_T;
        break;
    case 0x55:
        *key = WKEY_U;
        break;
    case 0x56:
        *key = WKEY_V;
        break;
    case 0x57:
        *key = WKEY_W;
        break;
    case 0x58:
        *key = WKEY_X;
        break;
    case 0x59:
        *key = WKEY_Y;
        break;
    case 0x6A:
        *key = WKEY_Z;
        break;

    case VK_OEM_MINUS:
        *key = WKEY_MINUS;
        break;
    case VK_OEM_PLUS:
        *key = WKEY_PLUS;
        break;

    default:
        return false;
    }
    return true;
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

#endif
#endif
