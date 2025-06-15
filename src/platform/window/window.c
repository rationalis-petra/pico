#include "platform/window/window.h"
#include "platform/machine_info.h"

#if OS_FAMILY == UNIX

#include <GLFW/glfw3.h>

struct Window {
    GLFWwindow* window_impl;
};

static Allocator* wsa = NULL;

int init_window_system(Allocator* a) {
    wsa = a;
    return !glfwInit();
}

void teardown_window_system() {
    glfwTerminate();
}

Window *create_window(String name, int width, int height) {
    GLFWwindow* iwin = glfwCreateWindow(width, height, (const char*)name.bytes, NULL, NULL);
    if (iwin) {
        Window* win = mem_alloc(sizeof(Window), wsa);
        *win = (Window){.window_impl = iwin};
        return win;
    } else {
        return NULL;
    }
}

void destroy_window(Window *window) {
    GLFWwindow* inner = window->window_impl;
    mem_free(window, wsa);
    glfwDestroyWindow(inner);
}

bool window_should_close(Window *window) {
    return glfwWindowShouldClose(window->window_impl);
}

#elif OS_FAMILY == WINDOWS

#include <windows.h>

static Allocator* wsa = NULL;
static HINSTANCE app_handle = 0;
static WNDCLASS wind_class;
static const char* wind_class_name = "Relic Window Class";

struct Window {
    HWND impl;
};

LRESULT CALLBACK WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
    if (uMsg == WM_CLOSE) {
        PostQuitMessage(0);
        return 0;
    } else {
        return DefWindowProc(hwnd, uMsg, wParam, lParam);
    }
}

int init_window_system(Allocator* a) {
    wsa = a;
    app_handle = GetModuleHandle(NULL);

    wind_class.lpfnWndProc = WindowProc;
    wind_class.hInstance = app_handle;
    wind_class.lpszClassName = wind_class_name;
    RegisterClass(&wind_class);
    return app_handle ? 0 : 1;
}

void teardown_window_system() {
    // Dummy method
}

Window *create_window(String name, int width, int height) {
    HWND window = CreateWindowEx(0, // styles (optional)
                                 wind_class_name,
                                 name.bytes,
                                 WS_OVERLAPPEDWINDOW, // window style/type
                                 CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,  // position/size
                                 NULL, // parent
                                 NULL, // Menu
                                 app_handle,
                                 NULL // Additional app data
                                );
    if (window) {
        ShowWindow(window, SW_SHOWDEFAULT);
        Window* win = mem_alloc(sizeof(Window), wsa);
        *win = (Window){.impl = window};
        return win;
    } else {
        return NULL;
    }
}

void destroy_window(Window *window) {
    HWND impl = window->impl;
    mem_free(window, wsa);
    DestroyWindow(impl);
}

bool window_should_close(Window *window) {
    MSG msg;
    return (GetMessage(&msg, NULL, 0, 0) > 0);
}

#endif
