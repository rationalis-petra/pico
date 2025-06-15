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

void desetroy_window(Window *window) {
    GLFWwindow* inner = window->window_impl;
    mem_free(window, wsa);
    glfwDestroyWindow(inner);
}

bool window_should_close(Window *window) {
    return glfwWindowShouldClose(window->window_impl);
}

#elif OS_FAMILY == WINDOWS

#include <windows.h>

struct Window {
    HINSTANCE window_handle;
};

int init_window_system(Allocator* a) {
    return 0; // Dummy method, 'success'
}

void teardown_window_system() {
    // Dummy method
}

#endif
