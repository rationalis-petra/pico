#include "platform/machine_info.h"
#include "platform/window/window.h"
#include "data/string.h"

#ifndef WINDOW_SYSTEM
// Not using a window system!
struct PlWindow {};

#elif (OS_FAMILY == UNIX) && (WINDOW_SYSTEM == 1)
#include <X11/Xlib.h>

Display* get_x11_display();

struct PlWindow {
    Window x11_window;

    uint32_t width;
    uint32_t height;

    // Internal state (used by us!)
    bool should_close;
};

#elif (OS_FAMILY == UNIX) && (WINDOW_SYSTEM == 2)
#include <wayland-client.h>

struct wl_display* get_wl_display();

struct PlWindow {
    struct wl_surface* surface; // Window surface (from compositor)
    struct wl_buffer* buffer; // control/access to shared memory buffer
    struct xdg_toplevel* toplevel; // This represents the window, and allows us
                                   // to do things like interact with header
                                   // bars (if they exist) etc.
    struct xdg_surface* xdg_surface; // ??
    uint8_t* pixles; // pointer to the shared memory (pixels)

    String name;
    uint32_t width;
    uint32_t height;

    // Internal state (used by us!)
    bool should_close;
    WinMessageArray messages;
};

#elif OS_FAMILY == WINDOWS

#include <windows.h>

struct PlWindow {
    HWND impl;
    bool should_close;
    WinMessageArray messages;

    uint32_t width;
    uint32_t height;
};

#else
#error "Unrecognized OS"
#endif
