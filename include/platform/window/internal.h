#include "platform/machine_info.h"
#include "platform/window/window.h"
#include "data/string.h"

#if OS_FAMILY == UNIX
#include <wayland-client.h>

struct wl_display* get_wl_display();

struct Window {
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

struct Window {
    HWND impl;
    bool should_close;
    WinMessageArray messages;

    uint32_t width;
    uint32_t height;
};

#else
#error "Unrecognized OS"
#endif
