#include "platform/machine_info.h"
#include "data/string.h"

#if OS_FAMILY == UNIX
#include <wayland-client.h>

// The wayland display represents the "connection" between this application
// and the wayland display
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
    uint16_t width;
    uint16_t height;

    // Internal state (used by us!)
    bool should_close;
};

#elif OS_FAMILY == WINDOWS

struct Window {
    HWND impl;
    bool should_close;
};

#include <windows.h>

#else
#error "Unrecognized OS"
#endif
