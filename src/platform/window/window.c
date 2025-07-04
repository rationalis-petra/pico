#include "data/meta/array_impl.h"
#include "platform/window/window.h"
#include "platform/window/internal.h"
#include "platform/machine_info.h"

ARRAY_COMMON_IMPL(WinMessage, wm, WinMessage)

#if OS_FAMILY == UNIX
#include <string.h> // for memset, TODO: remove?

#include "platform/window/xdg-shell-protocol.h"
#include <unistd.h>
#include <fcntl.h> // for O_ constants
#include <sys/mman.h>
#include <sys/stat.h>

// for more info, it may be good to read
// /usr/include/wayland-client-protocol.h
// /usr/include/wayland-client-core.h
#include <wayland-client.h>

static Allocator* wsa = NULL;

// The wayland display represents the "connection" between this application
// and the wayland display
static struct wl_display* wl_display;

// The wayland registry represents the "mailbox" that allows the server to send
// messages to this application
static struct wl_registry* wl_registry;

static struct wl_compositor* wl_compositor;

static struct wl_shm* wl_sharer;

static struct xdg_wm_base* xdg_shell; // ??

// A seat is a group of keyboards, pointer and touch devices. This
// object is published as a global during start up, or when such a
// device is hot plugged.
struct wl_seat* seat;

static struct wl_keyboard* kb;

int32_t alc_shm(uint64_t sz) {
    // TODO: make a unique string/name - this can be done by, e.g. using printf
    // on the window pointer
    const char* name = "wl_64-shared-memory";

    int32_t fd = shm_open(name, O_RDWR | O_CREAT | O_EXCL, S_IWUSR | S_IRUSR | S_IWOTH | S_IROTH);
    shm_unlink(name); // this name is no longer used??
    ftruncate(fd, sz);

    return fd;
}

// Resize:
void resize(Window* window) {
    const size_t memsize = window->width * window->height * 4;
    int32_t fd = alc_shm(memsize); // rgba

    window->pixles = mmap(0, memsize, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
    
    struct wl_shm_pool* pool = wl_shm_create_pool(wl_sharer, fd, memsize);
    window->buffer = wl_shm_pool_create_buffer(pool,0, window->width, window->height, window->width * 4, WL_SHM_FORMAT_ARGB8888);
    wl_shm_pool_destroy(pool);
    close(fd);
}

void draw(Window* window) {
    // colour = grep
    uint8_t colour = 128;
	memset(window->pixles, colour, window->width * window->height * 4);

    // 0, 0 = position
    wl_surface_attach(window->surface, window->buffer, 0, 0);

    // update a specific portion of the surface, starting at x, y, with width & height
    wl_surface_damage(window->surface, 0, 0, window->width, window->height); 

    // we are done rendering, we are done drawing, it's yours now
    wl_surface_commit(window->surface); 
}

// The xdg surface configure event is sent by wayland whenever the window's configuration
// 
void xdg_surface_conf(void *data, struct xdg_surface* xdg_surface, uint32_t serial) {
    Window* window = data;

    xdg_surface_ack_configure(xdg_surface, serial);
    // TODO: see if we can get the window passed in via data?
    if (!window->pixles) resize(window);
    //draw(window);
}

// callback
void shell_ping(void* data, struct xdg_wm_base* sh, uint32_t ser) {
	xdg_wm_base_pong(sh, ser);
}

// callback: toplevel window configuration changes This includes
// - When a window is resized  
// - When a window is minimised or maximised
// - ...
void xdg_toplevel_conf(void *data, struct xdg_toplevel *top, int32_t width, int32_t height, struct wl_array* states) {
    // TODO: check that a resize actually happened.
    // TODO: check width, height >= 0
    Window* window = data;
    if (window->width != width || window->height != height) {
        window->width = width;
        window->height = height;

        WinMessage message = (WinMessage) {
            .type = WindowResized,
            .dims.width = width,
            .dims.height = height,
        };

        window->width = width;
        window->height = height;

        push_wm(message, &window->messages);
    }
}

void xdg_toplevel_close(void *data, struct xdg_toplevel *top) {
    Window* win = data;
    win->should_close = true;
}


void kb_map(void* data, struct wl_keyboard* kb, uint32_t frmt, int32_t fd, uint32_t sz) {
	
}

void kb_enter(void* data, struct wl_keyboard* kb, uint32_t ser, struct wl_surface* srfc, struct wl_array* keys) {
	
}

void kb_leave(void* data, struct wl_keyboard* kb, uint32_t ser, struct wl_surface* srfc) {
	
}

void kb_key(void* data, struct wl_keyboard* kb, uint32_t ser, uint32_t t, uint32_t key, uint32_t stat) {
}

void kb_mod(void* data, struct wl_keyboard* kb, uint32_t ser, uint32_t dep, uint32_t lat, uint32_t lock, uint32_t grp) {
	
}

void kb_rep(void* data, struct wl_keyboard* kb, int32_t rate, int32_t del) {
	
}

struct wl_keyboard_listener kb_listener = {
	.keymap = kb_map,
	.enter = kb_enter,
	.leave = kb_leave,
	.key = kb_key,
	.modifiers = kb_mod,
	.repeat_info = kb_rep
};

void seat_capabilities(void* data, struct wl_seat* seat, uint32_t cap) {
	if (cap & WL_SEAT_CAPABILITY_KEYBOARD && !kb) {
		kb = wl_seat_get_keyboard(seat);
		wl_keyboard_add_listener(kb, &kb_listener, data);
	}
}

void seat_name(void* data, struct wl_seat* seat, const char* name) {
		
}

static struct xdg_surface_listener xdg_listener = (struct xdg_surface_listener) {
    .configure = xdg_surface_conf,
};

static struct xdg_toplevel_listener toplevel_listener = (struct xdg_toplevel_listener) {
    .configure = xdg_toplevel_conf,
    .close = xdg_toplevel_close,
};

struct xdg_wm_base_listener shell_listener = {
	.ping = shell_ping
};

struct wl_seat_listener seat_listener = {
	.capabilities = seat_capabilities,
    .name = seat_name,
};

void reg_glob_rem(void *data, struct wl_registry *reg, uint32_t name) {
    // TODO? remove 
}

// what globals we need (consistent between server and client)
void reg_glob(void *data, struct wl_registry *reg, uint32_t name, const char *intf, uint32_t v) {
    String resource_name = mv_string(intf);

    // We need the compositor, which is what we grab surfaces (windows) from:
    if (string_cmp(resource_name, mv_string(wl_compositor_interface.name)) == 0) {
        wl_compositor = wl_registry_bind(reg, name, &wl_compositor_interface, 4); // 4 = version 4
    } else if (string_cmp(resource_name, mv_string(wl_shm_interface.name)) == 0) {
		wl_sharer = wl_registry_bind(reg, name, &wl_shm_interface, 1);
    } else if (string_cmp(resource_name, mv_string(xdg_wm_base_interface.name)) == 0) {
		xdg_shell = wl_registry_bind(reg, name, &xdg_wm_base_interface, 1);
		xdg_wm_base_add_listener(xdg_shell, &shell_listener, 0);
	} else if (!strcmp(intf, wl_seat_interface.name)) {
		seat = wl_registry_bind(reg, name, &wl_seat_interface, 1);
		wl_seat_add_listener(seat, &seat_listener, 0);
	}
}

static struct wl_registry_listener wl_listener = (struct wl_registry_listener) {
    .global = reg_glob,
    .global_remove = reg_glob_rem,
};

struct wl_display *get_wl_display() {
    return wl_display;
}

int init_window_system(Allocator* a) {
    wsa = a;
    wl_display = wl_display_connect(NULL);
    if (!wl_display) return 1;
    wl_registry = wl_display_get_registry(wl_display);
    wl_registry_add_listener(wl_registry, &wl_listener, 0);

    wl_display_roundtrip(wl_display); // tell the server that we are looking for data
    return 0;
}

void teardown_window_system() {
    wl_display_disconnect(wl_display);
}

Window *create_window(String name, int width, int height) {
    Window* window = mem_alloc(sizeof(Window), wsa);
    *window = (Window) {
        .width = width,
        .height = height,
        .name = name,
        .should_close = false,
        .messages = mk_wm_array(8, wsa),
    };

    struct wl_surface* surface = wl_compositor_create_surface(wl_compositor);
    window->surface = surface;
    // TODO (BUG) : check to ensure surface was created

    struct xdg_surface* xsurface = xdg_wm_base_get_xdg_surface(xdg_shell, surface);
    window->xdg_surface = xsurface;
    xdg_surface_add_listener(xsurface, &xdg_listener, window);

    struct xdg_toplevel* top = xdg_surface_get_toplevel(xsurface);
    window->toplevel = top;
    xdg_toplevel_add_listener(top, &toplevel_listener, window);

    // TODO (INVESTIGATE BUG): check the encoding used by wayland - if not
    // UTF-8, convert!
    xdg_toplevel_set_title(top, (char*)name.bytes);

    wl_surface_commit(surface); // TOOD: necessary?

    return window;
}

void destroy_window(Window *window) {
    if (window->buffer) {
        wl_buffer_destroy(window->buffer);
    }
    if (window->pixles) {
        munmap(window->pixles, 4 * window->width * window->height);
    }
    // TODO: we may be leaking the shared memory in window->pixles!

    xdg_toplevel_destroy(window->toplevel);
    xdg_surface_destroy(window->xdg_surface);
    wl_surface_destroy(window->surface);

    mem_free(window, wsa);

    // TOOD: check if we want to keep this here? possibly we need to wait on a
    //       sync message from the compositor.
    wl_display_dispatch(wl_display);
}

bool window_should_close(Window *window) {
    return window->should_close;
}

WinMessageArray poll_events(Window* window, Allocator* a) {
    wl_display_dispatch_pending(wl_display);
    WinMessageArray out = scopy_wm_array(window->messages, a);
    window->messages.len = 0;
    return out;
}

#elif OS_FAMILY == WINDOWS

#include <windows.h>

static Allocator* wsa = NULL;
static HINSTANCE app_handle = 0;
static WNDCLASS wind_class;
static const char* wind_class_name = "Relic Window Class";


LRESULT CALLBACK WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam) {
    Window* window;
    if (uMsg == WM_NCCREATE) {
        CREATESTRUCT* pCreate = (CREATESTRUCT*)lParam;
        window = pCreate->lpCreateParams;
        SetWindowLongPtr(hwnd, GWLP_USERDATA, (LONG_PTR)window);

        RECT windowArea;
        GetClientRect(hwnd, &windowArea);
        window->width = windowArea.right - windowArea.left;
        window->height = windowArea.bottom - windowArea.top;

    } else {
        window = (Window*) GetWindowLongPtr(hwnd, GWLP_USERDATA);
    }

    switch (uMsg) {
    case WM_SIZE: {
        uint32_t width = LOWORD(lParam);
        uint32_t height = HIWORD(lParam);
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
    case WM_CLOSE:
        window->should_close = true;
        break;
    default:
        return DefWindowProc(hwnd, uMsg, wParam, lParam);
    }
    return 0;
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
    Window *win = mem_alloc(sizeof(Window), wsa);
    HWND window = CreateWindowEx(0, // styles (optional)
                                 wind_class_name,
                                 name.bytes,
                                 WS_OVERLAPPEDWINDOW, // window style/type
                                 CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,  // position/size
                                 NULL, // parent
                                 NULL, // Menu
                                 app_handle,
                                 win // This will get passed into the WindowProc function as LPARAM
                                );
    if (window) {
        ShowWindow(window, SW_SHOWDEFAULT);
        *win = (Window){
            .should_close = false,
            .impl = window,
        };
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
    return window->should_close;
}

WinMessageArray poll_events(Window* window, Allocator* a) {
    MSG msg;
        while (PeekMessage(&msg, window->impl,  0, 0, PM_REMOVE))  {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }
    WinMessageArray out = scopy_wm_array(window->messages, a);
    window->messages.len = 0;
    return out;
}

#else
#error "Unrecognized OS"
#endif
