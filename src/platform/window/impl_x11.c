#include "platform/machine_info.h"

#if (OS_FAMILY == UNIX) && (WINDOW_SYSTEM == 1)

// TODO: investigate whehter to use XCB over xlib?

#include "platform/window/window.h"
#include "platform/window/internal.h"
#include "platform/signals.h"

#include "data/stringify.h"

#include <X11/Xlib.h>
#include <unistd.h>

static Allocator* wsa;

static Display* x11_display = NULL;

Atom WM_DELETE_WINDOW;

// Internal
// ------------

Display* get_x11_display() {
    return x11_display;
}

// External
// ------------

int pl_init_window_system(Allocator* a) {
    wsa = a;
    x11_display = XOpenDisplay(NULL);

    if (x11_display == NULL) {
        return 1;
    }

	WM_DELETE_WINDOW = XInternAtom(x11_display, "WM_DELETE_WINDOW", False);
    return 0;
}

void pl_teardown_window_system() {
    XCloseDisplay(x11_display);
}

PlWindow* pl_create_window(String name, int width, int height) {
    PlWindow* win = mem_alloc(sizeof(PlWindow), wsa);
    if (win == NULL) return win;

    int blackColour = BlackPixel(x11_display, DefaultScreen(x11_display));


    Window xwin = XCreateSimpleWindow(x11_display, DefaultRootWindow(x11_display), 0, 0, width, height, 0, blackColour, blackColour);
	XSetWMProtocols(x11_display, xwin, &WM_DELETE_WINDOW, 1);

    XMapWindow(x11_display, xwin);
    XSelectInput(x11_display, xwin, KeyPressMask | ButtonPressMask | StructureNotifyMask);
    XMapRaised(x11_display, xwin);
    XFlush(x11_display);

    *win = (PlWindow) {
        .x11_window = xwin,
        .width = width,
        .height = height,
        .should_close = false,
    };
    
    return win;
}

void pl_destroy_window(PlWindow *window) {
   XUnmapWindow(x11_display, window->x11_window);
   XDestroyWindow(x11_display, window->x11_window);
   XFlush(x11_display);
   mem_free(window, wsa);
}

bool pl_window_should_close(PlWindow *window) {
    return window->should_close;
}

#include <stdio.h>
WinMessageArray pl_poll_events(PlWindow* window, Allocator* a) {
    WinMessageArray out = mk_wm_array(8, a);
    XEvent event;
    while (XCheckWindowEvent(x11_display, window->x11_window, ButtonPressMask | StructureNotifyMask, &event)) {
        switch (event.type) {
        case DestroyNotify:
        case ButtonPress:
            window->should_close = true;
            break;
        case ConfigureNotify: {
            uint32_t width = event.xconfigure.width;
            uint32_t height = event.xconfigure.height;
            if (window->height != height || window->width != width) {
                WinMessage message = (WinMessage) {
                    .type = WindowResized,
                    .dims.width = width,
                    .dims.height = height,
                };
                push_wm(message, &out);
            }
            break;
        }

        // The following events are ones that we have determined can be safely
        // ignored, i.e. they do not need to be acted on AND are not exposed in
        // Relic's windowing library
        case ReparentNotify:
        case MapNotify:
            break;
        default: {
            String message = string_cat(mv_string("unknown X11 event received with type: "), string_int(event.type, a), a);
            panic(message);
        }
        }
    }

    while (XPending(x11_display) > 0) {
        XEvent event;
        XNextEvent(x11_display, &event);

        // TODO: for multiple windows, need to associate X window with platform window  
        //   pointer, which we retrieve with the call below:

        /*
        PlWindow* pl_window;
        if (XFindContext(x11_display,
                         event.xany.window,
                         x11.context,
                         (XPointer*) &pl_window) != 0) {
        // This is an event for a window that has already been destroyed
            return;
        }
        */

        switch (event.type) {
        case ClientMessage: {
            if ((Atom)event.xclient.data.l[0] == WM_DELETE_WINDOW) {
                window->should_close = 1;
            }
            break;
        }
        default: {
            printf("unprocessed event with type: %d\n", event.type);
        }
        }
    }
    return out;
}

#endif
