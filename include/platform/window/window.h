#ifndef __PLATFORM_WINDOW_WINDOW_H
#define __PLATFORM_WINDOW_WINDOW_H

#include "data/meta/array_header.h"
#include "data/string.h"

typedef struct PlWindow PlWindow;

// Initialize the window system
// returns zero on success, exit code on failure
int pl_init_window_system(Allocator* a);
void pl_teardown_window_system();

PlWindow* pl_create_window(String name, int width, int height);
void pl_destroy_window(PlWindow* window);

bool pl_window_should_close(PlWindow* window);


typedef enum : uint64_t {
    WindowResized,

    MouseMoved,
    MouseLButtonDown,
    MouseRButtonDown,
    MouseMButtonDown,
    MouseLButtonUp,
    MouseRButtonUp,
    MouseMButtonUp,

} MessageType;

typedef struct {
    int64_t xpos;
    int64_t ypos;
} MousePos;

typedef struct {
    uint32_t width;
    uint32_t height;
} WindowDimensions;

typedef struct {
    MessageType type;
    union {
        WindowDimensions dims;
    };
} WinMessage;

ARRAY_HEADER(WinMessage, wm, WinMessage);

WinMessageArray pl_poll_events(PlWindow* window, Allocator* a);

#endif
