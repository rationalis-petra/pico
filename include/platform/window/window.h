#ifndef __PLATFORM_WINDOW_WINDOW_H
#define __PLATFORM_WINDOW_WINDOW_H

#include "data/meta/array_header.h"
#include "data/string.h"

typedef struct Window Window;

// Initialize the window system
// returns zero on success, exit code on failure
int init_window_system(Allocator* a);
void teardown_window_system();

Window* create_window(String name, int width, int height);
void destroy_window(Window* window);

bool window_should_close(Window* window);


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

WinMessageArray poll_events(Window* window, Allocator* a);

#endif
