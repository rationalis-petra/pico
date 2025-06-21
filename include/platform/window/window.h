#ifndef __PLATFORM_WINDOW_WINDOW_H
#define __PLATFORM_WINDOW_WINDOW_H

#include "data/string.h"

typedef struct Window Window;

// Initialize the window system
// returns zero on success, exit code on failure
int init_window_system(Allocator* a);
void teardown_window_system();

Window* create_window(String name, int width, int height);
void destroy_window(Window* window);

bool window_should_close(Window* window);

void poll_events();

#endif
