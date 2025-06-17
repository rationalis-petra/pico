#ifndef __PLATFORM_HEDRON_HEDRON_H
#define __PLATFORM_HEDRON_HEDRON_H

#include "platform/memory/allocator.h"
#include <stdbool.h>

// forward-declaration of window (platform/window/window.h)
struct Window;

typedef struct HedronSurface HedronSurface;

bool is_hedron_supported();
HedronSurface* create_window_surface(struct Window* window);
void destroy_window_surface(HedronSurface*);

int init_hedron(Allocator* a);
void teardown_hedron();

#endif
