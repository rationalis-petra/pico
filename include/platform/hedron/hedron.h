#ifndef __PLATFORM_HEDRON_HEDRON_H
#define __PLATFORM_HEDRON_HEDRON_H

#include "data/array.h"
#include "platform/memory/allocator.h"
#include <stdbool.h>

// forward-declaration of window (platform/window/window.h)
struct Window;

typedef struct HedronSurface HedronSurface;
typedef struct HedronShaderModule HedronShaderModule;
typedef struct HedronPipeline HedronPipeline;

// Global Utility - supported, setup & teardown
bool is_hedron_supported();

int init_hedron(Allocator* a);
void teardown_hedron();

// Window System Interaction Surfaces 
HedronSurface* create_window_surface(struct Window* window);
void destroy_window_surface(HedronSurface*);

HedronShaderModule* create_shader_module(U8Array code);
void destroy_shader_module(HedronShaderModule* module);

HedronPipeline* create_pipeline(PtrArray shaders, HedronSurface* surface);
void destroy_pipeline(HedronPipeline* pipeline);

#endif
