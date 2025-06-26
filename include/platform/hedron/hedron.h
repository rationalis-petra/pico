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
typedef struct HedronCommandPool HedronCommandPool;
typedef struct HedronCommandBuffer HedronCommandBuffer;

typedef struct HedronSemaphore HedronSemaphore;
typedef struct HedronFence HedronFence;

// Global Utility - supported, setup & teardown
bool is_hedron_supported();

int init_hedron(Allocator* a);
void teardown_hedron();

// Window System Interaction Surfaces 
HedronSurface* create_window_surface(struct Window* window);
void destroy_window_surface(HedronSurface*);

uint32_t num_swapchain_images(HedronSurface*);

HedronShaderModule* create_shader_module(U8Array code);
void destroy_shader_module(HedronShaderModule* module);

HedronPipeline* create_pipeline(PtrArray shaders, HedronSurface* surface);
void destroy_pipeline(HedronPipeline* pipeline);

HedronCommandPool* create_command_pool();
void destroy_command_pool();

HedronCommandBuffer* create_command_buffer(HedronCommandPool* pool);
//void destroy_command_buffer(); 

// Command buffer usage
void queue_submit(HedronCommandBuffer *buffer, HedronFence *fence, HedronSemaphore* wait, HedronSemaphore* signal);
void queue_present(HedronSurface* surface, HedronSemaphore* wait, uint32_t image_index);

void command_begin(HedronCommandBuffer* buffer);
void command_end(HedronCommandBuffer* buffer);

void reset_command_buffer(HedronCommandBuffer* buffer);

void command_begin_render_pass(HedronCommandBuffer* buffer, HedronSurface* surface, uint32_t image_index);
void command_end_render_pass(HedronCommandBuffer* buffer);

void command_bind_pipeline(HedronCommandBuffer* buffer, HedronPipeline* pipeline);
void command_set_surface(HedronCommandBuffer *buffer, HedronSurface *surface);
void command_draw(HedronCommandBuffer *buffer, uint32_t vertex_count,
                  uint32_t instance_cont, uint32_t first_vertex,
                  uint32_t first_instance);

// Syncrhonisation
HedronSemaphore* create_semaphore(); 
void destroy_semaphore(HedronSemaphore* semaphore); 

HedronFence* create_fence(); 
void destroy_fence(HedronFence* fence); 
void wait_for_fence(HedronFence* fence);
void reset_fence(HedronFence* fence);

void wait_for_device();

uint32_t acquire_next_image(HedronSurface* surface, HedronSemaphore* semaphore);

#endif
