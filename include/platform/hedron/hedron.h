#ifndef __PLATFORM_HEDRON_HEDRON_H
#define __PLATFORM_HEDRON_HEDRON_H

#include "data/array.h"
#include "platform/memory/allocator.h"
#include <stdbool.h>

// forward-declaration of window (platform/window/window.h)
struct Window;

typedef struct {
    uint32_t width;
    uint32_t height;
} Extent;

// -------------------------------------------
//
//   Context: Instances, Devices, Windows
// 
// -------------------------------------------

typedef struct HedronSurface HedronSurface;
typedef struct HedronShaderModule HedronShaderModule;
typedef struct HedronPipeline HedronPipeline;

// Global Utility - supported, setup & teardown
bool is_hedron_supported();

int init_hedron(Allocator* a);
void teardown_hedron();

// Window System Interaction Surfaces 
HedronSurface* create_window_surface(struct Window* window);
void resize_window_surface(HedronSurface* surface, Extent extent);
void destroy_window_surface(HedronSurface*);

uint32_t num_swapchain_images(HedronSurface*);

HedronShaderModule* create_shader_module(U8Array code);
void destroy_shader_module(HedronShaderModule* module);

typedef enum : uint64_t {Vertex, Instance} InputRate;
typedef enum : uint64_t {Float_1, Float_2, Float_3} VertexFormat;

typedef struct {
    uint32_t binding;
    uint32_t stride;
    InputRate input_rate; 
} BindingDescription;

typedef struct {
    uint32_t binding;
    uint32_t location;
    VertexFormat format;
    uint32_t offset; 
} AttributeDescription;

ARRAY_HEADER_TYPE(BindingDescription, BindingDescription)
ARRAY_HEADER_TYPE(AttributeDescription, AttributeDescription)

HedronPipeline* create_pipeline(BindingDescriptionArray bdesc, AttributeDescriptionArray adesc, PtrArray shaders, HedronSurface* surface);
void destroy_pipeline(HedronPipeline* pipeline);

// -------------------------------------------
//
// Data contract (vertex/input formats, etc.)
// 
// -------------------------------------------

typedef struct HedronBuffer HedronBuffer;

HedronBuffer* create_buffer(uint64_t size);
void destroy_buffer(HedronBuffer* buffer);

void set_buffer_data(HedronBuffer* buffer, void* data);

// -----------------------------
// 
//     Synchronisation
//
// ----------------------------

typedef struct HedronSemaphore HedronSemaphore;
typedef struct HedronFence HedronFence;

HedronSemaphore* create_semaphore(); 
void destroy_semaphore(HedronSemaphore* semaphore); 

HedronFence* create_fence(); 
void destroy_fence(HedronFence* fence); 
void wait_for_fence(HedronFence* fence);
void reset_fence(HedronFence* fence);

void wait_for_device();

typedef enum : uint64_t {IROk, Resized} ImageResultType;
typedef struct {
    ImageResultType type;
    uint32_t image;
} ImageResult;
ImageResult acquire_next_image(HedronSurface* surface, HedronSemaphore* semaphore);

// -------------------------------------------
//
//        Commands, Queues and Drawing
// 
// -------------------------------------------

typedef struct HedronCommandPool HedronCommandPool;
typedef struct HedronCommandBuffer HedronCommandBuffer;


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
void command_end_render_pass(HedronCommandBuffer* commands);

void command_bind_pipeline(HedronCommandBuffer* commands, HedronPipeline* pipeline);
void command_bind_buffer(HedronCommandBuffer* commands, HedronBuffer* buffer);

void command_set_surface(HedronCommandBuffer *commands, HedronSurface *surface);
void command_draw(HedronCommandBuffer *commands, uint32_t vertex_count,
                  uint32_t instance_cont, uint32_t first_vertex,
                  uint32_t first_instance);


#endif
