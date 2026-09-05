#include "data/meta/array_header.h"
#include "platform/machine_info.h"

#ifndef WINDOW_SYSTEM
#define NO_PLATFORM_AVAILABLE
#elif (OS_FAMILY == UNIX) && (WINDOW_SYSTEM == 1)
#define VK_USE_PLATFORM_XLIB_KHR
#elif (OS_FAMILY == UNIX) && (WINDOW_SYSTEM == 2)
#define VK_USE_PLATFORM_WAYLAND_KHR
#elif OS_FAMILY == WINDOWS
#define VK_USE_PLATFORM_WIN32_KHR
#else 
#error "unrecognized OS"
#endif


#include <vulkan/vulkan.h>

#include "data/meta/array_header.h"
#include "data/meta/amap_header.h"

#include "platform/hedron/hedron.h"
#include "platform/window/internal.h"

HdError convert_error_type(VkResult desc);
#define CHECK_RESULT(result) {if (result != VK_SUCCESS) { return (HdPtrResult) { .type = Err, .error = convert_error_type(result),};}};

// Instance & Devices
struct HdInstance {
    VkInstance vk_instance;
    Allocator* gpa;

    size_t num_devices;
    HdPhysicalDevice* devices;
};

struct HdPhysicalDevice {
    VkPhysicalDevice device;
};

struct HdQueue {
    VkQueue queue;
    uint32_t queue_family;
    HdLogicalDevice* device;
};

struct HdCommandBuffer {
    HdQueue* queue;
    VkCommandBuffer buffer;
    // TODO: investigate the following
    // Each command buffer owns its' own pool so recording needs no
    // cross-thread synchronization
    VkCommandPool pool;
    HdLogicalDevice* device;
    HdPipeline* current_pipeline;
};

typedef struct {
    HdCommandBuffer* commands;
    uint64_t value;
} PendingBuffer;

ARRAY_HEADER(PendingBuffer, pbuf, PendingBuffer);
AMAP_HEADER(HdSemaphore*, PendingBufferArray, sem_bufs, SemBufs);

typedef struct HdAllocation HdAllocation;
ARRAY_HEADER(HdAllocation, hdalloc, HdAllocation)

struct HdLogicalDevice {
    VkDevice device;
    VkPhysicalDevice physical_device;
    Allocator* gpa; // Allocator is accessed often, so keep it high up.

    // The api exposes a sort of 'automatic' command buffer management: you
    // simply request a new command buffer from a queue, then submit a command
    // buffer (at which point it is considered done/discarded)
    //
    // Internally, we maintain an array of usable command buffers (returned when
    // asking for a new one).
    // Once a command buffers has been submitted, we cannot reuse that buffer
    // until it is not completed. We thus command buffers that are waiting to
    // complete. These pending buffers are stored in a map based off of the
    // semaphore they will signal, so when we are signalled by a semaphore, we
    // can pop off all buffers that are attached to it.
    PtrArray usable_buffers;
    SemBufsAMap pending_buffers;

    // We keep track of allocations in the device because we only provide
    // device/host addresses via the API, but vulkan requires that we keep track
    // of more state.
    // TODO: Use a mutex to lock the allocations, ensuring that num_allocations
    //       + allocations stay coherent when hammered by multiple threads.
    HdAllocationArray allocations;

    HdQueue queue;

    // Every graphics/compute pipeline shares the same layout, so instead of
    // wasting resources (and memory) recreating the pipeline layout every time,
    // they are cached here: 
    VkPipelineLayout compute_pipeline_layout;
    VkPipelineLayout graphics_pipeline_layout;
    VkDescriptorSetLayout descriptor_set_layout; // part of pipeline, needs cleanup
    uint64_t max_sampled_images; // TODO: check datatype (uint32_t?)
};

// called during device creation
void initialize_pipeline_layouts(HdLogicalDevice* device);
void deinitialize_pipeline_layouts(HdLogicalDevice* device);

// Surfaces
struct HdSurface {
    VkSurfaceKHR surface;
    PlWindow* window;
    HdInstance* instance;
};

// Swapchain
struct HdSwapchain {
    VkSwapchainKHR swapchain;
    HdLogicalDevice* device;
    HdExtent extent;
    uint32_t num_images;
    VkImage* images;
    VkImageView* image_views;
    VkSemaphore* render_complete_semaphores;
};

// Pipeline
struct HdPipeline {
    VkPipeline pipeline;
    VkPipelineBindPoint bind_point;
};

// Semaphores
struct HdSemaphore {
    VkSemaphore semaphore;
};
