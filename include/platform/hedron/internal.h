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

typedef struct HdAllocation HdAllocation;
ARRAY_HEADER(HdAllocation, hdalloc, HdAllocation)

struct HdLogicalDevice {
    VkDevice device;
    VkPhysicalDevice physical_device;
    Allocator* gpa; // Allocator is accessed often, so keep it high up.

    // We keep track of allocations in the device because we only provide
    // device/host addresses via the API, but vulkan requires that we keep track
    // of more state.
    // TODO: Use a mutex to lock the allocations, ensuring that num_allocations
    //       + allocations stay coherent when hammered by multiple threads.
    HdAllocationArray allocations;

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

