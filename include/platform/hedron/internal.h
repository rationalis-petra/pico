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

struct HdLogicalDevice {
    VkDevice device;
    VkPhysicalDevice physical_device;
    Allocator* gpa;
};

// Surfaces
struct HdSurface {
    VkSurfaceKHR surface;
    HdInstance* instance;
};

/*
struct HdSurface {
    VkSurfaceKHR surface;
    HdInstance* instance;
    // presentation queue family;
    uint32_t present_family;

    // Render passes and framebuffers
    // these are likely to be detached later
    VkRenderPass renderpass;

    uint32_t num_buffers;
    VkFramebuffer* buffers;
};

struct HdSwapchain {
    uint32_t image_count;
    VkFormat format;
    VkExtent2D extent;
    VkPresentModeKHR mode;
    VkSwapchainKHR swapchain;

    uint32_t num_images;
    VkImage* swapchain_images;
    VkImageView* image_views;
};

typedef struct {
    VkSurfaceCapabilitiesKHR capabilities;
    uint32_t num_formats;
    VkSurfaceFormatKHR* formats;

    uint32_t num_present_modes;
    VkPresentModeKHR* present_modes;
} SwapChainSupportDetails;

struct HedronShaderModule {
    VkShaderModule module;
};

struct HedronPipeline {
    VkPipelineLayout layout;
    VkPipeline pipeline;
};

struct HedronDescriptorSetLayout {
    VkDescriptorSetLayout layout;
};

struct HedronBuffer {
    VkBuffer vk_buffer;
    VkDeviceMemory device_memory;
    uint64_t size;
};

struct HedronImage {
    VkImage vk_image;
    VkDeviceMemory image_memory;
};

struct HedronImageView {
    VkImageView vk_image_view;
};

struct HedronSampler {
    VkSampler vk_sampler;
};

struct HedronDescriptorSet {
    VkDescriptorSet vk_set;
};

struct HedronDescriptorPool {
    VkDescriptorPool pool;
    HedronDescriptorSet* sets;
    uint32_t num_sets;
};

struct HedronCommandPool {
    VkCommandPool pool;
    PtrArray buffers;
};

struct HedronCommandBuffer {
    VkCommandBuffer buffer;
};

struct HedronFence {
    VkFence fence;
};

struct HedronSemaphore {
    VkSemaphore semaphore;
};

*/
