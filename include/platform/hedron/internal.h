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
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#ifndef NOMINMAX
#define NOMINMAX
#endif
#else 
#error "unrecognized OS"
#endif

#define VK_ENABLE_BETA_EXTENSIONS
//#define VK_NO_PROTOTYPES
#include <vulkan/vulkan.h>

#include "data/meta/array_header.h"
#include "data/meta/amap_header.h"

#include "platform/hedron/hedron.h"
#include "platform/window/internal.h"

#define MAX_COLOUR_ATTACHMENTS 8
#define ADDRESS_FLAGS (VK_ADDRESS_COMMAND_FULLY_BOUND_BIT_KHR)
#define FORMAT_COUNT ((uint32_t)Format_Count)

// Instance & Devices
struct HdInstance {
    VkInstance vk_instance;
    Allocator* gpa;

    size_t num_devices;
    HdPhysicalDevice* devices;
};

// Use this to store device properties, so when we need them we don't have to
// re-query the API. 
struct HdPhysicalDevice {
    VkPhysicalDevice device;
    VkPhysicalDeviceProperties properties;
    VkPhysicalDeviceMemoryProperties memory_properties;
    VkPhysicalDeviceDescriptorHeapPropertiesEXT heap_properties;
};

typedef struct {
    VkQueue queue;
    uint32_t queue_family;
    HdLogicalDevice* device;
} HdQueue;

struct HdCommandBuffer {
    HdQueue* queue;
    VkCommandBuffer buffer;

    // TODO: investigate the following
    // Each command buffer owns its' own pool so recording needs no
    // cross-thread synchronization
    VkCommandPool pool;
    HdLogicalDevice* device;

    // State
    HdPipeline* current_pipeline;
    bool rendering;
};

typedef struct {
    HdCommandBuffer* commands;
    uint64_t value;
} PendingBuffer;

ARRAY_HEADER(PendingBuffer, pbuf, PendingBuffer);
AMAP_HEADER(HdSemaphore*, PendingBufferArray, sem_bufs, SemBufs);

typedef struct {
    PFN_vkWriteSamplerDescriptorsEXT vkWriteSamplerDescriptorsEXT;
    PFN_vkWriteResourceDescriptorsEXT vkWriteResourceDescriptorsEXT;
    PFN_vkCmdBindSamplerHeapEXT vkCmdBindSamplerHeapEXT;
    PFN_vkCmdBindResourceHeapEXT vkCmdBindResourceHeapEXT;
    PFN_vkCmdPushDataEXT vkCmdPushDataEXT;
    PFN_vkCmdBindIndexBuffer3KHR vkCmdBindIndexBuffer3KHR;
    PFN_vkCmdDrawIndirect2KHR vkCmdDrawIndirect2KHR;
    PFN_vkCmdDrawIndexedIndirect2KHR vkCmdDrawIndexedIndirect2KHR;
    PFN_vkCmdDispatchIndirect2KHR vkCmdDispatchIndirect2KHR;
} DeviceFunctions;

typedef struct HdTextureInitializationList HdTextureInitializationList;
typedef struct HdTextureInitialization HdTextureInitialization;

struct HdTextureInitialization {
    VkImage image;
    VkImageAspectFlags aspect_mask;
    uint32_t mip_levels;
    uint32_t array_layers;
    HdTextureInitialization* previous;
    HdTextureInitialization* next;
    HdTextureInitializationList* owner;
};

struct HdTextureInitializationList {
    HdTextureInitialization* first;
    HdTextureInitialization* last;
};

void append_texture_initialization(HdTextureInitializationList* list, HdTextureInitialization* initialization);
void remove_texture_initialization(HdTextureInitialization* initialization);

struct HdLogicalDevice {
    VkDevice device;
    Allocator* gpa; // Allocator is accessed often, so keep it high up.

    PtrArray swapchains;
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

    HdQueue queue;
    HdTextureInitializationList pending_texture_initializations;
    
    // Instead of needing to re-query the physical device for it's properties,
    // we store them here. We also store some cached data about the device
    HdPhysicalDevice* physical_device;
    VkFormatFeatureFlags2* format_features;
    uint32_t texture_memory_type;
    uint32_t texture_heap_alignment;

    // Function pointers for extensions we need go here.
    DeviceFunctions fns;

    HdDeviceCapabilities capabilities; 
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

typedef struct {
    VkSemaphore acquired;
    VkSemaphore rendered;
    VkFence presented;
    bool present_pending;
} HdPresentContext;

struct HdRenderView { 
    HdLogicalDevice* device;
    VkImageView image_view;
    HdExtent extent;
    HdSwapchain* swapchain;
};

// Swapchain
struct HdSwapchain {
    // Context 
    VkSwapchainKHR swapchain;
    HdLogicalDevice* device;
    HdSurface* surface;
    HdExtent extent;

    // Current State - will change as program executes
    uint32_t next_present_context;
    uint32_t current_present;
    uint32_t current_image;
    bool acquired;
    bool recreate_required;

    // Needed for wrapping presentation/submission cleanly
    uint32_t num_images;
    VkImage* images;
    HdRenderView* render_views;
    HdPresentContext* present_contexts;
    // When uninitialized, images will have layout 'undefined'. 
    // After that, they will have layout 'present source'. We need this
    // information, so store it here.
    bool* initialized; 

    // If we try begin a render pass with a command buffer and an attachment
    // points to the swapchain, and the swapchain hasn't been transitioned
    // to a new layout, this claimed_by will be set, to record the command
    // buffer which is rendering to the swapchain.
    // If the swapchain has been claimed by a different command buffer, we
    // return an error.
    HdCommandBuffer* claimed_by;
};

// Textures
struct HdTexture {
    HdLogicalDevice* device;
    VkImage image;
    uint32_t width;
    uint32_t height;
    uint32_t depth;
    uint32_t layer_count;
    HdTextureShape shape;
    HdFormat format;
    HdTextureInitialization initialization;
};

// Pipeline
struct HdPipeline {
    VkPipeline pipeline;
    VkPipelineBindPoint bind_point;
    size_t data_size;
};

// Semaphores
struct HdSemaphore {
    VkSemaphore semaphore;
};

// Conversions & Utility, used by internal functions
HdError convert_error_type(VkResult desc);
#define CHECK_RESULT(result) {if (result != VK_SUCCESS) { return (HdPtrResult) { .type = Err, .error = convert_error_type(result),};}};

VkCullModeFlags cull_to_vk(HdCull cull);
VkCullModeFlags blend_factor_to_vk(HdBlendFactor blend);
VkBlendOp blend_op_to_vk(HdBlendOp op);
VkAttachmentLoadOp load_op_to_vk(LoadOp op);
VkAttachmentStoreOp store_op_to_vk(StoreOp op);
VkFormat format_to_vk(HdFormat format);
VkImageViewType texshape_to_vk_view(HdTextureShape type);
VkImageType texshape_to_vk(HdTextureShape type);

// Get information about things in useful formats
uint32_t popcount(uint32_t value);
bool has_depth_aspect(HdFormat format);
bool has_stencil_aspect(HdFormat format);
VkMemoryRequirements image_memory_requirements(HdLogicalDevice* device, const VkImageCreateInfo image_info);
VkFormatFeatureFlags2 required_format_features(HdTextureUsage usage);

bool find_memory_type(uint32_t bits, VkMemoryPropertyFlags required, VkMemoryPropertyFlags preferred, VkDeviceSize minimum_heap_size,
                      uint32_t* output, VkMemoryPropertyFlags avoided, HdLogicalDevice* device);
