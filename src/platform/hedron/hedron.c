#ifdef USE_VULKAN

#include <string.h>

#include "platform/machine_info.h"
#include "platform/hedron/hedron.h"
#include "platform/signals.h"
#include "data/string.h"
 
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
#include "platform/window/internal.h"

struct HedronSurface {
    VkSurfaceKHR surface;
    // presentation queue family;
    uint32_t present_family;

    // swapchain details
    uint32_t image_count;
    VkFormat format;
    VkExtent2D extent;
    VkPresentModeKHR mode;
    VkSwapchainKHR swapchain;

    uint32_t num_images;
    VkImage* swapchain_images;
    VkImageView* image_views;

    // Render passes and framebuffers
    // these are likely to be detached later
    VkRenderPass renderpass;

    uint32_t num_buffers;
    VkFramebuffer* buffers;
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

typedef enum {
    QUEUE_GRAPHICS = 0x1,
    QUEUE_PRESENT = 0x2,
} QueueFamilyFlags;

typedef struct  {
    uint32_t available;
    uint32_t graphics_family;
    // uint32_t present_family; TODO: check if we want graphics/present family different? 
} QueueFamilyIndices;

static VkInstance rl_vk_instance;
static VkPhysicalDevice physical_device = VK_NULL_HANDLE;
static VkDevice logical_device = VK_NULL_HANDLE;
static Allocator* hd_alloc;
static PiAllocator hd_pi_alloc;

// -----------------------------------------------------------------------------
//
//  Conversions
// 
// -----------------------------------------------------------------------------

VkDescriptorType convert_descriptor_type(DescriptorType desc) {
    VkDescriptorType vk_desc = 0;
    switch (desc) {
    case UniformBufferDesc: 
        vk_desc = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
        break;
    case CombinedImageSamplerDesc: 
        vk_desc = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
        break;
    }
    return vk_desc;
}

VkShaderStageFlagBits convert_shader_type(ShaderStage shader) {
    VkShaderStageFlagBits vk_shader = 0;
    switch (shader) {
    case VertexShader: 
        vk_shader = VK_SHADER_STAGE_VERTEX_BIT;
        break;
    case FragmentShader: 
        vk_shader = VK_SHADER_STAGE_FRAGMENT_BIT;
        break;
    }
    return vk_shader;
}

VkPipelineStageFlags convert_pipeline_stage(PipelineStage stage) {
    VkPipelineStageFlags out_stages = 0;
    switch (stage) {
    case StageTopOfPipe:
        out_stages = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT;
        break;
    case StageFragmentShader:
        out_stages = VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT;
        break;
    case StageColourAttachmentOutput:
        out_stages = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;
        break;
    case StageTransfer:
        out_stages = VK_PIPELINE_STAGE_TRANSFER_BIT;
        break;
    }
    return out_stages;
}

VkFormat convert_image_format(ImageFormat format) {
    VkFormat vk_format;
    switch (format) {
    case R8G8B8A8_SRGB:
        vk_format = VK_FORMAT_R8G8B8A8_SRGB;
    }
    return vk_format;
}

VkImageLayout convert_image_layout(ImageLayout layout) {
    VkImageLayout vk_layout = VK_IMAGE_LAYOUT_UNDEFINED;
    switch (layout) {
    case Undefined:
        vk_layout = VK_IMAGE_LAYOUT_UNDEFINED;
        break;
    case TransferDestOptimal:
        vk_layout = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;
        break;
    case ShaderReadOptimal:
        vk_layout = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
        break;
    }
    return vk_layout;
}
VkAccessFlagBits convert_access_flags(Access access) {
    VkAccessFlagBits vk_access = 0;
    switch (access) {
    case AccessNone:
        break;
    case AccessShaderRead:
        vk_access = VK_ACCESS_SHADER_READ_BIT;
        break;
    case AccessShaderWrite:
        vk_access = VK_ACCESS_SHADER_WRITE_BIT;
        break;
    case AccessTransferRead:
        vk_access = VK_ACCESS_TRANSFER_READ_BIT;
        break;
    case AccessTransferWrite:
        vk_access = VK_ACCESS_TRANSFER_WRITE_BIT;
        break;
    } 
    return vk_access;
}

#ifndef WINDOW_SYSTEM
const uint32_t num_required_extensions = 1;
const char *required_extensions[] = {
    VK_KHR_SURFACE_EXTENSION_NAME,
};
#else
const uint32_t num_required_extensions = 2;
const char *required_extensions[] = {
    VK_KHR_SURFACE_EXTENSION_NAME,
    // No window extension needed.
#if (OS_FAMILY == UNIX) && (WINDOW_SYSTEM == 1)
    VK_KHR_XLIB_SURFACE_EXTENSION_NAME,
#elif (OS_FAMILY == UNIX) && (WINDOW_SYSTEM == 2)
    VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME,
#elif OS_FAMILY == WINDOWS
    VK_KHR_WIN32_SURFACE_EXTENSION_NAME,
#else 
#error "unrecognized OS!"
#endif
};
#endif

const uint32_t num_required_device_extensions = 1;
const char *required_device_extensions[] = {
    VK_KHR_SWAPCHAIN_EXTENSION_NAME,
    VK_EXT_SURFACE_MAINTENANCE_1_EXTENSION_NAME,
};


// Validation layers, which are used when compiling in debug mode 
const uint32_t num_required_validation_layers = 1;
const char* required_validation_layers[] = {"VK_LAYER_KHRONOS_validation"};
#ifdef DEBUG
const bool enable_validation = true;
#else 
const bool enable_validation = false;
#endif

bool check_validation_layer_support(Allocator* a) {
    bool layer_found = false;
    uint32_t layer_count;
    vkEnumerateInstanceLayerProperties(&layer_count, NULL);

    VkLayerProperties* available_layers = mem_alloc(layer_count * sizeof(VkLayerProperties), a);
    vkEnumerateInstanceLayerProperties(&layer_count, available_layers);

    for (size_t i = 0; i < num_required_validation_layers; i++) {
        String layer_name = mv_string(required_validation_layers[i]);

        for (size_t j = 0; j < layer_count; j++) {
            String available_layer_name = mv_string(available_layers[j].layerName);
            if (string_cmp(layer_name, available_layer_name) == 0) {
                layer_found = true;
                break;
            }
        }

        if (!layer_found) {
            mem_free(available_layers, a);
            return false;
        }
    }

    mem_free(available_layers, a);
    return layer_found;
}

VkResult create_instance(Allocator* a) {
    if (enable_validation && !check_validation_layer_support(a)) {
          panic(mv_string("Expected validation layer support, but none present!"));
    }

    VkApplicationInfo app_info = (VkApplicationInfo){};
    app_info.sType = VK_STRUCTURE_TYPE_APPLICATION_INFO;
    app_info.pApplicationName = "Relic";
    app_info.applicationVersion = VK_MAKE_VERSION(1, 0, 0);
    app_info.pEngineName = "No Engine";
    app_info.engineVersion = VK_MAKE_VERSION(1, 0, 0);
    app_info.apiVersion = VK_API_VERSION_1_0;

    VkInstanceCreateInfo create_info = (VkInstanceCreateInfo){};
    create_info.sType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
    create_info.pApplicationInfo = &app_info;

    create_info.enabledExtensionCount = num_required_extensions;
    create_info.ppEnabledExtensionNames = required_extensions;

    if (enable_validation) {
        create_info.enabledLayerCount = num_required_validation_layers;
        create_info.ppEnabledLayerNames = required_validation_layers;
    } else {
        create_info.enabledLayerCount = 0;
    }

    // TODO (FEATURE): see 'Message Calback' for the vk validation layers in vulkan-tutorial.com

    return vkCreateInstance(&create_info, NULL, &rl_vk_instance);
}

QueueFamilyIndices find_queue_families(VkPhysicalDevice device) {
    QueueFamilyIndices indices = (QueueFamilyIndices){};
    uint32_t queue_family_count = 0;
    vkGetPhysicalDeviceQueueFamilyProperties(device, &queue_family_count, NULL);

    VkQueueFamilyProperties* queue_families = mem_alloc(queue_family_count * sizeof(VkQueueFamilyProperties), hd_alloc);
    vkGetPhysicalDeviceQueueFamilyProperties(device, &queue_family_count, queue_families);

    for (uint32_t i = 0; i < queue_family_count; i++) {
        VkQueueFamilyProperties queue_family = queue_families[i];
        if (queue_family.queueFlags & VK_QUEUE_GRAPHICS_BIT) {
            indices.available |= QUEUE_GRAPHICS;
            indices.graphics_family = i;
        }
    }

    mem_free(queue_families, hd_alloc);
    return indices;
}

bool check_device_extension_support(VkPhysicalDevice device, Allocator* a) {
    uint32_t extension_count;
    vkEnumerateDeviceExtensionProperties(device, NULL, &extension_count, NULL);

    VkExtensionProperties* available_extensions = mem_alloc(extension_count * sizeof(VkExtensionProperties), a);
    vkEnumerateDeviceExtensionProperties(device, NULL, &extension_count, available_extensions);

    // TODO : do we need to handle repeated extensions? perhaps replace with a set?
    size_t supported_extension_count = 0;

    for (size_t i = 0; i < num_required_device_extensions; i++) {
        String req_name = mv_string(required_device_extensions[i]);
        for (size_t j = 0; j < extension_count; j++) {
            String ext_name = mv_string(available_extensions[j].extensionName);
            if (string_cmp(ext_name, req_name) == 0) {
                supported_extension_count++;
                break;
            }
        }
    }

    mem_free(available_extensions, a);
    return supported_extension_count == num_required_device_extensions;
}

bool is_device_suitable(VkPhysicalDevice device, Allocator* a) {
    VkPhysicalDeviceProperties device_properties;
    VkPhysicalDeviceFeatures device_features;
    vkGetPhysicalDeviceProperties(device, &device_properties);
    vkGetPhysicalDeviceFeatures(device, &device_features);

    QueueFamilyIndices indices = find_queue_families(device);
    const uint32_t required_indices = QUEUE_GRAPHICS;

    const bool extensions_supported = check_device_extension_support(device, a);

    return (device_features.geometryShader
            && device_features.samplerAnisotropy
            && extensions_supported
            && (indices.available & required_indices));
}

VkResult pick_physical_device(Allocator* a) {
    uint32_t device_count = 0;
    vkEnumeratePhysicalDevices(rl_vk_instance, &device_count, NULL);
    if (device_count == 0) {
        panic(mv_string("failed to find GPUs with Vulkan support"));
    }

    VkPhysicalDevice* devices = mem_alloc(device_count * sizeof(VkPhysicalDevice), a);
    vkEnumeratePhysicalDevices(rl_vk_instance, &device_count, devices);

    // TODO (FEAT): score devices & pick "best" device.
    for (size_t i = 0; i < device_count; i++) {
        if (is_device_suitable(devices[i], a)) {
            physical_device = devices[i];
            break;
        }
    }

    if (physical_device == VK_NULL_HANDLE) {
        panic(mv_string("failed to find a suitable GPU!"));
    }

    mem_free(devices, a);
    return VK_SUCCESS;
}

VkResult create_logical_device(Allocator* a) {
    QueueFamilyIndices indices = find_queue_families(physical_device);
    VkDeviceQueueCreateInfo queue_create_info = {};
    queue_create_info.sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
    queue_create_info.queueFamilyIndex = indices.graphics_family;
    queue_create_info.queueCount = 1;

    float queue_priority = 1.0f;
    queue_create_info.pQueuePriorities = &queue_priority;

    VkDeviceCreateInfo create_info = {};
    create_info.sType = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;

    create_info.pQueueCreateInfos = &queue_create_info;
    create_info.queueCreateInfoCount = 1;

    // TODO: add extensions + device features to public (relic) API
    VkPhysicalDeviceFeatures device_features = {
        .samplerAnisotropy = VK_TRUE,
    };
    create_info.pEnabledFeatures = &device_features;

    // Note: technically, in modern Vulkan implementations, this will probably do nothing
    // However, it is a good idea to do this anyway as it allows us to support validation
    // layers on older vulkan implementations.
    create_info.enabledExtensionCount = num_required_device_extensions;
    create_info.ppEnabledExtensionNames = required_device_extensions;

    if (enable_validation) {
        create_info.enabledLayerCount = num_required_validation_layers;
        create_info.ppEnabledLayerNames = required_validation_layers;
    } else {
        create_info.enabledLayerCount = 0;
    }

    return vkCreateDevice(physical_device, &create_info, NULL, &logical_device);
}

int init_hedron(Allocator* a) {
    hd_alloc = a;
    hd_pi_alloc = convert_to_pallocator(a);
    VkResult result = create_instance(a);
    if (result != VK_SUCCESS) return 1;

    // TODO: setup_debug_messenger();

    result = pick_physical_device(a);
    if (result != VK_SUCCESS) return 1;

    result = create_logical_device(a);
    if (result != VK_SUCCESS) return 1;

    return 0;
} 

void teardown_hedron() {
    vkDestroyDevice(logical_device, NULL);
    vkDestroyInstance(rl_vk_instance, NULL);
}

SwapChainSupportDetails query_swapchain_support_details(VkPhysicalDevice device, VkSurfaceKHR surface, Allocator* a) {
    SwapChainSupportDetails details = (SwapChainSupportDetails){};
    vkGetPhysicalDeviceSurfaceCapabilitiesKHR(device, surface, &details.capabilities);

    uint32_t num_formats;
    vkGetPhysicalDeviceSurfaceFormatsKHR(device, surface, &num_formats, NULL);
    details.num_formats = num_formats;

    if (num_formats != 0) {
        details.formats = mem_alloc(num_formats * sizeof(VkSurfaceFormatKHR), a);
        vkGetPhysicalDeviceSurfaceFormatsKHR(device, surface, &num_formats, details.formats);
    }

    uint32_t num_present_modes;
    vkGetPhysicalDeviceSurfacePresentModesKHR(device, surface, &num_present_modes, NULL);

    if (num_present_modes != 0) {
        details.num_present_modes = num_present_modes;
        details.present_modes = mem_alloc(num_present_modes * sizeof(VkPresentModeKHR), a);
        vkGetPhysicalDeviceSurfacePresentModesKHR(device, surface, &num_present_modes, details.present_modes);
    }
    
    return details;
}

void free_swapchain_details(SwapChainSupportDetails swap_chain, Allocator *a) {
    if (swap_chain.num_formats > 0)
        mem_free(swap_chain.formats, a);
    if (swap_chain.num_present_modes > 0)
        mem_free(swap_chain.present_modes, a);
}

VkSurfaceFormatKHR choose_swap_surface_format(const VkSurfaceFormatKHR* available_formats, uint32_t num_formats) {
    for (size_t i = 0; i < num_formats; i++) {
        VkSurfaceFormatKHR available_format = available_formats[i];
        if (available_format.format == VK_FORMAT_B8G8R8A8_SRGB && available_format.colorSpace == VK_COLOR_SPACE_SRGB_NONLINEAR_KHR) {
            return available_format;
        }
    }

    return available_formats[0];
}

VkPresentModeKHR choose_swap_present_mode(VkPresentModeKHR* available_present_modes, uint32_t num_modes) {
    for (size_t i = 0; i < num_modes; i++) {
        VkPresentModeKHR present_mode = available_present_modes[i];
        if (present_mode == VK_PRESENT_MODE_MAILBOX_KHR) {
            return present_mode;
        }
    }

    // FIFO is guaranteed to be available by the vulkan standard
    return VK_PRESENT_MODE_FIFO_KHR;
}

uint32_t clamp(uint32_t val, uint32_t min, uint32_t max) {
    if (val < min) return min;
    if (val > max) return max;
    return val;
}

VkExtent2D choose_swap_extent(VkSurfaceCapabilitiesKHR capabilities, uint32_t width, uint32_t height) {
    if (capabilities.currentExtent.width != UINT32_MAX) {
        return capabilities.currentExtent;
    } else {
        VkExtent2D actualExtent = {
            .width = width,
            .height = height,
        };

        actualExtent.width = clamp(actualExtent.width, capabilities.minImageExtent.width, capabilities.maxImageExtent.width);
        actualExtent.height = clamp(actualExtent.height, capabilities.minImageExtent.height, capabilities.maxImageExtent.height);

        return actualExtent;
    }
}

void create_swapchain(SwapChainSupportDetails swap_chain_details, VkSurfaceKHR surface, uint32_t width, uint32_t height, HedronSurface* hd_surface) {
    VkSurfaceFormatKHR surface_format = choose_swap_surface_format(swap_chain_details.formats, swap_chain_details.num_formats);
    VkPresentModeKHR mode = choose_swap_present_mode(swap_chain_details.present_modes, swap_chain_details.num_present_modes);
    // TODO: push extent back to window!
    VkExtent2D extent = choose_swap_extent(swap_chain_details.capabilities, width, height);

    uint32_t image_count = swap_chain_details.capabilities.minImageCount + 1;
    if (swap_chain_details.capabilities.maxImageCount > 0 && image_count > swap_chain_details.capabilities.maxImageCount) {
        image_count = swap_chain_details.capabilities.maxImageCount;
    }

    VkSwapchainCreateInfoKHR create_info = (VkSwapchainCreateInfoKHR){};
    create_info.sType = VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR;
    create_info.surface = surface;

    create_info.minImageCount = image_count;
    create_info.imageFormat = surface_format.format;
    create_info.imageColorSpace = surface_format.colorSpace;
    create_info.imageExtent = extent;
    create_info.imageArrayLayers = 1;
    create_info.imageUsage = VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT;

    // TODO: when we allow the presentation and graphics queue to be different,
    //       add a check here
    create_info.imageSharingMode = VK_SHARING_MODE_EXCLUSIVE;
    create_info.queueFamilyIndexCount = 0; // Optional
    create_info.pQueueFamilyIndices = NULL; // Optional

    create_info.preTransform = swap_chain_details.capabilities.currentTransform;
    create_info.compositeAlpha = VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR;

    create_info.presentMode = mode;
    create_info.clipped = VK_TRUE;
    create_info.oldSwapchain = VK_NULL_HANDLE;

    VkSwapchainKHR swap_chain;
    if (vkCreateSwapchainKHR(logical_device, &create_info, NULL, &swap_chain) != VK_SUCCESS) {
        // TODO: proper error reporting
        panic(mv_string("swapchain creation failed"));
    }

    vkGetSwapchainImagesKHR(logical_device, swap_chain, &image_count, NULL);
    VkImage* images = mem_alloc(image_count * sizeof(VkImage), hd_alloc);
    vkGetSwapchainImagesKHR(logical_device, swap_chain, &image_count, images);

    VkImageView* image_views = mem_alloc(image_count * sizeof(VkImageView), hd_alloc);
    for (size_t i = 0; i < image_count; i++) {
        VkImageViewCreateInfo createInfo = (VkImageViewCreateInfo) {
            .sType = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO,
            .image = images[i],

            .viewType = VK_IMAGE_VIEW_TYPE_2D,
            .format = surface_format.format,

            .components.r = VK_COMPONENT_SWIZZLE_IDENTITY,
            .components.g = VK_COMPONENT_SWIZZLE_IDENTITY,
            .components.b = VK_COMPONENT_SWIZZLE_IDENTITY,
            .components.a = VK_COMPONENT_SWIZZLE_IDENTITY,

            .subresourceRange.aspectMask = VK_IMAGE_ASPECT_COLOR_BIT,
            .subresourceRange.baseMipLevel = 0,
            .subresourceRange.levelCount = 1,
            .subresourceRange.baseArrayLayer = 0,
            .subresourceRange.layerCount = 1,
        };
        if (vkCreateImageView(logical_device, &createInfo, NULL, &image_views[i]) != VK_SUCCESS) {
            panic(mv_string("failed to create image views!"));
        }
    }

    hd_surface->swapchain = swap_chain;
    hd_surface->mode = mode;
    hd_surface->format = surface_format.format;
    hd_surface->extent = extent;
    hd_surface->num_images = image_count;
    hd_surface->swapchain_images = images;
    hd_surface->image_views = image_views;
}

void create_renderpass(HedronSurface *surface) {
    VkAttachmentDescription colour_attachment = (VkAttachmentDescription){
        .format = surface->format,
        .samples = VK_SAMPLE_COUNT_1_BIT,
        .loadOp = VK_ATTACHMENT_LOAD_OP_CLEAR,
        .storeOp = VK_ATTACHMENT_STORE_OP_STORE,
        .stencilLoadOp = VK_ATTACHMENT_LOAD_OP_DONT_CARE,
        .stencilStoreOp = VK_ATTACHMENT_STORE_OP_DONT_CARE,
        .initialLayout = VK_IMAGE_LAYOUT_UNDEFINED,
        .finalLayout = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR,
    };

    VkAttachmentReference colour_attachment_ref = (VkAttachmentReference) {
        .attachment = 0,
        .layout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
    };

    VkSubpassDescription subpass = (VkSubpassDescription) {
        .pipelineBindPoint = VK_PIPELINE_BIND_POINT_GRAPHICS,
        .colorAttachmentCount = 1,
        .pColorAttachments = &colour_attachment_ref,
    };

    VkSubpassDependency dependency = (VkSubpassDependency) {
        .srcSubpass = VK_SUBPASS_EXTERNAL,
        .dstSubpass = 0,
        .srcStageMask = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
        .srcAccessMask = 0,
        .dstStageMask = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
        .dstAccessMask = VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT,
    };

    VkRenderPassCreateInfo render_pass_info = (VkRenderPassCreateInfo) {
        .sType = VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO,
        .attachmentCount = 1,
        .pAttachments = &colour_attachment,
        .subpassCount = 1,
        .pSubpasses = &subpass,
        .dependencyCount = 1,
        .pDependencies = &dependency,
    };

    VkRenderPass render_pass;
    if (vkCreateRenderPass(logical_device, &render_pass_info, NULL, &render_pass) != VK_SUCCESS) {
        panic(mv_string("failed to create render pass!"));
    }
    surface->renderpass = render_pass;
}

void create_framebuffers(HedronSurface *surface) {
    VkFramebuffer* buffers = mem_alloc(surface->num_images * sizeof(VkFramebuffer), hd_alloc);
    for (size_t i = 0; i < surface->num_images; i++) {
        VkImageView attachments[] = {
            surface->image_views[i]
        };

        VkFramebufferCreateInfo framebuffer_info = (VkFramebufferCreateInfo) {
            .sType = VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO,
            .renderPass = surface->renderpass,
            .attachmentCount = 1,
            .pAttachments = attachments,
            .width = surface->extent.width,
            .height = surface->extent.height,
            .layers = 1,
        };

        if (vkCreateFramebuffer(logical_device, &framebuffer_info,NULL, &buffers[i]) != VK_SUCCESS) {
            panic(mv_string("failed to create framebuffer!"));
        }
    }
    surface->num_buffers = surface->num_images;
    surface->buffers = buffers;
}

void cleanup_swap_chain(HedronSurface *surface) {
    for (size_t i = 0; i < surface->num_buffers; i++) {
        vkDestroyFramebuffer(logical_device, surface->buffers[i], NULL);
    }
    mem_free(surface->buffers, hd_alloc);
    for (size_t i = 0; i < surface->num_images; i++) {
        vkDestroyImageView(logical_device, surface->image_views[i], NULL);
    }
    mem_free(surface->image_views, hd_alloc);
    vkDestroySwapchainKHR(logical_device, surface->swapchain, NULL);
}

#ifdef WINDOW_SYSTEM
HedronSurface *create_window_surface(struct PlWindow *window) {
    VkSurfaceKHR surface;

#if (OS_FAMILY == UNIX) && (WINDOW_SYSTEM == 1)
    VkXlibSurfaceCreateInfoKHR create_info = (VkXlibSurfaceCreateInfoKHR){
        .sType = VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR,
        .dpy = get_x11_display(),
        .window = window->x11_window,
    };

    VkResult result = vkCreateXlibSurfaceKHR(rl_vk_instance, &create_info, NULL, &surface);
    if (result != VK_SUCCESS) return NULL;

#elif (OS_FAMILY == UNIX) && (WINDOW_SYSTEM == 2)
    VkWaylandSurfaceCreateInfoKHR create_info = (VkWaylandSurfaceCreateInfoKHR){};
    create_info.sType = VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR;
    create_info.display = get_wl_display();
    create_info.surface = window->surface;

    VkResult result = vkCreateWaylandSurfaceKHR(rl_vk_instance, &create_info, NULL, &surface);
    if (result != VK_SUCCESS) return NULL;

    // TODO: check for present support on graphics queue

#elif OS_FAMILY == WINDOWS
    VkWin32SurfaceCreateInfoKHR create_info = (VkWin32SurfaceCreateInfoKHR) {
        .sType = VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR,
        .hwnd = window->impl,
        .hinstance = GetModuleHandle(NULL),
    };

    VkResult result = vkCreateWin32SurfaceKHR(rl_vk_instance, &create_info, NULL, &surface);
    if (result != VK_SUCCESS) return NULL;
#else
#error "unrecognized OS"
#endif


    // TODO: postpone device selection until AFTER surface creation!
    // TODO: possibly the graphics and present families are different?
    VkBool32 present_support = false;

    QueueFamilyIndices indices = find_queue_families(physical_device);
    vkGetPhysicalDeviceSurfaceSupportKHR(physical_device, indices.graphics_family, surface, &present_support);

    if (!present_support) {
        return NULL;
    }

    // TODO: do we need to confirm that the swapchain extension is supported?
    //       possibly: check in debug mode.
    bool swap_chain_ok = false;
    SwapChainSupportDetails swap_chain_details = query_swapchain_support_details(physical_device, surface, hd_alloc);
    swap_chain_ok = (swap_chain_details.num_formats != 0) && (swap_chain_details.num_present_modes != 0);
    if (!swap_chain_ok) {
        // TODO (FEATURE): proper error type
        vkDestroySurfaceKHR(rl_vk_instance, surface, NULL);
        free_swapchain_details(swap_chain_details, hd_alloc);
        return NULL;
    }

    // The surface is populated with swapchain details by create_swapchain


    //create_swapchain(SwapChainSupportDetails swap_chain_details, VkSurfaceKHR surface, struct Window *window, HedronSurface* hd_surface) {
    HedronSurface* hd_surface = mem_alloc(sizeof(HedronSurface), hd_alloc);
    create_swapchain(swap_chain_details, surface, window->width, window->height, hd_surface);
    free_swapchain_details(swap_chain_details, hd_alloc);

    create_renderpass(hd_surface);
    create_framebuffers(hd_surface);

    hd_surface->surface = surface;
    hd_surface->present_family = indices.graphics_family;
    return hd_surface;
}
#endif

void resize_window_surface(HedronSurface* surface, Extent extent) {
    vkDeviceWaitIdle(logical_device);

    cleanup_swap_chain(surface);
    mem_free(surface->swapchain_images, hd_alloc);

    uint32_t width = extent.width;
    uint32_t height = extent.height;

    SwapChainSupportDetails swap_chain_details = query_swapchain_support_details(physical_device, surface->surface, hd_alloc);
    create_swapchain(swap_chain_details, surface->surface, width, height, surface);
    create_framebuffers(surface);
    free_swapchain_details(swap_chain_details, hd_alloc);
}

void destroy_window_surface(HedronSurface *surface) {
    cleanup_swap_chain(surface);
    vkDestroySurfaceKHR(rl_vk_instance, surface->surface, NULL);
    vkDestroyRenderPass(logical_device, surface->renderpass, NULL);
    mem_free(surface->swapchain_images, hd_alloc);
    mem_free(surface, hd_alloc);
}

uint32_t num_swapchain_images(HedronSurface* surface) {
    return surface->num_images;
}

HedronShaderModule* create_shader_module(U8PiList code) {
    VkShaderModuleCreateInfo create_info = (VkShaderModuleCreateInfo){};
    create_info.sType = VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO;
    // TODO: check if code.len % 4 == 0??
    create_info.codeSize = code.len;
    // TODO: confirm alignment is ok?
    create_info.pCode = (uint32_t*)code.data;

    VkShaderModule shader_module;
    if (vkCreateShaderModule(logical_device, &create_info, NULL, &shader_module) != VK_SUCCESS) {
        panic(mv_string("failed to create shader module!"));
    }

    HedronShaderModule* hdm = mem_alloc(sizeof(HedronShaderModule), hd_alloc);
    hdm->module = shader_module;

    return hdm;
}

void destroy_shader_module(HedronShaderModule* module) {
    vkDestroyShaderModule(logical_device, module->module, NULL);
    mem_free(module, hd_alloc);
}

HedronDescriptorSetLayout* create_descriptor_set_layout(DescriptorBindingPiList bdesc) {
    VkDescriptorSetLayoutBinding* bindings = mem_alloc(sizeof(VkDescriptorSetLayoutBinding) * bdesc.len, hd_alloc);

    for (size_t i = 0; i < bdesc.len; i++) {
        VkDescriptorSetLayoutBinding layoutBinding = {
            .binding = 0,
            .descriptorType = convert_descriptor_type(bdesc.data[i].type),
            .stageFlags = convert_shader_type(bdesc.data[i].shader_type),
            .descriptorCount = 1,
            .pImmutableSamplers = NULL, // Optional
        };
        bindings[i] = layoutBinding;
    }


    VkDescriptorSetLayoutCreateInfo layoutInfo = {
        .sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO,
        .bindingCount = bdesc.len,
        .pBindings = bindings,
    };

    VkDescriptorSetLayout descriptorSetLayout;
    if (vkCreateDescriptorSetLayout(logical_device, &layoutInfo, NULL, &descriptorSetLayout) != VK_SUCCESS) {
        panic(mv_string("failed to create descriptor set layout!"));
    }

    mem_free(bindings, hd_alloc);

    HedronDescriptorSetLayout* layout = mem_alloc(sizeof(HedronDescriptorSetLayout), hd_alloc);
    layout->layout = descriptorSetLayout;

    return layout;
}

void destroy_descriptor_set_layout(HedronDescriptorSetLayout* layout) {
    vkDestroyDescriptorSetLayout(logical_device, layout->layout, NULL);
    mem_free(layout, hd_alloc);
}

HedronDescriptorPool* create_descriptor_pool(HedronDescriptorPoolSizePiList sizes, uint32_t max_sets) {
    VkDescriptorPoolSize* pool_sizes = mem_alloc(sizeof(VkDescriptorPoolSize) * sizes.len, hd_alloc);
    for (size_t i = 0; i < sizes.len; i++) {
        HedronDescriptorPoolSize hd_psize = sizes.data[i];
        pool_sizes[i] = (VkDescriptorPoolSize) {
            .type = convert_descriptor_type(hd_psize.type),
            .descriptorCount = hd_psize.descriptor_count,
        };
    }

    VkDescriptorPoolCreateInfo pool_info = {
        .sType = VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO,
        .poolSizeCount = 1,
        .pPoolSizes = pool_sizes,
        .maxSets = max_sets,
    };

    VkDescriptorPool descriptorPool;
    if (vkCreateDescriptorPool(logical_device, &pool_info, NULL, &descriptorPool) != VK_SUCCESS) {
        panic(mv_string("failed to create descriptor pool!"));
    }
    mem_free(pool_sizes, hd_alloc);

    HedronDescriptorPool* pool = mem_alloc(sizeof(HedronDescriptorPool), hd_alloc);
    *pool = (HedronDescriptorPool) {
        .pool = descriptorPool,
        .sets = mem_alloc(sizeof(HedronDescriptorSet) * max_sets, hd_alloc),
        .num_sets = 0,
    };
    return pool;
}

void destroy_descriptor_pool(HedronDescriptorPool *pool) {
    vkDestroyDescriptorPool(logical_device, pool->pool, NULL);
    mem_free(pool->sets, hd_alloc);
    mem_free(pool, hd_alloc);
}

AddrPiList alloc_descriptor_sets(uint32_t set_count, HedronDescriptorSetLayout* layout, HedronDescriptorPool *pool) {
    VkDescriptorSetLayout* vk_layouts = mem_alloc(sizeof(VkDescriptorSetLayout) * set_count, hd_alloc);
    for (size_t i =  0; i < set_count; i++) {
        vk_layouts[i] = layout->layout;
    }

    VkDescriptorSetAllocateInfo alloc_info = {
        .sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO,
        .descriptorPool = pool->pool,
        .descriptorSetCount = set_count,
        .pSetLayouts = vk_layouts,
    };

    VkDescriptorSet* vk_descriptor_sets = mem_alloc(sizeof(VkDescriptorSet) * set_count, hd_alloc);
    if (vkAllocateDescriptorSets(logical_device, &alloc_info, vk_descriptor_sets) != VK_SUCCESS) {
        panic(mv_string("failed to allocate descriptor sets!"));
    };

    AddrPiList descriptor_sets = mk_addr_list(set_count, &hd_pi_alloc);
    for (size_t i = 0; i < set_count; i++) {
        HedronDescriptorSet* hd_set = &pool->sets[pool->num_sets + i];
        descriptor_sets.data[i] = hd_set;
        hd_set->vk_set = vk_descriptor_sets[i];
    }
    pool->num_sets += set_count;

    mem_free(vk_layouts, hd_alloc);
    mem_free(vk_descriptor_sets, hd_alloc);
    return descriptor_sets;
}

void update_descriptor_sets(HedronWriteDescriptorSetPiList writes, HedronCopyDescriptorSetPiList copies) {
    VkWriteDescriptorSet* vk_writes = mem_alloc(sizeof(VkWriteDescriptorSet), hd_alloc);
    VkDescriptorBufferInfo* vk_write_buffer_info = mem_alloc(sizeof(VkDescriptorBufferInfo), hd_alloc);
    VkDescriptorImageInfo* vk_write_image_info = mem_alloc(sizeof(VkDescriptorImageInfo), hd_alloc);

    for (size_t i = 0; i < writes.len; i++) {
        HedronWriteDescriptorSet descriptor_write = writes.data[i];


        VkDescriptorType dtype = 0;
        VkDescriptorBufferInfo* binfo = NULL;
        VkDescriptorImageInfo* iinfo = NULL; 
        switch (descriptor_write.descriptor_type) {
        case BufferInfo: {
            HedronBuffer* buffer = descriptor_write.buffer_info.buffer;
            dtype = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;

            vk_write_buffer_info[i] = (VkDescriptorBufferInfo) {
                .buffer = buffer->vk_buffer,
                .offset = descriptor_write.buffer_info.offset,
                .range = descriptor_write.buffer_info.range,
            };
            binfo = vk_write_buffer_info + i;
            break;
        }
        case ImageInfo: 
            dtype = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;

            vk_write_image_info[i] = (VkDescriptorImageInfo) {
                .sampler = descriptor_write.image_info.sampler->vk_sampler,
                .imageView = descriptor_write.image_info.image_view->vk_image_view,
                .imageLayout = convert_image_layout(descriptor_write.image_info.layout),
            };
            iinfo = vk_write_image_info + i;
            break;
        }


        vk_writes[i] = (VkWriteDescriptorSet) {
            .sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET,
            .dstSet = descriptor_write.descriptor_set->vk_set,
            .dstBinding = 0,
            .dstArrayElement = 0,
            .descriptorType = dtype,
            .descriptorCount = 1,
            .pBufferInfo = binfo,
            .pImageInfo = iinfo,
            .pTexelBufferView = NULL,
        };
    }

    if (copies.len > 0) {
        panic(mv_string("update_descriptor_sets does not yet support descriptor set copies!"));
    }

    VkCopyDescriptorSet* vk_copies = mem_alloc(sizeof(VkCopyDescriptorSet), hd_alloc);

    vkUpdateDescriptorSets(logical_device, writes.len, vk_writes, copies.len, vk_copies);
    mem_free(vk_writes, hd_alloc);
    mem_free(vk_write_buffer_info, hd_alloc);
    mem_free(vk_write_image_info, hd_alloc);
    mem_free(vk_copies, hd_alloc);
}

HedronPipeline* create_pipeline(AddrPiList descriptor_set_layouts,
                                BindingDescriptionPiList bdesc,
                                AttributeDescriptionPiList adesc,
                                AddrPiList shaders,
                                HedronSurface* surface) {

    if (shaders.len != 2) {
        panic(mv_string("pipeline expects exactly 2 shaders: vertex and fragment"));
    }
    HedronShaderModule* vert_shader = shaders.data[0];
    HedronShaderModule* frag_shader = shaders.data[1];

    VkPipelineShaderStageCreateInfo vertex_shader_info = (VkPipelineShaderStageCreateInfo) {
        .sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
        .stage = VK_SHADER_STAGE_VERTEX_BIT,
        .module = vert_shader->module,
        .pName = "main",
    };

    VkPipelineShaderStageCreateInfo fragment_shader_info = (VkPipelineShaderStageCreateInfo) {
        .sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
        .stage = VK_SHADER_STAGE_FRAGMENT_BIT,
        .module = frag_shader->module,
        .pName = "main",
    };

    VkPipelineShaderStageCreateInfo shader_stages[2] = {vertex_shader_info, fragment_shader_info};

    VkVertexInputBindingDescription binding_descriptions[bdesc.len];
    for (size_t i = 0; i < bdesc.len; i++) {
        uint32_t input_rate = bdesc.data[i].input_rate == Vertex
            ? VK_VERTEX_INPUT_RATE_VERTEX
            : VK_VERTEX_INPUT_RATE_INSTANCE;

        binding_descriptions[i] = (VkVertexInputBindingDescription) {
            .binding = bdesc.data[i].binding,
            .stride = bdesc.data[i].stride,
            .inputRate = input_rate,
        };
    }

    VkVertexInputAttributeDescription attribute_descriptions[adesc.len];
    for (size_t i = 0; i < adesc.len; i++) {
        uint32_t format = {};
        switch (adesc.data[i].format) {
        case Float_1:
            format = VK_FORMAT_R32_SFLOAT;
            break;
        case Float_2:
            format = VK_FORMAT_R32G32_SFLOAT;
            break;
        case Float_3:
            format = VK_FORMAT_R32G32B32_SFLOAT;
            break;
        }; 

        attribute_descriptions[i] = (VkVertexInputAttributeDescription) {
            .binding = adesc.data[i].binding,
            .location = adesc.data[i].location,
            .format = format,
            .offset = adesc.data[i].offset,
        };
    }

    VkPipelineVertexInputStateCreateInfo vertex_input_info = (VkPipelineVertexInputStateCreateInfo) {
        .sType = VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO,
        .vertexBindingDescriptionCount = bdesc.len,
        .pVertexBindingDescriptions = binding_descriptions,
        .vertexAttributeDescriptionCount = adesc.len,
        .pVertexAttributeDescriptions = attribute_descriptions,
    };

    VkPipelineInputAssemblyStateCreateInfo input_assembly = (VkPipelineInputAssemblyStateCreateInfo) {
        .sType = VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO,
        .topology = VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST,
        .primitiveRestartEnable = VK_FALSE,
    };

    VkViewport viewport = (VkViewport) {
        viewport.x = 0.0f,
        viewport.y = 0.0f,
        viewport.width = (float) surface->extent.width,
        viewport.height = (float) surface->extent.height,
        viewport.minDepth = 0.0f,
        viewport.maxDepth = 1.0f,
    };

    VkRect2D scissor = (VkRect2D) {
        .offset = {.x = 0, .y = 0},
        .extent = surface->extent,
    };

    VkDynamicState dynamic_state_arr[2] = {
        VK_DYNAMIC_STATE_VIEWPORT,
        VK_DYNAMIC_STATE_SCISSOR
    };

    VkPipelineDynamicStateCreateInfo dynamic_states = (VkPipelineDynamicStateCreateInfo){
        .sType = VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO,
        .dynamicStateCount = 2,
        .pDynamicStates = dynamic_state_arr,
    };

    VkPipelineViewportStateCreateInfo viewport_state = (VkPipelineViewportStateCreateInfo){
        .sType = VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO,
        .viewportCount = 1,
        .pViewports = &viewport,
        .scissorCount = 1,
        .pScissors = &scissor,
    };

    VkPipelineRasterizationStateCreateInfo rasterizer = (VkPipelineRasterizationStateCreateInfo){};
    rasterizer.sType = VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO;
    rasterizer.depthClampEnable = VK_FALSE;
    rasterizer.rasterizerDiscardEnable = VK_FALSE;
    rasterizer.polygonMode = VK_POLYGON_MODE_FILL;
    rasterizer.lineWidth = 1.0f;
    rasterizer.cullMode = VK_CULL_MODE_BACK_BIT;
    rasterizer.frontFace = VK_FRONT_FACE_CLOCKWISE;
    rasterizer.depthBiasEnable = VK_FALSE;
    rasterizer.depthBiasConstantFactor = 0.0f; // Optional
    rasterizer.depthBiasClamp = 0.0f; // Optional
    rasterizer.depthBiasSlopeFactor = 0.0f; // Optional

    VkPipelineMultisampleStateCreateInfo multisampling = (VkPipelineMultisampleStateCreateInfo){};
    multisampling.sType = VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO;
    multisampling.sampleShadingEnable = VK_FALSE;
    multisampling.rasterizationSamples = VK_SAMPLE_COUNT_1_BIT;
    multisampling.minSampleShading = 1.0f; // Optional
    multisampling.pSampleMask = NULL; // Optional
    multisampling.alphaToCoverageEnable = VK_FALSE; // Optional
    multisampling.alphaToOneEnable = VK_FALSE; // Optional

    VkPipelineColorBlendAttachmentState colour_blend_attachment = (VkPipelineColorBlendAttachmentState){};
    colour_blend_attachment.colorWriteMask = VK_COLOR_COMPONENT_R_BIT | VK_COLOR_COMPONENT_G_BIT | VK_COLOR_COMPONENT_B_BIT | VK_COLOR_COMPONENT_A_BIT;
    colour_blend_attachment.blendEnable = VK_FALSE;
    colour_blend_attachment.srcColorBlendFactor = VK_BLEND_FACTOR_ONE; // Optional
    colour_blend_attachment.dstColorBlendFactor = VK_BLEND_FACTOR_ZERO; // Optional
    colour_blend_attachment.colorBlendOp = VK_BLEND_OP_ADD; // Optional
    colour_blend_attachment.srcAlphaBlendFactor = VK_BLEND_FACTOR_ONE; // Optional
    colour_blend_attachment.dstAlphaBlendFactor = VK_BLEND_FACTOR_ZERO; // Optional
    colour_blend_attachment.alphaBlendOp = VK_BLEND_OP_ADD; // Optional
    colour_blend_attachment.blendEnable = VK_TRUE;
    colour_blend_attachment.srcColorBlendFactor = VK_BLEND_FACTOR_SRC_ALPHA;
    colour_blend_attachment.dstColorBlendFactor = VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA;
    colour_blend_attachment.colorBlendOp = VK_BLEND_OP_ADD;
    colour_blend_attachment.srcAlphaBlendFactor = VK_BLEND_FACTOR_ONE;
    colour_blend_attachment.dstAlphaBlendFactor = VK_BLEND_FACTOR_ZERO;
    colour_blend_attachment.alphaBlendOp = VK_BLEND_OP_ADD;

    VkPipelineColorBlendStateCreateInfo colour_blending = (VkPipelineColorBlendStateCreateInfo){};
    colour_blending.sType = VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO;
    colour_blending.logicOpEnable = VK_FALSE;
    colour_blending.logicOp = VK_LOGIC_OP_COPY; // Optional
    colour_blending.attachmentCount = 1;
    colour_blending.pAttachments = &colour_blend_attachment;
    colour_blending.blendConstants[0] = 0.0f; // Optional
    colour_blending.blendConstants[1] = 0.0f; // Optional
    colour_blending.blendConstants[2] = 0.0f; // Optional
    colour_blending.blendConstants[3] = 0.0f; // Optional

    VkDescriptorSetLayout* vk_ds_layouts = mem_alloc(sizeof(VkDescriptorSetLayout) * descriptor_set_layouts.len, hd_alloc);
    for (size_t i = 0; i < descriptor_set_layouts.len; i++) {
        HedronDescriptorSetLayout* layout = descriptor_set_layouts.data[i];
        vk_ds_layouts[i] = layout->layout;
    }

    VkPipelineLayoutCreateInfo pipeline_layout_info = (VkPipelineLayoutCreateInfo) {
        .sType = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
        .setLayoutCount = descriptor_set_layouts.len,
        .pSetLayouts = vk_ds_layouts,
        .pushConstantRangeCount = 0,
        .pPushConstantRanges = NULL,
    };

    VkPipelineLayout pipeline_layout;

    if (vkCreatePipelineLayout(logical_device, &pipeline_layout_info, NULL, &pipeline_layout) != VK_SUCCESS) {
        panic(mv_string("failed to create pipeline layout!"));
    }

    VkGraphicsPipelineCreateInfo pipeline_info = (VkGraphicsPipelineCreateInfo) {
        .sType = VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO,
        .stageCount = 2,
        .pStages = shader_stages,
        .pVertexInputState = &vertex_input_info,
        .pInputAssemblyState = &input_assembly,
        .pViewportState = &viewport_state,
        .pRasterizationState = &rasterizer,
        .pMultisampleState = &multisampling,
        .pDepthStencilState = NULL,
        .pColorBlendState = &colour_blending,
        .pDynamicState = &dynamic_states,
        .layout = pipeline_layout,
        .renderPass = surface->renderpass,
        .subpass = 0,
        .basePipelineHandle = VK_NULL_HANDLE,
        .basePipelineIndex = -1,
    };

    VkPipeline vk_pipeline;
    if (vkCreateGraphicsPipelines(logical_device, VK_NULL_HANDLE, 1, &pipeline_info, NULL, &vk_pipeline) != VK_SUCCESS) {
        panic(mv_string("failed to create graphics pipeline!"));
    }

    mem_free(vk_ds_layouts, hd_alloc);
    HedronPipeline *pipeline = mem_alloc(sizeof(HedronPipeline), hd_alloc);
    *pipeline = (HedronPipeline) {
        .pipeline = vk_pipeline,
        .layout = pipeline_layout,
    };
    return pipeline;
}

void destroy_pipeline(HedronPipeline *pipeline) {
    vkDestroyPipeline(logical_device, pipeline->pipeline, NULL);
    vkDestroyPipelineLayout(logical_device, pipeline->layout, NULL);
    mem_free(pipeline, hd_alloc);
}




// -------------------------------------------
//
// Data contract (vertex/input formats, etc.)
// 
// -------------------------------------------

uint32_t find_memory_type(uint32_t filter, VkMemoryPropertyFlags properties) {
    VkPhysicalDeviceMemoryProperties mem_properties;
    vkGetPhysicalDeviceMemoryProperties(physical_device, &mem_properties);

    for (uint32_t i = 0; i < mem_properties.memoryTypeCount; i++) {
        if (filter & (1 << i) && (mem_properties.memoryTypes[i].propertyFlags & properties) == properties) {
            return i;
        }
    }

    panic(mv_string("failed to find suitable memory type!"));
}


HedronBuffer* create_buffer(BufferType type, uint64_t size) {
    uint32_t buffer_usage = 0;
    switch (type) {
    case VertexBuffer: buffer_usage |= VK_BUFFER_USAGE_VERTEX_BUFFER_BIT;
        break;
    case IndexBuffer: buffer_usage |= VK_BUFFER_USAGE_INDEX_BUFFER_BIT;
        break;
    case UniformBuffer: buffer_usage |= VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT;
        break;
    case TransferSourceBuffer: buffer_usage |= VK_BUFFER_USAGE_TRANSFER_SRC_BIT;
        break;
    case TransferDestinationBuffer: buffer_usage |= VK_BUFFER_USAGE_TRANSFER_DST_BIT;
        break;
    }

    VkBufferCreateInfo buffer_info = (VkBufferCreateInfo){
        .sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO,
        .size = size,
        .usage = buffer_usage,
        .sharingMode = VK_SHARING_MODE_EXCLUSIVE,
    };

    VkBuffer vertex_buffer;
    if (vkCreateBuffer(logical_device, &buffer_info, NULL, &vertex_buffer) != VK_SUCCESS) {
        panic(mv_string("failed to create vulkan buffer!"));
    }

    VkMemoryRequirements mem_requirements;
    vkGetBufferMemoryRequirements(logical_device, vertex_buffer, &mem_requirements);

    VkMemoryAllocateInfo alloc_info = (VkMemoryAllocateInfo) {
        .sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
        .allocationSize = mem_requirements.size,
        .memoryTypeIndex = find_memory_type(mem_requirements.memoryTypeBits, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
    };

    VkDeviceMemory vertex_buffer_memory;
    if (vkAllocateMemory(logical_device, &alloc_info, NULL, &vertex_buffer_memory) != VK_SUCCESS) {
        panic(mv_string("failed to allocate vertex buffer memory!"));
    }

    vkBindBufferMemory(logical_device, vertex_buffer, vertex_buffer_memory, 0);

    HedronBuffer* out = mem_alloc(sizeof(HedronBuffer), hd_alloc);
    *out = (HedronBuffer) {
        .vk_buffer = vertex_buffer,
        .device_memory = vertex_buffer_memory,
        .size = size,
    };

    return out;
}

void destroy_buffer(HedronBuffer *buffer) {
    vkFreeMemory(logical_device, buffer->device_memory, NULL);
    vkDestroyBuffer(logical_device, buffer->vk_buffer, NULL);
    mem_free(buffer, hd_alloc);
}

void set_buffer_data(HedronBuffer* buffer, void* source) {
    void* data;
    vkMapMemory(logical_device, buffer->device_memory, 0, buffer->size, 0, &data);
    memcpy(data, source, buffer->size);
    vkUnmapMemory(logical_device, buffer->device_memory);
}

HedronImage* create_image(uint32_t width, uint32_t height, ImageFormat format) {
    VkImageCreateInfo imageInfo = {
        .sType = VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO,
        .imageType = VK_IMAGE_TYPE_2D,
        .extent.width = width,
        .extent.height = height,
        .extent.depth = 1,
        .mipLevels = 1,
        .arrayLayers = 1,
        .format = convert_image_format(format),
        .tiling = VK_IMAGE_TILING_OPTIMAL,
        .initialLayout = VK_IMAGE_LAYOUT_UNDEFINED,
        .usage = VK_IMAGE_USAGE_TRANSFER_DST_BIT | VK_IMAGE_USAGE_SAMPLED_BIT,
        .sharingMode = VK_SHARING_MODE_EXCLUSIVE,
        .samples = VK_SAMPLE_COUNT_1_BIT,
        .flags = 0,
    };

    VkImage vk_image;
    if (vkCreateImage(logical_device, &imageInfo, NULL, &vk_image)) {
        panic(mv_string("failed to create image"));
    }
    VkMemoryRequirements memRequirements;
    vkGetImageMemoryRequirements(logical_device, vk_image, &memRequirements);

    VkMemoryAllocateInfo alloc_info = {
        .sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
        .allocationSize = memRequirements.size,
        .memoryTypeIndex = find_memory_type(memRequirements.memoryTypeBits, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
    };

    VkDeviceMemory image_memory;
    if (vkAllocateMemory(logical_device, &alloc_info, NULL, &image_memory) != VK_SUCCESS) {
        panic(mv_string("failed to allocate image memory!"));
    }

    vkBindImageMemory(logical_device, vk_image, image_memory, 0);

    HedronImage* out = mem_alloc(sizeof(HedronImage), hd_alloc);
    *out = (HedronImage) {
        .vk_image = vk_image,
        .image_memory = image_memory,
    };
    
    return out;
}

void destroy_image(HedronImage* image) {
    vkDestroyImage(logical_device, image->vk_image, NULL);
    vkFreeMemory(logical_device, image->image_memory, NULL);
    mem_free(image, hd_alloc);
}

HedronImageView* create_image_view(HedronImage *image, ImageFormat format) {
    VkImageViewCreateInfo createInfo = (VkImageViewCreateInfo) {
        .sType = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO,
        .image = image->vk_image,

        .viewType = VK_IMAGE_VIEW_TYPE_2D,
        .format = convert_image_format(format),

        .components.r = VK_COMPONENT_SWIZZLE_IDENTITY,
        .components.g = VK_COMPONENT_SWIZZLE_IDENTITY,
        .components.b = VK_COMPONENT_SWIZZLE_IDENTITY,
        .components.a = VK_COMPONENT_SWIZZLE_IDENTITY,

        .subresourceRange.aspectMask = VK_IMAGE_ASPECT_COLOR_BIT,
        .subresourceRange.baseMipLevel = 0,
        .subresourceRange.levelCount = 1,
        .subresourceRange.baseArrayLayer = 0,
        .subresourceRange.layerCount = 1,
    };

    VkImageView vk_image_view;
    if (vkCreateImageView(logical_device, &createInfo, NULL, &vk_image_view) != VK_SUCCESS) {
        panic(mv_string("failed to create image views!"));
    }
    HedronImageView* image_view = mem_alloc(sizeof(HedronImageView), hd_alloc);
    image_view->vk_image_view = vk_image_view;
    return image_view;
}

void destroy_image_view(HedronImageView *image_view) {
    vkDestroyImageView(logical_device, image_view->vk_image_view, NULL);
    mem_free(image_view, hd_alloc);
}

HedronSampler* create_sampler() {
    VkPhysicalDeviceProperties properties;
    vkGetPhysicalDeviceProperties(physical_device, &properties);

    VkSamplerCreateInfo sampler_info = {
        .sType = VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO,
        .magFilter = VK_FILTER_LINEAR,
        .minFilter = VK_FILTER_LINEAR,
        .addressModeU = VK_SAMPLER_ADDRESS_MODE_REPEAT,
        .addressModeV = VK_SAMPLER_ADDRESS_MODE_REPEAT,
        .addressModeW = VK_SAMPLER_ADDRESS_MODE_REPEAT,
        .anisotropyEnable = VK_TRUE,
        .maxAnisotropy = properties.limits.maxSamplerAnisotropy,
        .borderColor = VK_BORDER_COLOR_INT_OPAQUE_BLACK,
        .unnormalizedCoordinates = VK_FALSE,
        .compareEnable = VK_FALSE,
        .compareOp = VK_COMPARE_OP_ALWAYS,
        .mipmapMode = VK_SAMPLER_MIPMAP_MODE_LINEAR,
        .mipLodBias = 0.0f,
        .minLod = 0.0f,
        .maxLod = 0.0f,
    };

    HedronSampler* out_sampler = mem_alloc(sizeof(HedronSampler), hd_alloc);
    if (vkCreateSampler(logical_device, &sampler_info, NULL, &out_sampler->vk_sampler) != VK_SUCCESS) {
        panic(mv_string("failed to create texture sampler!"));
    }
    return out_sampler;
}

void destroy_sampler(HedronSampler* sampler) {
    vkDestroySampler(logical_device, sampler->vk_sampler, NULL);
    mem_free(sampler, hd_alloc);
}

// -----------------------------------------------------------------------------
//
//                                Syncrhonisation
//
// -----------------------------------------------------------------------------

HedronSemaphore* create_semaphore() {
    HedronSemaphore* sem = mem_alloc(sizeof(HedronSemaphore), hd_alloc);
    VkSemaphoreCreateInfo semaphore_info = (VkSemaphoreCreateInfo) {
        .sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO,
    };

    if (vkCreateSemaphore(logical_device, &semaphore_info, NULL, &sem->semaphore) != VK_SUCCESS) {
        panic(mv_string("Semaphore creation failed!"));
    }
    return sem;
}

void destroy_semaphore(HedronSemaphore* semaphore) {
    vkDestroySemaphore(logical_device, semaphore->semaphore, NULL);
    mem_free(semaphore, hd_alloc);
}

HedronFence* create_fence() {
    HedronFence* fence = mem_alloc(sizeof(HedronFence), hd_alloc);
    VkFenceCreateInfo fence_info = (VkFenceCreateInfo) {
        .sType = VK_STRUCTURE_TYPE_FENCE_CREATE_INFO,
        .flags = VK_FENCE_CREATE_SIGNALED_BIT,
    };

    if (vkCreateFence(logical_device, &fence_info, NULL, &fence->fence) != VK_SUCCESS) {
        panic(mv_string("Fence creation failed!"));
    }
    return fence;
}
void destroy_fence(HedronFence* fence) {
    vkDestroyFence(logical_device, fence->fence, NULL);
    mem_free(fence, hd_alloc);
}

void wait_for_fence(HedronFence *fence) {
    vkWaitForFences(logical_device, 1, &fence->fence, VK_TRUE, UINT64_MAX);
}

void reset_fence(HedronFence *fence) {
    vkResetFences(logical_device, 1, &fence->fence);
}

void wait_for_device() {
    vkDeviceWaitIdle(logical_device);
}

ImageResult acquire_next_image(HedronSurface *surface, HedronSemaphore *semaphore) {
    uint32_t index;
    int result = vkAcquireNextImageKHR(logical_device, surface->swapchain, UINT64_MAX, semaphore->semaphore, VK_NULL_HANDLE, &index);
    if (result == VK_ERROR_OUT_OF_DATE_KHR) {
        return (ImageResult){.type = Resized, .image = index};
    }
    if (result != VK_SUCCESS && result != VK_SUBOPTIMAL_KHR) {
        panic(mv_string("vulkan failure in acqurie next image!"));
    }
    return (ImageResult){.type = IROk, .image = index};
}


// -------------------------------------------
//
//        Commands, Queues and Drawing
// 
// -------------------------------------------

HedronCommandPool* create_command_pool() {
    QueueFamilyIndices queues = find_queue_families(physical_device);

    if (!(queues.available & QUEUE_GRAPHICS)) {
        panic(mv_string("can't create command-pool, as device does not have graphics queue"));
    }

    VkCommandPoolCreateInfo pool_info = (VkCommandPoolCreateInfo) {
        .sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
        .flags = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,
        .queueFamilyIndex = queues.graphics_family,
    };

    VkCommandPool pool;
    if (vkCreateCommandPool(logical_device, &pool_info, NULL, &pool) != VK_SUCCESS) {
        panic(mv_string("failed to create command pool!"));
    }
    
    PtrArray command_buffers = mk_ptr_array(8, hd_alloc);

    HedronCommandPool* hd_pool = mem_alloc(sizeof(HedronCommandPool), hd_alloc);
    *hd_pool = (HedronCommandPool) {
        .pool = pool,
        .buffers = command_buffers,
    };
    return hd_pool;
}

void destroy_command_pool(HedronCommandPool* pool) {
    vkDestroyCommandPool(logical_device, pool->pool, NULL);
    for (size_t i = 0; i < pool->buffers.len; i++) {
        mem_free(pool->buffers.data[i], hd_alloc);
    }
    sdelete_ptr_array(pool->buffers);
    mem_free(pool, hd_alloc);
}

HedronCommandBuffer* create_command_buffer(HedronCommandPool* pool) {
    VkCommandBufferAllocateInfo cba_info = (VkCommandBufferAllocateInfo) {
        .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
        .commandPool = pool->pool,
        .level = VK_COMMAND_BUFFER_LEVEL_PRIMARY,
        .commandBufferCount = 1,
    };

    VkCommandBuffer buffer;
    if (vkAllocateCommandBuffers(logical_device, &cba_info, &buffer) != VK_SUCCESS) {
        panic(mv_string("failed to allocate command buffer!"));
    }


    HedronCommandBuffer* hd_buffer = mem_alloc(sizeof(HedronCommandBuffer), hd_alloc);
    *hd_buffer = (HedronCommandBuffer) {
        .buffer = buffer,
    };
    push_ptr(hd_buffer, &pool->buffers);
    return hd_buffer; 
}

void free_command_buffer(HedronCommandPool *pool, HedronCommandBuffer *buffer) {
    vkFreeCommandBuffers(logical_device, pool->pool, 1, &buffer->buffer);
}

void command_begin(HedronCommandBuffer *buffer, CommandBufferUsage usage) {
    VkCommandBufferBeginInfo begin_info = (VkCommandBufferBeginInfo) {
        .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
        .flags = usage,
        .pInheritanceInfo = NULL,
    };

    if (vkBeginCommandBuffer(buffer->buffer, &begin_info) != VK_SUCCESS) {
        panic(mv_string("failed to begin recording command buffer!"));
    }
}

void command_end(HedronCommandBuffer *buffer) {
    if (vkEndCommandBuffer(buffer->buffer) != VK_SUCCESS) {
        panic(mv_string("failed to record command buffer!"));
    }
}

void reset_command_buffer(HedronCommandBuffer *buffer) {
    vkResetCommandBuffer(buffer->buffer, 0);
}

void queue_submit(HedronCommandBuffer *buffer, PtrOption fence, SemaphoreStagePairPiList wait, AddrPiList signals) {
    VkSemaphore* wait_semaphores = mem_alloc(sizeof(VkSemaphore) * wait.len, hd_alloc);
    VkPipelineStageFlags* wait_stages = mem_alloc(sizeof(VkPipelineStageFlags) * wait.len, hd_alloc);
    for (size_t i = 0; i < wait.len; i++) {
        wait_semaphores[i] = wait.data[i].semaphore->semaphore;
        wait_stages[i] = convert_pipeline_stage(wait.data[i].stage);
    }

    VkSemaphore* signal_semaphores = mem_alloc(sizeof(VkSemaphore) * signals.len, hd_alloc);
    for (size_t i = 0; i < signals.len; i++) {
        HedronSemaphore* semaphore = signals.data[i];
        signal_semaphores[i] = semaphore->semaphore;
    }

    VkSubmitInfo submit_info = (VkSubmitInfo) {
        .sType = VK_STRUCTURE_TYPE_SUBMIT_INFO,
        .waitSemaphoreCount = wait.len,
        .pWaitSemaphores = wait_semaphores,
        .pWaitDstStageMask = wait_stages,
        .commandBufferCount = 1,
        .pCommandBuffers = &buffer->buffer,
        .signalSemaphoreCount = signals.len,
        .pSignalSemaphores = signal_semaphores,
    };

    QueueFamilyIndices indices = find_queue_families(physical_device);
    if (!(indices.available & QUEUE_GRAPHICS)) {
        panic(mv_string("graphics queue not available to submit to!"));
    }
    VkQueue graphics_queue;
    vkGetDeviceQueue(logical_device, indices.graphics_family, 0, &graphics_queue);

    HedronFence* hd_fence = fence.val;
    VkFence vk_fence = fence.type == Some ? hd_fence->fence : VK_NULL_HANDLE;
    if (vkQueueSubmit(graphics_queue, 1, &submit_info, vk_fence) != VK_SUCCESS) {
        panic(mv_string("failed to submit draw command buffer!"));
    }
    mem_free(signal_semaphores, hd_alloc);
    mem_free(wait_semaphores, hd_alloc);
    mem_free(wait_stages, hd_alloc);
}

void queue_present(HedronSurface *surface, HedronSemaphore *wait, uint32_t image_index) {
    VkSwapchainKHR swap_chains[] = {surface->swapchain};

    VkPresentInfoKHR present_info = (VkPresentInfoKHR) {
        .sType = VK_STRUCTURE_TYPE_PRESENT_INFO_KHR,
        .waitSemaphoreCount = 1,
        .pWaitSemaphores = &wait->semaphore,
        .swapchainCount = 1,
        .pSwapchains = swap_chains,
        .pImageIndices = &image_index,
    };

    // TODO: a more proper treatment when we allow graphics/present queue to be separate. 
    QueueFamilyIndices indices = find_queue_families(physical_device);
    VkQueue present_queue;
    vkGetDeviceQueue(logical_device, indices.graphics_family, 0, &present_queue);

    vkQueuePresentKHR(present_queue, &present_info);
}

void queue_wait_idle() {
    QueueFamilyIndices indices = find_queue_families(physical_device);

    VkQueue graphics_queue;
    vkGetDeviceQueue(logical_device, indices.graphics_family, 0, &graphics_queue);

    vkQueueWaitIdle(graphics_queue);
}

void command_begin_render_pass(HedronCommandBuffer* buffer, HedronSurface* surface, uint32_t image_index) {
    VkClearValue clear_colour = {{{0.0f, 0.0f, 0.0f, 1.0f}}};
    VkRenderPassBeginInfo renderpass_info = (VkRenderPassBeginInfo) {
        .sType = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO,
        .renderPass = surface->renderpass,
        .framebuffer = surface->buffers[image_index],

        .renderArea.offset = {0, 0},
        .renderArea.extent = surface->extent,
        .clearValueCount = 1,
        .pClearValues = &clear_colour,
    };

    vkCmdBeginRenderPass(buffer->buffer, &renderpass_info, VK_SUBPASS_CONTENTS_INLINE);
}

void command_end_render_pass(HedronCommandBuffer *buffer) {
    vkCmdEndRenderPass(buffer->buffer);
}

void command_pipeline_barrier(HedronCommandBuffer* commands,
                              PipelineStage source_stage,
                              PipelineStage dest_stage,
                              MemoryBarrierPiList memory_barriers,
                              BufferMemoryBarrierPiList buffer_memory_barriers,
                              ImageMemoryBarrierPiList image_memory_barriers) {
    VkMemoryBarrier* vk_barriers = mem_alloc(sizeof(VkMemoryBarrier), hd_alloc);
    VkBufferMemoryBarrier* vk_buffer_barriers = mem_alloc(sizeof(VkBufferMemoryBarrier), hd_alloc);
    VkImageMemoryBarrier* vk_image_barriers = mem_alloc(sizeof(VkImageMemoryBarrier), hd_alloc);

    for (size_t i = 0; i < memory_barriers.size; i++) {
        panic(mv_string("command-pipeline-barrier does not yet support memory-barriers"));
    }
    for (size_t i = 0; i < buffer_memory_barriers.size; i++) {
        panic(mv_string("command-pipeline-barrier does not yet support buffer-memory-barriers"));
    }
    for (size_t i = 0; i < image_memory_barriers.size; i++) {
        ImageMemoryBarrier ibarrier = image_memory_barriers.data[i];
        vk_image_barriers[i] = (VkImageMemoryBarrier) {
            .sType = VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER,
            .oldLayout = convert_image_layout(ibarrier.old_layout),
            .newLayout = convert_image_layout(ibarrier.new_layout),
            .srcQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED,
            .dstQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED,
            .image = ibarrier.image->vk_image,
            .subresourceRange.aspectMask = VK_IMAGE_ASPECT_COLOR_BIT,
            .subresourceRange.baseMipLevel = 0,
            .subresourceRange.levelCount = 1,
            .subresourceRange.baseArrayLayer = 0,
            .subresourceRange.layerCount = 1,
            .srcAccessMask = convert_access_flags(ibarrier.src_access_flags),
            .dstAccessMask = convert_access_flags(ibarrier.dest_access_flags),
        };
    }

    vkCmdPipelineBarrier(commands->buffer,
                         convert_pipeline_stage(source_stage),
                         convert_pipeline_stage(dest_stage),
                         0, // VkDependencyFlags dependencyFlags,
                         memory_barriers.len,
                         vk_barriers,
                         buffer_memory_barriers.len,
                         vk_buffer_barriers,
                         image_memory_barriers.len,
                         vk_image_barriers);

    mem_free(vk_barriers , hd_alloc);
    mem_free(vk_buffer_barriers, hd_alloc);
    mem_free(vk_image_barriers, hd_alloc);
}

void command_copy_buffer_to_image(HedronCommandBuffer *commands,
                                  HedronBuffer *buffer, HedronImage *image,
                                  uint32_t width, uint32_t height) {
    VkBufferImageCopy region = {
        .bufferOffset = 0,
        .bufferRowLength = 0,
        .bufferImageHeight = 0,

        .imageSubresource.aspectMask = VK_IMAGE_ASPECT_COLOR_BIT,
        .imageSubresource.mipLevel = 0,
        .imageSubresource.baseArrayLayer = 0,
        .imageSubresource.layerCount = 1,

        .imageOffset = {0, 0, 0},
        .imageExtent = {
            width,
            height,
            1
        },
    };

    vkCmdCopyBufferToImage(commands->buffer,
                           buffer->vk_buffer,
                           image->vk_image,
                           VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                           1,
                           &region);
}

void command_bind_descriptor_set(HedronCommandBuffer *commands,
                                 HedronPipeline *pipeline,
                                 HedronDescriptorSet *descriptor_set) {
  vkCmdBindDescriptorSets(commands->buffer, VK_PIPELINE_BIND_POINT_GRAPHICS,
                          pipeline->layout, 0,
                          1, &descriptor_set->vk_set,
                          0, NULL);
}

void command_bind_pipeline(HedronCommandBuffer *commands, HedronPipeline *pipeline) {
    vkCmdBindPipeline(commands->buffer, VK_PIPELINE_BIND_POINT_GRAPHICS, pipeline->pipeline);
}

void command_bind_vertex_buffer(HedronCommandBuffer *commands, HedronBuffer *buffer) {
    VkBuffer vertex_buffers[1] = {buffer->vk_buffer};
    VkDeviceSize offsets[1] = {0};
    vkCmdBindVertexBuffers(commands->buffer, 0, 1, vertex_buffers, offsets);
}

void command_bind_index_buffer(HedronCommandBuffer *commands, HedronBuffer *buffer, IndexFormat format) {
    vkCmdBindIndexBuffer(commands->buffer, buffer->vk_buffer, 0, (VkIndexType)format);
}

void command_set_surface(HedronCommandBuffer *buffer, HedronSurface *surface) {
    VkViewport viewport = (VkViewport) {
        .x = 0.0f,
        .y = 0.0f,
        .width = (float) surface->extent.width,
        .height = (float) surface->extent.height,
        .minDepth = 0.0f,
        .maxDepth = 1.0f,
    };
    vkCmdSetViewport(buffer->buffer, 0, 1, &viewport);

    VkRect2D scissor = (VkRect2D) {
        .offset = {0, 0},
        .extent = surface->extent,
    };
    vkCmdSetScissor(buffer->buffer, 0, 1, &scissor);
}

void command_draw(HedronCommandBuffer *commands,
                  uint32_t vertex_count, uint32_t instance_count,
                  uint32_t first_vertex, uint32_t first_instance) {
    vkCmdDraw(commands->buffer, vertex_count, instance_count, first_vertex, first_instance);
}


void command_draw_indexed(HedronCommandBuffer *commands, uint32_t index_count,
                          uint32_t instance_count, uint32_t first_index,
                          int32_t vertex_offset, uint32_t first_instance) { 
    vkCmdDrawIndexed(commands->buffer, index_count, instance_count, first_index, vertex_offset, first_instance);
}

#endif


