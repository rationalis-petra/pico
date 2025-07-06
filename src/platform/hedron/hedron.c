#include "platform/machine_info.h"
#include "platform/hedron/hedron.h"
#include "platform/signals.h"
#include "data/string.h"

#ifdef USE_VULKAN

#if OS_FAMILY == UNIX
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

struct HedronCommandPool {
    VkCommandPool pool;
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

const uint32_t num_required_extensions = 2;
const char *required_extensions[] = {
    VK_KHR_SURFACE_EXTENSION_NAME,
#if OS_FAMILY == UNIX
    VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME,
#elif OS_FAMILY == WINDOWS
    VK_KHR_WIN32_SURFACE_EXTENSION_NAME,
#else 
#error "unrecognized OS!"
#endif
};

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
   
    VkPhysicalDeviceFeatures device_features = {};
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

bool is_hedron_supported() {
    return true;
}

int init_hedron(Allocator* a) {
    hd_alloc = a;
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
        VkImageViewCreateInfo createInfo = (VkImageViewCreateInfo){};
        createInfo.sType = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
        createInfo.image = images[i];

        createInfo.viewType = VK_IMAGE_VIEW_TYPE_2D;
        createInfo.format = surface_format.format;

        createInfo.components.r = VK_COMPONENT_SWIZZLE_IDENTITY;
        createInfo.components.g = VK_COMPONENT_SWIZZLE_IDENTITY;
        createInfo.components.b = VK_COMPONENT_SWIZZLE_IDENTITY;
        createInfo.components.a = VK_COMPONENT_SWIZZLE_IDENTITY;

        createInfo.subresourceRange.aspectMask = VK_IMAGE_ASPECT_COLOR_BIT;
        createInfo.subresourceRange.baseMipLevel = 0;
        createInfo.subresourceRange.levelCount = 1;
        createInfo.subresourceRange.baseArrayLayer = 0;
        createInfo.subresourceRange.layerCount = 1;
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
    for (size_t i = 0; i < surface->num_images; i++) {
        vkDestroyImageView(logical_device, surface->image_views[i], NULL);
    }
    vkDestroySwapchainKHR(logical_device, surface->swapchain, NULL);
}

HedronSurface *create_window_surface(struct Window *window) {
    VkSurfaceKHR surface;

#if OS_FAMILY == UNIX 
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

void resize_window_surface(HedronSurface* surface, Extent extent) {
    vkDeviceWaitIdle(logical_device);

    cleanup_swap_chain(surface);
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
    mem_free(surface, hd_alloc);
}

uint32_t num_swapchain_images(HedronSurface* surface) {
    return surface->num_images;
}

HedronShaderModule* create_shader_module(U8Array code) {
    VkShaderModuleCreateInfo create_info = (VkShaderModuleCreateInfo){};
    create_info.sType = VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO;
    // TODO: check if code.len % 4 == 0??
    create_info.codeSize = code.len;
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

HedronPipeline *create_pipeline(PtrArray shaders, HedronSurface* surface) {
    if (shaders.len != 2) {
        panic(mv_string("pipeline expects exactly 2 shaders: vertex and fragment"));
    }
    HedronShaderModule* vert_shader = shaders.data[0];
    HedronShaderModule* frag_shader = shaders.data[1];

    VkPipelineShaderStageCreateInfo vertex_shader_info = (VkPipelineShaderStageCreateInfo){};
    vertex_shader_info.sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
    vertex_shader_info.stage = VK_SHADER_STAGE_VERTEX_BIT;
    vertex_shader_info.module = vert_shader->module;
    vertex_shader_info.pName = "main";

    VkPipelineShaderStageCreateInfo fragment_shader_info = (VkPipelineShaderStageCreateInfo){};
    fragment_shader_info.sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
    fragment_shader_info.stage = VK_SHADER_STAGE_FRAGMENT_BIT;
    fragment_shader_info.module = frag_shader->module;
    fragment_shader_info.pName = "main";

    VkPipelineShaderStageCreateInfo shader_stages[2] = {vertex_shader_info, fragment_shader_info};
    
    // TODO: the vertex input data layout should be provided as an argument to
    // this function! 
    VkPipelineVertexInputStateCreateInfo vertex_input_info = (VkPipelineVertexInputStateCreateInfo){};
    vertex_input_info.sType = VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO;
    vertex_input_info.vertexBindingDescriptionCount = 0;
    vertex_input_info.pVertexBindingDescriptions = NULL; // Optional
    vertex_input_info.vertexAttributeDescriptionCount = 0;
    vertex_input_info.pVertexAttributeDescriptions = NULL; // Optional

    VkPipelineInputAssemblyStateCreateInfo input_assembly = (VkPipelineInputAssemblyStateCreateInfo){};
    input_assembly.sType = VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO;
    input_assembly.topology = VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
    input_assembly.primitiveRestartEnable = VK_FALSE;

    VkViewport viewport = (VkViewport) {
        viewport.x = 0.0f,
        viewport.y = 0.0f,
        viewport.width = (float) surface->extent.width, // TODO: how to get input
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

    VkPipelineLayoutCreateInfo pipeline_layout_info = (VkPipelineLayoutCreateInfo) {
        .sType = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
        .setLayoutCount = 0,
        .pSetLayouts = NULL,
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


    HedronPipeline *pipeline = mem_alloc(sizeof(HedronPipeline), hd_alloc);
    *pipeline = (HedronPipeline) {
        .pipeline = vk_pipeline,
        .layout = pipeline_layout,
    };
    return pipeline;
}

void destroy_pipeline(HedronPipeline *pipeline) {
    vkDestroyPipelineLayout(logical_device, pipeline->layout, NULL);
    vkDestroyPipeline(logical_device, pipeline->pipeline, NULL);
    mem_free(pipeline, hd_alloc);
}

HedronCommandPool *create_command_pool() {
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
    
    HedronCommandPool* hd_pool = mem_alloc(sizeof(HedronCommandPool), hd_alloc);
    hd_pool->pool = pool;
    return hd_pool;
}

void destroy_command_pool(HedronCommandPool* pool) {
    vkDestroyCommandPool(logical_device, pool->pool, NULL);
    mem_free(pool, hd_alloc);
}

HedronCommandBuffer *create_command_buffer(HedronCommandPool* pool) {
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
    hd_buffer->buffer = buffer;
    return hd_buffer; 
}

void command_begin(HedronCommandBuffer *buffer) {
    VkCommandBufferBeginInfo begin_info = (VkCommandBufferBeginInfo) {
        .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
        .flags = 0,
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

void queue_submit(HedronCommandBuffer *buffer, HedronFence *fence, HedronSemaphore* wait, HedronSemaphore* signal) {
    VkSemaphore wait_semaphores[] = {wait->semaphore};
    VkPipelineStageFlags wait_stages[] = {VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT};
    VkSemaphore signal_semaphores[] = {signal->semaphore};

    VkSubmitInfo submit_info = (VkSubmitInfo) {
        .sType = VK_STRUCTURE_TYPE_SUBMIT_INFO,
        .waitSemaphoreCount = 1,
        .pWaitSemaphores = wait_semaphores,
        .pWaitDstStageMask = wait_stages,
        .commandBufferCount = 1,
        .pCommandBuffers = &buffer->buffer,
        .signalSemaphoreCount = 1,
        .pSignalSemaphores = signal_semaphores,
    };

    QueueFamilyIndices indices = find_queue_families(physical_device);
    if (!(indices.available & QUEUE_GRAPHICS)) {
        panic(mv_string("graphics queue not available to submit to!"));
    }
    VkQueue graphics_queue;
    vkGetDeviceQueue(logical_device, indices.graphics_family, 0, &graphics_queue);

    if (vkQueueSubmit(graphics_queue, 1, &submit_info, fence->fence) != VK_SUCCESS) {
        panic(mv_string("failed to submit draw command buffer!"));
    }
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

void command_bind_pipeline(HedronCommandBuffer *buffer, HedronPipeline *pipeline) {
    vkCmdBindPipeline(buffer->buffer, VK_PIPELINE_BIND_POINT_GRAPHICS, pipeline->pipeline);
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

void command_draw(HedronCommandBuffer *buffer,
                  uint32_t vertex_count, uint32_t instance_count,
                  uint32_t first_vertex, uint32_t first_instance) {
    vkCmdDraw(buffer->buffer, vertex_count, instance_count, first_vertex, first_instance);
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
        //return (ImageResult){.type = Resized};
        return (ImageResult){.type = IROk, .image = index};
    }
    if (result != VK_SUCCESS && result != VK_SUBOPTIMAL_KHR) {
        panic(mv_string("acqurie next image!"));
    }
    return (ImageResult){.type = IROk, .image = index};
}

#else

bool is_hedron_supported() {
    return false;
}

int init_hedron(Allocator*)
  {panic(mv_string("Hedron not supported on this build"));}
void teardown_hedron()
  {panic(mv_string("Hedron not supported on this build"));}
HedronSurface *create_window_surface(struct Window * win)
  {panic(mv_string("Hedron not supported on this build"));}
void resize_window_surface(HedronSurface* surface, Extent extent)
  {panic(mv_string("Hedron not supported on this build"));}
void destroy_window_surface(HedronSurface* surface)
  {panic(mv_string("Hedron not supported on this build"));}
uint32_t num_swapchain_images(HedronSurface* surface)
  {panic(mv_string("Hedron not supported on this build"));}
HedronShaderModule* create_shader_module(U8Array code)
  {panic(mv_string("Hedron not supported on this build"));}
void destroy_shader_module(HedronShaderModule* module)
  {panic(mv_string("Hedron not supported on this build"));}
HedronPipeline* create_pipeline(PtrArray shaders, HedronSurface* surface)
  {panic(mv_string("Hedron not supported on this build"));}
void destroy_pipeline(HedronPipeline* pipeline)
  {panic(mv_string("Hedron not supported on this build"));}
HedronCommandPool* create_command_pool()
  {panic(mv_string("Hedron not supported on this build"));}
void destroy_command_pool()
  {panic(mv_string("Hedron not supported on this build"));}
HedronCommandBuffer* create_command_buffer(HedronCommandPool* pool)
  {panic(mv_string("Hedron not supported on this build"));}
void queue_submit(HedronCommandBuffer *buffer, HedronFence *fence, HedronSemaphore* wait, HedronSemaphore* signal)
  {panic(mv_string("Hedron not supported on this build"));}
void queue_present(HedronSurface* surface, HedronSemaphore* wait, uint32_t image_index)
  {panic(mv_string("Hedron not supported on this build"));}
void command_begin(HedronCommandBuffer* buffer)
  {panic(mv_string("Hedron not supported on this build"));}
void command_end(HedronCommandBuffer* buffer)
  {panic(mv_string("Hedron not supported on this build"));}
void reset_command_buffer(HedronCommandBuffer* buffer)
  {panic(mv_string("Hedron not supported on this build"));}
void command_begin_render_pass(HedronCommandBuffer* buffer, HedronSurface* surface, uint32_t image_index)
  {panic(mv_string("Hedron not supported on this build"));}
void command_end_render_pass(HedronCommandBuffer* buffer)
  {panic(mv_string("Hedron not supported on this build"));}
void command_bind_pipeline(HedronCommandBuffer* buffer, HedronPipeline* pipeline)
  {panic(mv_string("Hedron not supported on this build"));}
void command_set_surface(HedronCommandBuffer *buffer, HedronSurface *surface)
  {panic(mv_string("Hedron not supported on this build"));}
void command_draw(HedronCommandBuffer *buffer, uint32_t vertex_count, uint32_t instance_cont, uint32_t first_vertex, uint32_t first_instance)
  {panic(mv_string("Hedron not supported on this build"));}
HedronSemaphore* create_semaphore()
  {panic(mv_string("Hedron not supported on this build"));}
void destroy_semaphore(HedronSemaphore* semaphore)
  {panic(mv_string("Hedron not supported on this build"));}
HedronFence* create_fence()
  {panic(mv_string("Hedron not supported on this build"));}
void destroy_fence(HedronFence* fence)
  {panic(mv_string("Hedron not supported on this build"));}
void wait_for_fence(HedronFence* fence)
  {panic(mv_string("Hedron not supported on this build"));}
void reset_fence(HedronFence* fence)
  {panic(mv_string("Hedron not supported on this build"));}
void wait_for_device()
  {panic(mv_string("Hedron not supported on this build"));}
uint32_t acquire_next_image(HedronSurface* surface, HedronSemaphore* semaphore)
  {panic(mv_string("Hedron not supported on this build"));}

#endif


