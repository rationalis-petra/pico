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
};

bool is_hedron_supported() {
    return true;
}

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
    //VK_KHR_SWAPCHAIN_EXTENSION_NAME,
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

QueueFamilyIndices find_queue_families(VkPhysicalDevice device, Allocator* a) {
    QueueFamilyIndices indices = (QueueFamilyIndices){};
    uint32_t queue_family_count = 0;
    vkGetPhysicalDeviceQueueFamilyProperties(device, &queue_family_count, NULL);

    VkQueueFamilyProperties* queue_families = mem_alloc(queue_family_count * sizeof(VkQueueFamilyProperties), a);
    vkGetPhysicalDeviceQueueFamilyProperties(device, &queue_family_count, queue_families);

    for (uint32_t i = 0; i < queue_family_count; i++) {
        VkQueueFamilyProperties queue_family = queue_families[i];
        if (queue_family.queueFlags & VK_QUEUE_GRAPHICS_BIT) {
            indices.available |= QUEUE_GRAPHICS;
            indices.graphics_family = i;
        }

        VkBool32 present_support = false;
        // TODO: postpone device selection until AFTER surface creation!
        //vkGetPhysicalDeviceSurfaceSupportKHR(device, i, surface, &present_support);

        if (present_support) {
            //indices.present_family = i;
            indices.available |= QUEUE_PRESENT;
        }
    }

    mem_free(queue_families, a);
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

    QueueFamilyIndices indices = find_queue_families(device, a);
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
    QueueFamilyIndices indices = find_queue_families(physical_device, a);
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

HedronSurface *create_window_surface(struct Window *window) {
    VkSurfaceKHR surface;

#if OS_FAMILY == UNIX 
    VkWaylandSurfaceCreateInfoKHR create_info = (VkWaylandSurfaceCreateInfoKHR){};
    create_info.sType = VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR;
    create_info.display = get_wl_display();
    create_info.surface = window->surface;

    VkResult result = vkCreateWaylandSurfaceKHR(rl_vk_instance, &create_info, NULL, &surface);
    if (result != VK_SUCCESS) return NULL;

#elif OS_FAMILY == WINDOWS 
#else
#error "unrecognized OS"
#endif

    HedronSurface* hd_surface = mem_alloc(sizeof(HedronSurface), hd_alloc);
    *hd_surface = (HedronSurface) {
        .surface = surface,
    };
    return hd_surface;
}

void destroy_window_surface(HedronSurface *surface) {
    vkDestroySurfaceKHR(rl_vk_instance, surface->surface, NULL);
    mem_free(surface, hd_alloc);
}

#else

bool is_hedron_supported() {
    return false;
}

int init_hedron(Allocator*) {
    panic(mv_string("Hedron not supported on this build"));
}

void teardown_hedron() {
    panic(mv_string("Hedron not supported on this build"));
}

HedronSurface *create_hedron_window_surface(struct Window *) {
    panic(mv_string("Hedron not supported on this build"));
}

void destroy_window_surface(HedronSurface*) {
    panic(mv_string("Hedron not supported on this build"));
}

#endif


