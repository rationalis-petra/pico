#ifdef USE_VULKAN

#include "platform/signals.h"
#include "platform/hedron/hedron.h"
#include "platform/hedron/internal.h"

#ifndef WINDOW_SYSTEM
const uint32_t num_required_extensions = 1;
const char *required_extensions[] = {
    VK_KHR_SURFACE_EXTENSION_NAME,
    //VK_KHR_SURFACE_MAINTENANCE_1_EXTENSION_NAME,
};
#else
const uint32_t num_required_extensions = 2;
const char *required_extensions[] = {
    VK_KHR_SURFACE_EXTENSION_NAME,
    //VK_KHR_SURFACE_MAINTENANCE_1_EXTENSION_NAME,
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

HdPtrResult create_hedron_instance(Allocator* a) {
    if (enable_validation && !check_validation_layer_support(a)) {
          panic(mv_string("Expected validation layer support, but none present!"));
    }

    VkApplicationInfo app_info = (VkApplicationInfo){};
    app_info.sType = VK_STRUCTURE_TYPE_APPLICATION_INFO;
    app_info.pApplicationName = "Relic";
    app_info.applicationVersion = VK_MAKE_VERSION(1, 0, 0);
    app_info.pEngineName = "No Engine";
    app_info.engineVersion = VK_MAKE_VERSION(1, 0, 0);

    /**
     * Note: As hedron seeks to vastly simplify the Graphis programming API
     * surface compared to vulkan etc. we have a high minimum version
     * requirement, as this gives us access to more general features that mean
     * we can exporse less features via hedron without loosing expressive power.
     * This does, however, limit us to GPUs released post ~2022, and iGPUs
     * released post ~2024.
     * 
     */
    app_info.apiVersion = VK_API_VERSION_1_4;

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

    VkInstance out;
    VkResult result = vkCreateInstance(&create_info, NULL, &out);
    if (result != VK_SUCCESS) {
        return (HdPtrResult) {.type = Err, .error = convert_error_type(result)};
    };
    HdInstance* instance = mem_alloc(sizeof(HdInstance), a);
    *instance = (HdInstance) {
        .vk_instance = out,
        .gpa = a,
    };
    return (HdPtrResult) {
        .type = Ok,
        .val = instance,
    };
}

void teardown_hedron_instance(HdInstance* instance) {
    vkDestroyInstance(instance->vk_instance, NULL);
    if (instance->devices) {
        mem_free(instance->devices, instance->gpa);
        instance->devices = NULL;
    }
    mem_free(instance, instance->gpa);
}


#endif
