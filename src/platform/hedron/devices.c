#ifdef USE_VULKAN

#include <string.h>
#include "platform/signals.h"
#include "platform/hedron/hedron.h"
#include "platform/hedron/internal.h"

// 
// Device management, see hedron.h any functions marked with static are note
// exposed as part of the public API
//   
//  

const uint32_t num_required_device_extensions = 4;
const char *required_device_extensions[] = {
    VK_KHR_SWAPCHAIN_EXTENSION_NAME,
    VK_EXT_SHADER_OBJECT_EXTENSION_NAME,
    VK_EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME,
    VK_EXT_DESCRIPTOR_BUFFER_EXTENSION_NAME, // NGAPI
    //VK_EXT_MESH_SHADER_EXTENSION_NAME, // Note: not supported on laptop :(
};

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
    VkPhysicalDeviceDescriptorBufferFeaturesEXT desc_buffer_features = {
        .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_FEATURES_EXT,
        .pNext = NULL
    };
    VkPhysicalDeviceVulkan14Features supported_features_14 = {
      .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_4_FEATURES,
      .pNext = &desc_buffer_features,
    };
    VkPhysicalDeviceVulkan13Features supported_features_13 = {
      .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_3_FEATURES,
      .pNext = &supported_features_14,
    };

    VkPhysicalDeviceVulkan12Features supported_features_12 = {
      .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES,
      .pNext = &supported_features_13,
    };
    VkPhysicalDeviceVulkan11Features supported_features_11 = {
      .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_FEATURES,
      .pNext = &supported_features_12,
    };
    VkPhysicalDeviceFeatures2 supported_features = {
      .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2,
      .pNext = &supported_features_11,
    };
    vkGetPhysicalDeviceFeatures2(device, &supported_features);

    //VkPhysicalDeviceProperties device_properties;
    //vkGetPhysicalDevicePropertiers(device, &device_properties);

    const bool extensions_supported = check_device_extension_support(device, a);

    // TODO: move some (or all) of these checks into the hedron API
    return (desc_buffer_features.descriptorBuffer
            && supported_features_13.dynamicRendering
            && supported_features_12.timelineSemaphore // needed?? 
            && supported_features_12.descriptorIndexing  // NGAPI
            && supported_features_12.bufferDeviceAddress // NGAPI
            && supported_features_12.descriptorBindingPartiallyBound
            && supported_features_12.descriptorBindingVariableDescriptorCount
            && supported_features_13.synchronization2
            && extensions_supported);
}

PtrSlice get_physical_devices(HdInstance* instance, Allocator* a) {
    if (instance->devices) {
        PtrSlice suitable_devices = {
            .len = instance->num_devices,
            .data = mem_alloc( instance->num_devices * sizeof(HdPhysicalDevice*), a),
        };
        for (size_t i = 0; i < instance->num_devices; i++) {
            suitable_devices.data[i] = &instance->devices[i];
        }
        return suitable_devices;
    } else {
        uint32_t device_count = 0;
        vkEnumeratePhysicalDevices(instance->vk_instance, &device_count, NULL);

        VkPhysicalDevice* devices = mem_alloc(device_count * sizeof(VkPhysicalDevice), a);
        vkEnumeratePhysicalDevices(instance->vk_instance, &device_count, devices);

        size_t num_suitable_devices = 0;
        // TODO (FEAT): score devices & pick "best" device.
        for (size_t i = 0; i < device_count; i++) {
            if (is_device_suitable(devices[i], a)) {
                num_suitable_devices++;
            }
        }

        HdPhysicalDevice* suitable_devices = mem_alloc( num_suitable_devices * sizeof(HdPhysicalDevice), a);
        size_t suitable_device_index = 0;
        for (size_t i = 0; i < device_count; i++) {
            if (is_device_suitable(devices[i], a)) {
                suitable_devices[suitable_device_index] = (HdPhysicalDevice) {
                    .device = devices[i],
                };
                suitable_device_index++;
            }
        }

        mem_free(devices, a);
        instance->num_devices = num_suitable_devices;
        instance->devices = suitable_devices;
        return get_physical_devices(instance, a);
    }
}

uint32_t get_graphics_queue(VkPhysicalDevice device, Allocator* a) {
    uint32_t queue_family_count = 0;
    vkGetPhysicalDeviceQueueFamilyProperties2(device, &queue_family_count, NULL);

    VkQueueFamilyProperties2* queue_families = mem_alloc(queue_family_count * sizeof(VkQueueFamilyProperties2), a);
    for (size_t i = 0; i < queue_family_count; i++) {
      queue_families[i] = (VkQueueFamilyProperties2) {
          .sType = VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2,
      };
    }
    vkGetPhysicalDeviceQueueFamilyProperties2(device, &queue_family_count, queue_families);

    bool suitable_queue = false;
    uint32_t found_queue;
    for (uint32_t i = 0; i < queue_family_count; i++) {
        // TODO: There is checking that this specific queue can support swapping to
        //       the given surface! We probably want to abstract that into a check
        //       called by a hedron user 
        //       use vkGetPhysicalDeviceSurfaceSupportKHR

        VkQueueFamilyProperties2 queue_family = queue_families[i];
        if (queue_family.queueFamilyProperties.queueFlags & VK_QUEUE_GRAPHICS_BIT) {
            suitable_queue = true;
            found_queue = i;
        }
    }
    mem_free(queue_families, a);
    if (!suitable_queue) {
        panic(mv_string("Cannot get graphics queue for device!"));
    }

    return found_queue;
}

HdPtrResult create_logical_device(HdPhysicalDevice* device, HdInstance* instance) {
// 1. Prepare descriptor buffer properties struct
    VkPhysicalDeviceDescriptorBufferPropertiesEXT desc_buffer_props = {
        .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_PROPERTIES_EXT,
        .pNext = NULL
    };

    // 2. Chain into standard PhysicalDeviceProperties2
    VkPhysicalDeviceProperties2 device_props = {
        .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2,
        .pNext = &desc_buffer_props
    };

    // 3. Query physical device
    vkGetPhysicalDeviceProperties2(device->device, &device_props);
    uint64_t max_sampled_images = desc_buffer_props.maxResourceDescriptorBufferBindings;

    VkPhysicalDeviceDescriptorBufferFeaturesEXT desc_buffer_features = {
        .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_FEATURES_EXT,
        .descriptorBuffer = VK_TRUE,
        .pNext = NULL
    };
    VkPhysicalDeviceVulkan14Features features_14 = {
      .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_4_FEATURES,
      .pNext = &desc_buffer_features,
    };
    VkPhysicalDeviceVulkan13Features features_13 = {
      .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_3_FEATURES,
      .pNext = &features_14,
      .synchronization2 = VK_TRUE,
      .dynamicRendering = VK_TRUE,
    };

    VkPhysicalDeviceVulkan12Features features_12 = {
      .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES,
      .pNext = &features_13,
      .timelineSemaphore = VK_TRUE,
      .descriptorIndexing = VK_TRUE,
      .bufferDeviceAddress = VK_TRUE,
      .descriptorBindingPartiallyBound = VK_TRUE,
      .descriptorBindingVariableDescriptorCount = VK_TRUE,
    };
    VkPhysicalDeviceFeatures2 features = {
      .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2,
      .pNext = &features_12,
    };

    size_t graphics_family = get_graphics_queue(device->device, instance->gpa);

    float queue_priority = 1.0f;
    VkDeviceQueueCreateInfo queue_create_info = {
        .sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO,
        .queueFamilyIndex = graphics_family,
        .queueCount = 1,
        .pQueuePriorities = &queue_priority,
    };

    VkDeviceCreateInfo create_info = {
        .sType = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO,
        .pNext = &features,

        .queueCreateInfoCount = 1,
        .pQueueCreateInfos = &queue_create_info,

        // Note: technically, in modern Vulkan implementations, this will probably do nothing
        // However, it is a good idea to do this anyway as it allows us to support validation
        // layers on older vulkan implementations.
        .enabledExtensionCount = num_required_device_extensions,
        .ppEnabledExtensionNames = required_device_extensions,
    };
    
    VkDevice vk_ldevice;
    VkResult res = vkCreateDevice(device->device, &create_info, NULL, &vk_ldevice);
    CHECK_RESULT(res);

    HdLogicalDevice* ldevice = mem_alloc(sizeof(HdLogicalDevice), instance->gpa);
    *ldevice = (HdLogicalDevice) {
        .device = vk_ldevice,
        .physical_device = device->device,
        .gpa = instance->gpa,

        .allocations = mk_hdalloc_array(8, instance->gpa),

        .max_sampled_images = max_sampled_images,
        .compute_pipeline_layout = VK_NULL_HANDLE,
        .graphics_pipeline_layout = VK_NULL_HANDLE,
    };

    initialize_pipeline_layouts(ldevice);
    return (HdPtrResult) {.type = Ok, .val = ldevice};
}

void destroy_logical_device(HdLogicalDevice* device) {
    deinitialize_pipeline_layouts(device);
    vkDestroyDevice(device->device, NULL);
    // TODO: make this a debug only panic/add debugging facility
    if (device->allocations.len != 0) {
        panic(mv_string("You haven't freed all device/shared memory that was allocated."));
    }
    sdelete_hdalloc_array(device->allocations);
    mem_free(device, device->gpa);
}


#endif
