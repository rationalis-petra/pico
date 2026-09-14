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

// TODO: add to extra/detail fiel?
uint32_t count_leading_zeros(uint32_t val) {
  return __builtin_clz(val);
}

const uint32_t num_required_device_extensions = 9;
const char *required_device_extensions[] = {
    // Presentation extensions
    VK_KHR_SWAPCHAIN_EXTENSION_NAME,
    VK_KHR_SWAPCHAIN_MAINTENANCE_1_EXTENSION_NAME,
    VK_KHR_SHADER_UNTYPED_POINTERS_EXTENSION_NAME,

    // Other extensions
    VK_KHR_MAINTENANCE_5_EXTENSION_NAME,
    VK_KHR_DEVICE_ADDRESS_COMMANDS_EXTENSION_NAME,
    VK_EXT_SHADER_OBJECT_EXTENSION_NAME,
    VK_EXT_EXTENDED_DYNAMIC_STATE_EXTENSION_NAME,
    VK_EXT_DESCRIPTOR_BUFFER_EXTENSION_NAME, 
    VK_EXT_DESCRIPTOR_HEAP_EXTENSION_NAME,
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

typedef struct {
    VkPhysicalDeviceSwapchainMaintenance1FeaturesKHR swapchain_maintenance1;
    VkPhysicalDeviceShaderUntypedPointersFeaturesKHR untyped_pointers;
    VkPhysicalDeviceDescriptorHeapFeaturesEXT descriptor_heap;
    VkPhysicalDeviceDescriptorBufferFeaturesEXT desc_buffer_features;

    VkPhysicalDeviceVulkan14Features vulkan_14;
    VkPhysicalDeviceVulkan13Features vulkan_13;
    VkPhysicalDeviceVulkan12Features vulkan_12;
    VkPhysicalDeviceVulkan11Features vulkan_11;
    VkPhysicalDeviceFeatures2 base;
} QueriedFeatures;

void populate_queried_features(QueriedFeatures* features) {
    *features = (QueriedFeatures) {
        .swapchain_maintenance1 = {
            .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SWAPCHAIN_MAINTENANCE_1_FEATURES_KHR,
            .pNext = NULL,
        },
        .untyped_pointers = {
            .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_UNTYPED_POINTERS_FEATURES_KHR,
            .pNext = &features->swapchain_maintenance1,
        },
        .descriptor_heap = {
            .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_HEAP_FEATURES_EXT,
            .pNext = &features->untyped_pointers,
        },
        .desc_buffer_features = {
            .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_FEATURES_EXT,
            .pNext = &features->descriptor_heap,
        },

        .vulkan_14 = {
            .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_4_FEATURES,
            .pNext = &features->desc_buffer_features,
        },
        .vulkan_13 = {
            .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_3_FEATURES,
            .pNext = &features->vulkan_14,
        },
        .vulkan_12 = {
            .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES,
            .pNext = &features->vulkan_13,
        },
        .vulkan_11 = {
            .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_FEATURES,
            .pNext = &features->vulkan_12,
        },
        .base = {
            .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2,
            .pNext = &features->vulkan_11,
        },
    };
}

bool device_supports_features(VkPhysicalDevice device, Allocator* a) {
    QueriedFeatures features; 
    populate_queried_features(&features);
    vkGetPhysicalDeviceFeatures2(device, &features.base);

    const bool extensions_supported = check_device_extension_support(device, a);

    return (features.swapchain_maintenance1.swapchainMaintenance1
            && features.untyped_pointers.shaderUntypedPointers
            && features.desc_buffer_features.descriptorBuffer
            && features.descriptor_heap.descriptorHeap
            && features.vulkan_14.maintenance5
            && features.vulkan_13.dynamicRendering
            && features.vulkan_13.synchronization2
            && features.vulkan_12.timelineSemaphore
            && features.vulkan_12.scalarBlockLayout
            && features.vulkan_12.descriptorIndexing
            && features.vulkan_12.bufferDeviceAddress
            && features.vulkan_12.descriptorBindingPartiallyBound
            && features.vulkan_12.descriptorBindingVariableDescriptorCount
            && features.vulkan_11.storageBuffer16BitAccess
            && extensions_supported);
}

bool check_and_populate_physical_device(VkPhysicalDevice device, Allocator* a, HdPhysicalDevice* out) {
  if (!device_supports_features(device, a))
    return false;
  *out = (HdPhysicalDevice){};
  out->device = device;
  vkGetPhysicalDeviceMemoryProperties(device, &out->memory_properties);

  out->heap_properties = (VkPhysicalDeviceDescriptorHeapPropertiesEXT) {
    .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_HEAP_PROPERTIES_EXT
  };
  VkPhysicalDeviceProperties2 properties2 = {
    .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2,
    .pNext = &out->heap_properties,
  };
  vkGetPhysicalDeviceProperties2(device, &properties2);
  out->properties = properties2.properties;

  return true;
};


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

        HdPhysicalDevice* suitable_devices = mem_alloc( device_count * sizeof(HdPhysicalDevice), a);
        size_t num_suitable_devices = 0;
        // TODO (FEAT): score devices & pick "best" device.
        for (size_t i = 0; i < device_count; i++) {
          if (check_and_populate_physical_device(devices[i], a, &suitable_devices[num_suitable_devices])) {
                num_suitable_devices++;
            }
        }

        mem_free(devices, a);
        instance->num_devices = num_suitable_devices;
        instance->devices = suitable_devices;

        // We have now locally memoized the physical devices; call the function
        // again to return a a copy of the memoized devices.

        return get_physical_devices(instance, a);
    }
}

/** 
 * LOGICAL DEVICE STARTS HERE
 */

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

void populate_device_functions(HdLogicalDevice* device) {
    VkDevice vkdevice = device->device;
    device->fns = (DeviceFunctions) {
      .vkWriteSamplerDescriptorsEXT = (PFN_vkWriteSamplerDescriptorsEXT)vkGetDeviceProcAddr(vkdevice, "vkWriteSamplerDescriptorsEXT"),
      .vkWriteResourceDescriptorsEXT = (PFN_vkWriteResourceDescriptorsEXT)vkGetDeviceProcAddr(vkdevice, "vkWriteResourceDescriptorsEXT"),
      .vkCmdBindSamplerHeapEXT=  (PFN_vkCmdBindSamplerHeapEXT)vkGetDeviceProcAddr(vkdevice, "vkCmdBindSamplerHeapEXT"),
      .vkCmdBindResourceHeapEXT = (PFN_vkCmdBindResourceHeapEXT)vkGetDeviceProcAddr(vkdevice, "vkCmdBindResourceHeapEXT"),
      .vkCmdPushDataEXT = (PFN_vkCmdPushDataEXT)vkGetDeviceProcAddr(vkdevice, "vkCmdPushDataEXT"),
      .vkCmdBindIndexBuffer3KHR = (PFN_vkCmdBindIndexBuffer3KHR)vkGetDeviceProcAddr(vkdevice, "vkCmdBindIndexBuffer3KHR"),
      .vkCmdDrawIndirect2KHR = (PFN_vkCmdDrawIndirect2KHR)vkGetDeviceProcAddr(vkdevice, "vkCmdDrawIndirect2KHR"),
      .vkCmdDrawIndexedIndirect2KHR = (PFN_vkCmdDrawIndexedIndirect2KHR)vkGetDeviceProcAddr(vkdevice, "vkCmdDrawIndexedIndirect2KHR"),
      .vkCmdDispatchIndirect2KHR = (PFN_vkCmdDispatchIndirect2KHR)vkGetDeviceProcAddr(vkdevice, "vkCmdDispatchIndirect2KHR"),
    };
}

void include_texture_heap_alignment(HdLogicalDevice* device, const VkMemoryRequirements requirements) {
    if (requirements.alignment > device->texture_heap_alignment)
        device->texture_heap_alignment = requirements.alignment;
}

bool fits_image_format_properties(const VkImageCreateInfo image_info, const VkImageFormatProperties properties) {
    return image_info.extent.width <= properties.maxExtent.width &&
           image_info.extent.height <= properties.maxExtent.height &&
           image_info.extent.depth <= properties.maxExtent.depth &&
           image_info.mipLevels <= properties.maxMipLevels &&
           image_info.arrayLayers <= properties.maxArrayLayers;
}


bool supports_image_create_info(HdPhysicalDevice* device, const VkImageCreateInfo image_info, VkImageFormatProperties* output) {
    const VkPhysicalDeviceImageFormatInfo2 format_info = {
        .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2,
        .format = image_info.format,
        .type = image_info.imageType,
        .tiling = image_info.tiling,
        .usage = image_info.usage,
        .flags = image_info.flags,
    };
    VkImageFormatProperties2 properties = {
        .sType = VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2,
    };
    const VkResult result = vkGetPhysicalDeviceImageFormatProperties2(device->device, &format_info, &properties);
    if (result == VK_ERROR_FORMAT_NOT_SUPPORTED)
        return false;
    //require_vk(result);
    if (output)
        *output = properties.imageFormatProperties;
    return fits_image_format_properties(image_info, properties.imageFormatProperties);
}

bool select_texture_memory_type(VkPhysicalDevice device, HdLogicalDevice* hd_device) {
  HdPhysicalDevice* ph_device = hd_device->physical_device;
  const VkFormatFeatureFlags2 color_features = hd_device->format_features[(uint32_t)(Format_RGBA8_UNorm)];
    if ((color_features & VK_FORMAT_FEATURE_2_SAMPLED_IMAGE_BIT) == 0)
        return false;
    // Probes cover DCC-capable color, broad 3D, and sampled depth layouts.
    // Resource Memory Association makes the color mask common to ordinary optimal-tiled images. Intersect every public depth/stencil format below.
    const uint32_t probe_2d_size = ph_device->properties.limits.maxImageDimension2D < 2048
                                       ? ph_device->properties.limits.maxImageDimension2D
                                       : 2048;
    VkImageUsageFlags color_usage = VK_IMAGE_USAGE_SAMPLED_BIT;
    if ((color_features & VK_FORMAT_FEATURE_2_COLOR_ATTACHMENT_BIT) != 0)
        color_usage |= VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT;
    VkImageCreateInfo image_info = {
        .sType = VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO,
        .imageType = VK_IMAGE_TYPE_2D,
        .format = VK_FORMAT_R8G8B8A8_UNORM,
        .extent = {.width = probe_2d_size, .height = probe_2d_size, .depth = 1},
        .mipLevels = 1,
        .arrayLayers = 1,
        .samples = VK_SAMPLE_COUNT_1_BIT,
        .tiling = VK_IMAGE_TILING_OPTIMAL,
        .usage = color_usage,
        .sharingMode = VK_SHARING_MODE_EXCLUSIVE,
        .initialLayout = VK_IMAGE_LAYOUT_UNDEFINED,
    };
    if (!supports_image_create_info(ph_device, image_info, NULL)) {
        image_info.usage = VK_IMAGE_USAGE_SAMPLED_BIT;
        if (!supports_image_create_info(ph_device, image_info, NULL))
            return false;
    }
    const VkMemoryRequirements colour_requirements = image_memory_requirements(hd_device, image_info);
    uint32_t memory_type_bits = colour_requirements.memoryTypeBits;
    include_texture_heap_alignment(hd_device, colour_requirements);

    const HdTextureUsage broad_texture_usage = UsageSampled | UsageStorage | UsageTransferDestination;
    const VkFormatFeatureFlags2 broad_features = hd_device->format_features[(uint32_t)(Format_RGBA32_Float)];
    const VkFormatFeatureFlags2 broad_required_features = required_format_features(broad_texture_usage);
    if ((broad_features & broad_required_features) == broad_required_features) {
        const uint32_t probe_3d_size = ph_device->properties.limits.maxImageDimension3D < 2048
                                           ? ph_device->properties.limits.maxImageDimension3D
                                           : 2048;
        image_info.imageType = VK_IMAGE_TYPE_3D;
        image_info.format = VK_FORMAT_R32G32B32A32_SFLOAT;
        image_info.extent = (VkExtent3D) {.width = probe_3d_size, .height = probe_3d_size, .depth = probe_3d_size < 4 ? probe_3d_size : 4};
        image_info.mipLevels = (32u - count_leading_zeros(probe_3d_size));
        image_info.usage = VK_IMAGE_USAGE_SAMPLED_BIT | VK_IMAGE_USAGE_STORAGE_BIT | VK_IMAGE_USAGE_TRANSFER_DST_BIT;
        if (supports_image_create_info(ph_device, image_info, NULL))
            include_texture_heap_alignment(hd_device, image_memory_requirements(hd_device, image_info));
    }

    const HdFormat depth_stencil_formats[] = {
        Format_D16_UNorm,
        Format_D24_UNorm_S8_UInt,
        Format_D32_Float,
        Format_S8_UInt,
        Format_D32_Float_S8_UInt,
    };
    const VkFormatFeatureFlags2 storage_features = required_format_features(UsageStorage);
    for (size_t i = 0; i < 5; i++) {
      HdFormat format = depth_stencil_formats[i];
      const VkFormatFeatureFlags2 features = hd_device->format_features[(uint32_t)format];
        const bool combined = has_depth_aspect(format) && has_stencil_aspect(format);
        VkImageUsageFlags compatibility_usage = 0;
        if ((features & VK_FORMAT_FEATURE_2_SAMPLED_IMAGE_BIT) != 0) compatibility_usage = VK_IMAGE_USAGE_SAMPLED_BIT;
        else if ((features & VK_FORMAT_FEATURE_2_DEPTH_STENCIL_ATTACHMENT_BIT) != 0) compatibility_usage = VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT;
        else if ((features & storage_features) == storage_features) compatibility_usage = VK_IMAGE_USAGE_STORAGE_BIT;
        else if (!combined && (features & VK_FORMAT_FEATURE_2_TRANSFER_SRC_BIT) != 0) compatibility_usage = VK_IMAGE_USAGE_TRANSFER_SRC_BIT;
        else if (!combined && (features & VK_FORMAT_FEATURE_2_TRANSFER_DST_BIT) != 0) compatibility_usage = VK_IMAGE_USAGE_TRANSFER_DST_BIT;
        if (compatibility_usage != 0)
        {
            image_info.imageType = VK_IMAGE_TYPE_2D;
            image_info.format = format_to_vk(format);
            image_info.extent = (VkExtent3D) {.width = 1, .height = 1, .depth = 1};
            image_info.mipLevels = 1;
            image_info.usage = compatibility_usage;
            VkImageFormatProperties compatibility_properties = {};
            if (supports_image_create_info(ph_device, image_info, &compatibility_properties)) {
                image_info.extent = (VkExtent3D) {.width = 512, .height = 512, .depth = 1};
                if ((features & VK_FORMAT_FEATURE_2_SAMPLED_IMAGE_BIT) != 0 && (features & VK_FORMAT_FEATURE_2_DEPTH_STENCIL_ATTACHMENT_BIT) != 0)
                    image_info.usage = VK_IMAGE_USAGE_SAMPLED_BIT | VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT;

                bool supported = image_info.usage == compatibility_usage
                  ? fits_image_format_properties(image_info, compatibility_properties)
                  : supports_image_create_info(ph_device, image_info, NULL);
                if (!supported && image_info.usage != compatibility_usage)
                {
                    image_info.usage = compatibility_usage;
                    supported = fits_image_format_properties(image_info, compatibility_properties);
                }
                if (!supported)
                {
                    image_info.extent = (VkExtent3D) {.width = 1, .height = 1, .depth = 1};
                    image_info.usage = compatibility_usage;
                }

                const VkMemoryRequirements requirements = image_memory_requirements(hd_device, image_info);
                memory_type_bits &= requirements.memoryTypeBits;
                include_texture_heap_alignment(hd_device, requirements);
            }
        }
    }
    return find_memory_type(memory_type_bits,
                            VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, 0, 1,
                            &hd_device->texture_memory_type,
                            VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT, hd_device);
}

VkFormatFeatureFlags2 optimal_format_features(VkPhysicalDevice physical_device, HdFormat format) {
    VkFormatProperties3 properties3 = {
        .sType = VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_3,
    };
    VkFormatProperties2 properties2 = {
        .sType = VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2,
        .pNext = &properties3,
    };
    vkGetPhysicalDeviceFormatProperties2(physical_device, format_to_vk(format), &properties2);
    return properties3.optimalTilingFeatures;
}

HdPtrResult create_logical_device(HdPhysicalDevice* device, HdInstance* instance) {
    // Enable feature on physical device features chain during device creation
    // Chain deviceFeatures2 into VkDeviceCreateInfo::pNext
    QueriedFeatures features;
    populate_queried_features(&features);
    features.swapchain_maintenance1.swapchainMaintenance1 = VK_TRUE;
    features.untyped_pointers.shaderUntypedPointers = VK_TRUE;
    features.descriptor_heap.descriptorHeap = VK_TRUE;
    features.desc_buffer_features.descriptorBuffer = VK_TRUE;

    features.vulkan_14.maintenance5 = VK_TRUE;
    features.vulkan_13.synchronization2 = VK_TRUE;
    features.vulkan_13.dynamicRendering = VK_TRUE;
    features.vulkan_12.timelineSemaphore = VK_TRUE;
    features.vulkan_12.descriptorIndexing = VK_TRUE;
    features.vulkan_12.bufferDeviceAddress = VK_TRUE;
    features.vulkan_12.scalarBlockLayout = VK_TRUE;
    features.vulkan_12.descriptorBindingPartiallyBound = VK_TRUE;
    features.vulkan_12.descriptorBindingVariableDescriptorCount = VK_TRUE;
    features.vulkan_11.storageBuffer16BitAccess = VK_TRUE;

    // TODO: move this to the physical device being populated.
    uint32_t graphics_family = get_graphics_queue(device->device, instance->gpa);

    float queue_priority = 1.0f;
    VkDeviceQueueCreateInfo queue_create_info = {
        .sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO,
        .queueFamilyIndex = graphics_family,
        .queueCount = 1,
        .pQueuePriorities = &queue_priority,
    };

    VkDeviceCreateInfo create_info = {
        .sType = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO,
        .pNext = &features.base,

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

    VkDeviceQueueInfo2 queue_info = {
        .sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2,
        .flags = 0,
        .queueFamilyIndex = graphics_family,
        .queueIndex = 0,
    };


    VkFormatFeatureFlags2* format_features = mem_alloc(sizeof(VkFormatFeatureFlags2) * FORMAT_COUNT, instance->gpa);
    for (uint32_t value = 0; value < FORMAT_COUNT; ++value) {
      format_features[value] = optimal_format_features(device->device, (HdFormat)value);
    }

    HdLogicalDevice* ldevice = mem_alloc(sizeof(HdLogicalDevice), instance->gpa);
    *ldevice = (HdLogicalDevice) {
        .device = vk_ldevice,
        .physical_device = device,
        .gpa = instance->gpa,

        .swapchains = mk_ptr_array(2, instance->gpa),
        .usable_buffers = mk_ptr_array(8, instance->gpa),
        .pending_buffers = mk_sem_bufs_amap(8, instance->gpa),
        .format_features = format_features,
    };

    if (!select_texture_memory_type(device->device, ldevice)) {
      panic(mv_string("TODO: move this to device filtering!"));
    }
    populate_device_functions(ldevice);

    VkQueue vk_queue = VK_NULL_HANDLE;
    vkGetDeviceQueue2(vk_ldevice, &queue_info, &vk_queue);
    HdQueue queue = {
        .queue = vk_queue,
        .device = ldevice,
    };
    ldevice->queue = queue;

    // Populate Capabilities (exposed to user)
    ldevice->capabilities = (HdDeviceCapabilities) {
        .name = mv_string(device->properties.deviceName),
        .max_push_data_size = device->heap_properties.maxPushDataSize,
        .texture_heap_alignment = ldevice->texture_heap_alignment,
        .texture_descriptor_size = device->heap_properties.imageDescriptorSize,
        .sampler_descriptor_size = device->heap_properties.samplerDescriptorSize,
        .timestamp_period_ns = device->properties.limits.timestampPeriod,
        .sub_texel_precision_bits = device->properties.limits.subTexelPrecisionBits,
        //.texture_compression_bc = device->texture_compression_bc,
        //.texture_compression_astc = device->texture_compression_astc,
        //.storage_input_output16 = device->storage_input_output16,
    };

    return (HdPtrResult) {.type = Ok, .val = ldevice};
}

void destroy_logical_device(HdLogicalDevice* device) {
    // TODO: make this a debug only panic/add debugging facility
    for (size_t i = 0; i < device->usable_buffers.len; i++) {
        HdCommandBuffer* buffer = device->usable_buffers.data[i];
        vkDestroyCommandPool(device->device, buffer->pool, NULL);
        mem_free(buffer, device->gpa);
    }
    sdelete_ptr_array(device->usable_buffers);

    // Swapchains ought to be cleanud up independently.
    sdelete_ptr_array(device->swapchains);
    for (size_t i = 0; i < device->pending_buffers.len; i++) {
        PendingBufferArray arr = device->pending_buffers.data[i].val;
        // TODO: panic/report error if arr.len > 0 - all pending buffest ought
        // to be deleted.
        sdelete_pbuf_array(arr);
    }
    sdelete_sem_bufs_amap(device->pending_buffers);
    mem_free(device->format_features, device->gpa);

    vkDestroyDevice(device->device, NULL);
    mem_free(device, device->gpa);

}

HdDeviceCapabilities get_device_capabilities(HdLogicalDevice* device) {
    return device->capabilities;
}

#endif
