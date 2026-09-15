#ifdef USE_VULKAN

#include "platform/signals.h"
#include "platform/hedron/hedron.h"
#include "platform/hedron/internal.h"

uint32_t popcount(uint32_t value) {
    // Keep the core library usable on x86-64 CPUs without POPCNT.
    value -= (value >> 1) & 0x55555555u;
    value = (value & 0x33333333u) + ((value >> 2) & 0x33333333u);
    value = (value + (value >> 4)) & 0x0f0f0f0fu;
    return (value * 0x01010101u) >> 24;
}

bool has_depth_aspect(HdFormat format) {
    return get_texture_format_info(format).depth;
}

bool has_stencil_aspect(HdFormat format) {
    return get_texture_format_info(format).stencil;
}

#define FORBIDDEN_MEMORY_PROPERTIES \
    (VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT |            \
     VK_MEMORY_PROPERTY_PROTECTED_BIT |                   \
     VK_MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD |         \
     VK_MEMORY_PROPERTY_DEVICE_UNCACHED_BIT_AMD)

bool is_usable_memory_type(VkPhysicalDeviceMemoryProperties properties, uint32_t index) {
    const VkMemoryType type = properties.memoryTypes[index];
    if ((type.propertyFlags & FORBIDDEN_MEMORY_PROPERTIES) != 0)
        return false;
    return (properties.memoryHeaps[type.heapIndex].flags & VK_MEMORY_HEAP_TILE_MEMORY_BIT_QCOM) == 0;
}

VkFormatFeatureFlags2 required_format_features(HdTextureUsage usage) {
    VkFormatFeatureFlags2 result = 0;
    if (usage & UsageSampled)
        result |= VK_FORMAT_FEATURE_2_SAMPLED_IMAGE_BIT;
    if (usage & UsageStorage)
        result |= VK_FORMAT_FEATURE_2_STORAGE_IMAGE_BIT |
                  VK_FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT |
                  VK_FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT;
    if (usage & UsageColourAttachment)
        result |= VK_FORMAT_FEATURE_2_COLOR_ATTACHMENT_BIT;
    if (usage & UsageDepthStencilAttachment)
        result |= VK_FORMAT_FEATURE_2_DEPTH_STENCIL_ATTACHMENT_BIT;
    if (usage & UsageTransferSource)
        result |= VK_FORMAT_FEATURE_2_TRANSFER_SRC_BIT;
    if (usage & UsageTransferDestination)
        result |= VK_FORMAT_FEATURE_2_TRANSFER_DST_BIT;
    return result;
}

VkMemoryRequirements image_memory_requirements(HdLogicalDevice* device, const VkImageCreateInfo image_info) {
    const VkDeviceImageMemoryRequirements requirements_info = {
        .sType = VK_STRUCTURE_TYPE_DEVICE_IMAGE_MEMORY_REQUIREMENTS,
        .pCreateInfo = &image_info,
    };
    VkMemoryRequirements2 requirements = {
        .sType = VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2,
    };
    vkGetDeviceImageMemoryRequirements(device->device, &requirements_info, &requirements);
    return requirements.memoryRequirements;
}

bool find_memory_type(uint32_t bits, VkMemoryPropertyFlags required, VkMemoryPropertyFlags preferred, VkDeviceSize minimum_heap_size,
                      uint32_t* output, VkMemoryPropertyFlags avoided, HdLogicalDevice* device) {
    bool has_best = false;
    bool best_is_avoided = false;
    uint32_t best = 0;
    uint32_t best_score = 0;
    VkPhysicalDeviceMemoryProperties memory_properties = device->physical_device->memory_properties;
    VkDeviceSize best_heap_size = 0;
    for (uint32_t i = 0; i < memory_properties.memoryTypeCount; i++) {
        if ((bits & (1u << i)) == 0)
            continue;
        const VkMemoryPropertyFlags flags = memory_properties.memoryTypes[i].propertyFlags;
        if ((flags & required) != required)
            continue;
        if (!is_usable_memory_type(memory_properties, i))
            continue;
        const VkMemoryHeap heap = memory_properties.memoryHeaps[memory_properties.memoryTypes[i].heapIndex];
        if (heap.size < minimum_heap_size) {
            continue;
        }
        const bool is_avoided = (flags & avoided) != 0;
        const uint32_t score = (uint32_t)(popcount(flags & preferred));
        if (!has_best || (best_is_avoided && !is_avoided) ||
            (best_is_avoided == is_avoided &&
             (score > best_score || (score == best_score && heap.size > best_heap_size)))) {
            best = i;
            has_best = true;
            best_is_avoided = is_avoided;
            best_score = score;
            best_heap_size = heap.size;
        }
    }
    if (!has_best)
        return false;
    *output = best;
    return true;
}

#endif
