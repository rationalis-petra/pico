#ifdef USE_VULKAN

#include "platform/signals.h"
#include "platform/hedron/hedron.h"
#include "platform/hedron/internal.h"

struct HdTextureHeapOwner {
    HdLogicalDevice* device;
    VkDeviceMemory memory;
};

// Internal funcions
void append_texture_initialization(HdTextureInitializationList* list, HdTextureInitialization* initialization) {
    // TODO: debug layer
    //assert(!initialization.owner && !initialization.previous && !initialization.next);
    initialization->owner = list;
    initialization->previous = list->last;
    if (list->last)
        list->last->next = initialization;
    else
        list->first = initialization;
    list->last = initialization;
}

void remove_texture_initialization(HdTextureInitialization* initialization) {
    HdTextureInitializationList* list = initialization->owner;
    //assert(list);
    if (initialization->previous)
        initialization->previous->next = initialization->next;
    else
        list->first = initialization->next;
    if (initialization->next)
        initialization->next->previous = initialization->previous;
    else
        list->last = initialization->previous;
    initialization->owner = NULL;
    initialization->previous = NULL;
    initialization->next = NULL;
}

// Interface
HdTextureHeap create_texture_heap(size_t memsize, HdLogicalDevice* device) {
    // TODO: Debug layer
    //assert(device && "create_texture_heap called with a null device");
    HdTextureHeapOwner *owner = mem_alloc(sizeof(HdTextureHeapOwner), device->gpa);
    *owner = (HdTextureHeapOwner) {
        .device = device,
    };
    const VkMemoryAllocateInfo allocate_info = {
        .sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
        .allocationSize = memsize,
        // TODO: add texture memory type filder to device creation!
        .memoryTypeIndex = device->texture_memory_type,
    };
    VkResult result = vkAllocateMemory(device->device, &allocate_info, NULL, &owner->memory);
    if (result != VK_SUCCESS) {
        panic(mv_string("TODO: correctly handle failure to create texture heap."));
    }
    return (HdTextureHeap) {
        .owner = owner,
        .memsize = memsize,
    };
}

void destroy_texture_heap(HdTextureHeap heap) {
    // TODO: debug layer
    //if (!heap.owner) return;
    vkFreeMemory(heap.owner->device->device, heap.owner->memory, NULL);
    mem_free(heap.owner, heap.owner->device->gpa);
}

bool compatible_view_formats(HdFormat image_format, HdFormat view_format)  {
    if (image_format == view_format) return true;
    const HdTextureFormatInfo image = get_texture_format_info(image_format);
    const HdTextureFormatInfo view = get_texture_format_info(view_format);
    if (image.depth || image.stencil || view.depth || view.stencil) return false;
    if (image.block_extent.width == 1 && view.block_extent.width == 1)
        return image.bytes_per_block != 0 && image.bytes_per_block == view.bytes_per_block;
    return (image_format == Format_ASTC_4x4_UNorm && view_format == Format_ASTC_4x4_SRGB) ||
           (image_format == Format_ASTC_4x4_SRGB && view_format == Format_ASTC_4x4_UNorm) ||
           (image_format == Format_BC3_UNorm && view_format == Format_BC3_SRGB) ||
           (image_format == Format_BC3_SRGB && view_format == Format_BC3_UNorm) ||
           (image_format == Format_BC6H_UFloat && view_format == Format_BC6H_SFloat) ||
           (image_format == Format_BC6H_SFloat && view_format == Format_BC6H_UFloat) ||
           (image_format == Format_BC7_UNorm && view_format == Format_BC7_SRGB) ||
           (image_format == Format_BC7_SRGB && view_format == Format_BC7_UNorm);
}

typedef struct {
    VkFormat view_formats[FORMAT_COUNT];
    VkImageFormatListCreateInfo format_list;
    VkImageCreateInfo image_info;
} PreparedTexture;

void prepare_texture(HdLogicalDevice* device, const HdTextureDescription desc, PreparedTexture* output) {
    *output = (PreparedTexture) {
        .format_list = {
            .sType = VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO,
        },
        .image_info = {
            .sType = VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO,
        },
    };

    VkImageUsageFlags usage = 0;
    if (desc.usage & UsageSampled) usage |= VK_IMAGE_USAGE_SAMPLED_BIT;
    if (desc.usage & UsageStorage) usage |= VK_IMAGE_USAGE_STORAGE_BIT;
    if (desc.usage & UsageColourAttachment) usage |= VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT;
    if (desc.usage & UsageDepthStencilAttachment) usage |= VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT;
    if (desc.usage & UsageTransferSource) usage |= VK_IMAGE_USAGE_TRANSFER_SRC_BIT;
    if (desc.usage & UsageTransferDestination) usage |= VK_IMAGE_USAGE_TRANSFER_DST_BIT;

    const VkImageType image_type = texshape_to_vk(desc.shape);
    const VkFormat format = format_to_vk(desc.format);
    const VkFormatFeatureFlags2 sampled_features = required_format_features(UsageSampled);
    const VkFormatFeatureFlags2 storage_features = required_format_features(UsageStorage);
    uint32_t view_format_count = 1;
    output->view_formats[0] = format;
    for (uint32_t value = 0; desc.mutable_format && value < FORMAT_COUNT; value++) {
        const HdFormat view_format = (HdFormat)value;
        if (view_format == desc.format || !compatible_view_formats(desc.format, view_format))
            continue;
        const VkFormatFeatureFlags2 view_features = device->format_features[value];
        const bool sampled = (desc.usage & UsageSampled) && (view_features & sampled_features) == sampled_features;
        const bool storage = (desc.usage & UsageStorage) && (view_features & storage_features) == storage_features;
        if (!sampled && !storage)
            continue;
        // TODO: layer
        //assert(view_format_count < format_count);
        output->view_formats[view_format_count++] = format_to_vk(view_format);
    }

    VkImageCreateFlags image_flags = 0;
    if (desc.shape == TxCube || desc.shape == TxCubeArray) image_flags |= VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT;
    if (view_format_count > 1) image_flags |= VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT;

    output->format_list.viewFormatCount = view_format_count;
    output->format_list.pViewFormats = output->view_formats;
    output->image_info = (VkImageCreateInfo) {
        .sType = VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO,
        .pNext = view_format_count > 1 ? &output->format_list : NULL,
        .flags = image_flags,
        .imageType = image_type,
        .format = format,
        .extent = {
            .width = desc.extent[0],
            .height = desc.extent[1],
            .depth = desc.extent[2],
        },
        .mipLevels = desc.mip_levels,
        .arrayLayers = desc.layer_count,
        .samples = VK_SAMPLE_COUNT_1_BIT,
        .tiling = VK_IMAGE_TILING_OPTIMAL,
        .usage = usage,
        .sharingMode = VK_SHARING_MODE_EXCLUSIVE,
        .initialLayout = VK_IMAGE_LAYOUT_UNDEFINED,
    };
}

SizeAlign get_texture_size_align(HdTextureDescription desc, HdLogicalDevice* device) {
    // TODO: debug layer
    //assert(device && "get_texture_size_align called with a null device");
    PreparedTexture texture;
    prepare_texture(device, desc, &texture);
    const VkMemoryRequirements requirements = image_memory_requirements(device, texture.image_info);
    return (SizeAlign) {
        .size = requirements.size,
        .align = requirements.alignment,
    };
}

HdTexture* create_texture(HdTextureDescription desc, HdTextureHeap heap, uint64_t offset) {
    const HdTextureHeapOwner* owner = heap.owner;
    HdLogicalDevice* device = owner->device;
    Allocator* gpa = device->gpa;
    // TODO: debug layer
    //assert(device && owner && owner->state == device && owner->memory && "create_texture requires a texture heap from the same device");
    //assert(device->active_command_buffers == 0 && "create_texture is not allowed while a command buffer is recording");
    PreparedTexture texture;
    prepare_texture(device, desc, &texture);

    HdTexture* result = mem_alloc(sizeof(HdTexture), gpa);
    *result = (HdTexture) {
        .device = device,
        .width = desc.extent[0],
        .height = desc.extent[1],
        .depth = desc.extent[2],
        .layer_count = desc.layer_count,
        .shape = desc.shape,
        .format = desc.format,
    };
    VkResult vkres = vkCreateImage(device->device, &texture.image_info, NULL, &result->image);
    if (vkres != VK_SUCCESS)
        panic(mv_string("TODO: handle errors in create_image"));
    vkres = vkBindImageMemory(device->device, result->image, owner->memory, offset);
    if (vkres != VK_SUCCESS)
        panic(mv_string("TODO: handle errors in create_image"));

    result->initialization = (HdTextureInitialization) {
        .image = result->image,
        .aspect_mask = image_aspects(desc.format),
        .mip_levels = desc.mip_levels,
        .array_layers = desc.layer_count,
    };
    append_texture_initialization(&device->pending_texture_initializations, &result->initialization);
    return result;
}

void destroy_texture(HdTexture* texture) {
    if (texture->initialization.owner) remove_texture_initialization(&texture->initialization);
    vkDestroyImage(texture->device->device, texture->image, NULL);
    mem_free(texture, texture->device->gpa);
}

HdRenderView* create_render_view(HdTexture* texture, HdRenderViewDescription desc) {
    // TODO: debug layer
    //assert(texture && texture->state);
    HdLogicalDevice* device = texture->device;
    Allocator* a = device->gpa;

    uint32_t width = texture->width >> desc.mip_level;
    uint32_t height = texture->height >> desc.mip_level;
    if (width == 0) width = 1;
    if (height == 0) height = 1;
    HdRenderView* result = mem_alloc(sizeof(HdRenderView), a);
    *result = (HdRenderView) {
        .device = device,
        .extent.width = width,
        .extent.height = height,
    };
    const VkImageViewCreateInfo view_info ={
        .sType = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO,
        .image = texture->image,
        .viewType = VK_IMAGE_VIEW_TYPE_2D,
        .format = format_to_vk(texture->format),
        .subresourceRange = {
            .aspectMask = image_aspects(texture->format),
            .baseMipLevel = desc.mip_level,
            .levelCount = 1,
            .baseArrayLayer = desc.slice,
            .layerCount = 1,
        },
    };
    VkResult vkresult = vkCreateImageView(device->device, &view_info, NULL, &result->image_view);
    if (vkresult != VK_SUCCESS) 
        panic(mv_string("TODO: errorhandling in create-render-view"));
    return result;
}

void destroy_render_view(HdRenderView* render_view) {
    if (render_view->swapchain) {
        panic(mv_string("TODO: Render Views deletion for swapchain views is the parent swapchain!"));
    }
    vkDestroyImageView(render_view->device->device, render_view->image_view, NULL);
}

void write_texture_descriptor(void *cpu_destination, HdTexture *texture,
                              HdTextureDescriptorType type,
                              HdTextureDescriptorDescription desc,
                              HdLogicalDevice* device) {
    // TODO: debug layer
    //assert(device && texture);

    const VkImageUsageFlags descriptor_usage = (VkImageUsageFlags)(
        type == TDescSampled ? VK_IMAGE_USAGE_SAMPLED_BIT : VK_IMAGE_USAGE_STORAGE_BIT);

    VkImageAspectFlags descriptor_aspect = 0;
    switch (desc.aspect) {
    case TxAAutomatic:
      descriptor_aspect = has_depth_aspect(texture->format)
          ? VK_IMAGE_ASPECT_DEPTH_BIT
          : has_stencil_aspect(texture->format) 
            ? VK_IMAGE_ASPECT_STENCIL_BIT 
            : VK_IMAGE_ASPECT_COLOR_BIT;
        break;
    case TxAColour: descriptor_aspect = VK_IMAGE_ASPECT_COLOR_BIT; break;
    case TxADepth: descriptor_aspect = VK_IMAGE_ASPECT_DEPTH_BIT; break;
    case TxAStencil: descriptor_aspect = VK_IMAGE_ASPECT_STENCIL_BIT; break;
    }
    const VkImageViewUsageCreateInfo view_usage = {
        .sType = VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO,
        .usage = descriptor_usage,
    };
    const VkImageViewCreateInfo view_info = {
        .sType = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO,
        .pNext = &view_usage,
        .image = texture->image,
        .viewType = texshape_to_vk_view(texture->shape),
        .format = format_to_vk(desc.format.type == None ? texture->format : desc.format.val),
        .subresourceRange = {
            .aspectMask = descriptor_aspect,
            .baseMipLevel = desc.base_mip,
            .levelCount = desc.mip_count == 0 ? VK_REMAINING_MIP_LEVELS : desc.mip_count,
            .baseArrayLayer = desc.base_layer,
            .layerCount = desc.layer_count == 0 ? VK_REMAINING_ARRAY_LAYERS : desc.layer_count,
        },
    };
    const VkImageDescriptorInfoEXT image_descriptor = {
        .sType = VK_STRUCTURE_TYPE_IMAGE_DESCRIPTOR_INFO_EXT,
        .pView = &view_info,
        .layout = VK_IMAGE_LAYOUT_GENERAL,
    };
    const VkResourceDescriptorInfoEXT descriptor_info = {
        .sType = VK_STRUCTURE_TYPE_RESOURCE_DESCRIPTOR_INFO_EXT,
        .type = type == TDescSampled
            ? VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE
            : VK_DESCRIPTOR_TYPE_STORAGE_IMAGE,
        .data = {.pImage = &image_descriptor},
    };
    const VkHostAddressRangeEXT destination = {
        .address = cpu_destination,
        .size = device->physical_device->heap_properties.imageDescriptorSize,
    };
    VkResult result = device->fns.vkWriteResourceDescriptorsEXT(device->device, 1, &descriptor_info, &destination);
    if (result != VK_SUCCESS)
        panic(mv_string("TODO: handle this failure apporpriately"));
}

void write_sampler_descriptor(void* cpu_destination, HdSamplerDescription desc, HdLogicalDevice* device) {
    //assert(device);
    const VkSamplerCreateInfo sampler_info = {
        .sType = VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO,
        // TODO: conversion over cast - probably gets optimized?? 
        .magFilter = (VkFilter)desc.mag_filter,
        .minFilter = (VkFilter)desc.min_filter,
        .mipmapMode = (VkSamplerMipmapMode)desc.mip_filter,
        .addressModeU = (VkSamplerAddressMode)desc.address_u,
        .addressModeV = (VkSamplerAddressMode)desc.address_v,
        .addressModeW = (VkSamplerAddressMode)desc.address_w,
        .anisotropyEnable = desc.anisotropic ? VK_TRUE : VK_FALSE,
        .maxAnisotropy = desc.anisotropic ? 4.0f : 1.0f,
        .compareEnable = desc.comp.type == Some ? VK_TRUE : VK_FALSE,
        .compareOp = (VkCompareOp)desc.comp.val,
        .maxLod = VK_LOD_CLAMP_NONE,
    };
    const VkHostAddressRangeEXT destination = {
        .address = cpu_destination,
        .size = device->physical_device->heap_properties.samplerDescriptorSize,
    };
    VkResult result = device->fns.vkWriteSamplerDescriptorsEXT(device->device, 1, &sampler_info, &destination);
    if (result != VK_SUCCESS)
        panic(mv_string("TODO: handle this failure apporpriately"));
}

#endif
