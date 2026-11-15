#ifdef USE_VULKAN

#include <string.h>
#include "platform/signals.h"
#include "platform/hedron/hedron.h"
#include "platform/hedron/internal.h"

// 
// Scheduling Commands  
//

void barrier(HdCommandBuffer* cb, HdStage before, HdAccess before_access, HdStage after, HdAccess after_access) {
    // TODO: debug layer
    //assert(commands);
    const VkMemoryBarrier2 memory_barrier = {
        .sType = VK_STRUCTURE_TYPE_MEMORY_BARRIER_2,
        .srcStageMask = stage_to_vk(before),
        .srcAccessMask = access_to_vk(before_access),
        .dstStageMask = stage_to_vk(after),
        .dstAccessMask = access_to_vk(after_access),
    };
    const VkDependencyInfo dependency = {
        .sType = VK_STRUCTURE_TYPE_DEPENDENCY_INFO,
        .memoryBarrierCount = 1,
        .pMemoryBarriers = &memory_barrier,
    };
    vkCmdPipelineBarrier2(cb->buffer, &dependency);
}

//void signal_after(HdCommandBuffer* cb, HdStage before, void *ptrGpu, uint64_t value, HdSignal signal);
//void wait_before(HdCommandBuffer* cb, HdStage after, void *ptrGpu, uint64_t value, HdCompOp op, HdHazardFlags hazards, uint64_t mask);

// 
// State Commands: update gpu state
//  

void set_pipeline(HdCommandBuffer* cb, HdPipeline* pipeline) {
    cb->current_pipeline = pipeline;
    vkCmdBindPipeline(cb->buffer, pipeline->bind_point, pipeline->pipeline);
}

void set_viewport(HdCommandBuffer* cb, HdViewport viewport) {
    const VkViewport vk_viewport = {
        .x = viewport.x,
        .y = viewport.y,
        .width = viewport.width,
        .height = viewport.height,
        .minDepth = viewport.min_depth,
        .maxDepth = viewport.max_depth,
    };
    vkCmdSetViewportWithCount(cb->buffer, 1, &vk_viewport);
}

void set_scissor(HdCommandBuffer* cb, HdScissor scissor) {
    const VkRect2D vk_scissor = {
        .offset = {.x = scissor.x, .y = scissor.y},
        .extent = {.width = scissor.width, .height = scissor.height},
    };
    vkCmdSetScissorWithCount(cb->buffer, 1, &vk_scissor);
}

void set_depth_stencil(HdCommandBuffer* cb, HdDepthStencilState state) {
    vkCmdSetDepthTestEnable(cb->buffer, state.depth_test);
    if (state.depth_test) {
        vkCmdSetDepthWriteEnable(cb->buffer, state.depth_write);
        // TODO: proper conversion?
        vkCmdSetDepthCompareOp(cb->buffer, (VkCompareOp)state.depth_compare);
    }
    vkCmdSetStencilTestEnable(cb->buffer, state.stencil_test);
    if (!state.stencil_test)
        return;

    // TODO: proper conversions here?
    vkCmdSetStencilOp(cb->buffer, VK_STENCIL_FACE_FRONT_BIT,
                      (VkStencilOp)state.front.fail, (VkStencilOp)state.front.pass,
                      (VkStencilOp)state.front.depth_fail, (VkCompareOp)state.front.compare);
    vkCmdSetStencilOp(cb->buffer, VK_STENCIL_FACE_BACK_BIT,
                      (VkStencilOp)(state.back.fail), (VkStencilOp)state.back.pass,
                      (VkStencilOp)(state.back.depth_fail), (VkCompareOp)state.back.compare);
    vkCmdSetStencilCompareMask(cb->buffer, VK_STENCIL_FACE_FRONT_AND_BACK, state.stencil_read_mask);
    vkCmdSetStencilWriteMask(cb->buffer, VK_STENCIL_FACE_FRONT_AND_BACK, state.stencil_write_mask);
    vkCmdSetStencilReference(cb->buffer, VK_STENCIL_FACE_FRONT_BIT, state.front.reference);
    vkCmdSetStencilReference(cb->buffer, VK_STENCIL_FACE_BACK_BIT, state.back.reference);
}


static uint64_t align_up(uint64_t size, uint64_t alignment) {
    return ((size + alignment - 1) / alignment) * alignment;
}
void make_heap_bind_info(DeviceRange heap, VkDeviceSize reserved_alignment, VkDeviceSize reserved_size, VkBindHeapInfoEXT* output) {
    const VkDeviceSize reserved_offset = align_up(heap.size, reserved_alignment);
    *output = (VkBindHeapInfoEXT) {
        .sType = VK_STRUCTURE_TYPE_BIND_HEAP_INFO_EXT,
        .heapRange = {
            .address = (VkDeviceAddress)(heap.address.val),
            .size = reserved_offset + reserved_size,
        },
        .reservedRangeOffset = reserved_offset,
        .reservedRangeSize = reserved_size,
    };
}

void set_texture_descriptor_heap(HdCommandBuffer* commands, DeviceRange heap) {
    // TODO: debug layer
    //assert(commands && commands->state);
    const VkPhysicalDeviceDescriptorHeapPropertiesEXT properties = commands->device->physical_device->heap_properties;
    VkBindHeapInfoEXT bind_info = {};
    make_heap_bind_info(
        heap,
        properties.imageDescriptorAlignment > properties.bufferDescriptorAlignment
            ? properties.imageDescriptorAlignment
            : properties.bufferDescriptorAlignment,
        properties.minResourceHeapReservedRange,
        &bind_info);
    commands->device->fns.vkCmdBindResourceHeapEXT(commands->buffer, &bind_info);
}

void set_sampler_descriptor_heap(HdCommandBuffer* commands, DeviceRange heap) {
    // TODO: debug layer
    //assert(commands && commands->state);
    const VkPhysicalDeviceDescriptorHeapPropertiesEXT properties = commands->device->physical_device->heap_properties;
    VkBindHeapInfoEXT bind_info = {};
    make_heap_bind_info(heap, properties.samplerDescriptorAlignment, properties.minSamplerHeapReservedRange, &bind_info);
    commands->device->fns.vkCmdBindSamplerHeapEXT(commands->buffer, &bind_info);
}

void copy_memory(HdCommandBuffer* cb, DeviceRange source, DeviceRange destination) {
    // TODO: debug layer
    //assert(commands && commands->state);
    const VkDeviceMemoryCopyKHR region = {
        .sType = VK_STRUCTURE_TYPE_DEVICE_MEMORY_COPY_KHR,
        .srcRange = {
            .address = (VkDeviceAddress)source.address.val,
            .size = source.size,
        },
        .srcFlags = ADDRESS_FLAGS,
        .dstRange = {
            .address = (VkDeviceAddress)destination.address.val,
            .size = destination.size,
        },
        .dstFlags = ADDRESS_FLAGS,
    };
    const VkCopyDeviceMemoryInfoKHR info = {
        .sType = VK_STRUCTURE_TYPE_COPY_DEVICE_MEMORY_INFO_KHR,
        .regionCount = 1,
        .pRegions = &region,
    };
    cb->device->fns.vkCmdCopyMemoryKHR(cb->buffer, &info);
}

uint64_t divide_up(uint64_t value, uint64_t divisor) {
    //assert(divisor != 0);
    return value / divisor + (value % divisor != 0 ? 1u : 0u);
}

VkDeviceMemoryImageCopyKHR make_texture_copy_region(HdTexture texture, TextureCopyDesc copy, DeviceRange memory) {
    uint32_t mip_width = texture.width >> copy.mip_level;
    uint32_t mip_height = texture.height >> copy.mip_level;
    uint32_t mip_depth = texture.depth >> copy.mip_level;
    if (mip_width == 0) mip_width = 1;
    if (mip_height == 0) mip_height = 1;
    if (mip_depth == 0) mip_depth = 1;

    const uint32_t width = copy.extent.width == 0 ? mip_width - copy.offset.width : copy.extent.width;
    const HdTextureFormatInfo format_info = get_texture_format_info(texture.format);
    const uint64_t row_pitch = copy.row_pitch_bytes == 0
                                 ? divide_up(width, format_info.block_extent.width) * format_info.bytes_per_block
                                 : copy.row_pitch_bytes;
    // TODO: debug layer
    //assert((copy.slice_pitch_bytes == 0 || row_pitch != 0) && "slice pitch conversion requires a non-zero row pitch");
    return (VkDeviceMemoryImageCopyKHR) {
        .sType = VK_STRUCTURE_TYPE_DEVICE_MEMORY_IMAGE_COPY_KHR,
        .addressRange = {
            .address = (VkDeviceAddress)memory.address.val,
            .size = memory.size,
        },
        .addressFlags = ADDRESS_FLAGS,
        .addressRowLength = (uint32_t)(copy.row_pitch_bytes / format_info.bytes_per_block * format_info.block_extent.width),
        .addressImageHeight = copy.slice_pitch_bytes == 0 ? 0u : (uint32_t)(copy.slice_pitch_bytes / row_pitch * format_info.block_extent.height),
        .imageSubresource = {
            .aspectMask = image_aspects(texture.format),
            .mipLevel = copy.mip_level,
            .baseArrayLayer = copy.base_slice,
            .layerCount = copy.slice_count == 0 ? texture.layer_count - copy.base_slice : copy.slice_count,
        },
        .imageLayout = VK_IMAGE_LAYOUT_GENERAL,
        .imageOffset = {
            .x = copy.offset.width,
            .y = copy.offset.height,
            .z = copy.offset.depth,
        },
        .imageExtent = {
            .width = width,
            .height = copy.extent.height == 0 ? mip_height - copy.offset.height : copy.extent.height,
            .depth = copy.extent.depth == 0 ? mip_depth - copy.offset.depth : copy.extent.depth,
        },
    };
}

void copy_memory_to_texture(HdCommandBuffer* cb, DeviceRange source, HdTexture* destination, TextureCopyDesc copy) {
    // TODO: debug layer
    //assert(commands && commands->state && destination);
    const VkDeviceMemoryImageCopyKHR region = make_texture_copy_region(*destination, copy, source);
    const VkCopyDeviceMemoryImageInfoKHR info = {
        .sType = VK_STRUCTURE_TYPE_COPY_DEVICE_MEMORY_IMAGE_INFO_KHR,
        .image = destination->image,
        .regionCount = 1,
        .pRegions = &region,
    };
    cb->device->fns.vkCmdCopyMemoryToImageKHR(cb->buffer, &info);
}

void copy_texture_to_memory(HdCommandBuffer* cb, HdTexture* source, DeviceRange destination, TextureCopyDesc copy) {
    // TODO: debug layer
    //assert(commands && commands->state && source);
    const VkDeviceMemoryImageCopyKHR region = make_texture_copy_region(*source, copy, destination);
    const VkCopyDeviceMemoryImageInfoKHR info = {
        .sType = VK_STRUCTURE_TYPE_COPY_DEVICE_MEMORY_IMAGE_INFO_KHR,
        .image = source->image,
        .regionCount = 1,
        .pRegions = &region,
    };
    cb->device->fns.vkCmdCopyImageToMemoryKHR(cb->buffer, &info);
}

void emit_root_data(HdCommandBuffer* commands, U8Slice data) {
    const VkPushDataInfoEXT info = {
        .sType = VK_STRUCTURE_TYPE_PUSH_DATA_INFO_EXT,
        .data = {
            .address = data.data,
            .size = data.len,
        },
    };
    commands->device->fns.vkCmdPushDataEXT(commands->buffer, &info);
}

void dispatch(HdCommandBuffer* cb, U8Slice data, UVec3 group_count) {
    // TODO: input validation with debug layers

    emit_root_data(cb, data);
    vkCmdDispatch(cb->buffer, group_count.x, group_count.y, group_count.z);
}

void dispatch_indirect(HdCommandBuffer* cb, U8Slice data, DeviceRange arguments) {
    // TODO: debug layer/asserts
    //assert(commands && commands->state);
    emit_root_data(cb, data);
    const VkDispatchIndirect2InfoKHR info = {
        .sType = VK_STRUCTURE_TYPE_DISPATCH_INDIRECT_2_INFO_KHR,
        .addressRange = {
            .address = (VkDeviceAddress)arguments.address.val,
            .size = arguments.size,
        },
        .addressFlags = ADDRESS_FLAGS,
    };
    cb->device->fns.vkCmdDispatchIndirect2KHR(cb->buffer, &info);
}

void draw(HdCommandBuffer* cb, U8Slice data, uint32_t vertex_count, uint32_t instance_count, uint32_t first_vertex, uint32_t first_instance) {
    // TODO: debug tests
    //const bool valid = commands && commands->recording && commands->rendering;
    //assert(valid && "draw requires an active rendering scope");

    emit_root_data(cb, data);
    vkCmdDraw(cb->buffer, vertex_count, instance_count, first_vertex, first_instance);
}

void draw_indexed(HdCommandBuffer* cb, U8Slice data,
                  DeviceRange indices, IndexType type,
                  uint32_t index_count, uint32_t instance_count, uint32_t first_index,
                  int32_t vertex_offset, uint32_t first_instance) {
    //assert(commands && commands->state);
    emit_root_data(cb, data);
    const VkBindIndexBuffer3InfoKHR bind_info = {
        .sType = VK_STRUCTURE_TYPE_BIND_INDEX_BUFFER_3_INFO_KHR,
        .addressRange = {
            .address = (VkDeviceAddress)(indices.address.val),
            .size = indices.size,
        },
        .addressFlags = ADDRESS_FLAGS,
        .indexType = (VkIndexType)type,
    };
    cb->device->fns.vkCmdBindIndexBuffer3KHR(cb->buffer, &bind_info);
    vkCmdDrawIndexed(cb->buffer, index_count, instance_count, first_index, vertex_offset, first_instance);
}

void draw_indirect(HdCommandBuffer* cb, U8Slice data, DeviceRange arguments, uint32_t draw_count, uint32_t stride) {
    //assert(commands && commands->state);
    emit_root_data(cb, data);
    const VkDrawIndirect2InfoKHR info = {
        .sType = VK_STRUCTURE_TYPE_DRAW_INDIRECT_2_INFO_KHR,
        .addressRange = {
            .address = (VkDeviceAddress)arguments.address.val,
            .size = arguments.size,
            .stride = stride == 0 ? sizeof(VkDrawIndirectCommand) : stride,
        },
        .addressFlags = ADDRESS_FLAGS,
        .drawCount = draw_count,
    };
    cb->device->fns.vkCmdDrawIndirect2KHR(cb->buffer, &info);
}

void draw_indexed_indirect(HdCommandBuffer *cb, U8Slice data,
                           DeviceRange indices, IndexType type, DeviceRange arguments, uint32_t draw_count,
                           uint32_t stride) {
    //assert(commands && commands->state);
    emit_root_data(cb, data);
    const VkBindIndexBuffer3InfoKHR bind_info = {
        .sType = VK_STRUCTURE_TYPE_BIND_INDEX_BUFFER_3_INFO_KHR,
        .addressRange = {
            .address = (VkDeviceAddress)(indices.address.val),
            .size = indices.size,
        },
        .addressFlags = ADDRESS_FLAGS,
        .indexType = (VkIndexType)(type),
    };
    cb->device->fns.vkCmdBindIndexBuffer3KHR(cb->buffer, &bind_info);
    const VkDrawIndirect2InfoKHR info = {
        .sType = VK_STRUCTURE_TYPE_DRAW_INDIRECT_2_INFO_KHR,
        .addressRange = {
            .address = (VkDeviceAddress)arguments.address.val,
            .size = arguments.size,
            .stride = stride == 0 ? sizeof(VkDrawIndexedIndirectCommand) : stride,
        },
        .addressFlags = ADDRESS_FLAGS,
        .drawCount = draw_count,
    };
    cb->device->fns.vkCmdDrawIndexedIndirect2KHR(cb->buffer, &info);
}

void start_render_pass(HdCommandBuffer* cb, HdRenderDesc desc) {
    // TODO: input validation (begin_render_pass)

    VkRenderingAttachmentInfo colour_attachments[MAX_COLOUR_ATTACHMENTS] = {};
    uint32_t width = 0;
    uint32_t height = 0;

    for (size_t index = 0; index < desc.colours.len; ++index) {
        const HdColourAttachment attachment = desc.colours.data[index];
        HdRenderView* render_view = attachment.render_view;
        bool first_claim = false;
        if (render_view->swapchain) {
            if (render_view->swapchain->claimed_by && render_view->swapchain->claimed_by != cb) {
                // TODO: debug layer
                panic(mv_string("Starting render pass in command buffer, where target is swapchain already claimed."));
            }
            // The claim is the first one true only if the swapchain has an
            // empty 'claimed by' field.
            first_claim = !render_view->swapchain->claimed_by;
                
            render_view->swapchain->claimed_by = cb;
        }
        // We are claiming the current image on the swapchain, so must
        // transition the image layout  
        if (first_claim) {
            HdSwapchain* swapchain = render_view->swapchain;
            /* TODO: debug layer
            assert((swapchain->state == device && swapchain->acquired &&
                    swapchain->present_context &&
                    swapchain->image_index < swapchain->image_count) && "the acquired swapchain state is invalid");
            */
            const VkImageMemoryBarrier2 barrier = {
                .sType = VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2,
                .srcStageMask = VK_PIPELINE_STAGE_2_NONE,
                .dstStageMask = VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT,
                .dstAccessMask = VK_ACCESS_2_MEMORY_READ_BIT | VK_ACCESS_2_MEMORY_WRITE_BIT,
                .oldLayout = swapchain->initialized[swapchain->current_image] ? VK_IMAGE_LAYOUT_PRESENT_SRC_KHR : VK_IMAGE_LAYOUT_UNDEFINED,
                .newLayout = VK_IMAGE_LAYOUT_GENERAL,
                .srcQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED,
                .dstQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED,
                .image = swapchain->images[swapchain->current_image],
                .subresourceRange = {
                    .aspectMask = VK_IMAGE_ASPECT_COLOR_BIT,
                    .levelCount = 1,
                    .layerCount = 1,
                },
            };
            const VkDependencyInfo dependency = {
                .sType = VK_STRUCTURE_TYPE_DEPENDENCY_INFO,
                .imageMemoryBarrierCount = 1,
                .pImageMemoryBarriers = &barrier,
            };
            vkCmdPipelineBarrier2(cb->buffer, &dependency);
        }


        //const bool valid_color = render_view && render_view->state == commands->state && render_view->view;
        //assert(valid_color && "every color render target must be a live view from the device");
        if (width == 0) {
            width = render_view->extent.width;
            height = render_view->extent.height;
        }
        colour_attachments[index] = (VkRenderingAttachmentInfo) {
            .sType = VK_STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO,
            .imageView = render_view->image_view,
            .imageLayout = VK_IMAGE_LAYOUT_GENERAL,
            .loadOp = load_op_to_vk(attachment.load),
            .storeOp = store_op_to_vk(attachment.store),
            .clearValue = {
                .color = {
                    .float32 = {
                        attachment.clear.x,
                        attachment.clear.y,
                        attachment.clear.z,
                        attachment.clear.w,
                    },
                },
            },
        };
    }

    HdRenderView* depth_view = desc.depth.type == Some ? desc.depth.val.render_view : NULL;
    HdRenderView* stencil_view = desc.stencil.type == Some ? desc.stencil.val.render_view : NULL;
    // TODO: debug reporting
    //const bool has_attachment = desc.colours.size != 0 || depth_view || stencil_view;
    //assert(has_attachment && "begin_render_pass requires at least one attachment");
    if (depth_view) {
        //const bool valid_depth = depth_view->state == commands->state && depth_view->view;
        //assert(valid_depth && "depth render target must be a live view from the device");
        if (width == 0) {
            width = depth_view->extent.width;
            height = depth_view->extent.height;
        }
    }
    if (stencil_view) {
        //const bool valid_stencil = stencil_view->state == commands->state && stencil_view->view;
        //assert(valid_stencil && "stencil render target must be a live view from the device");
        if (width == 0) {
            width = stencil_view->extent.width;
            height = stencil_view->extent.height;
        }
    }

    const VkRenderingAttachmentInfo depth_attachment = {
        .sType = VK_STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO,
        .imageView = depth_view ? depth_view->image_view : VK_NULL_HANDLE,
        .imageLayout = VK_IMAGE_LAYOUT_GENERAL,
        .loadOp = load_op_to_vk(depth_view ? desc.depth.val.load : LOpLoad),
        .storeOp = store_op_to_vk(depth_view ? desc.depth.val.store : SOpStore),
        .clearValue = {
            .depthStencil = {
                .depth = depth_view ? desc.depth.val.clear : 1.0,
            },
        },
    };
    const VkRenderingAttachmentInfo stencil_attachment = {
        .sType = VK_STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO,
        .imageView = stencil_view ? stencil_view->image_view : VK_NULL_HANDLE,
        .imageLayout = VK_IMAGE_LAYOUT_GENERAL,
        .loadOp = load_op_to_vk(stencil_view ? desc.stencil.val.load : LOpLoad),
        .storeOp = store_op_to_vk(stencil_view ? desc.stencil.val.store : SOpStore),
        .clearValue = {
            .depthStencil = {
                .depth = 1.0f,
                .stencil = stencil_view ? desc.stencil.val.clear : 0.0,
            },
        },
    };
    const VkRenderingInfo rendering_info = {
        .sType = VK_STRUCTURE_TYPE_RENDERING_INFO,
        .renderArea = {
            .extent = {.width = width, .height = height},
        },
        .layerCount = 1,
        .colorAttachmentCount = desc.colours.len,
        .pColorAttachments = desc.colours.len ? colour_attachments : NULL,
        .pDepthAttachment = depth_view ? &depth_attachment : NULL,
        .pStencilAttachment = stencil_view ? &stencil_attachment : NULL,
    };
    vkCmdBeginRendering(cb->buffer, &rendering_info);

    // TODO?
    HdViewport viewport = {
        .width = width,
        .height = height,
        .min_depth = 0.0f,
        .max_depth = 1.0f,
    };
    set_viewport(cb, viewport);

    HdScissor scissor = {
        .width = viewport.width,
        .height = viewport.height,
    };
    set_scissor(cb, scissor);


    StencilFaceState stencil_null = {
        .compare = OpAlways,
        .fail = StKeep,
        .pass = StKeep,
        .depth_fail = StKeep,
        .reference = 0,
    };

     HdDepthStencilState depth_stencil_null = {
        .depth_test = false,
        .depth_write = false,
        .depth_compare = OpLessEqual,
        .stencil_test = false,
        .stencil_read_mask = 0xff,
        .stencil_write_mask = 0xff,
        .front = stencil_null, 
        .back = stencil_null,
    };
    set_depth_stencil(cb, depth_stencil_null);
    cb->rendering = true;
}

void end_render_pass(HdCommandBuffer* cb) {
    // TODO: debug layer
    //const bool valid = commands && commands->recording && commands->rendering;
    //assert(valid && "end_render_pass requires an active rendering scope");
    vkCmdEndRendering(cb->buffer);
    cb->rendering = false;
}

#endif
