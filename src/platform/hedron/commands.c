#ifdef USE_VULKAN

#include <string.h>
#include "platform/signals.h"
#include "platform/hedron/hedron.h"
#include "platform/hedron/internal.h"

// 
// Command submission
//   
//  

void set_pipeline(HdCommandBuffer* cb, HdPipeline* pipeline) {
    vkCmdBindPipeline(cb->buffer,
                      pipeline->bind_point,
                      pipeline->pipeline);
    cb->current_pipeline = pipeline;
    vkCmdBindPipeline(cb->buffer, pipeline->bind_point, pipeline->pipeline);
}

void emit_root_data(HdCommandBuffer* commands, void* data) {
    const VkPushDataInfoEXT info = {
        .sType = VK_STRUCTURE_TYPE_PUSH_DATA_INFO_EXT,
        .data = {
            .address = data,
            .size = commands->current_pipeline->data_size,
        },
    };
    commands->device->vkCmdPushDataEXT(commands->buffer, &info);
}

void dispatch(HdLogicalDevice* device, HdCommandBuffer* cb, void* data, UVec3 group_count) {
    // TODO: input validation with debug layers

    emit_root_data(cb, data);
    vkCmdDispatch(cb->buffer, group_count.x, group_count.y, group_count.z);
}

#endif
