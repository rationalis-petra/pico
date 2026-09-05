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

void dispatch(HdLogicalDevice* device, HdCommandBuffer* cb, void* data, UVec3 gridDimensions) {
    //VkDeviceAddress address = reinterpret_cast<VkDeviceAddress>(dataGpu);
    VkPipelineLayout layout = cb->current_pipeline->bind_point == VK_PIPELINE_BIND_POINT_COMPUTE
        ? device->compute_pipeline_layout
        : device->graphics_pipeline_layout;
    vkCmdPushConstants(
        cb->buffer,
        layout,
        VK_SHADER_STAGE_COMPUTE_BIT,
        0,
        sizeof(VkDeviceAddress) * 4,
        data);

    vkCmdDispatch(cb->buffer,
        gridDimensions.x,
        gridDimensions.y,
        gridDimensions.z);
}

#endif
