#ifdef USE_VULKAN

#include "platform/signals.h"
#include "platform/hedron/hedron.h"
#include "platform/hedron/internal.h"

// Pipeline
typedef enum {
    ComputePipeline,
    GraphicsPipeline,
} PipelineType;

const char* universal_entry_point = "main";
HdPipeline* create_compute_pipeline(U32Slice compute_ir, size_t data_size, HdLogicalDevice* device) {
    // TODO: debug layer: device not null
    // TODO: debug layer: compute_ir not empty/null

    const VkShaderModuleCreateInfo module_info = {
        .sType = VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
        .codeSize = compute_ir.len * sizeof(uint32_t),
        .pCode = compute_ir.data,
    };
    const VkPipelineShaderStageCreateInfo stage = {
        .sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
        .pNext = &module_info,
        .stage = VK_SHADER_STAGE_COMPUTE_BIT,
        .pName = "main",
    };
    const VkPipelineCreateFlags2CreateInfo flags_info = {
        .sType = VK_STRUCTURE_TYPE_PIPELINE_CREATE_FLAGS_2_CREATE_INFO,
        .flags = VK_PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT,
    };
    const VkComputePipelineCreateInfo pipeline_info = {
        .sType = VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO,
        .pNext = &flags_info,
        .stage = stage,
        .basePipelineIndex = -1,
    };
    VkPipeline pipeline;
    const VkResult result = vkCreateComputePipelines(device->device, VK_NULL_HANDLE, 1, &pipeline_info, NULL, &pipeline);
    if (result != VK_SUCCESS) {
        panic(mv_string("TODO: handle failure to create pipeline"));
    }

    HdPipeline* hd_pipeline = mem_alloc(sizeof(HdPipeline), device->gpa); 
    *hd_pipeline = (HdPipeline) {
      .pipeline = pipeline,
      .bind_point = VK_PIPELINE_BIND_POINT_COMPUTE,
      .data_size = data_size,
    };
    return hd_pipeline;
}

HdPipeline* create_graphics_pipeline(U32Slice initial_IR, U32Slice pixel_IR, HdRasterDescription raster_desc, bool is_meshlet, size_t data_size, HdLogicalDevice* device) {
    // TODO: if we look at aaltonen's create_raster_pso, is asserts some things
    // about formats existing & being implemented. We want to make sure we use
    // constructive types to eliminate these checks, e.g. (depth_enabled = true
    // => some values must be defined) must be reified as an optional type etc.
    const bool depth_enabled = raster_desc.depth_format.type == Some;
    const bool depth_bias_enabled = raster_desc.depth_bias.type == Some;
    const bool stencil_enabled = raster_desc.stencil_format.type == Some;

    const VkShaderModuleCreateInfo first_stage_module_info = {
        .sType = VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
        .codeSize = initial_IR.len * sizeof(uint32_t),
        .pCode = initial_IR.data,
    };
    const VkShaderModuleCreateInfo fragment_module_info = {
        .sType = VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
        .codeSize = pixel_IR.len * sizeof(uint32_t),
        .pCode = pixel_IR.data,
    };
    const VkPipelineShaderStageCreateInfo stages[] ={
        (VkPipelineShaderStageCreateInfo) {
            .sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
            .pNext = &first_stage_module_info,
            .stage = is_meshlet ? VK_SHADER_STAGE_MESH_BIT_EXT : VK_SHADER_STAGE_VERTEX_BIT,
            .pName = "main",
        },
        (VkPipelineShaderStageCreateInfo) {
            .sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
            .pNext = &fragment_module_info,
            .stage = VK_SHADER_STAGE_FRAGMENT_BIT,
            .pName = "main",
        },
    };

    const VkPipelineVertexInputStateCreateInfo vertex_input = {
        .sType = VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO,
    };
    const VkPipelineInputAssemblyStateCreateInfo input_assembly = {
        .sType = VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO,
        .topology = VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST,
    };
    const VkPipelineViewportStateCreateInfo viewport_state = {
        .sType = VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO,
    };
    const VkPipelineRasterizationStateCreateInfo rasterization = {
        .sType = VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO,
        .polygonMode = VK_POLYGON_MODE_FILL,
        .cullMode = cull_to_vk(raster_desc.cull),
        .frontFace = raster_desc.cull == CullCCW ? VK_FRONT_FACE_CLOCKWISE : VK_FRONT_FACE_COUNTER_CLOCKWISE,
        .depthBiasEnable = depth_bias_enabled,
        .depthBiasConstantFactor = depth_bias_enabled ? raster_desc.depth_bias.val.constant : 0.0,
        .depthBiasClamp = depth_bias_enabled ? raster_desc.depth_bias.val.clamp : 0.0,
        .depthBiasSlopeFactor = depth_bias_enabled ? raster_desc.depth_bias.val.slope : 0.0,
        .lineWidth = 1.0f,
    };
    const VkPipelineMultisampleStateCreateInfo multisample = {
        .sType = VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO,
        .rasterizationSamples = VK_SAMPLE_COUNT_1_BIT,
    };
    const VkPipelineDepthStencilStateCreateInfo depth_stencil = {
        .sType = VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO,
    };
    
    VkPipelineColorBlendAttachmentState colour_attachments[MAX_COLOUR_ATTACHMENTS] = {};
    for (size_t i = 0; i < raster_desc.colour_targets.len; i++) {
        const HdColourTarget target = raster_desc.colour_targets.data[i];
        if (target.blend.type == Some) {
            colour_attachments[i] = (VkPipelineColorBlendAttachmentState) {
                .blendEnable = true,
                .srcColorBlendFactor = blend_factor_to_vk(target.blend.val.colour.source),
                .dstColorBlendFactor = blend_factor_to_vk(target.blend.val.colour.destination),
                .colorBlendOp = blend_op_to_vk(target.blend.val.colour.operation),
                .srcAlphaBlendFactor = blend_factor_to_vk(target.blend.val.alpha.source),
                .dstAlphaBlendFactor = blend_factor_to_vk(target.blend.val.alpha.destination),
                .alphaBlendOp = blend_op_to_vk(target.blend.val.alpha.operation),
                .colorWriteMask = target.write_mask,
            };
        } else {
            colour_attachments[i] = (VkPipelineColorBlendAttachmentState) {
                .blendEnable = false,
                .colorWriteMask = target.write_mask,
            };
        }
    }
    const VkPipelineColorBlendStateCreateInfo colour_blend = {
        .sType = VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO,
        .attachmentCount = raster_desc.colour_targets.len,
        .pAttachments = raster_desc.colour_targets.len ? colour_attachments : NULL,
    };
    const VkDynamicState dynamic_states[] = {
        VK_DYNAMIC_STATE_VIEWPORT_WITH_COUNT,
        VK_DYNAMIC_STATE_SCISSOR_WITH_COUNT,
        VK_DYNAMIC_STATE_DEPTH_TEST_ENABLE,
        VK_DYNAMIC_STATE_DEPTH_WRITE_ENABLE,
        VK_DYNAMIC_STATE_DEPTH_COMPARE_OP,
        VK_DYNAMIC_STATE_STENCIL_TEST_ENABLE,
        VK_DYNAMIC_STATE_STENCIL_OP,
        VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK,
        VK_DYNAMIC_STATE_STENCIL_WRITE_MASK,
        VK_DYNAMIC_STATE_STENCIL_REFERENCE,
    };
    const VkPipelineDynamicStateCreateInfo dynamic_state = {
        .sType = VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO,
        .dynamicStateCount = sizeof(dynamic_states) / sizeof(dynamic_states[0]),
        .pDynamicStates = dynamic_states,
    };
    VkFormat colour_formats[MAX_COLOUR_ATTACHMENTS] = {};
    for (size_t index = 0; index < raster_desc.colour_targets.len; ++index) {
        colour_formats[index] = format_to_vk(raster_desc.colour_targets.data[index].format);
    }
    const VkFormat vk_depth_format = depth_enabled ? format_to_vk(raster_desc.depth_format.val) : VK_FORMAT_UNDEFINED;
    const VkFormat vk_stencil_format = stencil_enabled ? format_to_vk(raster_desc.stencil_format.val) : VK_FORMAT_UNDEFINED;
    const VkPipelineRenderingCreateInfo rendering_info = {
        .sType = VK_STRUCTURE_TYPE_PIPELINE_RENDERING_CREATE_INFO,
        .colorAttachmentCount = raster_desc.colour_targets.len,
        .pColorAttachmentFormats = raster_desc.colour_targets.len ? colour_formats : NULL,
        .depthAttachmentFormat = vk_depth_format,
        .stencilAttachmentFormat = vk_stencil_format,
    };
    const VkPipelineCreateFlags2CreateInfo flags_info = {
        .sType = VK_STRUCTURE_TYPE_PIPELINE_CREATE_FLAGS_2_CREATE_INFO,
        .pNext = &rendering_info,
        .flags = VK_PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT,
    };
    const VkGraphicsPipelineCreateInfo pipeline_info = {
        .sType = VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO,
        .pNext = &flags_info,
        .stageCount = (sizeof(stages) / sizeof(stages[0])),
        .pStages = stages,
        .pVertexInputState = is_meshlet ? NULL : &vertex_input,
        .pInputAssemblyState = is_meshlet ? NULL : &input_assembly,
        .pViewportState = &viewport_state,
        .pRasterizationState = &rasterization,
        .pMultisampleState = &multisample,
        .pDepthStencilState = &depth_stencil,
        .pColorBlendState = &colour_blend,
        .pDynamicState = &dynamic_state,
        .basePipelineIndex = -1,
    };
    VkPipeline pipeline;
    VkResult result = vkCreateGraphicsPipelines(device->device, VK_NULL_HANDLE, 1, &pipeline_info, NULL, &pipeline);
    if (result != VK_SUCCESS) {
        panic(mv_string("TODO: handle failure to create pipeline"));
    }

    HdPipeline* hd_pipeline = mem_alloc(sizeof(HdPipeline), device->gpa); 
    *hd_pipeline = (HdPipeline) {
      .pipeline = pipeline,
      .bind_point = VK_PIPELINE_BIND_POINT_GRAPHICS,
      .data_size = data_size,
    };
    return hd_pipeline;
}

void destroy_pipeline(HdPipeline* pipeline, HdLogicalDevice* device) {
    vkDestroyPipeline(device->device, pipeline->pipeline, NULL);
    mem_free(pipeline, device->gpa);
}

#endif
