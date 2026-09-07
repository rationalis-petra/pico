#ifdef USE_VULKAN

#include "platform/signals.h"
#include "platform/hedron/hedron.h"
#include "platform/hedron/internal.h"


// Pipeline
typedef enum {
    ComputePipeline,
    GraphicsPipeline,
} PipelineType;

// Called when device is initialize
void initialize_pipeline_layouts(HdLogicalDevice* device) {
    VkDescriptorSetLayoutBinding bindings[] = {
    // Binding 0: Fixed array of static engine samplers (e.g., 16 total)
    //     TODO: Allocate a larger number up-front if necessary
    //     TODO: possiply split some of these so that there are ~8-16 pImmuble
    //           samplers, with an array of dynamic ones.
    {
        .binding = 0,
        .descriptorType = VK_DESCRIPTOR_TYPE_SAMPLER,
        .descriptorCount = 16,
        .stageFlags = VK_SHADER_STAGE_ALL
    },
    // Binding 1: Variable-sized unbounded texture array (Must be last!)
    //    Must be last so that it can vary in size.
    {
        .binding = 1,
        .descriptorType = VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE,
        .descriptorCount = device->max_sampled_images, // Variable up to GPU hardware limit
        .stageFlags = VK_SHADER_STAGE_ALL
    }
    };

    VkDescriptorBindingFlags binding_flags[2] = {
        0, // Binding 0 (Samplers)
        VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT | 
        VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT // Binding 1 (Textures)
    };

    VkDescriptorSetLayoutBindingFlagsCreateInfo binding_flags_info = {
        .sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO,
        .pNext = NULL,
        .bindingCount = 2,               // Must match layout bindingCount!
        .pBindingFlags = binding_flags,  // Pointer to the array above
    };

    VkDescriptorSetLayoutCreateInfo layout_info = {
        .sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO,
        // Crucial Flag: Indicates this layout is used for Descriptor Buffers
        .flags = VK_DESCRIPTOR_SET_LAYOUT_CREATE_DESCRIPTOR_BUFFER_BIT_EXT,
        .bindingCount = 2,
        .pBindings = bindings,
        .pNext = &binding_flags_info,
    };

    VkDescriptorSetLayout layout;
    vkCreateDescriptorSetLayout(device->device, &layout_info, NULL, &layout);
    device->descriptor_set_layout = layout;

    // Compute
    {
      VkPushConstantRange pushConstantRange = {
        .stageFlags = VK_SHADER_STAGE_COMPUTE_BIT,
        .offset = 0,
        // Guaranteed by the vulkan spec.
        .size = 128,
        /* TODO:
         *  We want our own shading language which allows the main to take
         *  values of arbitrary type, compiling to use push-constatns behind the
         *  scenes.
         *  Make this grow to device size/limit?
         */
      };

        VkPipelineLayoutCreateInfo pipelineLayoutCreateInfo = {
            .sType = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
            .setLayoutCount = 1,
            .pSetLayouts = &layout,
            .pushConstantRangeCount = 1,
            .pPushConstantRanges = &pushConstantRange,
        };

        VkPipelineLayout pipelineLayout;
        vkCreatePipelineLayout(device->device, &pipelineLayoutCreateInfo, NULL, &pipelineLayout);

        device->compute_pipeline_layout = pipelineLayout;
    }
    // Graphics
    {
        VkPushConstantRange pushConstantRange = {
            .stageFlags =
                VK_SHADER_STAGE_VERTEX_BIT     |
                //VK_SHADER_STAGE_TASK_BIT_EXT | TODO: enable me when we add
                //VK_SHADER_STAGE_MESH_BIT_EXT |       proper meshlet pipeline support 
                VK_SHADER_STAGE_FRAGMENT_BIT,
            .offset = 0,
            .size = sizeof(VkDeviceAddress) * 3, // vertex/mesh + pixel + indirect multi strides
        };

        VkPipelineLayoutCreateInfo pipelineLayoutCreateInfo = {
            .sType = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
            .setLayoutCount = 1,
            .pSetLayouts = &layout,
            .pushConstantRangeCount = 1,
            .pPushConstantRanges = &pushConstantRange,
        };

        VkPipelineLayout pipelineLayout;
        vkCreatePipelineLayout(device->device, &pipelineLayoutCreateInfo, NULL, &pipelineLayout);

        device->graphics_pipeline_layout = pipelineLayout;
    }

    // Ray tracing ignored for now
}

void deinitialize_pipeline_layouts(HdLogicalDevice* device) {
    vkDestroyPipelineLayout(device->device, device->compute_pipeline_layout, NULL);
    vkDestroyPipelineLayout(device->device, device->graphics_pipeline_layout, NULL);
    vkDestroyDescriptorSetLayout(device->device, device->descriptor_set_layout, NULL);
}

const char* universal_entry_point = "main";
HdPipeline* create_compute_pipeline(U32Slice compute_IR, size_t data_size, HdLogicalDevice* device) {
    //VulkanDevice* vulkanDevice = device->vulkanDevice;
    // TODO: extract static sampler info from complied shader. This will reqiure 
    //       replacing the U32Slice with a richer shader datatype.

    const VkShaderModuleCreateInfo shader_module_info = {
        .sType = VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
        .codeSize = compute_IR.len * sizeof(uint32_t),
        .pCode = compute_IR.data,
    };

    VkShaderModule shader_module;
    VkResult result = vkCreateShaderModule(device->device, &shader_module_info, NULL, &shader_module);
    if (result != VK_SUCCESS) {
        panic(mv_string("TODO: handle shader creation failure appropriately"));
    }

    VkComputePipelineCreateInfo pipelineCreateInfo = {
        .sType = VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO,
        .layout = device->compute_pipeline_layout,
        .flags = VK_PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT,
        .stage.sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
        .stage.stage = VK_SHADER_STAGE_COMPUTE_BIT,
        .stage.module = shader_module,
        //.stage.pSpecializationInfo = samplerSpecInfo,
        .stage.pName = universal_entry_point,
    };

    // Cooperative-matrix shaders use Subgroup memory scope and require a fixed,
    // fully-populated subgroup size; pin it (clamped to the device's supported
    // range) so the dispatch doesn't fault on hardware whose default subgroup
    // size differs from what the shader was compiled for.
    /*
    VkPipelineShaderStageRequiredSubgroupSizeCreateInfo requiredSubgroupSizeInfo = {};
    const VkPhysicalDeviceSubgroupSizeControlProperties& subgroupLimits = vulkanDevice->subgroupSizeControlProperties;
    if (requiredSubgroupSize != 0 && (subgroupLimits.requiredSubgroupSizeStages & VK_SHADER_STAGE_COMPUTE_BIT)) {
        uint32_t clamped = std::clamp(requiredSubgroupSize, subgroupLimits.minSubgroupSize, subgroupLimits.maxSubgroupSize);
        requiredSubgroupSizeInfo.sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_REQUIRED_SUBGROUP_SIZE_CREATE_INFO;
        requiredSubgroupSizeInfo.requiredSubgroupSize = clamped;
        pipelineCreateInfo.stage.pNext = &requiredSubgroupSizeInfo;
        pipelineCreateInfo.stage.flags |= VK_PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT;
    }
    */

    VkPipeline pipeline;
    result = vkCreateComputePipelines(device->device, VK_NULL_HANDLE, 1, &pipelineCreateInfo, NULL, &pipeline);
    vkDestroyShaderModule(device->device, shader_module, NULL);

    if (result != VK_SUCCESS) {
        panic(mv_string("TODO: gracefully handle pipeline creation failure")) ;
    }

    HdPipeline* out = mem_alloc(sizeof(HdPipeline), device->gpa);
    *out = (HdPipeline) {
        .pipeline = pipeline,
        .bind_point = VK_PIPELINE_BIND_POINT_COMPUTE,
        .data_size = data_size,
    };
    return out;
}

VkPipeline creage_graphics_pipeline_internal(U32Slice vertex_IR, U32Slice meshlet_IR, U32Slice pixel_IR, HdRasterDescription desc, VkDevice device) {
    panic(mv_string("Not implemented: create_graphics_pipeline_internal"));
    //bool vertex = vertex_IR.len > 0;
    //U8Slice actual_IR = vertex ? vertex_IR : meshlet_IR;

    /**
     * A sampler is just a description of how a texture ought to be read, i.e.
     * • Whether to upscale/downscale
     * • What to do if coordinates are out of bounds (clamp vs repeat etc.)
     * • ...
     * 
     * There are traditionally 3 kinds of samplers
     * • Dynamic: The GPU/shader code knows that there is a sampler, but not
     *     what the parametes are. These can be set by the CPU at runtime
     * • Static: The GPU/shader code knows that there is a sampler sampler is,
     *     but does not specify the parameters. 
     *     Parameters are baked into the pipeline by the CPU.
     * • Inline: The GPU/shader code specifies the samlpler parameters directly.
     * 
     * At current, we only support dynamic and inline samplers: static shaders
     * will require a custom SPIR-V complier that also emits the information
     * needed for static shaders. Note that these are needed because static
     * shaders support more features than inline ones (I think, should do more
     * research to verify this is 100% true).
     */

    /*
    StaticSamplerStage vertexSamplerStage;
    const VkSpecializationInfo* vertexSamplerSpecInfo = NULL;
    U8Array vertex_module_IR = vertexSamplerStage.prepare(vulkanDevice, actual_IR, &vertexSamplerSpecInfo);
    */
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
