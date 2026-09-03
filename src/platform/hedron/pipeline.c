#ifdef USE_VULKAN

#include "platform/signals.h"
#include "platform/hedron/hedron.h"
#include "platform/hedron/internal.h"


// Pipeline
struct HdPipeline {
    VkPipeline pipeline;
    VkPipelineBindPoint bind_point;
};

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
            .size = sizeof(VkDeviceAddress),
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
HdPipeline* create_compute_pipeline(U32Slice compute_IR, HdLogicalDevice* device) {
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

HdPipeline* create_graphics_pipeline(U32Slice vertex_IR, U32Slice pixel_IR, HdRasterDescription desc, HdLogicalDevice* device) {
    U32Slice null_IR = {};
    VkPipeline pipeline = creage_graphics_pipeline_internal(vertex_IR, null_IR, pixel_IR, desc, device->device);
    HdPipeline* hd_pipeline = mem_alloc(sizeof(HdPipeline), device->gpa); 
    *hd_pipeline = (HdPipeline) {
      .pipeline = pipeline,
      .bind_point = VK_PIPELINE_BIND_POINT_GRAPHICS,
    };
    return hd_pipeline;
}

HdPipeline* create_graphics_meshlet_pipeline(U32Slice meshlet_IR, U32Slice pixel_IR, HdRasterDescription desc, HdLogicalDevice* device) {
    U32Slice null_IR = {};
    VkPipeline pipeline = creage_graphics_pipeline_internal(null_IR, meshlet_IR, pixel_IR, desc, device->device);
    HdPipeline* hd_pipeline = mem_alloc(sizeof(HdPipeline), device->gpa); 
    *hd_pipeline = (HdPipeline) {
      .pipeline = pipeline,
      .bind_point = VK_PIPELINE_BIND_POINT_GRAPHICS,
    };
    return hd_pipeline;
}

void free_pipeline(HdPipeline* pipeline, HdLogicalDevice* device) {
    vkDestroyPipeline(device->device, pipeline->pipeline, NULL);
    mem_free(pipeline, device->gpa);
}

#endif
