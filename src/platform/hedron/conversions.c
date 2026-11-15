#ifdef USE_VULKAN

#include "platform/signals.h"
#include "platform/hedron/hedron.h"
#include "platform/hedron/internal.h"

HdError convert_error_type(VkResult desc) {
// Provided by VK_VERSION_1_0
    switch (desc) {
    case VK_SUCCESS:
        panic(mv_string("There is a bug in the Hedron implementation: calling convert_error_type on a success."));
        break;
    case VK_NOT_READY:
        return HD_NOT_READY;
    case VK_TIMEOUT:
        return HD_TIMEOUT;
    case VK_EVENT_SET:
        return HD_EVENT_SET;
    case VK_EVENT_RESET:
        return HD_EVENT_RESET;
    case VK_INCOMPLETE:
        return HD_INCOMPLETE;
    case VK_ERROR_OUT_OF_HOST_MEMORY:
        return HD_ERROR_OUT_OF_HOST_MEMORY;
    case VK_ERROR_OUT_OF_DEVICE_MEMORY:
        return HD_ERROR_OUT_OF_DEVICE_MEMORY;
    case VK_ERROR_INITIALIZATION_FAILED:
        return HD_ERROR_INITIALIZATION_FAILED;
    case VK_ERROR_DEVICE_LOST:
        return HD_ERROR_DEVICE_LOST;
    case VK_ERROR_MEMORY_MAP_FAILED:
        return HD_ERROR_MEMORY_MAP_FAILED;
    case VK_ERROR_LAYER_NOT_PRESENT:
        return HD_ERROR_LAYER_NOT_PRESENT;
    case VK_ERROR_EXTENSION_NOT_PRESENT:
        return HD_ERROR_EXTENSION_NOT_PRESENT;
    case VK_ERROR_FEATURE_NOT_PRESENT:
        return HD_ERROR_FEATURE_NOT_PRESENT;
    case VK_ERROR_INCOMPATIBLE_DRIVER:
        return HD_ERROR_INCOMPATIBLE_DRIVER;
    case VK_ERROR_TOO_MANY_OBJECTS:
        return HD_ERROR_TOO_MANY_OBJECTS;
    case VK_ERROR_FORMAT_NOT_SUPPORTED:
        return HD_ERROR_FORMAT_NOT_SUPPORTED;
    case VK_ERROR_FRAGMENTED_POOL:
        return HD_ERROR_FRAGMENTED_POOL;
    case VK_ERROR_UNKNOWN:
        return HD_ERROR_UNKNOWN;
    case VK_ERROR_VALIDATION_FAILED_EXT:
        return HD_ERROR_VALIDATION_FAILED;
    case VK_ERROR_OUT_OF_POOL_MEMORY:
        return HD_ERROR_OUT_OF_POOL_MEMORY;
    case VK_ERROR_INVALID_EXTERNAL_HANDLE:
        return HD_ERROR_INVALID_EXTERNAL_HANDLE;
    case VK_ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS:
        return HD_ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS;
    case VK_ERROR_FRAGMENTATION:
        return HD_ERROR_FRAGMENTATION;
    case VK_PIPELINE_COMPILE_REQUIRED:
        return HD_PIPELINE_COMPILE_REQUIRED;
    case VK_ERROR_NOT_PERMITTED:
        return HD_ERROR_NOT_PERMITTED;
    case VK_ERROR_SURFACE_LOST_KHR:
        return HD_ERROR_SURFACE_LOST;
    case VK_ERROR_NATIVE_WINDOW_IN_USE_KHR:
        return HD_ERROR_NATIVE_WINDOW_IN_USE;
    case VK_SUBOPTIMAL_KHR:
        return HD_SUBOPTIMAL;
    case VK_ERROR_OUT_OF_DATE_KHR:
        return HD_ERROR_OUT_OF_DATE;
    case VK_ERROR_INCOMPATIBLE_DISPLAY_KHR:
        return HD_ERROR_INCOMPATIBLE_DISPLAY;
    default:
        panic(mv_string("Unexpected vulkan error code reported to convert_error_type"));
    }
};

String view_error_string(HdError error) {
    switch(error) {
    case HD_NOT_READY:
        return mv_string("HD_NOT_READY");
    case HD_TIMEOUT:
        return mv_string("HD_TIMEOUT");
    case HD_EVENT_SET:
        return mv_string("HD_EVENT_SET");
    case HD_EVENT_RESET:
        return mv_string("HD_EVENT_RESET");
    case HD_INCOMPLETE:
        return mv_string("HD_INCOMPLETE");
    case HD_ERROR_OUT_OF_HOST_MEMORY:
        return mv_string("HD_ERROR_OUT_OF_HOST_MEMORY");
    case HD_ERROR_OUT_OF_DEVICE_MEMORY:
        return mv_string("HD_ERROR_OUT_OF_DEVICE_MEMORY");
    case HD_ERROR_INITIALIZATION_FAILED:
        return mv_string("HD_ERROR_INITIALIZATION_FAILED");
    case HD_ERROR_DEVICE_LOST:
        return mv_string("HD_ERROR_DEVICE_LOST");
    case HD_ERROR_MEMORY_MAP_FAILED:
        return mv_string("HD_ERROR_MEMORY_MAP_FAILED");
    case HD_ERROR_LAYER_NOT_PRESENT:
        return mv_string("HD_ERROR_LAYER_NOT_PRESENT");
    case HD_ERROR_EXTENSION_NOT_PRESENT:
        return mv_string("HD_ERROR_EXTENSION_NOT_PRESENT");
    case HD_ERROR_FEATURE_NOT_PRESENT:
        return mv_string("HD_ERROR_FEATURE_NOT_PRESENT");
    case HD_ERROR_INCOMPATIBLE_DRIVER:
        return mv_string("HD_ERROR_INCOMPATIBLE_DRIVER");
    case HD_ERROR_TOO_MANY_OBJECTS:
        return mv_string("HD_ERROR_TOO_MANY_OBJECTS");
    case HD_ERROR_FORMAT_NOT_SUPPORTED:
        return mv_string("HD_ERROR_FORMAT_NOT_SUPPORTED");
    case HD_ERROR_FRAGMENTED_POOL:
        return mv_string("HD_ERROR_FRAGMENTED_POOL");
    case HD_ERROR_UNKNOWN:
        return mv_string("HD_ERROR_UNKNOWN");
        // Provided by VK_VERSION_1_0
    case HD_ERROR_VALIDATION_FAILED:
        return mv_string("HD_ERROR_VALIDATION_FAILED");
        // Provided by VK_VERSION_1_1
    case HD_ERROR_OUT_OF_POOL_MEMORY:
        return mv_string("HD_ERROR_OUT_OF_POOL_MEMORY");
        // Provided by VK_VERSION_1_1
    case HD_ERROR_INVALID_EXTERNAL_HANDLE:
        return mv_string("HD_ERROR_INVALID_EXTERNAL_HANDLE");
        // Provided by VK_VERSION_1_2
    case HD_ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS:
        return mv_string("HD_ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS");
        // Provided by VK_VERSION_1_2
    case HD_ERROR_FRAGMENTATION:
        return mv_string("HD_ERROR_FRAGMENTATION");
        // Provided by VK_VERSION_1_3
    case HD_PIPELINE_COMPILE_REQUIRED:
        return mv_string("HD_PIPELINE_COMPILE_REQUIRED");
        // Provided by VK_VERSION_1_4
    case HD_ERROR_NOT_PERMITTED:
        return mv_string("HD_ERROR_NOT_PERMITTED");
        // Provided by VK_KHR_surface
    case HD_ERROR_SURFACE_LOST:
        return mv_string("HD_ERROR_SURFACE_LOST");
        // Provided by VK_KHR_surface
    case HD_ERROR_NATIVE_WINDOW_IN_USE:
        return mv_string("HD_ERROR_NATIVE_WINDOW_IN_USE");

        // Provided by VK_KHR_swapchain
    case HD_SUBOPTIMAL:
        return mv_string("HD_SUBOPTIMAL");
        // Provided by VK_KHR_swapchain
    case HD_ERROR_OUT_OF_DATE:
        return mv_string("HD_ERROR_OUT_OF_DATE");
        // Provided by VK_KHR_display_swapchain
    case HD_ERROR_INCOMPATIBLE_DISPLAY:
        return mv_string("HD_ERROR_INCOMPATIBLE_DISPLAY");
    case HD_INCOMPATIBLE_SHADER_BINARY:
        return mv_string("HD_INCOMPATIBLE_SHADER_BINARY");
    default:
        panic(mv_string("Hedron imlementation error: unexpected vulkan error code."));
    }
}

VkCullModeFlags cull_to_vk(HdCull cull) {
    switch (cull) {
    case CullNone: return VK_CULL_MODE_NONE;
    case CullCW:   return VK_CULL_MODE_BACK_BIT;
    case CullCCW:  return VK_CULL_MODE_FRONT_BIT;
    case CullAll:  return VK_CULL_MODE_FRONT_AND_BACK;
    }
    panic(mv_string("unknown cull mode"));
}

VkCullModeFlags blend_factor_to_vk(HdBlendFactor factor) {
    switch (factor) {
    case FactorZero: return VK_BLEND_FACTOR_ZERO;
    case FactorOne: return VK_BLEND_FACTOR_ONE;
    case FactorSrcColour: return VK_BLEND_FACTOR_SRC_COLOR;
    case FactorDstColour: return VK_BLEND_FACTOR_DST_COLOR;
    case FactorSrcAlpha: return VK_BLEND_FACTOR_SRC_ALPHA;
    case FactorDstAlpha: return VK_BLEND_FACTOR_DST_ALPHA;
    case FactorOneMinusSrcColour: return VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR;
    case FactorOneMinusDstColour: return VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR;
    case FactorOneMinusSrcAlpha: return VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA;
    case FactorOneMinusDstAlpha: return VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA;
    case FactorSrcAlphaSaturate: return VK_BLEND_FACTOR_SRC_ALPHA_SATURATE;
    }
    panic(mv_string("Unknown blend factor."));
}

VkBlendOp blend_op_to_vk(HdBlendOp op) {
    switch (op) {
    case BlendAdd: return VK_BLEND_OP_ADD;
    case BlendSubtract: return VK_BLEND_OP_SUBTRACT;
    case BlendRevSubtract: return VK_BLEND_OP_REVERSE_SUBTRACT;
    case BlendMin: return VK_BLEND_OP_MIN;
    case BlendMax: return VK_BLEND_OP_MAX;
    }
    panic(mv_string("Unknown blend operation."));
}

VkAttachmentLoadOp load_op_to_vk(LoadOp op) {
    switch (op) {
    case LOpLoad: return VK_ATTACHMENT_LOAD_OP_LOAD;
    case LOpClear: return VK_ATTACHMENT_LOAD_OP_CLEAR;
    case LOpDiscard: return VK_ATTACHMENT_LOAD_OP_DONT_CARE;
    }
    panic(mv_string("unknown attachment load operation"));
}

VkAttachmentStoreOp store_op_to_vk(StoreOp op) {
    switch (op) {
    case SOpStore: return VK_ATTACHMENT_STORE_OP_STORE;
    case SOpDiscard: return VK_ATTACHMENT_STORE_OP_DONT_CARE;
    }
    panic(mv_string("unknown attachment store operation"));
}

VkFormat format_to_vk(HdFormat format) {
    switch (format) {
    case Format_R8_SRGB: return VK_FORMAT_R8_SRGB;
    case Format_RG8_SRGB: return VK_FORMAT_R8G8_SRGB;
        //case Format_RGB8_SRGB: return VK_FORMAT_R8G8B8_SRGB;
    case Format_RGBA8_SRGB: return VK_FORMAT_R8G8B8A8_SRGB;
    case Format_BGRA8_SRGB: return VK_FORMAT_B8G8R8A8_SRGB;

    case Format_RGBA4_UNorm: return VK_FORMAT_R4G4B4A4_UNORM_PACK16;
    case Format_R5G5B5A1_UNorm: return VK_FORMAT_R5G5B5A1_UNORM_PACK16;
    case Format_R5G6B5_UNorm: return VK_FORMAT_R5G6B5_UNORM_PACK16;

    case Format_R8_UNorm: return VK_FORMAT_R8_UNORM;
    case Format_RG8_UNorm: return VK_FORMAT_R8G8_UNORM;
        //case Format_RGB8_UNorm: return VK_FORMAT_R8G8B8_UNORM;
    case Format_RGBA8_UNorm: return VK_FORMAT_R8G8B8A8_UNORM;
    case Format_BGRA8_UNorm: return VK_FORMAT_B8G8R8A8_UNORM;
    case Format_R16_UNorm: return VK_FORMAT_R16_UNORM;
    case Format_RG16_UNorm: return VK_FORMAT_R16G16_UNORM;
        //case Format_RGB16_UNORM: return VK_FORMAT_R16G16B16_UNORM;
    case Format_RGBA16_UNorm: return VK_FORMAT_R16G16B16A16_UNORM;
    case Format_R8_UInt: return VK_FORMAT_R8_UINT;
    case Format_RG8_UInt: return VK_FORMAT_R8G8_UINT;
        //case Format_RGB8_UInt: return VK_FORMAT_R8G8B8_UINT;
    case Format_RGBA8_UInt: return VK_FORMAT_R8G8B8A8_UINT;
    case Format_BGRA8_UInt: return VK_FORMAT_B8G8R8A8_UINT;
    case Format_R16_UInt: return VK_FORMAT_R16_UINT;
    case Format_RG16_UInt: return VK_FORMAT_R16G16_UINT;
        //case Format_RGB16_UInt: return VK_FORMAT_R16G16B16_UINT;
    case Format_RGBA16_UInt: return VK_FORMAT_R16G16B16A16_UINT;
    case Format_R32_UInt: return VK_FORMAT_R32_UINT;
    case Format_RG32_UInt: return VK_FORMAT_R32G32_UINT;
    case Format_RGB32_UInt: return VK_FORMAT_R32G32B32_UINT;
    case Format_RGBA32_UInt: return VK_FORMAT_R32G32B32A32_UINT;
    case Format_R16_Float: return VK_FORMAT_R16_SFLOAT;
    case Format_RG16_Float: return VK_FORMAT_R16G16_SFLOAT;
        //case Format_RGB16_Float: return VK_FORMAT_R16G16B16_SFLOAT;
    case Format_RGBA16_Float: return VK_FORMAT_R16G16B16A16_SFLOAT;
    case Format_R32_Float: return VK_FORMAT_R32_SFLOAT;
    case Format_RG32_Float: return VK_FORMAT_R32G32_SFLOAT;
    case Format_RGB32_Float: return VK_FORMAT_R32G32B32_SFLOAT;
    case Format_RGBA32_Float: return VK_FORMAT_R32G32B32A32_SFLOAT;
    case Format_RGB10A2_UNorm: return VK_FORMAT_A2B10G10R10_UNORM_PACK32;
    case Format_RG11B10_Float: return VK_FORMAT_B10G11R11_UFLOAT_PACK32;
    case Format_D16_UNorm: return VK_FORMAT_D16_UNORM;
    case Format_D24_UNorm_S8_UInt: return VK_FORMAT_D24_UNORM_S8_UINT;
    case Format_D32_Float: return VK_FORMAT_D32_SFLOAT;
    case Format_S8_UInt: return VK_FORMAT_S8_UINT;
    case Format_D32_Float_S8_UInt: return VK_FORMAT_D32_SFLOAT_S8_UINT;
    case Format_EAC_RG: return VK_FORMAT_EAC_R11G11_UNORM_BLOCK;
    case Format_ASTC_4x4_SRGB: return VK_FORMAT_ASTC_4x4_SRGB_BLOCK;
    case Format_ASTC_4x4_UNorm: return VK_FORMAT_ASTC_4x4_UNORM_BLOCK;
    case Format_BC3_SRGB: return VK_FORMAT_BC3_SRGB_BLOCK;
    case Format_BC3_UNorm: return VK_FORMAT_BC3_UNORM_BLOCK;
    case Format_BC5_RG: return VK_FORMAT_BC5_UNORM_BLOCK;
    case Format_BC6H_UFloat: return VK_FORMAT_BC6H_UFLOAT_BLOCK;
    case Format_BC6H_SFloat: return VK_FORMAT_BC6H_SFLOAT_BLOCK;
    case Format_BC7_SRGB: return VK_FORMAT_BC7_SRGB_BLOCK;
    case Format_BC7_UNorm: return VK_FORMAT_BC7_UNORM_BLOCK;
    case Format_Count:
        panic(mv_string("unknown image format"));
    }

    panic(mv_string("unknown image format"));
}

VkImageType texshape_to_vk(HdTextureShape type) {
    switch (type) {
    case Tx1d: return VK_IMAGE_TYPE_1D;
    case Tx3d: return VK_IMAGE_TYPE_3D;
    case Tx2d:
    case TxCube:
    case Tx2dArray:
    case TxCubeArray: return VK_IMAGE_TYPE_2D;
    }
    return VK_IMAGE_TYPE_MAX_ENUM;
}

VkImageViewType texshape_to_vk_view(HdTextureShape type) {
    switch (type) {
    case Tx1d: return VK_IMAGE_VIEW_TYPE_1D;
    case Tx2d: return VK_IMAGE_VIEW_TYPE_2D;
    case Tx3d: return VK_IMAGE_VIEW_TYPE_3D;
    case TxCube: return VK_IMAGE_VIEW_TYPE_CUBE;
    case Tx2dArray: return VK_IMAGE_VIEW_TYPE_2D_ARRAY;
    case TxCubeArray: return VK_IMAGE_VIEW_TYPE_CUBE_ARRAY;
    }
    return VK_IMAGE_VIEW_TYPE_MAX_ENUM;
}

VkPipelineStageFlags2 stage_to_vk(HdStage stages) {
    VkPipelineStageFlags2 result = 0;
    if (stages & StIndirect) result |= VK_PIPELINE_STAGE_2_DRAW_INDIRECT_BIT;
    if (stages & StIndexInput) result |= VK_PIPELINE_STAGE_2_INDEX_INPUT_BIT;
    if (stages & StVertex) result |= VK_PIPELINE_STAGE_2_VERTEX_SHADER_BIT;
    if (stages & StTask) result |= VK_PIPELINE_STAGE_2_TASK_SHADER_BIT_EXT;
    if (stages & StMesh) result |= VK_PIPELINE_STAGE_2_MESH_SHADER_BIT_EXT;
    if (stages & StDepthStencilTests) result |= VK_PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT | VK_PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT;
    if (stages & StFragment) result |= VK_PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT;
    if (stages & StColourOutput) result |= VK_PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT;
    if (stages & StCompute) result |= VK_PIPELINE_STAGE_2_COMPUTE_SHADER_BIT;
    if (stages & StTransfer) result |= VK_PIPELINE_STAGE_2_COPY_BIT;
    if (stages & StHost) result |= VK_PIPELINE_STAGE_2_HOST_BIT;
    if (stages & StAllCommands) result |= VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT;
    return result;
}

VkAccessFlags2 access_to_vk(HdAccess accesses) {
    VkAccessFlags2 result = 0;
    if (accesses & AccTransferRead) result |= VK_ACCESS_2_TRANSFER_READ_BIT;
    if (accesses & AccTransferWrite) result |= VK_ACCESS_2_TRANSFER_WRITE_BIT;
    if (accesses & AccShaderRead) result |= VK_ACCESS_2_SHADER_STORAGE_READ_BIT | VK_ACCESS_2_SHADER_SAMPLED_READ_BIT;
    if (accesses & AccShaderWrite) result |= VK_ACCESS_2_SHADER_STORAGE_WRITE_BIT;
    if (accesses & AccColourRead) result |= VK_ACCESS_2_COLOR_ATTACHMENT_READ_BIT;
    if (accesses & AccColourWrite) result |= VK_ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT;
    if (accesses & AccDepthStencilRead) result |= VK_ACCESS_2_DEPTH_STENCIL_ATTACHMENT_READ_BIT;
    if (accesses & AccDepthStencilWrite) result |= VK_ACCESS_2_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT;
    if (accesses & AccIndirectRead) result |= VK_ACCESS_2_INDIRECT_COMMAND_READ_BIT;
    if (accesses & AccIndexRead) result |= VK_ACCESS_2_INDEX_READ_BIT;
    if (accesses & AccHostRead) result |= VK_ACCESS_2_HOST_READ_BIT;
    if (accesses & AccDescriptorRead) result |= VK_ACCESS_2_SAMPLER_HEAP_READ_BIT_EXT | VK_ACCESS_2_RESOURCE_HEAP_READ_BIT_EXT;
    return result;
}

// ------------------------------------------------------------ 
// 
//    Queries 
// 
// ------------------------------------------------------------

HdTextureFormatInfo get_texture_format_info(HdFormat format) {
    switch (format) {
    case Format_R8_SRGB:
    case Format_R8_UNorm:
    case Format_R8_UInt:
    case Format_S8_UInt:
        return (HdTextureFormatInfo) {
            .block_extent = {.width = 1, .height = 1},
            .bytes_per_block = 1,
            .depth = false,
            .stencil = format == Format_S8_UInt,
        };
    case Format_RG8_SRGB:
    case Format_RGBA4_UNorm:
    case Format_R5G5B5A1_UNorm:
    case Format_R5G6B5_UNorm:
    case Format_RG8_UNorm:
    case Format_R16_UNorm:
    case Format_RG8_UInt:
    case Format_R16_UInt:
    case Format_R16_Float:
    case Format_D16_UNorm:
        return (HdTextureFormatInfo) {
            .block_extent = {.width = 1, .height = 1},
            .bytes_per_block = 2,
            .depth = format == Format_D16_UNorm,
            .stencil = false,
        };
    case Format_RGBA8_SRGB:
    case Format_BGRA8_SRGB:
    case Format_RGBA8_UNorm:
    case Format_BGRA8_UNorm:
    case Format_RG16_UNorm:
    case Format_RGBA8_UInt:
    case Format_BGRA8_UInt:
    case Format_RG16_UInt:
    case Format_R32_UInt:
    case Format_RG16_Float:
    case Format_R32_Float:
    case Format_RGB10A2_UNorm:
    case Format_RG11B10_Float:
    case Format_D24_UNorm_S8_UInt:
    case Format_D32_Float:
        return (HdTextureFormatInfo) {
            .block_extent = {.width = 1, .height = 1},
            .bytes_per_block = 4,
            .depth = format == Format_D24_UNorm_S8_UInt || format == Format_D32_Float,
            .stencil = format == Format_D24_UNorm_S8_UInt,
        };
    case Format_RGBA16_UNorm:
    case Format_RGBA16_UInt:
    case Format_RG32_UInt:
    case Format_RGBA16_Float:
    case Format_RG32_Float:
    case Format_D32_Float_S8_UInt:
        return (HdTextureFormatInfo) {
            .block_extent = {.width = 1, .height = 1},
            .bytes_per_block = 8,
            .depth = format == Format_D32_Float_S8_UInt,
            .stencil = format == Format_D32_Float_S8_UInt,
        };
    case Format_RGB32_UInt:
    case Format_RGB32_Float:
        return (HdTextureFormatInfo) {
            .block_extent = {.width = 1, .height = 1},
            .bytes_per_block = 12,
        };
    case Format_RGBA32_UInt:
    case Format_RGBA32_Float:
        return (HdTextureFormatInfo) {
            .block_extent = {.width = 1, .height = 1},
            .bytes_per_block = 16,
        };
    case Format_EAC_RG:
    case Format_ASTC_4x4_SRGB:
    case Format_ASTC_4x4_UNorm:
    case Format_BC3_SRGB:
    case Format_BC3_UNorm:
    case Format_BC5_RG:
    case Format_BC6H_UFloat:
    case Format_BC6H_SFloat:
    case Format_BC7_SRGB:
    case Format_BC7_UNorm:
        return (HdTextureFormatInfo) {
            .block_extent = {.width = 4, .height = 4},
            .bytes_per_block = 16,
        };
    case Format_Count:
        return (HdTextureFormatInfo) {};
    }
    // TODO: panic??
    return (HdTextureFormatInfo) {};
}

#endif
