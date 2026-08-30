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
    case VK_ERROR_VALIDATION_FAILED:
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

#endif
