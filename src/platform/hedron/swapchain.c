#ifdef USE_VULKAN

#include "platform/hedron/hedron.h"
#include "platform/hedron/internal.h"

static VkSurfaceFormatKHR choose_swap_surface_format(HdLogicalDevice* device, HdSurface* surface) {
    uint32_t num_formats;
    vkGetPhysicalDeviceSurfaceFormatsKHR(device->physical_device,
                                         surface->surface,
                                         &num_formats, NULL);
    VkSurfaceFormatKHR* available_formats = mem_alloc(sizeof(VkSurfaceFormatKHR) * num_formats, device->gpa);
    vkGetPhysicalDeviceSurfaceFormatsKHR(device->physical_device,
                                         surface->surface, &num_formats,
                                         available_formats);

    VkSurfaceFormatKHR selected_format = available_formats[0];
    for (size_t i = 0; i < num_formats; i++) {
        VkSurfaceFormatKHR available_format = available_formats[i];
        if (available_format.format == VK_FORMAT_B8G8R8A8_SRGB
             && available_format.colorSpace == VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
            ) {
            selected_format = available_format;
        }
    }
    mem_free(available_formats, device->gpa);

    return selected_format;
}

// Present mode affects the possibility of screentearing/stalling
/*
static VkPresentModeKHR choose_swap_present_mode(HdLogicalDevice *device, HdSurface *surface) {
   //(VkPresentModeKHR* available_present_modes, uint32_t num_modes) {

    uint32_t num_formats;
    vkGetPhysicalDeviceSurfaceFormatsKHR(device->physical_device,
                                         surface->surface,
                                         &num_formats, NULL);
    VkSurfaceFormatKHR* available_formats = mem_alloc(sizeof(VkSurfaceFormatKHR) * num_formats, device->gpa);
    vkGetPhysicalDeviceSurfaceFormatsKHR(device->physical_device,
                                         surface->surface, &num_formats,
                                         available_formats);
    
    for (size_t i = 0; i < num_modes; i++) {
        VkPresentModeKHR present_mode = available_present_modes[i];
        // Ideal: no screentearing or stalling
        if (present_mode == VK_PRESENT_MODE_MAILBOX_KHR) {
            return present_mode;
        }
    }

    // FIFO is guaranteed to be available by the vulkan standard
    // No screen-tearing, but may stall.
    return VK_PRESENT_MODE_FIFO_KHR;
}
*/

static uint32_t clamp(uint32_t val, uint32_t min, uint32_t max) {
    if (val < min) return min;
    if (val > max) return max;
    return val;
}

static VkExtent2D choose_swap_extent(VkSurfaceCapabilitiesKHR capabilities, HdExtent extent) {
    if (capabilities.currentExtent.width != UINT32_MAX) {
        return capabilities.currentExtent;
    } else {
        VkExtent2D actualExtent = {
            .width = extent.width,
            .height = extent.height,
        };

        actualExtent.width = clamp(actualExtent.width, capabilities.minImageExtent.width, capabilities.maxImageExtent.width);
        actualExtent.height = clamp(actualExtent.height, capabilities.minImageExtent.height, capabilities.maxImageExtent.height);

        return actualExtent;
    }
}

//void create_swapchain(SwapChainSupportDetails swap_chain_details, VkSurfaceKHR surface, uint32_t width, uint32_t height, HdSurface* hd_surface) {
HdPtrResult create_swapchain(HdLogicalDevice* device, HdSurface* surface) {
    VkSurfaceCapabilitiesKHR surface_capabilities;
    VkResult result = vkGetPhysicalDeviceSurfaceCapabilitiesKHR(device->physical_device, surface->surface, &surface_capabilities);
    CHECK_RESULT(result);

    // requested_image_count = min(max(info.requested, minPossiple), maxPossible)
    uint32_t requested_image_count =
        surface_capabilities.minImageCount > 2 ?
        surface_capabilities.minImageCount : 2;
    if (surface_capabilities.maxImageCount != 0) {
        requested_image_count =
            surface_capabilities.maxImageCount < requested_image_count ?
            surface_capabilities.maxImageCount : requested_image_count;
    }

    HdExtent desired_extent = {
        .width = surface->window->width,
        .height = surface->window->height,
    };
    VkExtent2D extent = choose_swap_extent(surface_capabilities, desired_extent);
    VkSurfaceFormatKHR surface_format = choose_swap_surface_format(device, surface);
    VkSwapchainCreateInfoKHR swapchain_create_info = {
        .sType = VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR,
        .surface = surface->surface,
        .minImageCount = requested_image_count,

        .imageFormat = surface_format.format,
        .imageColorSpace = surface_format.colorSpace,
        .imageExtent = extent,
        // I think this is only set > 1 for stereoscopic 3D
        .imageArrayLayers = 1,
        .imageUsage = VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
        // How to orient the image (accounts for, e.g. rotated monitors)
        .preTransform = surface_capabilities.currentTransform,
        // Make the window opaque (may later desire to have
        // transparent/tranclucent windows.
        .compositeAlpha = VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
        .presentMode = VK_PRESENT_MODE_FIFO_KHR,
    };
    VkSwapchainKHR vk_swapchain;
    result = vkCreateSwapchainKHR(device->device, &swapchain_create_info, NULL, &vk_swapchain);
    CHECK_RESULT(result);

    uint32_t num_images;
    vkGetSwapchainImagesKHR(device->device, vk_swapchain, &num_images, NULL);
    VkImage* images = mem_alloc(sizeof(VkImage) * num_images, device->gpa);
    vkGetSwapchainImagesKHR(device->device, vk_swapchain, &num_images, images);

    VkImageView* image_views = mem_alloc(sizeof(VkImage) * num_images, device->gpa);
    for (size_t i = 0; i < num_images; i++) {
        VkImageViewCreateInfo view_info = {
            .sType = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO,
            .image = images[i], 
            .viewType = VK_IMAGE_VIEW_TYPE_2D,
            .format = surface_format.format,
            .subresourceRange = {
                .aspectMask = VK_IMAGE_ASPECT_COLOR_BIT,
                .baseMipLevel = 0,
                .levelCount = 1,
                .baseArrayLayer = 0,
                .layerCount = 1,
            },
        };
        VkResult result = vkCreateImageView(device->device, &view_info, NULL, &image_views[i]);
        CHECK_RESULT(result);
    }

    VkSemaphore* semaphores = mem_alloc(sizeof(VkSemaphore) * num_images, device->gpa);
    for (size_t i = 0; i < num_images; i++) {
      VkSemaphoreCreateInfo semaphoreInfo = {
        .sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO,
      };
      VkResult result = vkCreateSemaphore(device->device, &semaphoreInfo, NULL, &semaphores[i]);
      CHECK_RESULT(result);
    }

    HdSwapchain* swapchain = mem_alloc(sizeof(HdSwapchain), device->gpa);
    *swapchain = (HdSwapchain) {
        .swapchain = vk_swapchain,
        .device = device,
        .extent = {.width = extent.width, .height = extent.height},
        .num_images = num_images,
        .images = images,
        .image_views = image_views,
        .render_complete_semaphores = semaphores,
    };
    return (HdPtrResult) {
        .type = Ok,
        .val = swapchain,
    };
}

void destroy_swapchain(HdSwapchain* swapchain) {
    for (size_t i = 0; i < swapchain->num_images; i++) {
        vkDestroyImageView(swapchain->device->device, swapchain->image_views[i], NULL);
    }
    for (size_t i = 0; i < swapchain->num_images; i++) {
        vkDestroySemaphore(swapchain->device->device, swapchain->render_complete_semaphores[i], NULL);
    }
    vkDestroySwapchainKHR(swapchain->device->device, swapchain->swapchain, NULL);
    mem_free(swapchain->render_complete_semaphores, swapchain->device->gpa);
    mem_free(swapchain->image_views, swapchain->device->gpa);
    mem_free(swapchain->images, swapchain->device->gpa);
    mem_free(swapchain, swapchain->device->gpa);
}

#endif
