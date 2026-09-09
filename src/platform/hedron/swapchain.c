#ifdef USE_VULKAN

#include "platform/signals.h"
#include "platform/hedron/hedron.h"
#include "platform/hedron/internal.h"

void retire_swapchain(HdSwapchain* swapchain);
void wait_present_context(HdLogicalDevice* device, HdPresentContext* context);

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
        if (available_format.format == VK_FORMAT_B8G8R8A8_SRGB &&
            available_format.colorSpace == VK_COLOR_SPACE_SRGB_NONLINEAR_KHR) {
            selected_format = available_format;
        }
    }
    mem_free(available_formats, device->gpa);

    return selected_format;
}

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

void create_present_context(HdLogicalDevice* device, HdPresentContext* out) {
    // TODO: debug checks
    out->present_pending = false; 
    const VkSemaphoreCreateInfo semaphore_info = {
        .sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO,
    };
    VkResult result = vkCreateSemaphore(device->device, &semaphore_info, NULL, &out->acquired);
    if (result != VK_SUCCESS)
        panic(mv_string("TODO: proper error handling in create_present_context"));

    result = vkCreateSemaphore(device->device, &semaphore_info, NULL, &out->rendered);
    if (result != VK_SUCCESS)
        panic(mv_string("TODO: proper error handling in create_present_context"));

    const VkFenceCreateInfo fence_info = {
        .sType = VK_STRUCTURE_TYPE_FENCE_CREATE_INFO,
    };
    result = vkCreateFence(device->device, &fence_info, NULL, &out->presented);
    if (result != VK_SUCCESS)
        panic(mv_string("TODO: proper error handling in create_present_context"));
}

VkResult rebuild_swapchain(HdSwapchain* swapchain) {
    // TODO: aaltonen's version requires a lot more checking/assertions...
    // probably want to add taht...
    HdLogicalDevice* device = swapchain->device;
    HdSurface* surface = swapchain->surface;
    // TODO: We do use check result here for 'correct' return values, but
    //       should make sure memory is cleaned up properly if/when that is the case.
    VkSurfaceCapabilitiesKHR surface_capabilities;
    VkResult result = vkGetPhysicalDeviceSurfaceCapabilitiesKHR(device->physical_device, surface->surface, &surface_capabilities);
    if (result != VK_SUCCESS) return result;

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
        .oldSwapchain = swapchain->swapchain,
    };
    VkSwapchainKHR vk_swapchain;
    result = vkCreateSwapchainKHR(device->device, &swapchain_create_info, NULL, &vk_swapchain);
    if (result != VK_SUCCESS) return result;

    // Free old resources associated with the swapchain, now that we have
    // created a new one (and pointed to the old swapchain)
    retire_swapchain(swapchain);

    // Get number of images
    uint32_t num_images;
    result = vkGetSwapchainImagesKHR(device->device, vk_swapchain, &num_images, NULL);
    if (result != VK_SUCCESS) return result;

    // Allocate and populate images
    VkImage* images = swapchain->images ? swapchain->images : mem_alloc(sizeof(VkImage) * num_images, device->gpa);
    result = vkGetSwapchainImagesKHR(device->device, vk_swapchain, &num_images, images);
    if (result != VK_SUCCESS) return result;

    HdRenderView* render_views = swapchain->render_views ? swapchain->render_views : mem_alloc(sizeof(HdRenderView) * num_images, device->gpa);
    for (size_t i = 0; i < num_images; i++) {
        VkImageViewCreateInfo view_info = {
            .sType = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO,
            .image = images[i], 
            .viewType = VK_IMAGE_VIEW_TYPE_2D,
            .format = surface_format.format,
            .subresourceRange = {
                .aspectMask = VK_IMAGE_ASPECT_COLOR_BIT,
                .levelCount = 1,
                .layerCount = 1,
            },
        };
        VkImageView image_view;
        VkResult result = vkCreateImageView(device->device, &view_info, NULL, &image_view);
        if (result != VK_SUCCESS) return result;
        render_views[i] = (HdRenderView) {
            .image_view = image_view,
            .extent.width = extent.width,
            .extent.height = extent.height,
        };
    }


    HdPresentContext *present_contexts = swapchain->present_contexts;
    if (present_contexts == NULL) {
        present_contexts = mem_alloc(sizeof(HdPresentContext) * num_images, device->gpa);
        for (size_t i = 0; i < num_images; i++) {
            create_present_context(device, &present_contexts[i]);
        }
    }
    bool* initialized = swapchain->initialized ? swapchain->initialized : mem_alloc(sizeof(bool) * num_images, device->gpa);
    for (size_t i = 0; i < num_images; i++) {
        initialized[i] = false;
    }

    *swapchain = (HdSwapchain) {
        .swapchain = vk_swapchain,
        .surface = surface,
        .device = device,
        .extent = {.width = extent.width, .height = extent.height},
        .num_images = num_images,
        .images = images,
        .render_views = render_views,
        .present_contexts = present_contexts,
        .initialized = initialized,
        .next_present_context = 0,
    };

    for (size_t i = 0; i < num_images; i++) {
        render_views[i].swapchain = swapchain;
    }

    return VK_SUCCESS;
}

HdPtrResult create_swapchain(HdLogicalDevice* device, HdSurface* surface) {
    HdSwapchain* swapchain = mem_alloc(sizeof(HdSwapchain), device->gpa);
    *swapchain = (HdSwapchain) {
        .swapchain = VK_NULL_HANDLE,
        .surface = surface,
        .device = device,
    };
    VkResult result = rebuild_swapchain(swapchain);
    if (result == VK_SUCCESS) {
        push_ptr(swapchain, &device->swapchains);
        return (HdPtrResult) {
            .type = Ok,
            .val = swapchain,
        };
    } else {
        return (HdPtrResult) {
            .type = Err,
            .error = convert_error_type(result),
        };
    }
}

//  Like destroy swapchain, but doesn't free memory, just releases resources.
void retire_swapchain(HdSwapchain* swapchain) {
    HdLogicalDevice* device = swapchain->device;
    for (size_t i = 0; i < swapchain->num_images; i++) {
        wait_present_context(device, &swapchain->present_contexts[i]);
    }
    for (size_t i = 0; i < swapchain->num_images; i++) {
        vkDestroyImageView(swapchain->device->device, swapchain->render_views[i].image_view, NULL);
    }
    vkDestroySwapchainKHR(swapchain->device->device, swapchain->swapchain, NULL);
}

void destroy_swapchain(HdSwapchain* swapchain) {
    HdLogicalDevice* device = swapchain->device;
    // IMPORTANT: wait untill all images are presented.
    retire_swapchain(swapchain);
    for (size_t i = 0; i < device->swapchains.len; i++) {
        if (swapchain == device->swapchains.data[i]) {
            device->swapchains.data[i] = device->swapchains.data[device->swapchains.len - 1];
            device->swapchains.len--;
            break;
        }
    }
    for (size_t i = 0; i < swapchain->num_images; i++) {
        vkDestroySemaphore(swapchain->device->device, swapchain->present_contexts[i].acquired, NULL);
        vkDestroySemaphore(swapchain->device->device, swapchain->present_contexts[i].rendered, NULL);
        vkDestroyFence(swapchain->device->device, swapchain->present_contexts[i].presented, NULL);
    }
    mem_free(swapchain->initialized, swapchain->device->gpa);
    mem_free(swapchain->present_contexts, swapchain->device->gpa);
    mem_free(swapchain->render_views, swapchain->device->gpa);
    mem_free(swapchain->images, swapchain->device->gpa);
    mem_free(swapchain, swapchain->device->gpa);
}

bool swapchain_surface_configuration_changed(HdSwapchain* swapchain) {
    VkSurfaceCapabilitiesKHR capabilities = {};
    VkResult result = vkGetPhysicalDeviceSurfaceCapabilitiesKHR(swapchain->device->physical_device, swapchain->surface->surface, &capabilities);
    if (result != VK_SUCCESS) {
        panic(mv_string("TODO: handle failure in swapchain_surface_configuration_changed"));
    }

    const VkExtent2D extent = capabilities.currentExtent;
    const uint32_t variable_extent = UINT32_MAX;
    return extent.width == variable_extent ||
           extent.height == variable_extent ||
           extent.width != swapchain->extent.width ||
           extent.height != swapchain->extent.height;
    //      capabilities.currentTransform != swapchain.transform ||
    //      choose_composite_alpha(capabilities.supportedCompositeAlpha) != swapchain.composite_alpha;
}

void wait_present_context(HdLogicalDevice* device, HdPresentContext* context) {
    if (!context->present_pending)
        return;
    // TODO: debug layer
    //assert_vk(vkWaitForFences(device, 1, &context.presented, VK_TRUE, ~uint64{0}));
    context->present_pending = false;
    VkResult result = vkWaitForFences(device->device, 1, &context->presented, VK_TRUE, UINT64_MAX);
    if (result != VK_SUCCESS)
        panic(mv_string("TODO: handle wait for fence failure in wait_present_context"));
    //finish_present_context(context)
}

HdRenderView* next_frame(HdSwapchain* swapchain) {
    // TODO: handle errors appropriately, also loop on some errors (such as
    // swapchain out of date??)
    HdLogicalDevice* device = swapchain->device;

    HdPresentContext* present_context = &swapchain->present_contexts[swapchain->next_present_context];
    wait_present_context(device, present_context);

    while (true) {
        if (swapchain->recreate_required) {
            rebuild_swapchain(swapchain);
        }

        uint32_t image_index = 0;
        VkResult result = vkAcquireNextImageKHR(swapchain->device->device,
                                                swapchain->swapchain,
                                                UINT64_MAX,
                                                present_context->acquired,
                                                VK_NULL_HANDLE,
                                                &image_index);
        if (result == VK_ERROR_OUT_OF_DATE_KHR) {
            swapchain->recreate_required = true;
            continue;
        }
        if (result != VK_SUCCESS && result != VK_SUBOPTIMAL_KHR)
            panic(mv_string("TODO: handle errors in next_frame"));

        swapchain->current_image = image_index;
        swapchain->current_present = swapchain->next_present_context;
        swapchain->acquired = true;
        swapchain->recreate_required = result == VK_SUBOPTIMAL_KHR && swapchain_surface_configuration_changed(swapchain);
        return &swapchain->render_views[image_index];
    }
}

void resize_notify(HdSwapchain* swapchain, HdExtent extent) {
    swapchain->recreate_required = true;
}

void present(HdSwapchain* swapchain)  {
    // TODO: debug layer
    //assert(swapchain.acquired && swapchain.ready_to_present && "Swapchain must be acquired and submitted before present");

    HdPresentContext* ctx = &swapchain->present_contexts[swapchain->current_present];

    const VkSwapchainPresentFenceInfoKHR fence_info = {
        .sType = VK_STRUCTURE_TYPE_SWAPCHAIN_PRESENT_FENCE_INFO_KHR,
        .swapchainCount = 1,
        .pFences = &ctx->presented,
    };

    const VkPresentInfoKHR present_info = {
        .sType = VK_STRUCTURE_TYPE_PRESENT_INFO_KHR,
        .pNext = &fence_info,
        .waitSemaphoreCount = 1,
        .pWaitSemaphores = &ctx->rendered,
        .swapchainCount = 1,
        .pSwapchains = &swapchain->swapchain,
        .pImageIndices = &swapchain->current_image,
    };

    HdLogicalDevice* device = swapchain->device;
    const VkResult result = vkQueuePresentKHR(device->queue.queue, &present_info);

    // Update state tracking on this swapchain instance
    // TODO: we want a similar flag on the present context for not yet
    // presenting but acquired/submitted???
    ctx->present_pending = true;
    //ctx->swapchain = swapchain->swapchain;

    swapchain->acquired = false;
    swapchain->initialized[swapchain->current_image] = true;
    //swapchain->ready_to_present = false;
    swapchain->next_present_context = (swapchain->current_present + 1) % swapchain->num_images;

    if (result == VK_ERROR_OUT_OF_DATE_KHR) {
        swapchain->recreate_required = true;
    }
    else if (result == VK_SUBOPTIMAL_KHR) {
        swapchain->recreate_required = swapchain->recreate_required || swapchain_surface_configuration_changed(swapchain);
    }
    else if (result != VK_SUCCESS) {
        panic(mv_string("TODO: handle failre to present"));
    }
}

#endif
