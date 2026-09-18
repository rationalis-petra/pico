#ifdef USE_VULKAN

#include "platform/signals.h"
#include "platform/hedron/hedron.h"
#include "platform/hedron/internal.h"

#ifdef WINDOW_SYSTEM
HdPtrResult create_window_surface(struct PlWindow *window, HdInstance* instance) {
    VkSurfaceKHR surface;

#if (OS_FAMILY == UNIX) && (WINDOW_SYSTEM == 1)
    VkXlibSurfaceCreateInfoKHR create_info = (VkXlibSurfaceCreateInfoKHR){
        .sType = VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR,
        .dpy = get_x11_display(),
        .window = window->x11_window,
    };

    VkResult result = vkCreateXlibSurfaceKHR(instance->vk_instance, &create_info, NULL, &surface);
    CHECK_RESULT(result);

#elif (OS_FAMILY == UNIX) && (WINDOW_SYSTEM == 2)
    VkWaylandSurfaceCreateInfoKHR create_info = (VkWaylandSurfaceCreateInfoKHR){};
    create_info.sType = VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR;
    create_info.display = get_wl_display();
    create_info.surface = window->surface;

    VkResult result = vkCreateWaylandSurfaceKHR(instance->vk_instance, &create_info, NULL, &surface);
    CHECK_RESULT(result);

    // TODO: check for present support on graphics queue

#elif OS_FAMILY == WINDOWS
    VkWin32SurfaceCreateInfoKHR create_info = (VkWin32SurfaceCreateInfoKHR) {
        .sType = VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR,
        .hwnd = window->impl,
        .hinstance = GetModuleHandle(NULL),
    };

    VkResult result = vkCreateWin32SurfaceKHR(instance->vk_instance, &create_info, NULL, &surface);
    CHECK_RESULT(result);
#else
#error "unrecognized OS"
#endif


    HdSurface* hd_surface = mem_alloc(sizeof(HdSurface), instance->gpa);
    *hd_surface = (HdSurface) {
        .instance = instance,
        .surface = surface,
        .window = window,
    };
    return (HdPtrResult) {
        .type = Ok,
        .val = hd_surface,
    };
}

void destroy_window_surface(HdSurface* surface) {
    vkDestroySurfaceKHR(surface->instance->vk_instance, surface->surface, NULL);
    mem_free(surface, surface->instance->gpa);
}

#endif

#endif
