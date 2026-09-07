#ifdef USE_VULKAN

#include <string.h>
#include "platform/signals.h"
#include "platform/hedron/hedron.h"
#include "platform/hedron/internal.h"

HdSemaphore* create_semaphore(HdLogicalDevice* device, uint64_t init_value) {
    VkSemaphoreTypeCreateInfo semaphore_type_info = {
        .sType = VK_STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO,
        .semaphoreType = VK_SEMAPHORE_TYPE_TIMELINE,
        .initialValue = init_value,
    };

    VkSemaphoreCreateInfo semaphore_info = {
        .sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO,
        .pNext = &semaphore_type_info,
    };

    VkSemaphore vk_semaphore;
    VkResult res = vkCreateSemaphore(device->device, &semaphore_info, NULL, &vk_semaphore);
    if (res != VK_SUCCESS) {
        panic(mv_string("TODO: export failure to create semaphore as part of interface."));
    }

    HdSemaphore* semaphore = mem_alloc(sizeof(HdSemaphore), device->gpa); 
    *semaphore = (HdSemaphore) {
        .semaphore = vk_semaphore,
    };
    return semaphore;
}

void wait_semaphore(HdLogicalDevice* device, HdSemaphore* sema, uint64_t value) {
    VkSemaphoreWaitInfo wait_info = {
        .sType = VK_STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO,
        .semaphoreCount = 1,
        .pSemaphores = &sema->semaphore,
        .pValues = &value,
    };

    // TODO: decide on timeout... expose in API?
    // this value is ~10 seconds, I think?
    const uint64_t timeout = 10000000000;
    vkWaitSemaphores(device->device, &wait_info, timeout);

    // TODO: lock the for loop, as it iterates over submitted command pools.
    // std::lock_guard lock(vulkanDevice->submitMutex);
    PendingBufferArray* array = sem_bufs_lookup(sema, device->pending_buffers);
    if (array) {
        for (size_t i = 0; i < array->len; i++) {
            PendingBuffer buf = array->data[i];
            if (buf.value <= value) {
                // The commands associated with this buffer have completed, so
                // recycle it into the command pool.
                push_ptr(buf.commands, &device->usable_buffers);
                array->data[i] = array->data[array->len - 1];
                array->len--;
                i--;
            }
        }
    }
}

void destroy_semaphore(HdLogicalDevice* device, HdSemaphore* sema) {
    vkDestroySemaphore(device->device, sema->semaphore, NULL);
    mem_free(sema, device->gpa);
}


#endif
