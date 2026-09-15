#ifdef USE_VULKAN

#include "data/meta/array_impl.h"
#include "data/meta/amap_impl.h"

#include "platform/signals.h"
#include "platform/hedron/hedron.h"
#include "platform/hedron/internal.h"

ARRAY_COMMON_IMPL(PendingBuffer, pbuf, PendingBuffer);
AMAP_IMPL(HdSemaphore*, PendingBufferArray, sem_bufs, SemBufs);

#define IMAGE_BARRIER_BATCH_SIZE 64 

void record_image_barriers(VkCommandBuffer command_buffer, VkImageMemoryBarrier2* barriers, uint32_t num_barriers) {
    // TODO: debug layer
    //assert((command_buffer && barriers.data && barriers.size != 0 && barriers.size <= UINT_MAX) && "image barrier batch is invalid");
    const VkDependencyInfo dependency = {
        .sType = VK_STRUCTURE_TYPE_DEPENDENCY_INFO,
        .imageMemoryBarrierCount = num_barriers,
        .pImageMemoryBarriers = barriers,
    };
    vkCmdPipelineBarrier2(command_buffer, &dependency);
}

void command_begin(HdCommandBuffer* buffer, HdLogicalDevice* device) {
    vkResetCommandPool(device->device, buffer->pool, 0);
    VkCommandBufferBeginInfo begin_info = {
      .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
      .flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
    };

    vkBeginCommandBuffer(buffer->buffer, &begin_info);
    if (device->pending_texture_initializations.first) {
        VkImageMemoryBarrier2 barriers[IMAGE_BARRIER_BATCH_SIZE] = {};
        uint32_t barrier_count = 0;
        while (device->pending_texture_initializations.first) {
            HdTextureInitialization* initialization = device->pending_texture_initializations.first;
            remove_texture_initialization(initialization);
            barriers[barrier_count++] = (VkImageMemoryBarrier2) {
                .sType = VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2,
                .srcStageMask = VK_PIPELINE_STAGE_2_NONE,
                .dstStageMask = VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT,
                .dstAccessMask = VK_ACCESS_2_MEMORY_READ_BIT | VK_ACCESS_2_MEMORY_WRITE_BIT,
                .oldLayout = VK_IMAGE_LAYOUT_UNDEFINED,
                .newLayout = VK_IMAGE_LAYOUT_GENERAL,
                .srcQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED,
                .dstQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED,
                .image = initialization->image,
                .subresourceRange = {
                    .aspectMask = initialization->aspect_mask,
                    .levelCount = initialization->mip_levels,
                    .layerCount = initialization->array_layers,
                },
            };
            if (barrier_count == IMAGE_BARRIER_BATCH_SIZE) {
                record_image_barriers(buffer->buffer, barriers, barrier_count);
                barrier_count = 0;
            }
        }
        if (barrier_count != 0) record_image_barriers(buffer->buffer, barriers, barrier_count);
    }
}

HdCommandBuffer* start_recording_commands(HdLogicalDevice* device) {
    HdQueue* queue = &device->queue;

  if (device->usable_buffers.len > 0) {
    HdCommandBuffer* buffer = pop_ptr(&device->usable_buffers);
    command_begin(buffer, queue->device);
    return buffer;
  }

  VkCommandPoolCreateInfo pool_info = {
    .sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
    .queueFamilyIndex = queue->queue_family,
    .flags = VK_COMMAND_POOL_CREATE_TRANSIENT_BIT,
  };

  VkCommandPool pool;
  VkResult result = vkCreateCommandPool(queue->device->device, &pool_info, NULL, &pool);
  if (result != VK_SUCCESS) {
    panic(mv_string("Failed to create command pool"));
  }

  VkCommandBufferAllocateInfo alloc_info = {
    .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
    .commandPool = pool,
    .level = VK_COMMAND_BUFFER_LEVEL_PRIMARY,
    .commandBufferCount = 1,
  };
  VkCommandBuffer buffer;
  vkAllocateCommandBuffers(queue->device->device, &alloc_info, &buffer);
  if (result != VK_SUCCESS) {
    panic(mv_string("Failed to create command pool"));
  }

  HdCommandBuffer* hd_buffer = mem_alloc(sizeof(HdCommandBuffer), queue->device->gpa);
  *hd_buffer = (HdCommandBuffer) {
    .queue = queue,
    .buffer = buffer,
    .pool = pool,
    .device = queue->device,
  };

  command_begin(hd_buffer, queue->device);
  return hd_buffer;
}

void submit_commands_internal(HdQueue *queue, PtrSlice command_buffers,
                              HdSemaphore *time_semaphore, uint64_t value,
                              VkSemaphore *wait_semaphores,
                              VkSemaphore *signal_semaphores,
                              uint32_t num_semaphores) {
    HdLogicalDevice* device = queue->device;
    /* TODO: debugging layer
    TimelineSemaphore* completion_semaphore = completion.semaphore;
    assert((completion_semaphore &&
                                  completion_semaphore->state == device &&
                                  completion_semaphore->semaphore) && "submission completion requires a live timeline semaphore owned by the device");
    assert(commands.data && commands.size != 0 && commands.size <= UINT_MAX);
    assert(device->active_command_buffers == commands.size && "submit must consume every begun command buffer");
    assert(!device->pending_texture_initializations.first && !device->pending_texture_initializations.last && "pending texture transitions were not recorded");
    assert(device->command_context_count >= commands.size);
    */
    VkCommandBufferSubmitInfo* command_submit_infos = mem_alloc(sizeof(VkCommandBufferSubmitInfo) * command_buffers.len, device->gpa);
    for (size_t i = 0; i < command_buffers.len; i++) {
        HdCommandBuffer* current = command_buffers.data[i];
        // TODO: debug layer!
        //assert((current && current->state == device) && "command buffer batch contains an invalid handle");
        //assert_vk();
        VkResult result = vkEndCommandBuffer(current->buffer);
        if (result != VK_SUCCESS) {
            panic(mv_string("TODO: handle errors in submission"));
        }

        // TODO: what is this for in aaltonen's version?
        //current->state = nullptr;
        //assert(device->active_command_buffers != 0);
        //--device->active_command_buffers;
        command_submit_infos[i] = (VkCommandBufferSubmitInfo) {
            .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO,
            .commandBuffer = current->buffer,
            .deviceMask = 1,
        };
    }
    // TODO: debug layer
    //assert(device->active_command_buffers == 0);
    //const uint64 retirement = device->next_command_retirement();
    VkSemaphoreSubmitInfo* wait_infos = mem_alloc(sizeof(VkSemaphoreSubmitInfo) * num_semaphores, device->gpa);
    for (size_t i = 0; i < num_semaphores; i++) {
        wait_infos[i] = (VkSemaphoreSubmitInfo) {
            .sType = VK_STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO,
            .semaphore = wait_semaphores[i],
            .stageMask = VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT,
        };
    }

    // + 1 is for the timeline semaphore.
    VkSemaphoreSubmitInfo* work_signal_infos = mem_alloc(sizeof(VkSemaphoreSubmitInfo) * (num_semaphores + 1), device->gpa);
    // Timeline semaphore
    work_signal_infos[0] = (VkSemaphoreSubmitInfo) {
        .sType = VK_STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO,
        .semaphore = time_semaphore->semaphore,
        .value = value,
        .stageMask = VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT,
    };
    // Render completion semaphores for swapchain
    for (size_t i = 0; i < num_semaphores; i++) {
      work_signal_infos[i + 1] = (VkSemaphoreSubmitInfo) {
        .sType = VK_STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO,
        .semaphore = signal_semaphores[i],
        .stageMask = VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT,
      };
    }

    /*
    const VkSemaphoreSubmitInfo retirement_signal_info = {
        .sType = VK_STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO,
        .semaphore = device->command_retirement,
        .value = retirement,
        .stageMask = VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT,
    };
    */

    const VkSubmitInfo2 submit_infos[2] = {
      {
        .sType = VK_STRUCTURE_TYPE_SUBMIT_INFO_2,
        .waitSemaphoreInfoCount = num_semaphores,
        .pWaitSemaphoreInfos = num_semaphores > 0 ? wait_infos : NULL,
        .commandBufferInfoCount = command_buffers.len,
        .pCommandBufferInfos = command_submit_infos,
        .signalSemaphoreInfoCount = num_semaphores + 1,
        .pSignalSemaphoreInfos = work_signal_infos,
      },
      /*
      {
        .sType = VK_STRUCTURE_TYPE_SUBMIT_INFO_2,
        .signalSemaphoreInfoCount = 1,
        .pSignalSemaphoreInfos = &retirement_signal_info,
      },
      */
    };
    VkResult result = vkQueueSubmit2(queue->queue, 1, submit_infos, VK_NULL_HANDLE);
    if (result != VK_SUCCESS)
        panic(mv_string("TODO: "));

    // Cleanup
    mem_free(work_signal_infos, device->gpa);
    mem_free(wait_infos, device->gpa);
    mem_free(command_submit_infos, device->gpa);


    // TODO: we want to check the thread safety of queue submissions 
    //       and ensure that both the adding to pending buffers and the
    //       queue submission itself are thread-safe 
    PendingBufferArray* arr = sem_bufs_lookup(time_semaphore, device->pending_buffers);
    if (arr == NULL) {
        sem_bufs_insert(time_semaphore, mk_pbuf_array(command_buffers.len, device->gpa), &device->pending_buffers);
        arr = sem_bufs_lookup(time_semaphore, device->pending_buffers);
    }

    for (size_t i = 0; i < command_buffers.len; i++) {
        PendingBuffer pbuf = {
            .commands = command_buffers.data[i],
            .value = value,
        };
        push_pbuf(pbuf, arr);
    }
}

void submit_commands(HdLogicalDevice* device, PtrSlice command_buffers, HdSemaphore* semaphore, uint64_t value) {
    HdQueue* queue = &device->queue;

    uint32_t num_semaphores = 0;
    VkSemaphore* wait_semaphores = mem_alloc(sizeof(VkSemaphore) * command_buffers.len, device->gpa);
    VkSemaphore* signal_semaphores = mem_alloc(sizeof(VkSemaphore) * command_buffers.len, device->gpa);
    for (size_t i = 0; i < device->swapchains.len; i++) {
        HdSwapchain* swapchain = device->swapchains.data[i];
        HdCommandBuffer* commands;
        for (size_t j = 0; j < command_buffers.len; j++)  {
            if (swapchain->claimed_by == command_buffers.data[j]) {
                commands = command_buffers.data[j];
                swapchain->claimed_by = NULL;
            }
        }
        if (commands == NULL) continue;

        HdPresentContext present_context = swapchain->present_contexts[swapchain->current_present];
        //Texture& texture = swapchain->textures[swapchain->image_index];
        const VkImageMemoryBarrier2 barrier = {
            .sType = VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2,
            .srcStageMask = VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT,
            .srcAccessMask = VK_ACCESS_2_MEMORY_WRITE_BIT,
            .dstStageMask = VK_PIPELINE_STAGE_2_NONE,
            .oldLayout = VK_IMAGE_LAYOUT_GENERAL,
            .newLayout = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR,
            .srcQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED,
            .dstQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED,
            .image = swapchain->images[swapchain->current_image],
            .subresourceRange = {
                .aspectMask = VK_IMAGE_ASPECT_COLOR_BIT,
                .levelCount = 1,
                .layerCount = 1,
            },
        };

        const VkDependencyInfo dependency = {
            .sType = VK_STRUCTURE_TYPE_DEPENDENCY_INFO,
            .imageMemoryBarrierCount = 1,
            .pImageMemoryBarriers = &barrier,
        };
        vkCmdPipelineBarrier2(commands->buffer, &dependency);
        // TODO: debug layer
        //assert(!present_context.present_pending && !present_context.swapchain);
        VkResult result = vkResetFences(device->device, 1, &present_context.presented);
        if (result != VK_SUCCESS)
            panic(mv_string("TODO: handle present barrier failure in submit!"));
        //swapchain->transition_commands = nullptr;
        wait_semaphores[num_semaphores] = present_context.acquired; 
        signal_semaphores[num_semaphores] = present_context.rendered; 
        num_semaphores++;
    }

    submit_commands_internal(queue, command_buffers, semaphore, value, wait_semaphores, signal_semaphores, num_semaphores);

    mem_free(wait_semaphores, device->gpa);
    mem_free(signal_semaphores, device->gpa);
}

#endif
