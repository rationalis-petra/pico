#ifdef USE_VULKAN

#include "data/meta/array_impl.h"
#include "data/meta/amap_impl.h"

#include "platform/signals.h"
#include "platform/hedron/hedron.h"
#include "platform/hedron/internal.h"

ARRAY_COMMON_IMPL(PendingBuffer, pbuf, PendingBuffer);
AMAP_IMPL(HdSemaphore*, PendingBufferArray, sem_bufs, SemBufs);

HdQueue* get_queue(HdLogicalDevice* device) {
    return &device->queue;
}

HdCommandBuffer* start_recording_commands(HdQueue* queue) {
  HdLogicalDevice* device = queue->device;

  if (device->usable_buffers.len > 0) {
    HdCommandBuffer* buffer = pop_ptr(&device->usable_buffers);

    VkCommandBufferBeginInfo begin_info = {
      .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
      .flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
    };
    vkBeginCommandBuffer(buffer->buffer, &begin_info);
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

  VkCommandBufferBeginInfo begin_info = {
    .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
    .flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
  };
  vkBeginCommandBuffer(buffer, &begin_info);
  
  HdCommandBuffer* hd_buffer = mem_alloc(sizeof(HdCommandBuffer), queue->device->gpa);
  *hd_buffer = (HdCommandBuffer) {
    .queue = queue,
    .buffer = buffer,
    .pool = pool,
    .device = queue->device,
  };
  return hd_buffer;
}

void submit_commands(HdQueue* queue, PtrSlice command_buffers, HdSemaphore* semaphore, uint64_t value) {
  VkCommandBuffer* vk_buffers = mem_alloc(sizeof(VkCommandBuffer) * command_buffers.len, queue->device->gpa);
  for (size_t i = 0; i < command_buffers.len; i++) {
    HdCommandBuffer* buffer = command_buffers.data[i];
    vkEndCommandBuffer(buffer->buffer);
    vk_buffers[i] = buffer->buffer;
  }

  VkTimelineSemaphoreSubmitInfo timeline_info = {
    .sType = VK_STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO,
    .signalSemaphoreValueCount = 1,
    .pSignalSemaphoreValues = &value,
  };

  VkSubmitInfo submit_info = {
    .sType = VK_STRUCTURE_TYPE_SUBMIT_INFO,
    .pNext = &timeline_info,
    .commandBufferCount = command_buffers.len,
    .pCommandBuffers = vk_buffers,
    .signalSemaphoreCount = 1,
    .pSignalSemaphores = &semaphore->semaphore,
  };

  vkQueueSubmit(queue->queue, 1, &submit_info, VK_NULL_HANDLE);

  // TODO: we want to check the thread safety of queue submissions 
  //       and ensure that both the adding to pending buffers and the
  //       queue submission itself are thread-safe 
  HdLogicalDevice* device = queue->device;
  PendingBufferArray* arr = sem_bufs_lookup(semaphore, device->pending_buffers);
  if (arr == NULL) {
    sem_bufs_insert(semaphore, mk_pbuf_array(command_buffers.len, device->gpa), &device->pending_buffers);
    arr = sem_bufs_lookup(semaphore, device->pending_buffers);
  }

  for (size_t i = 0; i < command_buffers.len; i++) {
    PendingBuffer pbuf = {
      .commands = command_buffers.data[i],
      .value = value,
    };
    push_pbuf(pbuf, arr);
  }
  mem_free(vk_buffers, queue->device->gpa);
}

#endif
