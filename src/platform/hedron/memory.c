#ifdef USE_VULKAN

#include "platform/signals.h"
#include "platform/hedron/hedron.h"
#include "platform/hedron/internal.h"


typedef struct HdBackingBuffer {
    VkBuffer buffer;
    VkDeviceMemory memory;
    void* mapped;
    VkDeviceAddress address;
} HdBackingBuffer;

struct HdHeapOwner {
    HdLogicalDevice* device;
    HdBackingBuffer backing;
};

#define GPU_ALLOCATION_ALIGNMENT 16

#define CPU_VISIBLE_MEMORY_PROPERTIES                                          \
    (VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT |\
     VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT |\
     VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)

#define UNIVERSAL_BUFFER_USAGE \
        (VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT |                    \
         VK_BUFFER_USAGE_INDEX_BUFFER_BIT | VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT | \
         VK_BUFFER_USAGE_TRANSFER_SRC_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT)

static uint64_t align_up(uint64_t size, uint64_t alignment) {
    return ((size + alignment - 1) / alignment) * alignment;
}

void create_backing_buffer(HdBackingBuffer* output, VkDeviceSize size, VkBufferUsageFlags usage, VkMemoryPropertyFlags required,
                           VkMemoryPropertyFlags preferred, VkMemoryPropertyFlags avoided, HdLogicalDevice* device)  {
    *output = (HdBackingBuffer){};
    HdBackingBuffer result = {};
    const VkBufferCreateInfo buffer_info = {
        .sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO,
        .size = size,
        .usage = usage,
        .sharingMode = VK_SHARING_MODE_EXCLUSIVE,
    };
    VkResult vkresult = vkCreateBuffer(device->device, &buffer_info, NULL, &result.buffer);
    if (vkresult != VK_SUCCESS)
        panic(mv_string("TODO: handle this failure elegantly (hedron)"));

    VkMemoryRequirements requirements = {};
    vkGetBufferMemoryRequirements(device->device, result.buffer, &requirements);
    uint32_t memory_type = 0;
    const bool has_memory_type = find_memory_type(requirements.memoryTypeBits, required, preferred,
                                                  requirements.size, &memory_type, avoided, device);
    //assert(has_memory_type);
    // TODO: make the above check happen at device creation time, and the below
    // check happen only as an internal 'the API has bugs' type error.
    if (!has_memory_type)
        panic(mv_string("Device does not have memory type to support this backing buffer."));

    const VkMemoryAllocateFlagsInfo flags_info = {
        .sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO,
        .flags = VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT,
    };
    const VkMemoryAllocateInfo allocate_info = {
        .sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
        .pNext = &flags_info,
        .allocationSize = requirements.size,
        .memoryTypeIndex = memory_type,
    };
    vkresult = vkAllocateMemory(device->device, &allocate_info, NULL, &result.memory);
    if (vkresult != VK_SUCCESS)
        panic(mv_string("TODO: handle this failure elegantly (hedron)"));
    vkresult = vkBindBufferMemory(device->device, result.buffer, result.memory, 0);
    if (vkresult != VK_SUCCESS)
        panic(mv_string("TODO: handle this failure elegantly (hedron)"));

    if ((required & VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) != 0) {
        vkresult = vkMapMemory(device->device, result.memory, 0, VK_WHOLE_SIZE, 0, &result.mapped);
        if (vkresult != VK_SUCCESS)
            panic(mv_string("TODO: handle this failure elegantly (hedron)"));
    }

    const VkBufferDeviceAddressInfo address_info = {
        .sType = VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO,
        .buffer = result.buffer,
    };
    result.address = vkGetBufferDeviceAddress(device->device, &address_info);
    *output = result;
}

HdHeap allocate_descriptor_heap(size_t size, MemoryType type, HdLogicalDevice* device) {
    //GpuHeap Device::allocate_descriptor_heap(VkDeviceSize size, MemoryType memory) noexcept
    //assert(memory == MemoryType::texture_descriptor_heap || memory == MemoryType::sampler_descriptor_heap);
    VkPhysicalDeviceDescriptorHeapPropertiesEXT heap_properties
        = device->physical_device->heap_properties;
    const bool texture_heap = type == MemoryTextureDescriptor;
    const VkDeviceSize resource_alignment = heap_properties.imageDescriptorAlignment > heap_properties.bufferDescriptorAlignment
                                                ? heap_properties.imageDescriptorAlignment
                                                : heap_properties.bufferDescriptorAlignment;
    const VkDeviceSize reserved_alignment = texture_heap ? resource_alignment : heap_properties.samplerDescriptorAlignment;
    const VkDeviceSize heap_alignment = texture_heap ? heap_properties.resourceHeapAlignment : heap_properties.samplerHeapAlignment;
    const VkDeviceSize reserved_size = texture_heap ? heap_properties.minResourceHeapReservedRange : heap_properties.minSamplerHeapReservedRange;

    const VkDeviceSize reserved_offset = align_up(size, reserved_alignment);
    const VkDeviceSize bind_size = reserved_offset + reserved_size;
    const VkDeviceSize allocation_alignment = heap_alignment > GPU_ALLOCATION_ALIGNMENT ? heap_alignment : GPU_ALLOCATION_ALIGNMENT;
    const VkDeviceSize alignment_padding = allocation_alignment - 1;
    const VkDeviceSize backing_size = bind_size + alignment_padding;

    HdHeapOwner *heap = mem_alloc(sizeof(HdHeapOwner), device->gpa);
    *heap = (HdHeapOwner){.device = device};

    create_backing_buffer(
        &heap->backing, backing_size,
        UNIVERSAL_BUFFER_USAGE | VK_BUFFER_USAGE_DESCRIPTOR_HEAP_BIT_EXT,
        CPU_VISIBLE_MEMORY_PROPERTIES, 0, 0, device);

    const VkDeviceAddress gpu_address = align_up(heap->backing.address, allocation_alignment);
    const VkDeviceSize allocation_offset = gpu_address - heap->backing.address;
    return (HdHeap) {
        .host = ((uint8_t*)heap->backing.mapped) + allocation_offset,
        .device = {gpu_address},
        .memsize = size,
        .owner = heap,
    };
}

HdHeap create_device_heap(size_t size, size_t align, MemoryType type, HdLogicalDevice* device) {
    if (type == MemoryTextureDescriptor || type == MemorySamplerDescriptor)
        return allocate_descriptor_heap(size, type, device);

    VkMemoryPropertyFlags required = 0;
    VkMemoryPropertyFlags preferred = 0;
    VkMemoryPropertyFlags avoided = 0;
    switch (type) {
    case MemoryCpuVisible:
        required = CPU_VISIBLE_MEMORY_PROPERTIES;
        break;
    case MemoryWriteback:
        required = CPU_VISIBLE_MEMORY_PROPERTIES;
        preferred = VK_MEMORY_PROPERTY_HOST_CACHED_BIT;
        break;
    case MemoryDevice:
        required = VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT;
        avoided = VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT;
        break;
    default:
        panic(mv_string("create_gpu_heap received an invalid memory type"));
    }

    HdHeapOwner *owner = mem_alloc(sizeof(HdHeapOwner), device->gpa);
    *owner = (HdHeapOwner) {
        .device = device,
    };
    create_backing_buffer(&owner->backing, size, UNIVERSAL_BUFFER_USAGE, required, preferred, avoided, device);
    return (HdHeap) {
        .host = owner->backing.mapped,
        .device = {owner->backing.address},
        .memsize = size,
        .owner = owner,
    };
}

void destroy_device_heap(HdHeap heap) {
    // TODO: acquire mutex
    // TODO: release mutex
    HdLogicalDevice* device = heap.owner->device;
    const VkDevice vkdevice = device->device;
    const HdBackingBuffer backing = heap.owner->backing;
    if (backing.mapped) vkUnmapMemory(vkdevice, backing.memory);
    vkDestroyBuffer(vkdevice, backing.buffer, NULL);
    vkFreeMemory(vkdevice, backing.memory, NULL);
    mem_free(heap.owner, device->gpa);
}

#endif
