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

#define FORBIDDEN_MEMORY_PROPERTIES \
    (VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT |            \
     VK_MEMORY_PROPERTY_PROTECTED_BIT |                   \
     VK_MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD |         \
     VK_MEMORY_PROPERTY_DEVICE_UNCACHED_BIT_AMD)

static uint64_t align_up(uint64_t size, uint64_t alignment) {
    return ((size + alignment - 1) / alignment) * alignment;
}

static uint32_t popcount(uint32_t value) {
    // Keep the core library usable on x86-64 CPUs without POPCNT.
    value -= (value >> 1) & 0x55555555u;
    value = (value & 0x33333333u) + ((value >> 2) & 0x33333333u);
    value = (value + (value >> 4)) & 0x0f0f0f0fu;
    return (value * 0x01010101u) >> 24;
}


bool is_usable_memory_type(VkPhysicalDeviceMemoryProperties properties, uint32_t index) {
    const VkMemoryType type = properties.memoryTypes[index];
    if ((type.propertyFlags & FORBIDDEN_MEMORY_PROPERTIES) != 0)
        return false;
    return (properties.memoryHeaps[type.heapIndex].flags & VK_MEMORY_HEAP_TILE_MEMORY_BIT_QCOM) == 0;
}

bool find_memory_type(uint32_t bits, VkMemoryPropertyFlags required, VkMemoryPropertyFlags preferred, VkDeviceSize minimum_heap_size,
                      uint32_t* output, VkMemoryPropertyFlags avoided, HdLogicalDevice* device) {
    bool has_best = false;
    bool best_is_avoided = false;
    uint32_t best = 0;
    uint32_t best_score = 0;
    VkPhysicalDeviceMemoryProperties memory_properties = device->physical_device->memory_properties;
    VkDeviceSize best_heap_size = 0;
    for (uint32_t i = 0; i < memory_properties.memoryTypeCount; i++) {
        if ((bits & (1u << i)) == 0)
            continue;
        const VkMemoryPropertyFlags flags = memory_properties.memoryTypes[i].propertyFlags;
        if ((flags & required) != required)
            continue;
        if (!is_usable_memory_type(memory_properties, i))
            continue;
        const VkMemoryHeap heap = memory_properties.memoryHeaps[memory_properties.memoryTypes[i].heapIndex];
        if (heap.size < minimum_heap_size) {
            continue;
        }
        const bool is_avoided = (flags & avoided) != 0;
        const uint32_t score = (uint32_t)(popcount(flags & preferred));
        if (!has_best || (best_is_avoided && !is_avoided) ||
            (best_is_avoided == is_avoided &&
             (score > best_score || (score == best_score && heap.size > best_heap_size)))) {
            best = i;
            has_best = true;
            best_is_avoided = is_avoided;
            best_score = score;
            best_heap_size = heap.size;
        }
    }
    if (!has_best)
        return false;
    *output = best;
    return true;
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
