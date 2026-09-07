#ifdef USE_VULKAN
#include "data/meta/array_impl.h"

#include "platform/signals.h"
#include "platform/hedron/hedron.h"
#include "platform/hedron/internal.h"

typedef enum {
    GpuAllocDefault,
    // Memory_Desriptor, // internal only, add after/with images?
    GpuAllocReadback,
    GpuAllocGpu,
} AllocationType;

struct HdAllocation {
    VkBuffer buffer;
    VkDeviceMemory memory;
    VkDeviceSize size;
    VkDeviceAddress address;
    void* ptr;
    VkBufferUsageFlags usage;
};

ARRAY_COMMON_IMPL(HdAllocation, hdalloc, HdAllocation);


/*
  // TODO: at device creation, check to ensure all necessary memory types are supported.
uint32_t find_memory_type(uint32_t filter, VkMemoryPropertyFlags properties) {
    VkPhysicalDeviceMemoryProperties mem_properties;
    vkGetPhysicalDeviceMemoryProperties(physical_device, &mem_properties);

    for (uint32_t i = 0; i < mem_properties.memoryTypeCount; i++) {
        if (filter & (1 << i) && (mem_properties.memoryTypes[i].propertyFlags & properties) == properties) {
            return i;
        }
    }

    panic(mv_string("failed to find suitable memory type!"));
}
*/

uint32_t find_memory_type(uint32_t typeFilter, VkMemoryPropertyFlags props, HdLogicalDevice* device) {
    VkPhysicalDeviceMemoryProperties phys_props;
    vkGetPhysicalDeviceMemoryProperties(device->physical_device, &phys_props);
    for (uint32_t i = 0; i < phys_props.memoryTypeCount; i++) {
        if ((typeFilter & (1 << i)) && (phys_props.memoryTypes[i].propertyFlags & props) == props) {
            return i;
        }
    }

    return UINT32_MAX;
}

static HdAllocation do_vk_allocation(size_t size, size_t align,
                                     VkBufferUsageFlags2KHR usage,
                                     VkMemoryPropertyFlagBits props,
                                     HdLogicalDevice *device) {
    VkBufferUsageFlags2CreateInfoKHR usage2Info = {
        .sType = VK_STRUCTURE_TYPE_BUFFER_USAGE_FLAGS_2_CREATE_INFO_KHR,
        .usage = usage,
    };

    VkBufferCreateInfo buffer_create = {
        .sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO,
        .size = size,
        .pNext = &usage2Info,
        // Ignored as we pass the actual usage via pNext
        .usage = 0,
        // Means that other queue families cannot access the data in this buffer.
        .sharingMode = VK_SHARING_MODE_EXCLUSIVE,
        // must be 0 as sharing mode is exclusive.
        .queueFamilyIndexCount = 0,
    };

    VkBuffer buffer;
    VkResult result = vkCreateBuffer(device->device, &buffer_create, NULL, &buffer);
    if (result != VK_SUCCESS) {
        panic(mv_string("Failure in internal GPU allocation procedure. TODO: allow gpu allocations to fail without panicing"));
    }

    VkMemoryRequirements memory_requirements;
    vkGetBufferMemoryRequirements(device->device, buffer, &memory_requirements);

    align = align > memory_requirements.alignment ? align : memory_requirements.alignment;
    VkDeviceSize alignedSize = (memory_requirements.size + align - 1) & ~(align - 1);

    VkMemoryAllocateFlagsInfo allocate_flags = {
        .sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO,
        .flags = VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT_KHR,
    };

    VkMemoryAllocateInfo allocate_info = {
        .sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
        .pNext = &allocate_flags,
        .allocationSize = alignedSize,
        .memoryTypeIndex = find_memory_type(memory_requirements.memoryTypeBits, props, device),
    };

    VkDeviceMemory device_memory;
    result = vkAllocateMemory(device->device, &allocate_info, NULL, &device_memory);
    if (result != VK_SUCCESS) {
        panic(mv_string("Failure in internal GPU allocation procedure. TODO: allow gpu allocations to fail without panicing"));
    }
    result = vkBindBufferMemory(device->device, buffer, device_memory, 0);
    if (result != VK_SUCCESS) {
        panic(mv_string("Failure in internal GPU allocation procedure. TODO: allow gpu allocations to fail without panicing"));
    }
    VkBufferDeviceAddressInfo address_info = {
        .sType = VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO,
        .buffer = buffer,
    };
    VkDeviceAddress address = vkGetBufferDeviceAddress(device->device, &address_info); 

    VkDeviceSize offset = (align - (address % align)) % align;
    address += offset;

    void* host_address = NULL;
    if (props & VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) {
        vkMapMemory(device->device, device_memory, 0, alignedSize, 0, &host_address);
        host_address = (uint8_t*)(host_address) + offset;
    }
    return (HdAllocation) {
        .buffer = buffer,
        .memory = device_memory,
        .size = size,
        .address = address,
        .ptr = host_address,

        .usage = usage,
    };
}

static HdAllocation create_allocation(size_t size, size_t align, AllocationType type, HdLogicalDevice* device) {
    switch (type) {
    case GpuAllocDefault: {
        // default: it may be used for basically anything, so we flag it as
        // such. On modern GPUs, expect minimal extra cost.
        // TODO: in theroy we only need a subset of these flags, investigate
        //       (Shader device addres + storage  + transfer)
        VkBufferUsageFlags2KHR usage = 
            VK_BUFFER_USAGE_2_SHADER_DEVICE_ADDRESS_BIT_KHR |
            VK_BUFFER_USAGE_2_TRANSFER_SRC_BIT_KHR |
            VK_BUFFER_USAGE_2_TRANSFER_DST_BIT_KHR |
            //VK_BUFFER_USAGE_2_UNIFORM_TEXEL_BUFFER_BIT_KHR |
            //VK_BUFFER_USAGE_2_STORAGE_TEXEL_BUFFER_BIT_KHR |
            //VK_BUFFER_USAGE_2_UNIFORM_BUFFER_BIT_KHR |
            VK_BUFFER_USAGE_2_STORAGE_BUFFER_BIT_KHR;
            //VK_BUFFER_USAGE_2_INDEX_BUFFER_BIT_KHR |
            //VK_BUFFER_USAGE_2_VERTEX_BUFFER_BIT_KHR |
            //VK_BUFFER_USAGE_2_INDIRECT_BUFFER_BIT_KHR |

            // Only required for ray tracing... how are we exposing this?
            //VK_BUFFER_USAGE_2_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR;

        VkMemoryPropertyFlagBits properties =
            VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT |
            VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT |
            VK_MEMORY_PROPERTY_HOST_COHERENT_BIT;
        //case GpuAlloc_Descriptor:
        return do_vk_allocation(size, align, usage, properties, device);
    }
    case GpuAllocReadback: {
        /** Note: reference had, why less usage?
        auto usage =
            VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT |
            VK_BUFFER_USAGE_TRANSFER_DST_BIT;
        */
        VkBufferUsageFlags2KHR usage = 
            VK_BUFFER_USAGE_2_SHADER_DEVICE_ADDRESS_BIT_KHR |
            VK_BUFFER_USAGE_2_TRANSFER_SRC_BIT_KHR |
            VK_BUFFER_USAGE_2_TRANSFER_DST_BIT_KHR |
            VK_BUFFER_USAGE_2_STORAGE_BUFFER_BIT_KHR;
        
        VkMemoryPropertyFlagBits properties =
            VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT |
            VK_MEMORY_PROPERTY_HOST_COHERENT_BIT |
            VK_MEMORY_PROPERTY_HOST_CACHED_BIT;
        return do_vk_allocation(size, align, usage, properties, device);
    }
    case GpuAllocGpu: {
        // Like Default, but with an additional 'acceleration structure storage
        // bit' flag.
        VkBufferUsageFlags2KHR usage = 
            VK_BUFFER_USAGE_2_SHADER_DEVICE_ADDRESS_BIT_KHR |
            VK_BUFFER_USAGE_2_TRANSFER_SRC_BIT_KHR |
            VK_BUFFER_USAGE_2_TRANSFER_DST_BIT_KHR |
            //VK_BUFFER_USAGE_2_UNIFORM_TEXEL_BUFFER_BIT_KHR |
            //VK_BUFFER_USAGE_2_STORAGE_TEXEL_BUFFER_BIT_KHR |
            //VK_BUFFER_USAGE_2_UNIFORM_BUFFER_BIT_KHR |
            VK_BUFFER_USAGE_2_STORAGE_BUFFER_BIT_KHR;
            //VK_BUFFER_USAGE_2_INDEX_BUFFER_BIT_KHR |
            //VK_BUFFER_USAGE_2_VERTEX_BUFFER_BIT_KHR |
            //VK_BUFFER_USAGE_2_INDIRECT_BUFFER_BIT_KHR |
            // Only required for ray tracing... how are we exposing this?
            //VK_BUFFER_USAGE_2_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR |
            //VK_BUFFER_USAGE_2_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR;
        VkMemoryPropertyFlagBits properties = VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT;
        return do_vk_allocation(size, align, usage, properties, device);
    }
    default:
        panic(mv_string("Invalid allocation type"));
    }
}

void free_allocation(HdAllocation allocation, HdLogicalDevice* device) {
    if (allocation.ptr) {
        vkUnmapMemory(device->device, allocation.memory);
    }

    vkFreeMemory(device->device, allocation.memory, NULL);
    vkDestroyBuffer(device->device, allocation.buffer, NULL);
}

SharedAddress alloc_shared_memory(size_t size, size_t align, MemoryType type, HdLogicalDevice* device) {
    AllocationType atype = type == MemoryWriteback
        ? GpuAllocReadback
        : GpuAllocDefault;
    HdAllocation hda = create_allocation(size, align, atype, device);
    // TODO: acquire lock
    push_hdalloc(hda, &device->allocations);
    // TODO: release lock
    return (SharedAddress) {
        .host = hda.ptr,
        .device = {.val = hda.address},
    };
}

void free_shared_memory(SharedAddress address, HdLogicalDevice* device) {
    // TODO: acquire mutex
    // TODO: release mutex
    bool valid = false;
    for (size_t i = 0; i < device->allocations.len; i++) {
        if (address.device.val == device->allocations.data[i].address) {
            free_allocation(device->allocations.data[i], device);
            device->allocations.data[i] = device->allocations.data[device->allocations.len - 1];
            pop_hdalloc(&device->allocations);
            valid = true;
        }
    }
    // TODO: add debug hook or similar for this check!
    if (!valid) {
        panic(mv_string("attempt to free invalid or already free'd shared address"));
    }
}

DeviceAddress alloc_device_memory(size_t size, size_t align, HdLogicalDevice* device) {
    HdAllocation hda = create_allocation(size, align, GpuAllocGpu, device);
    // TODO: acquire lock
    push_hdalloc(hda, &device->allocations);
    // TODO: release lock
    return (DeviceAddress) {.val = hda.address};
}

void free_device_memory(DeviceAddress address, HdLogicalDevice* device) {
    // TODO: acquire mutex
    // TODO: release mutex
    bool valid = false;
    for (size_t i = 0; i < device->allocations.len; i++) {
        if (address.val == device->allocations.data[i].address) {
            free_allocation(device->allocations.data[i], device);
            device->allocations.data[i] = device->allocations.data[device->allocations.len - 1];
            pop_hdalloc(&device->allocations);
        }
    }
    // TODO: add debug hook or similar for this check!
    if (!valid) {
        panic(mv_string("attempt to free invalid or already free'd device address"));
    }
}

#endif
