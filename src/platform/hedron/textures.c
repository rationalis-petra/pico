#ifdef USE_VULKAN

#include "platform/signals.h"
#include "platform/hedron/hedron.h"
#include "platform/hedron/internal.h"

struct HdTextureHeapOwner {
    HdLogicalDevice* device;
    VkDeviceMemory memory;
};

HdTextureHeap create_texture_heap(size_t memsize, HdLogicalDevice* device) {
    // TODO: Debug layer
    //assert(device && "create_texture_heap called with a null device");
    HdTextureHeapOwner *owner = mem_alloc(sizeof(HdTextureHeapOwner), device->gpa);
    *owner = (HdTextureHeapOwner) {
        .device = device,
    };
    const VkMemoryAllocateInfo allocate_info = {
        .sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
        .allocationSize = memsize,
        // TODO: add texture memory type filder to device creation!
        .memoryTypeIndex = VK_MAX_MEMORY_TYPES,//device->texture_memory_type,
    };
    VkResult result = vkAllocateMemory(device->device, &allocate_info, NULL, &owner->memory);
    if (result != VK_SUCCESS) {
        panic(mv_string("TODO: correctly handle failure to create texture heap."));
    }
    return (HdTextureHeap) {
        .owner = owner,
        .memsize = memsize,
    };
}

void destroy_texture_heap(HdTextureHeap heap) {
    // TODO: debug layer
    //if (!heap.owner) return;
    vkFreeMemory(heap.owner->device->device, heap.owner->memory, NULL);
    mem_free(heap.owner, heap.owner->device->gpa);
}

SizeAlign get_texture_size_align(HdLogicalDevice* device, HdTextureDescription desc);
HdTexture* create_texture(HdLogicalDevice* device, HdTextureDescription desc, HdTextureHeap heap, uint64_t offset);
void destroy_texture(HdTexture* texture);

HdRenderView* create_render_view(HdTexture* texture, HdRenderViewDescription desc);
void destroy_render_view(HdRenderView* render_view);

void write_texture_descriptor(void *cpu_destination, HdTexture *texture,
                              HdTextureDescriptorType type,
                              HdTextureDescriptorDescription desc,
                              HdLogicalDevice* device);
void write_sampler_descriptor(HdLogicalDevice* device, void* cpu_destination, HdSamplerDescription desc);

#endif
