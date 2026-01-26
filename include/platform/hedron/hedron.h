#ifndef __PLATFORM_HEDRON_HEDRON_H
#define __PLATFORM_HEDRON_HEDRON_H

#include "data/option.h"
#include "platform/memory/allocator.h"

#include "pico/data/client/list.h"
#include <stdbool.h>

// forward-declaration of window (platform/window/window.h)
struct PlWindow;

typedef struct {
    uint32_t width;
    uint32_t height;
} Extent;

// ----------------------------------------------------------------------------
//
//   Context: Instances, Devices, Windows
// 
// ----------------------------------------------------------------------------

typedef struct HedronSurface HedronSurface;
typedef struct HedronShaderModule HedronShaderModule;
typedef struct HedronPipeline HedronPipeline;

// Global Utility - supported, setup & teardown
int init_hedron(Allocator* a);
void teardown_hedron();

// Window System Interaction Surfaces 
HedronSurface* create_window_surface(struct PlWindow* window);
void resize_window_surface(HedronSurface* surface, Extent extent);
void destroy_window_surface(HedronSurface*);

uint32_t num_swapchain_images(HedronSurface*);

HedronShaderModule* create_shader_module(U8PiList code);
void destroy_shader_module(HedronShaderModule* module);


// ----------------------------------------------------------------------------
//
// Data contract (vertex/input formats, etc.)
// 
// ----------------------------------------------------------------------------

typedef enum {
    VertexBuffer = 0,
    IndexBuffer = 1,
    UniformBuffer = 2,
    TransferSourceBuffer = 3,
    TransferDestinationBuffer = 4,
} BufferType;

typedef enum {
    IndexU16 = 0, // Matches Vulkan spec
    IndexU32 = 1, // Matches Vulkan Spec
} IndexFormat;

typedef struct HedronBuffer HedronBuffer;

HedronBuffer* create_buffer(BufferType type, uint64_t size);
void destroy_buffer(HedronBuffer* buffer);

void set_buffer_data(HedronBuffer* buffer, void* data);

typedef struct HedronImage HedronImage;
typedef struct HedronImageView HedronImageView;
typedef struct HedronSampler HedronSampler;

typedef enum : uint64_t {
    R8G8B8A8_SRGB,
} ImageFormat;

typedef enum : uint64_t {
    Undefined,
    TransferDestOptimal,
    ShaderReadOptimal,
} ImageLayout;

HedronImage* create_image(uint32_t width, uint32_t height, ImageFormat format);
void destroy_image(HedronImage* image);

HedronImageView* create_image_view(HedronImage* image, ImageFormat format);
void destroy_image_view(HedronImageView* image_view);

HedronSampler* create_sampler();
void destroy_sampler(HedronSampler* image_sampler);

// Descriptor Sets
// ----------------------------------

typedef enum : uint64_t { UniformBufferDesc, CombinedImageSamplerDesc } DescriptorType;
typedef enum : uint64_t { VertexShader, FragmentShader } ShaderStage;

typedef struct {
    DescriptorType type;
    uint32_t descriptor_count;
} HedronDescriptorPoolSize;

typedef struct {
    DescriptorType type;
    ShaderStage shader_type; 
} DescriptorBinding;

typedef struct HedronDescriptorSet HedronDescriptorSet;
typedef struct HedronDescriptorSetLayout HedronDescriptorSetLayout;
typedef struct HedronDescriptorPool HedronDescriptorPool;

PICO_LIST_HEADER_TYPE(HedronDescriptorPoolSize, HedronDescriptorPoolSize);
PICO_LIST_HEADER_TYPE(DescriptorBinding, DescriptorBinding);

HedronDescriptorSetLayout* create_descriptor_set_layout(DescriptorBindingPiList binddesc);
void destroy_descriptor_set_layout(HedronDescriptorSetLayout* layout);

HedronDescriptorPool* create_descriptor_pool(HedronDescriptorPoolSizePiList sizes, uint32_t max_sets);
void destroy_descriptor_pool(HedronDescriptorPool* pool);

// Descriptor sets are allocated from the descriptor pool, so don't need to be deallocated
AddrPiList alloc_descriptor_sets(uint32_t set_count, HedronDescriptorSetLayout* descriptor_set_layout, HedronDescriptorPool* pool);

typedef struct {
    HedronBuffer* buffer;
    uint32_t offset;
    uint32_t range;
} HedronDescriptorBufferInfo;

typedef struct {
    HedronSampler* sampler;
    HedronImageView* image_view;
    ImageLayout layout;
} HedronDescriptorImageInfo;

typedef enum : uint64_t {
    BufferInfo,
    ImageInfo,
} HedronDescriptorWriteType;

typedef struct {
    HedronDescriptorWriteType write_type;
    union {
        HedronDescriptorBufferInfo buffer_info;
        HedronDescriptorImageInfo image_info;
    };
    DescriptorType descriptor_type;
    HedronDescriptorSet* descriptor_set;
} HedronWriteDescriptorSet;

typedef struct {
    HedronDescriptorBufferInfo buffer_info;
} HedronCopyDescriptorSet;

PICO_LIST_HEADER_TYPE(HedronWriteDescriptorSet, HedronWriteDescriptorSet);
PICO_LIST_HEADER_TYPE(HedronCopyDescriptorSet, HedronCopyDescriptorSet);

void update_descriptor_sets(HedronWriteDescriptorSetPiList writes, HedronCopyDescriptorSetPiList copies);

// ----------------------------------------------------------------------------
// 
//   Pipeline 
//
// ----------------------------------------------------------------------------

typedef enum : uint64_t {Vertex, Instance} InputRate;

typedef struct {
    uint32_t binding;
    uint32_t stride;
    InputRate input_rate; 
} BindingDescription;

typedef enum : uint64_t {Float_1, Float_2, Float_3} VertexFormat;
typedef struct {
    uint32_t binding;
    uint32_t location;
    VertexFormat format;
    uint32_t offset; 
} AttributeDescription;

PICO_LIST_HEADER_TYPE(BindingDescription, BindingDescription)
PICO_LIST_HEADER_TYPE(AttributeDescription, AttributeDescription)

HedronPipeline *create_pipeline(AddrPiList descriptor_set_layouts,
                                BindingDescriptionPiList bdesc,
                                AttributeDescriptionPiList adesc,
                                AddrPiList shaders,
                                HedronSurface* surface);
void destroy_pipeline(HedronPipeline* pipeline);

// ----------------------------------------------------------------------------
// 
//     Synchronisation
//
// ----------------------------------------------------------------------------

typedef struct HedronSemaphore HedronSemaphore;
typedef struct HedronFence HedronFence;

HedronSemaphore* create_semaphore(); 
void destroy_semaphore(HedronSemaphore* semaphore); 

HedronFence* create_fence(); 
void destroy_fence(HedronFence* fence); 
void wait_for_fence(HedronFence* fence);
void reset_fence(HedronFence* fence);

void wait_for_device();

typedef enum : uint64_t {IROk, Resized} ImageResultType;
typedef struct {
    ImageResultType type;
    uint32_t image;
} ImageResult;
ImageResult acquire_next_image(HedronSurface* surface, HedronSemaphore* semaphore);

// ----------------------------------------------------------------------------
//
//        Commands, Queues and Drawing
// 
// ----------------------------------------------------------------------------

typedef struct HedronCommandPool HedronCommandPool;
typedef struct HedronCommandBuffer HedronCommandBuffer;

typedef enum : uint64_t {
    AccessNone,
    AccessShaderRead,
    AccessShaderWrite,
    AccessTransferRead,
    AccessTransferWrite,
} Access;

typedef enum : uint64_t {
    StageTopOfPipe,
    StageFragmentShader,
    StageColourAttachmentOutput,
    StageTransfer,
} PipelineStage;

typedef struct {
    HedronSemaphore* semaphore;
    PipelineStage stage;
} SemaphoreStagePair;


PICO_LIST_HEADER_TYPE(SemaphoreStagePair, SemaphoreStagePair);

typedef enum : uint64_t {
    BUNone,
    BUOneTimeSubmit,
} CommandBufferUsage;

typedef struct {
} MemoryBarrier;

typedef struct {
} BufferMemoryBarrier;

typedef struct {
    ImageLayout old_layout;
    ImageLayout new_layout;
    Access      src_access_flags;
    Access      dest_access_flags;
    HedronImage* image;
} ImageMemoryBarrier;

PICO_LIST_HEADER_TYPE(MemoryBarrier, MemoryBarrier);
PICO_LIST_HEADER_TYPE(BufferMemoryBarrier, BufferMemoryBarrier);
PICO_LIST_HEADER_TYPE(ImageMemoryBarrier, ImageMemoryBarrier);

HedronCommandPool* create_command_pool();
void destroy_command_pool(HedronCommandPool* pool);

// Command buffers are allocated from a pool, and are destroyed at the
// point the pool is destroyed, so there is no function to release them
HedronCommandBuffer* create_command_buffer(HedronCommandPool* pool);
void free_command_buffer(HedronCommandPool* pool, HedronCommandBuffer* buffer);

// Command buffer usage
void queue_submit(HedronCommandBuffer *buffer, PtrOption fence, SemaphoreStagePairPiList wait, AddrPiList signals);
void queue_present(HedronSurface* surface, HedronSemaphore* wait, uint32_t image_index);
void queue_wait_idle();

void command_begin(HedronCommandBuffer* buffer, CommandBufferUsage usage);
void command_end(HedronCommandBuffer* buffer);

void reset_command_buffer(HedronCommandBuffer* buffer);

void command_begin_render_pass(HedronCommandBuffer* buffer, HedronSurface* surface, uint32_t image_index);
void command_end_render_pass(HedronCommandBuffer* commands);

// Synchronization: memory barriers
void command_pipeline_barrier(HedronCommandBuffer *commands,
                              PipelineStage source_stage,
                              PipelineStage dest_stage,
                              MemoryBarrierPiList memory_barriers,
                              BufferMemoryBarrierPiList buffer_memory_barriers,
                              ImageMemoryBarrierPiList image_memory_barriers);

// Data transfer
void command_copy_buffer_to_image(HedronCommandBuffer* commands, HedronBuffer* buffer, HedronImage* image, uint32_t width, uint32_t height);

// Bind things
void command_bind_descriptor_set(HedronCommandBuffer* commands, HedronPipeline* pipeline, HedronDescriptorSet* descriptor_set);
void command_bind_pipeline(HedronCommandBuffer* commands, HedronPipeline* pipeline);
void command_bind_vertex_buffer(HedronCommandBuffer* commands, HedronBuffer* buffer);
void command_bind_index_buffer(HedronCommandBuffer* commands, HedronBuffer* buffer, IndexFormat format);

void command_set_surface(HedronCommandBuffer *commands, HedronSurface *surface);
void command_draw(HedronCommandBuffer *commands, uint32_t vertex_count,
                  uint32_t instance_count, uint32_t first_vertex,
                  uint32_t first_instance);

void command_draw_indexed(HedronCommandBuffer *commands,
                          uint32_t index_count,
                          uint32_t instance_count,
                          uint32_t first_index,
                          int32_t vertex_offset,
                          uint32_t first_instance);

#endif
