#ifndef __PLATFORM_HEDRON_HEDRON_H
#define __PLATFORM_HEDRON_HEDRON_H

#include "data/result.h"
#include "data/option.h"
#include "data/slice.h"
#include "data/slice.h"
#include "platform/memory/allocator.h"

#include <stdbool.h>

/**
 * V2 API
 */
typedef enum : uint64_t {
    HD_NOT_READY,
    HD_TIMEOUT,
    HD_EVENT_SET,
    HD_EVENT_RESET,
    HD_INCOMPLETE,
    HD_ERROR_OUT_OF_HOST_MEMORY,
    HD_ERROR_OUT_OF_DEVICE_MEMORY,
    HD_ERROR_INITIALIZATION_FAILED,
    HD_ERROR_DEVICE_LOST,
    HD_ERROR_MEMORY_MAP_FAILED,
    HD_ERROR_LAYER_NOT_PRESENT,
    HD_ERROR_EXTENSION_NOT_PRESENT,
    HD_ERROR_FEATURE_NOT_PRESENT,
    HD_ERROR_INCOMPATIBLE_DRIVER,
    HD_ERROR_TOO_MANY_OBJECTS,
    HD_ERROR_FORMAT_NOT_SUPPORTED,
    HD_ERROR_FRAGMENTED_POOL,
    HD_ERROR_UNKNOWN,
    HD_ERROR_VALIDATION_FAILED,
    HD_ERROR_OUT_OF_POOL_MEMORY,
    HD_ERROR_INVALID_EXTERNAL_HANDLE,
    HD_ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS,
    HD_ERROR_FRAGMENTATION,
    HD_PIPELINE_COMPILE_REQUIRED,
    HD_ERROR_NOT_PERMITTED,
    HD_ERROR_SURFACE_LOST,
    HD_ERROR_NATIVE_WINDOW_IN_USE,
    HD_SUBOPTIMAL,
    HD_ERROR_OUT_OF_DATE,
    HD_ERROR_INCOMPATIBLE_DISPLAY,
    HD_INCOMPATIBLE_SHADER_BINARY,
} HdError;

String view_error_string(HdError error);

typedef struct {
    Result_t type;
    union {
        void* val;
        HdError error;
    };
} HdPtrResult;

// forward-declaration of window (platform/window/window.h)
struct PlWindow;

typedef struct {
    uint32_t width;
    uint32_t height;
} HdExtent;

typedef struct {
    uint32_t width;
    uint32_t height;
    uint32_t depth;
} HdExtent3D;

// Instances
typedef struct HdInstance HdInstance;
HdPtrResult create_hedron_instance(Allocator* a);
void teardown_hedron_instance(HdInstance* instance);

// Window System Interaction Surfaces
typedef struct HdSurface HdSurface;
HdPtrResult create_window_surface(struct PlWindow* window, HdInstance* instance);
void destroy_window_surface(HdSurface* surface);

// Physical & Logical Devices
typedef struct HdPhysicalDevice HdPhysicalDevice;
typedef struct HdLogicalDevice HdLogicalDevice;
typedef struct {
    String name;
    uint64_t max_push_data_size;
    uint64_t texture_heap_alignment;
    uint64_t texture_descriptor_size;
    uint64_t sampler_descriptor_size;
    float timestamp_period_ns;
    uint32_t sub_texel_precision_bits;
    bool texture_compression_bc;
    bool texture_compression_astc;
    bool storage_input_output16;
} HdDeviceCapabilities;

PtrSlice get_physical_devices(HdInstance* instance, Allocator* a);

HdPtrResult create_logical_device(HdPhysicalDevice* device, HdInstance* instance);
void destroy_logical_device(HdLogicalDevice* device);

HdDeviceCapabilities get_device_capabilities(HdLogicalDevice* device);

// Swapchain & Render Views
// -------------------------
// Under this API, the swapchain is considered an operating system concern. From
// the perspective of the API, we only care about getting a texture we can
// render into from the swapchain.
// TODO: determine how much complexity from the swapchain we want here, or
// whether we want it elsewhere? (window api?)
typedef struct HdSwapchain HdSwapchain;
typedef struct HdFrame HdFrame;
HdPtrResult create_swapchain(HdLogicalDevice* device, HdSurface* surfaceimages);
void destroy_swapchain(HdSwapchain* swapchain);

typedef struct HdRenderView HdRenderView;
HdRenderView* next_frame(HdSwapchain* swapchain);
// TODO: why isn't the surface notifying us that is should be resized??
void resize_notify(HdSwapchain* swapchain, HdExtent extent);
void present(HdSwapchain* swapchain);

// Memory & Resources
// ------------
// Three types of generic memory
//   - GPU only memmory (must use commands to copy from/to)
//   - Shared memory
//     - Default: fast for host to write, slow for host to read
//     - Writeback: 
typedef struct {
    size_t size;
    size_t align;
} SizeAlign;

typedef struct {
    uint64_t val;
} DeviceAddress;

typedef struct {
    DeviceAddress address;
    uint64_t size; // Number of bytes
} DeviceRange;

typedef struct HdHeapOwner HdHeapOwner;

typedef struct {
   void* host;
   DeviceAddress device;
   size_t memsize;
   HdHeapOwner *owner;
} HdHeap;

typedef enum : uint64_t {
  MemoryCpuVisible,
  MemoryWriteback,
  MemoryDevice,
  MemoryTextureDescriptor,
  MemorySamplerDescriptor,
} MemoryType;

HdHeap create_device_heap(size_t size, size_t align, MemoryType type, HdLogicalDevice* device);
void destroy_device_heap(HdHeap address);

//  Textures
// ------------
// 

typedef enum : uint64_t {
    Format_R8_SRGB,
    Format_RG8_SRGB,
    Format_RGBA8_SRGB,
    Format_BGRA8_SRGB,

    Format_RGBA4_UNorm,
    Format_R5G5B5A1_UNorm,
    Format_R5G6B5_UNorm,

    Format_R8_UNorm,
    Format_RG8_UNorm,
    Format_RGBA8_UNorm,
    Format_BGRA8_UNorm,
    Format_R16_UNorm,
    Format_RG16_UNorm,
    Format_RGBA16_UNorm,

    Format_R8_UInt,
    Format_RG8_UInt,
    Format_RGBA8_UInt,
    Format_BGRA8_UInt,
    Format_R16_UInt,
    Format_RG16_UInt,
    Format_RGBA16_UInt,
    Format_R32_UInt,
    Format_RG32_UInt,
    Format_RGB32_UInt,
    Format_RGBA32_UInt,

    Format_R16_Float,
    Format_RG16_Float,
    Format_RGBA16_Float,
    Format_R32_Float,
    Format_RG32_Float,
    Format_RGB32_Float,
    Format_RGBA32_Float,

    Format_RGB10A2_UNorm,
    Format_RG11B10_Float,

    Format_D16_UNorm,
    Format_D24_UNorm_S8_UInt,
    Format_D32_Float,
    Format_S8_UInt,
    Format_D32_Float_S8_UInt,

    Format_EAC_RG,
    Format_ASTC_4x4_SRGB,
    Format_ASTC_4x4_UNorm,
    Format_BC3_SRGB,
    Format_BC3_UNorm,
    Format_BC5_RG,
    Format_BC6H_UFloat,
    Format_BC6H_SFloat,
    Format_BC7_SRGB,
    Format_BC7_UNorm,
    // Not a real format, just used to keep track of the number of formats.
    Format_Count,
} HdFormat;

OPTION_TYPE(HdFormat, HdFormat);

typedef struct {
    HdExtent block_extent;
    uint32_t bytes_per_block;
    bool depth;
    bool stencil;
} HdTextureFormatInfo;

HdTextureFormatInfo get_texture_format_info(HdFormat format);

typedef enum : uint64_t {
    OpNever,
    OpLess,
    OpEqual,
    OpLessEqual,
    OpGreater,
    OpNotEqual,
    OpGreaterEqual,
    OpAlways
} HdCompOp;
OPTION_TYPE(HdCompOp, HdCompOp);

typedef enum : uint64_t {
    Tx1d, Tx2d, Tx3d, TxCube, Tx2dArray, TxCubeArray,
} HdTextureShape;

typedef enum : uint64_t {
    UsageNone                   = 0x0,
    UsageSampled                = 0x1,
    UsageStorage                = 0x2,
    UsageColourAttachment       = 0x4,
    UsageDepthStencilAttachment = 0x8,
    UsageTransferSource         = 0x10,
    UsageTransferDestination    = 0x20,
} HdTextureUsage;

typedef struct HdTexture HdTexture;
typedef struct HdTextureHeapOwner HdTextureHeapOwner;

typedef struct {
    HdTextureHeapOwner* owner;
    size_t memsize;
} HdTextureHeap;

typedef struct {
    HdTextureShape shape;
    uint32_t extent[3];
    uint32_t mip_levels;
    uint32_t layer_count;
    HdFormat format;
    bool mutable_format;
    HdTextureUsage usage;
} HdTextureDescription;

typedef struct {
    uint32_t mip_level;
    uint32_t base_slice; // Physical array slice; cube faces are individual slices.
    uint32_t slice_count; // Zero selects every remaining physical slice.
    HdExtent3D offset;
    HdExtent3D extent; // Zero components select the remaining mip extent.
    uint64_t row_pitch_bytes;   // Zero is tightly packed.
    uint64_t slice_pitch_bytes; // Zero is tightly packed.
} TextureCopyDesc;

typedef enum : uint64_t {
    TDescSampled, TDescStorage
} HdTextureDescriptorType;

typedef enum : uint64_t {
    TxAAutomatic,  TxAColour, TxADepth, TxAStencil
} HdTextureAspect;

typedef struct {
    HdFormatOption format; // Undefined inherits the texture format.
    HdTextureAspect aspect; // Automatic selects color, or depth before stencil.
    uint32_t base_mip;
    uint32_t mip_count; // Zero selects every remaining mip level.
    uint32_t base_layer; // Vulkan array layer; cube faces are individual layers.
    uint32_t layer_count; // Vulkan array layers; zero selects every remaining layer.
} HdTextureDescriptorDescription;

typedef enum : uint64_t {
    AMRepeat, AMMirroredRepeat, AMClampToEdge
} HdAddressMode;

typedef enum : uint64_t {
    Nearest, Linear,
} HdFilter;

typedef struct {
    HdFilter min_filter;
    HdFilter mag_filter;
    HdFilter mip_filter;
    HdAddressMode address_u;
    HdAddressMode address_v;
    HdAddressMode address_w;
    bool anisotropic;
    HdCompOpOption comp;
} HdSamplerDescription;

typedef struct {
    uint32_t mip_level;
    uint32_t slice; // Physical array slice; cube faces are individual slices.
} HdRenderViewDescription;

HdTextureHeap create_texture_heap(size_t memsize, HdLogicalDevice* device);
void destroy_texture_heap(HdTextureHeap heap);

SizeAlign get_texture_size_align(HdTextureDescription desc, HdLogicalDevice* device);
HdTexture* create_texture(HdTextureDescription desc, HdTextureHeap heap, uint64_t offset);
void destroy_texture(HdTexture* texture);

HdRenderView* create_render_view(HdTexture* texture, HdRenderViewDescription desc);
void destroy_render_view(HdRenderView* render_view);

void write_texture_descriptor(void* cpu_destination, HdTexture* texture,
                              HdTextureDescriptorType type,
                              HdTextureDescriptorDescription desc,
                              HdLogicalDevice* device);
void write_sampler_descriptor(void* cpu_destination, HdSamplerDescription desc, HdLogicalDevice* device);


//  Pipelines
// ------------
typedef enum : uint64_t {
  CullCCW,
  CullCW,
  CullAll,
  CullNone,
} HdCull;

typedef enum : uint64_t {
  BlendAdd,
  BlendSubtract,
  BlendRevSubtract,
  BlendMin,
  BlendMax,
} HdBlendOp;

typedef enum : uint64_t {
  FactorZero,
  FactorOne,
  FactorSrcColour,
  FactorDstColour,
  FactorSrcAlpha,
  FactorDstAlpha,
  FactorOneMinusSrcColour,
  FactorOneMinusDstColour,
  FactorOneMinusSrcAlpha,
  FactorOneMinusDstAlpha,
  FactorSrcAlphaSaturate,
} HdBlendFactor;

typedef struct {
    HdBlendFactor source;
    HdBlendFactor destination;
    HdBlendOp operation;
} BlendComponentState;

typedef struct {
    BlendComponentState colour;
    BlendComponentState alpha;
} HdBlendState;
OPTION_TYPE(HdBlendState, HdBlendState);

typedef struct {
    HdFormat format;
    HdBlendStateOption blend;
    uint8_t write_mask;
} HdColourTarget;
SLICE_TYPE(HdColourTarget, HdColourTarget);

typedef struct {
    float constant;
    float clamp;
    float slope;
} HdDepthBias;
OPTION_TYPE(HdDepthBias, HdDepthBias);

typedef struct {
    HdCull cull;
    HdFormatOption depth_format;
    HdDepthBiasOption depth_bias;
    HdFormatOption stencil_format;
    HdColourTargetSlice colour_targets;
} HdRasterDescription;

typedef struct HdPipeline HdPipeline;
HdPipeline* create_compute_pipeline(U32Slice computeIR, HdLogicalDevice* device);
HdPipeline* create_graphics_pipeline(U32Slice vertexIR, U32Slice pixelIR, HdRasterDescription desc, bool is_meshlet, HdLogicalDevice* device);
void destroy_pipeline(HdPipeline* pipeline, HdLogicalDevice* device);

// Semaphores
// -----------
// 
typedef struct HdSemaphore HdSemaphore;
HdSemaphore* create_semaphore(HdLogicalDevice* device, uint64_t init_value);
void wait_semaphore(HdLogicalDevice* device, HdSemaphore* sema, uint64_t value);
void destroy_semaphore(HdLogicalDevice* device, HdSemaphore* sema);

// Get & Submit Command Buffers
// ---------------------------------
// In Vulkan, queues are created at the same time as devices. 
// The Hedron API creates a single queue associated with the device, 
// meaning that we can just submit commands directly to the device. In the future, this
// may be expanded, if the flexibility of multible queues is justified.
typedef struct HdCommandBuffer HdCommandBuffer;

HdCommandBuffer* start_recording_commands(HdLogicalDevice* device);
void submit_commands(HdLogicalDevice* device, PtrSlice command_buffers, HdSemaphore* semaphore, uint64_t value);

// Commands
// ---------
// 
// TODO: stage meshlet shader??
// TODO: stage acceleration structure??
// State Objects
// --------------

typedef enum : uint64_t {
    StKeep,
    StZero,
    StReplace,
    StIncrementClamp,
    StDecrementClamp,
    StInvert,
    StIncrementWrap,
    StDecrementWrap,
} StencilOp;

typedef enum : uint64_t {
    // Specifically designed so that the values match/can be cast to
    // the vulkan equivalents
    StNone =              0,
    StIndirect =          1 << 0,
    StIndexInput =        1 << 1,
    StVertex =            1 << 2,
    StTask =              1 << 3,
    StMesh =              1 << 4,
    StDepthStencilTests = 1 << 5,
    StFragment =          1 << 6,
    StColourOutput =      1 << 7,
    StCompute =           1 << 8,
    StTransfer =          1 << 9,
    StHost =              1 << 10, // Barrier destination only, paired with host_read.
    StAllCommands =       1 << 11, // All GPU command stages; excludes host.
} HdStage;

// TODO: acceleration structure.
typedef enum : uint64_t {
    AccNone =              0,
    AccTransferRead =      1 << 0,
    AccTransferWrite =     1 << 1,
    AccShaderRead =        1 << 2,
    AccShaderWrite =       1 << 3,
    AccColourRead =        1 << 4,
    AccColourWrite =       1 << 5,
    AccDepthStencilRead =  1 << 6,
    AccDepthStencilWrite = 1 << 7,
    AccIndirectRead =      1 << 8,
    AccIndexRead =         1 << 9,
    AccHostRead =          1 << 10,
    AccDescriptorRead =    1 << 11,
} HdAccess; 

// TODO: investigate what signals need adding (if any)
typedef enum : uint64_t {
  SignalAtomicSet,
  SignalAtomicMax,
  SignalAtomicOr
} HdSignal;

typedef enum : uint64_t {
    LOpLoad,
    LOpClear,
    LOpDiscard,
} LoadOp;

typedef enum : uint64_t {
    SOpStore,
    SOpDiscard,
} StoreOp;

typedef struct {
    float x;
    float y;
    float z;
    float w;
} Vec4f;

typedef struct {
    HdRenderView* render_view;
    LoadOp load;
    StoreOp store;
    Vec4f clear;
} HdColourAttachment;
SLICE_TYPE(HdColourAttachment, ColourAttachment)

typedef struct {
    HdRenderView* render_view;
    LoadOp load;
    StoreOp store;
    float clear;
} HdDepthAttachment;
OPTION_TYPE(HdDepthAttachment, HdDepthAttachment)

typedef struct {
    HdRenderView* render_view;
    LoadOp load;
    StoreOp store;
    uint8_t clear;
} HdStencilAttachment;
OPTION_TYPE(HdStencilAttachment, HdStencilAttachment)

typedef struct {
    ColourAttachmentSlice colours;
    HdDepthAttachmentOption depth;
    HdStencilAttachmentOption stencil;
} HdRenderDesc;

typedef enum {IdxU16, IdxU32} IndexType;

/*
 * These are for signalling GPU/GPU dependencies. The barrier is for use within
 * a command buffer recording, whereas (I think) the signal after/wait before
 * can be used between queues? (maybe??)
 */
void barrier(HdCommandBuffer* cb, HdStage before, HdAccess before_access, HdStage after, HdAccess after_access);
//void signal_after(HdCommandBuffer* cb, HdStage before, void *ptrGpu, uint64_t value, HdSignal signal);
//void wait_before(HdCommandBuffer* cb, HdStage after, void *ptrGpu, uint64_t value, HdCompOp op, HdHazardFlags hazards, uint64_t mask);

typedef struct {
    float x;
    float y;
    float width;
    float height;
    float min_depth;
    float max_depth;
} HdViewport;

typedef struct {
    int32_t x;
    int32_t y;
    uint32_t width;
    uint32_t height;
} HdScissor;

typedef struct {
    HdCompOp compare;
    StencilOp fail;
    StencilOp pass;
    StencilOp depth_fail;
    uint8_t reference;
} StencilFaceState;

typedef struct {
    bool depth_test;
    bool depth_write;
    HdCompOp depth_compare;
    bool stencil_test;
    uint8_t stencil_read_mask;
    uint8_t stencil_write_mask;
    StencilFaceState front;
    StencilFaceState back;
} HdDepthStencilState;

// Generic commands: pipeline etc.
void set_pipeline(HdCommandBuffer* cb, HdPipeline* pipeline);
void set_viewport(HdCommandBuffer* cb, HdViewport viewport);
void set_scissor(HdCommandBuffer* cb, HdScissor scissor);
void set_depth_stencil(HdCommandBuffer* cb, HdDepthStencilState depth_stencil);

void set_texture_descriptor_heap(HdCommandBuffer* commands, DeviceRange heap);
void set_sampler_descriptor_heap(HdCommandBuffer* commands, DeviceRange heap);

void copy_memory(HdCommandBuffer* cb, DeviceRange source, DeviceRange destination);
void copy_memory_to_texture(HdCommandBuffer* cb, DeviceRange source, HdTexture* destination, TextureCopyDesc copy);
void copy_texture_to_memory(HdCommandBuffer* cb, HdTexture* source, DeviceRange destination, TextureCopyDesc copy);

// Dispatch Shaders (graphics/compute)
typedef struct {
    uint32_t x;
    uint32_t y;
    uint32_t z;
} UVec3;
void dispatch(HdCommandBuffer* cb, U8Slice data, UVec3 group_count);
void dispatch_indirect(HdCommandBuffer* cb, U8Slice data, DeviceRange arguments);

void draw(HdCommandBuffer *commands, U8Slice data,
          uint32_t vertex_count, uint32_t instance_count,
          uint32_t first_vertex, uint32_t first_instance);
void draw_indexed(HdCommandBuffer *cb, U8Slice data, DeviceRange indices,
                  IndexType type, uint32_t index_count, uint32_t instance_count,
                  uint32_t first_index, int32_t vertex_offset,
                  uint32_t first_instance);
void draw_indirect(HdCommandBuffer* cb, U8Slice data, DeviceRange arguments, uint32_t draw_count, uint32_t stride);
void draw_indexed_indirect(HdCommandBuffer* cb, U8Slice data, DeviceRange indices,
                           IndexType type, DeviceRange arguments,
                           uint32_t draw_count, uint32_t stride);

// Graphics Commands
void start_render_pass(HdCommandBuffer* cb, HdRenderDesc desc);
void end_render_pass(HdCommandBuffer* cb);

//void set_depth_stencil_state(HdCommandBuffer* cb, GpuDepthStencilState state);
//void set_blend_state(HdCommandBuffer* cb, GpuBlendState state); 

#endif
