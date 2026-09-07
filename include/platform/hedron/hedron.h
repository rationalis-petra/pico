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
PtrSlice get_physical_devices(HdInstance* instance, Allocator* a);

HdPtrResult create_logical_device(HdPhysicalDevice* device, HdInstance* instance);
void destroy_logical_device(HdLogicalDevice* device);

// Swapchain
// Under this API, the swapchain is considered an operating system concern. From
// the perspective of the API, we only care about getting a texture we can
// render into from the swapchain.
// TODO: determine how much complexity from the swapchain we want here, or
// whether we want it elsewhere? (window api?)
typedef struct HdSwapchain HdSwapchain;
HdPtrResult create_swapchain(HdLogicalDevice* device, HdSurface* surfaceimages);
void destroy_swapchain(HdSwapchain* swapchain);

/*
void resize_window_surface(HdSurface* surface, HdSwapchain* swapchain, HdPhysicalDevice* device, HdExtent extent);
*/

// Memory
// ------------
// Three types of generic memory
//   - GPU only memmory (must use commands to copy from/to)
//   - Shared memory
//     - Default: fast for host to write, slow for host to read
//     - Writeback: 
typedef struct {
    uint64_t val;
} DeviceAddress;

typedef struct {
   void* host;
   DeviceAddress device;
} SharedAddress;

typedef enum { MemoryDefault, MemoryWriteback } MemoryType;

SharedAddress alloc_shared_memory(size_t size, size_t align, MemoryType type, HdLogicalDevice* device);
void free_shared_memory(SharedAddress address, HdLogicalDevice* device);

DeviceAddress alloc_device_memory(size_t size, size_t align, HdLogicalDevice* device);
void free_device_memory(DeviceAddress address, HdLogicalDevice* device);

//  Textures
// ------------
// TODO

//  Pipelines
// ------------
typedef enum {
  CullCCW,
  CullCW,
  CullAll,
  CullNone,
} HdCull;

typedef enum {
  BlendAdd,
  BlendSubtract,
  BlendRevSubtract,
  BlendMin,
  BlendMax,
} HdBlendOp;

typedef enum {
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

typedef enum {
  Format_R8_SRGB,
  Format_RG8_SRGB,
  Format_RGB8_SRGB,
  Format_RGBA8_SRGB,
  Format_BGRA8_SRGB,
  Format_RGBA4_SRGB,
  Format_R5G5B5A1_UNORM,
  Format_R5G6B5_UNORM,
  Format_R8_UNORM,
  Format_RG8_UNORM,
  Format_RGB8_UNORM,
  Format_RGBA8_UNORM,
  Format_BRGA8_UNORM,
  Format_R16_UNORM,
  Format_RG16_UNORM,
  Format_RGB16_UNORM,
  Format_RGBA16_UNORM,
  Format_R8_UInt,
  Format_RG8_UInt,
  Format_RGB8_UInt,
  Format_RGBA8_UInt,
  Format_BRGA8_UInt,
  Format_R16_UInt,
  Format_RG16_UInt,
  Format_RGB16_UInt,
  Format_RGBA16_UInt,
  Format_R32_UInt,
  Format_RG32_UInt,
  Format_RGB32_UInt,
  Format_RGBA32_UInt,
  Format_R16_Float,
  Format_RG16_Float,
  Format_RGB16_Float,
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
  Format_ASTC_4X4_SRGB,
  Format_ASTC_4X4_UNorm,
  Format_BC3_SRGB,
  Format_BC3_UNorm,
  Format_BC5_RG,
  Format_BC7_SRGB,
  Format_BC7_UNorm,
  FormatUndefined,
} HdFormat;
OPTION_TYPE(HdFormat, HdFormat);

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
HdPipeline* create_compute_pipeline(U32Slice computeIR, size_t data_size, HdLogicalDevice* device);
HdPipeline* create_graphics_pipeline(U32Slice vertexIR, U32Slice pixelIR, HdRasterDescription desc, bool is_meshlet, size_t data_size, HdLogicalDevice* device);
void destroy_pipeline(HdPipeline* pipeline, HdLogicalDevice* device);

// State Objects
// --------------
typedef enum {
  OpNever,
  OpLess,
  OpEqual,
  OpLessEqual,
  OpGreater,
  OpNotEqual,
  OpGreaterEqual,
  OpAlways
} HdCompOp;

// Semaphores
// -----------
// 
typedef struct HdSemaphore HdSemaphore;
HdSemaphore* create_semaphore(HdLogicalDevice* device, uint64_t init_value);
void wait_semaphore(HdLogicalDevice* device, HdSemaphore* sema, uint64_t value);
void destroy_semaphore(HdLogicalDevice* device, HdSemaphore* sema);

// Queues
// ------------
// Queue types
// - Graphics (for drawing) (also guarantees can do transfer!)
// - Compute (for compute shaders)
// - Transfer (for transferring memory)
// - Video Decode
// - Video Encode
// In Vulkan, queues are created at the same time as devices. 
// The Hedron API creates a single queue associated with the device, 
// so 'get queue' just gets THE singular queue handle. In the future, this
// should be expanded, but with caution, only introducing extra complecity if it
// is justified.
typedef struct HdQueue HdQueue;
HdQueue* get_queue(HdLogicalDevice* device);

typedef struct HdCommandBuffer HdCommandBuffer;

HdCommandBuffer* start_recording_commands(HdQueue* queue);
void submit_commands(HdQueue* queue, PtrSlice command_buffers, HdSemaphore* semaphore, uint64_t value);

// Commands
// ---------
// 
// TODO: stage meshlet shader??
// TODO: stage acceleration structure??
typedef enum {
  StageTransfer,
  StageCompute,
  StageRasterColourOut,
  StagePixelShader,
  StageVertexShader
} HdStage;

// TODO: acceleration structure.
typedef enum {
  HazardDrawArguments = 0x1,
  HazardDescriptors = 0x2,
  HAZARD_DEPTH_STENCIL = 0x4
} HdHazardFlags; 

// TODO: investigate what signals need adding (if any)
typedef enum {
  SignalAtomicSet,
  SignalAtomicMax,
  SignalAtomicOr
} HdSignal;

typedef enum {
    LOpLoad,
    LOpClear,
    LOpDiscard,
} LoadOp;

typedef enum {
    SOpStore,
    SOpDiscard,
} StoreOp;

typedef struct {
    float x;
    float y;
    float z;
    float w;
} Vec4f;

typedef struct RenderView RenderView;
typedef struct {
    RenderView* render_view;
    LoadOp load;
    StoreOp store;
    Vec4f clear;
} HdColourAttachment;
SLICE_TYPE(HdColourAttachment, ColourAttachment)

typedef struct {
    RenderView* render_view;
    LoadOp load;
    StoreOp store;
    float clear;
} HdDepthAttachment;

typedef struct {
    RenderView* render_view;
    LoadOp load;
    StoreOp store;
    uint8_t clear;
} HdStencilAttachment;

typedef struct {
    ColourAttachmentSlice colours;
    HdDepthAttachment depth;
    HdStencilAttachment stencil;
} HdRenderDesc;

/*
 * These are for signalling GPU/GPU dependencies. The barrier is for use within
 * a command buffer recording, whereas (I think) the signal after/wait before
 * can be used between queues? (maybe??)
 */
void barrier(HdCommandBuffer* cb, HdStage before, HdStage after, HdHazardFlags hazards);
void signal_after(HdCommandBuffer* cb, HdStage before, void *ptrGpu, uint64_t value, HdSignal signal);
void wait_before(HdCommandBuffer* cb, HdStage after, void *ptrGpu, uint64_t value, HdCompOp op, HdHazardFlags hazards, uint64_t mask);

void set_pipeline(HdCommandBuffer* cb, HdPipeline* pipeline);
//void set_depth_stencil_state(HdCommandBuffer* cb, GpuDepthStencilState state);
//void set_blend_state(HdCommandBuffer* cb, GpuBlendState state); 

typedef struct {
    uint32_t x;
    uint32_t y;
    uint32_t z;
} UVec3;
void dispatch(HdLogicalDevice* device, HdCommandBuffer* cb, void* dataGpu, UVec3 gridDimensions);
void dispatch_indirect(HdCommandBuffer* cb, void* dataGpu, void* gridDimensionsGpu);

void begin_render_pass(HdCommandBuffer* cb, HdRenderDesc desc);
void end_render_pass(HdCommandBuffer* cb);


#endif
