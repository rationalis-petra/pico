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
  CullALL,
  CullNone,
} HdCull;

typedef enum {
  BlendAdd,
  BlendSubtract,
  BlendRevSubtract,
  BlendMin,
  BlendMax
} HdBlend;

typedef enum {
  FactorZero,
  FactorOne,
  FactorSrcColour,
  factorDstColour,
  FactorSrcAlpha,
  FactorOneMinusSrcColour,
  FactorOneMinusDstColour,
  FactorOneMinusSrcAlpha,
  FactorDstAlpha,
  FactorOneMinus_Dst_Alpha,
  // TODO: check this is all of them
} HdFactor;

typedef enum {
  TriangleList,
  TriangleStrip,
  TriangleFan,
} HdTopology;

typedef enum {
  FormatNone,
  FormatRGBA8Unorm,
  FormatD32Float,
  FormatRG11B10Float,
  FormatRGB10A2Unorm,
  FormatRGB32Float,
  FormatRG32Float,
  FormatRGBA32Float,
  FormatRGBA16Float, // TODO: fill out rest of list? 
} HdFormat;

typedef struct {
    HdFormat format;
    uint8_t writeMask;
} HdColourTarget;
SLICE_TYPE(HdColourTarget, HdColourTarget);

typedef struct {
    HdBlend colorOp;
    HdFactor srcColourFactor;
    HdFactor dstColourFactor;
    HdBlend alphaOp;
    HdFactor srcAlphaFactor;
    HdFactor dstAlphaFactor;
    uint8_t colorWriteMask;
} HdBlendDescription;
OPTION_TYPE(HdBlendDescription, HdBlendDescription);

typedef struct {
    HdTopology topology;
    HdCull cull;
    bool alphaToCoverage;
    bool supportDualSourceBlending;
    uint8_t sampleCount;
    HdFormat depthFormat;
    HdFormat stencilFormat;
    
    HdColourTargetSlice colorTargets;
    HdBlendDescriptionOption blendstate; // optional embedded blend state
} HdRasterDescription;

typedef struct HdPipeline HdPipeline;
HdPipeline* create_compute_pipeline(U32Slice computeIR, HdLogicalDevice* device);
HdPipeline* create_graphics_pipeline(U32Slice vertexIR, U32Slice pixelIR, HdRasterDescription desc, HdLogicalDevice* device);
HdPipeline* create_graphics_meshlet_pipeline(U32Slice meshletIR, U32Slice pixelIR, HdRasterDescription desc, HdLogicalDevice* device);
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
} HdOP;

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

/*
 * These are for signalling GPU/GPU dependencies. The barrier is for use within
 * a command buffer recording, whereas (I think) the signal after/wait before
 * can be used between queues? (maybe??)
 */
void barrier(HdCommandBuffer* cb, HdStage before, HdStage after, HdHazardFlags hazards);
void signal_after(HdCommandBuffer* cb, HdStage before, void *ptrGpu, uint64_t value, HdSignal signal);
void wait_before(HdCommandBuffer* cb, HdStage after, void *ptrGpu, uint64_t value, HdOP op, HdHazardFlags hazards, uint64_t mask);

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

#endif
