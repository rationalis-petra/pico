#ifdef USE_VULKAN

#include "platform/signals.h"
#include "platform/hedron/hedron.h"
#include "platform/memory/std_allocator.h"

#include "components/pretty/string_printer.h"

#include "pico/values/ctypes.h"
#include "pico/codegen/codegen.h"

#include "pico/stdlib/core/kernel.h"
#include "pico/stdlib/platform/submodules.h"

/**
 * V2 exposed/shared types
 */
static PiType* error_code_ty;

static PiType* instance_ty;

static PiType* surface_ty;

static PiType* physical_device_ty;
static PiType* logical_device_ty;

static PiType* swapchain_ty;

static PiType* alloc_sort_ty;
static PiType* device_address_ty;
static PiType* shared_address_ty;

static PiType* pipeline_ty;

static PiType* queue_ty;
static PiType* semaphore_ty;
static PiType* command_buffer_ty;

//  Errors
// --------
void build_view_error_string_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "error", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}), mk_string_ctype(pia));
    convert_c_fn(view_error_string, &fn_ctype, type, ass, a, point); 
}

//   Context: Instances, Devices, Windows
// ----------------------------------------

static HdPtrResult relic_create_hedron_instance() {
    Allocator* stdalloc = get_std_allocator();
    return create_hedron_instance(stdalloc);
}

static void build_create_hedron_instance_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 0, mk_result_ctype(pia, mk_voidptr_ctype(pia), mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned})));
    convert_c_fn(relic_create_hedron_instance, &fn_ctype, type, ass, a, point); 
}

static void build_teardown_hedron_instance_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "instance", mk_voidptr_ctype(pia), (CType){.sort = CSVoid});
    convert_c_fn(teardown_hedron_instance, &fn_ctype, type, ass, a, point); 
}


#ifdef WINDOW_SYSTEM
static void build_create_window_surface_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 2,
                                 "window", mk_voidptr_ctype(pia),
                                 "instance", mk_voidptr_ctype(pia),
                                 mk_result_ctype(pia, mk_voidptr_ctype(pia), mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned})));
    convert_c_fn(create_window_surface, &fn_ctype, type, ass, a, point); 
}

/*
  static void build_resize_window_surface_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType extent_type = mk_struct_ctype(pia, 2, "width", mk_primint_ctype((CPrimInt){.prim = CInt, .is_signed = Unsigned}),
  "height", mk_primint_ctype((CPrimInt){.prim = CInt, .is_signed = Unsigned}));
  CType fn_ctype = mk_fn_ctype(pia, 2,
  "window", mk_voidptr_ctype(pia),
  "device", mk_voidptr_ctype(pia),
  "extent", extent_type, (CType){.sort = CSVoid});
  convert_c_fn(resize_window_surface, &fn_ctype, type, ass, a, point); 
  }
*/

static void build_destroy_window_surface_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "surface", mk_voidptr_ctype(pia), (CType){.sort = CSVoid});
    convert_c_fn(destroy_window_surface, &fn_ctype, type, ass, a, point); 
}
#endif

static PtrSlice relic_get_physical_devices(HdInstance* instance) {
    PiAllocator curr = get_std_current_allocator();
    Allocator alloc = convert_to_callocator(&curr);
    return get_physical_devices(instance,&alloc);
}

static void build_get_physical_devices_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "instance", mk_voidptr_ctype(pia), mk_slice_ctype(pia));
    convert_c_fn(relic_get_physical_devices, &fn_ctype, type, ass, a, point); 
}

static void build_create_logical_device_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 2,
                                 "physical_device", mk_voidptr_ctype(pia),
                                 "instance", mk_voidptr_ctype(pia),
                                 mk_result_ctype(pia, mk_voidptr_ctype(pia), mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned})));
    convert_c_fn(create_logical_device, &fn_ctype, type, ass, a, point); 
}

static void build_destroy_logical_device_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1,
                                 "logical_device", mk_voidptr_ctype(pia),
                                 (CType){.sort = CSVoid});
    convert_c_fn(destroy_logical_device, &fn_ctype, type, ass, a, point); 
}

static uint64_t current_device; 
HdLogicalDevice* get_current_device() {
    HdLogicalDevice*** data = get_dynamic_memory();
    HdLogicalDevice** dyn = data[current_device]; 
    return *dyn;
}

//   Swapchain
// ------------

void build_create_swapchain_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 2,
                                 "device", mk_voidptr_ctype(pia),
                                 "surface", mk_voidptr_ctype(pia),
                                 mk_result_ctype(pia, mk_voidptr_ctype(pia), mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned})));
    convert_c_fn(create_swapchain, &fn_ctype, type, ass, a, point); 
}

void build_destroy_swapchain_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1,
                                 "swapchain", mk_voidptr_ctype(pia),
                                 (CType){.sort = CSVoid});
    convert_c_fn(destroy_swapchain, &fn_ctype, type, ass, a, point); 
}

//   Memory
// ---------

SharedAddress relic_alloc_shared_memory(size_t size, size_t align, MemoryType type) {
    HdLogicalDevice* device = get_current_device();
    return alloc_shared_memory(size, align, type, device);
}

void build_alloc_shared_memory_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType shared_mem_ctype = mk_struct_ctype(pia, 2,
                                             "host", mk_voidptr_ctype(pia),
                                             "device", mk_voidptr_ctype(pia));
    CType fn_ctype = mk_fn_ctype(pia, 3,
                                 "size", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                                 "align", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                                 "type", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                                 shared_mem_ctype);
    convert_c_fn(relic_alloc_shared_memory, &fn_ctype, type, ass, a, point); 
}

void relic_free_shared_memory(SharedAddress address) {
    HdLogicalDevice* device = get_current_device();
    free_shared_memory(address, device);
}

void build_free_shared_memory_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType shared_mem_ctype = mk_struct_ctype(pia, 2,
                                             "host", mk_voidptr_ctype(pia),
                                             "device", mk_voidptr_ctype(pia));
    CType fn_ctype = mk_fn_ctype(pia, 1, "mem", shared_mem_ctype, (CType){.sort = CSVoid});
    convert_c_fn(relic_free_shared_memory, &fn_ctype, type, ass, a, point); 
}

DeviceAddress relic_alloc_device_memory(size_t size, size_t align) {
    HdLogicalDevice* device = get_current_device();
    return alloc_device_memory(size, align, device);
}

void build_alloc_device_memory_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 3,
                                 "size", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                                 "align", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                                 "type", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                                 mk_voidptr_ctype(pia));
    convert_c_fn(relic_alloc_device_memory, &fn_ctype, type, ass, a, point); 
}

void relic_free_device_memory(DeviceAddress address) {
    HdLogicalDevice* device = get_current_device();
    free_device_memory(address, device);
}

void build_free_device_memory_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "mem", mk_voidptr_ctype(pia), (CType){.sort = CSVoid});
    convert_c_fn(relic_free_device_memory, &fn_ctype, type, ass, a, point); 
}

//   Textures 
// ---------
// (TODO)

//   Pipelines 
// -------------

HdPipeline* relic_create_compute_pipeline(U32Slice compute_IR) {
  HdLogicalDevice* device = get_current_device();
  return create_compute_pipeline(compute_IR, device);
}

void build_create_compute_pipeline_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 1, "compute_ir", mk_slice_ctype(pia), mk_voidptr_ctype(pia));
  convert_c_fn(relic_create_compute_pipeline, &fn_ctype, type, ass, a, point); 
}

//HdPipeline* create_graphics_pipeline(U32Slice vertexIR, U32Slice pixelIR, HdRasterDescription desc, HdLogicalDevice* device);
//HdPipeline* create_graphics_meshlet_pipeline(U32Slice meshletIR, U32Slice pixelIR, HdRasterDescription desc, HdLogicalDevice* device);

void relic_destroy_pipeline(HdPipeline* pipeline) {
  HdLogicalDevice* device = get_current_device();
  destroy_pipeline(pipeline, device);
}

void build_destroy_pipeline_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 1, "pipeline", mk_voidptr_ctype(pia), (CType){.sort = CSVoid});
  convert_c_fn(relic_destroy_pipeline, &fn_ctype, type, ass, a, point); 
}

//   Semaphores 
// -------------

HdSemaphore* create_semaphore(HdLogicalDevice* device, uint64_t init_value);
void wait_semaphore(HdLogicalDevice* device, HdSemaphore* sema, uint64_t value);
void destroy_semaphore(HdLogicalDevice* device, HdSemaphore* sema);

HdSemaphore* relic_create_semaphore(uint64_t initial_val) {
    HdLogicalDevice* device = get_current_device();
    return create_semaphore(device, initial_val);
}

void build_create_semaphore_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1,
                                 "initial_value", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                                 mk_voidptr_ctype(pia));
    convert_c_fn(relic_create_semaphore, &fn_ctype, type, ass, a, point); 
}

void relic_wait_semaphore(HdSemaphore* semaphore, uint64_t initial_val) {
    HdLogicalDevice* device = get_current_device();
    wait_semaphore(device, semaphore, initial_val);
}

void build_wait_semaphore_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 2,
                                 "semaphore", mk_voidptr_ctype(pia),
                                 "value", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                                 (CType){.sort = CSVoid});
    convert_c_fn(relic_wait_semaphore, &fn_ctype, type, ass, a, point); 
}

void relic_destroy_semaphore(HdSemaphore* semaphore) {
    HdLogicalDevice* device = get_current_device();
    destroy_semaphore(device, semaphore);
}

void build_destroy_semaphore_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "semaphore", mk_voidptr_ctype(pia), (CType){.sort = CSVoid});
    convert_c_fn(relic_destroy_semaphore, &fn_ctype, type, ass, a, point); 
}

//   Queues 
// -------------

HdQueue* relic_get_queue() {
    HdLogicalDevice* device = get_current_device();
    return get_queue(device);
}

void build_get_queue_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 0, mk_voidptr_ctype(pia));
    convert_c_fn(relic_get_queue, &fn_ctype, type, ass, a, point); 
}

void build_start_recording_commands_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "queue", mk_voidptr_ctype(pia), mk_voidptr_ctype(pia));
    convert_c_fn(start_recording_commands, &fn_ctype, type, ass, a, point); 
}

void build_submit_commands_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 4,
                               "queue", mk_voidptr_ctype(pia),
                               "commands", mk_slice_ctype(pia),
                               "semaphore", mk_voidptr_ctype(pia),
                               "value", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                               (CType){.sort = CSVoid});
    convert_c_fn(submit_commands, &fn_ctype, type, ass, a, point); 
}

//   Commands 
// -------------

void build_set_pipeline_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 2,
                               "commands", mk_voidptr_ctype(pia),
                               "pipeline", mk_voidptr_ctype(pia),
                               (CType){.sort = CSVoid});
    convert_c_fn(set_pipeline, &fn_ctype, type, ass, a, point); 
}

void relic_dispatch(HdCommandBuffer* cb, void* data, UVec3 grid_dimensions) {
    HdLogicalDevice* device = get_current_device();
    dispatch(device, cb, data, grid_dimensions);
}

void build_dispatch_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType dims = mk_struct_ctype(pia, 3,
                                 "x", mk_primint_ctype((CPrimInt){.prim = CInt, .is_signed = Unsigned}),
                                 "y", mk_primint_ctype((CPrimInt){.prim = CInt, .is_signed = Unsigned}),
                                 "z", mk_primint_ctype((CPrimInt){.prim = CInt, .is_signed = Unsigned}));
    CType fn_ctype = mk_fn_ctype(pia, 3,
                                 "commands", mk_voidptr_ctype(pia),
                                 "data", mk_voidptr_ctype(pia),
                                 "dimenstions", dims,
                                 (CType){.sort = CSVoid});
    convert_c_fn(relic_dispatch, &fn_ctype, type, ass, a, point); 
}

void add_hedron_module(Assembler *ass, Module *platform, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);
    PiAllocator pico_region = convert_to_pallocator(&ra);
    PiAllocator* pia = &pico_region;
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(0, &ra),
    };
    ReExports re_exports = (ReExports) {
        .clauses = mk_import_clause_array(0, &ra),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, &ra),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_name(mv_string("hedron")),
        .imports = imports,
        .re_exports = re_exports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(platform), platform);
    Name name;

    ModuleEntry* e;
    PiType type;
    PiType* typep;
    ErrorPoint point;
    if (catch_error(point)) {
        panic(doc_to_str(point.error_message, 120, &ra));
    }

    Segments prepped;
    Segments fn_segments = {.data = mk_u8_array(0, &ra),};
    Segments null_segments = (Segments) {
        .code = mk_u8_array(0, &ra),
        .data = mk_u8_array(0, &ra),
    };

    /**
     * Error handling  
     */
    type = (PiType) {.sort = TType};

    typep = mk_enum_type(pia, 31,
                         "not-ready", 0,
                         "timeout", 0,
                         "event-set", 0,
                         "event-reset", 0,
                         "incomplete", 0,
                         "error-out-of-host-memory", 0,
                         "error-out-of-device-memory", 0,
                         "error-initialization-failed", 0,
                         "error-device-lost", 0,
                         "error-memory-map-failed", 0,
                         "error-layer-not-present", 0,
                         "error-extension-not-present", 0,
                         "error-feature-not-present", 0,
                         "error-incompatible-driver", 0,
                         "error-too-many-objects", 0,
                         "error-format-not-supported", 0,
                         "error-fragmented-pool", 0,
                         "error-unknown", 0,
                         "error-validation-failed", 0,
                         "error-out-of-pool-memory", 0,
                         "error-invalid-external-handle", 0,
                         "error-invalid-opaque-capture-address", 0,
                         "error-fragmentation", 0,
                         "pipeline-compile-required", 0,
                         "error-not-permitted", 0,
                         "error-surface-lost", 0,
                         "error-native-window-in-use", 0,
                         "suboptimal", 0,
                         "error-out-of-date", 0,
                         "error-incompatible-display", 0,
                         "incompatible-shader-binary", 0);
    name = string_to_name(mv_string("ErrorCode"));
    add_def(module, name, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def_internal(name, module);
    error_code_ty = e->value;

    typep = mk_proc_type(pia, 1, error_code_ty, mk_string_type(pia));
    build_view_error_string_fn(typep, ass, pia, &ra, &point);
    name = string_to_name(mv_string("view-error-string"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);


    /** 
     * Init + teardown
     * ----------------
     * Initialization and teardown of a hedron instance.
     * 
     * TODO: the init function should return a (Result Instance Err). API calls
     *   are then expected to set an instance dyntamic variable. We can postpone
     *   this because it won't affect user code much (just the implementation).
     * 
     */

    type = (PiType) {.sort = TType};
    typep = mk_opaque_type(pia, "Instance", module, mk_prim_type(pia, Address));
    name = string_to_name(mv_string("Instance"));
    add_def(module, name, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def_internal(name, module);
    instance_ty = e->value;

    typep = mk_proc_type(pia, 0, mk_type_app(pia, get_result_type(), instance_ty, error_code_ty));
    build_create_hedron_instance_fn(typep, ass, pia, &ra, &point);
    name = string_to_name(mv_string("create-instance"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, instance_ty, mk_prim_type(pia, Unit));
    build_teardown_hedron_instance_fn(typep, ass, pia, &ra, &point);
    name = string_to_name(mv_string("destroy-instance"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    /** 
     * Window System Definitions.
     * -------------------------
     *  These allow hedron to interact with a window system, but are only present
     *  if the 'WINDOW_SYSTEM' component is requested in the build (see default.config).
     */
    typep = mk_opaque_type(pia, "Surface", module, mk_prim_type(pia, Address));
    type = (PiType) {.sort = TType};
    name = string_to_name(mv_string("Surface"));
    add_def(module, name, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def_internal(name, module);
    surface_ty = e->value;

#ifdef WINDOW_SYSTEM
    typep = mk_proc_type(pia, 2, get_window_ty(), instance_ty, mk_type_app(pia, get_result_type(), surface_ty, error_code_ty));
    build_create_window_surface_fn(typep, ass, pia, &ra, &point);
    name = string_to_name(mv_string("create-surface"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    /*
      typep = mk_proc_type(pia, 2, surface_ty, physical_device_ty,
      mk_type_app(pia, get_pair_type(), mk_prim_type(pia, UInt_32), mk_prim_type(pia, UInt_32)),
      mk_prim_type(pia, Unit));
      build_resize_window_surface_fn(typep, ass, pia, &ra, &point);
      name = string_to_name(mv_string("resize-window-surface"));
      fn_segments.code = get_instructions(ass);
      prepped = prep_target(module, fn_segments, ass, NULL);
      add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
      clear_assembler(ass);
    */

    typep = mk_proc_type(pia, 1, surface_ty, mk_prim_type(pia, Unit));
    build_destroy_window_surface_fn(typep, ass, pia, &ra, &point);
    name = string_to_name(mv_string("destroy-surface"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
#endif

    /** 
     * Device Management
     * ----------------
     * List physical devices, query their properties, select a physical
     * device/create a logical device. 
     * 
     */

    type = (PiType) {.sort = TType};
    typep = mk_opaque_type(pia, "PhysicalDevice", module, mk_prim_type(pia, Address));
    name = string_to_name(mv_string("PhysicalDevice"));
    add_def(module, name, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def_internal(name, module);
    physical_device_ty = e->value;

    type = (PiType) {.sort = TType};
    typep = mk_opaque_type(pia, "LogicalDevice", module, mk_prim_type(pia, Address));
    name = string_to_name(mv_string("LogicalDevice"));
    add_def(module, name, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def_internal(name, module);
    logical_device_ty = e->value;

    HdLogicalDevice* initial_val = NULL;
    typep = mk_dynamic_type(pia, logical_device_ty);
    current_device = mk_dynamic_var(sizeof(HdLogicalDevice*), &initial_val);
    name = string_to_name(mv_string("current-device"));
    add_def(module, name, *typep, &current_device, null_segments, NULL);

    typep = mk_proc_type(pia, 1, instance_ty, mk_type_app(pia, get_slice_type(), physical_device_ty));
    build_get_physical_devices_fn(typep, ass, pia, &ra, &point);
    name = string_to_name(mv_string("get-physical-devices"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 2, physical_device_ty, instance_ty, mk_type_app(pia, get_result_type(), logical_device_ty, error_code_ty));
    build_create_logical_device_fn(typep, ass, pia, &ra, &point);
    name = string_to_name(mv_string("create-logical-device"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, logical_device_ty, mk_prim_type(pia, Unit));
    build_destroy_logical_device_fn(typep, ass, pia, &ra, &point);
    name = string_to_name(mv_string("destroy-logical-device"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    /** 
     * Swapchain
     * ----------------
     * 
     */

    type = (PiType) {.sort = TType};
    typep = mk_opaque_type(pia, "Swapchain", module, mk_prim_type(pia, Address));
    name = string_to_name(mv_string("Swapchain"));
    add_def(module, name, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def_internal(name, module);
    swapchain_ty = e->value;

    typep = mk_proc_type(pia, 2, logical_device_ty, surface_ty, mk_type_app(pia, get_result_type(), swapchain_ty, error_code_ty));
    build_create_swapchain_fn(typep, ass, pia, &ra, &point);
    name = string_to_name(mv_string("create-swapchain"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, swapchain_ty, mk_prim_type(pia, Unit));
    build_destroy_swapchain_fn(typep, ass, pia, &ra, &point);
    name = string_to_name(mv_string("destroy-swapchain"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    /**
     *   Memory Allocation: 
     *  --------------------
     *  - Can allocate 3 types of memory
     *    - GPU memory that requires explicit copy operations
     *    - Mapped memory that visible for both, but optimized for CPU writes/GPU reads.
     *      Reading from this memory is legal, but extremely slow.
     *    - Mapped memory that visible for both, but and optimized for
     *      bidirectional communication. Slower 1-way than mapped memory. 
     * 
     *  Hedron divides these into two types of allocations:
     *  - GPU only allocation (returns a DeviceAddress)
     *  - Shared allocation (returns an Address, can get associated Device Address)
     */
    type = (PiType) {.sort = TType};

    typep = mk_enum_type(pia, 2, "default", 0, "writeback", 0);
    name = string_to_name(mv_string("AllocSort"));
    add_def(module, name, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def_internal(name, module);
    alloc_sort_ty = e->value;

    typep = mk_opaque_type(pia, "DeviceAddress", module, mk_prim_type(pia, Address));
    name = string_to_name(mv_string("DeviceAddress"));
    add_def(module, name, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def_internal(name, module);
    device_address_ty = e->value;

    typep =
        mk_named_type(pia, "SharedAddress",
                      mk_struct_type(pia, 2,
                                     "host", mk_prim_type(pia, Address),
                                     "device", device_address_ty));
    name = string_to_name(mv_string("SharedAddress"));
    add_def(module, name, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def_internal(name, module);
    shared_address_ty = e->value;


    typep = mk_proc_type(pia, 3, mk_prim_type(pia, UInt_64), mk_prim_type(pia, UInt_64), alloc_sort_ty, shared_address_ty);
    build_alloc_shared_memory_fn(typep, ass, pia, &ra, &point);
    name = string_to_name(mv_string("alloc-shared"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, shared_address_ty, mk_prim_type(pia, Unit));
    build_free_shared_memory_fn(typep, ass, pia, &ra, &point);
    name = string_to_name(mv_string("free-shared"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 3, mk_prim_type(pia, UInt_64), mk_prim_type(pia, UInt_64), alloc_sort_ty, device_address_ty);
    build_alloc_device_memory_fn(typep, ass, pia, &ra, &point);
    name = string_to_name(mv_string("alloc-device"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, device_address_ty, mk_prim_type(pia, Unit));
    build_free_device_memory_fn(typep, ass, pia, &ra, &point);
    name = string_to_name(mv_string("free-device"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    /** 
     * Textures 
     * ----------
     * TODO
     */

    /** 
     * Pipeline 
     * ----------
     * A pipeline is simply a series of shaders that get executed, and (in the
     * case of a graphics pipeline), the rasterizer state that the shaders use.
     */
    type = (PiType) {.sort = TType};

    typep = mk_opaque_type(pia, "Pipeline", module, mk_prim_type(pia, Address));
    name = string_to_name(mv_string("Pipeline"));
    add_def(module, name, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def_internal(name, module);
    pipeline_ty = e->value;

    typep = mk_proc_type(pia, 1, mk_type_app(pia, get_slice_type(), mk_prim_type(pia, UInt_32)), pipeline_ty);
    build_create_compute_pipeline_fn(typep, ass, pia, &ra, &point);
    name = string_to_name(mv_string("create-compute-pipeline"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, pipeline_ty, mk_prim_type(pia, Unit));
    build_destroy_pipeline_fn(typep, ass, pia, &ra, &point);
    name = string_to_name(mv_string("destroy-pipeline"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);


    /**
     * Semaphores
     * -----------
     * 
     */ 
    type = (PiType) {.sort = TType};

    typep = mk_opaque_type(pia, "Semaphore", module, mk_prim_type(pia, Address));
    name = string_to_name(mv_string("Semaphore"));
    add_def(module, name, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def_internal(name, module);
    semaphore_ty = e->value;

    typep = mk_proc_type(pia, 1, mk_prim_type(pia, UInt_64), semaphore_ty);
    build_create_semaphore_fn(typep, ass, pia, &ra, &point);
    name = string_to_name(mv_string("create-semaphore"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    void wait_semaphore(HdLogicalDevice* device, HdSemaphore* sema, uint64_t value);

    typep = mk_proc_type(pia, 2, semaphore_ty, mk_prim_type(pia, UInt_64), mk_prim_type(pia, Unit));
    build_wait_semaphore_fn(typep, ass, pia, &ra, &point);
    name = string_to_name(mv_string("wait-semaphore"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, semaphore_ty, mk_prim_type(pia, Unit));
    build_destroy_semaphore_fn(typep, ass, pia, &ra, &point);
    name = string_to_name(mv_string("destroy-semaphore"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    /**
     * Queues
     * ----------
     * Queues are an underdeveloped part of the API, primarily as they were
     * underspecified by Aaltonen, so we are starting very basic and adding
     * features only when necessary.
     *
     */
    type = (PiType) {.sort = TType};

    typep = mk_opaque_type(pia, "Queue", module, mk_prim_type(pia, Address));
    name = string_to_name(mv_string("Queue"));
    add_def(module, name, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def_internal(name, module);
    queue_ty = e->value;

    typep = mk_opaque_type(pia, "CommandBuffer", module, mk_prim_type(pia, Address));
    name = string_to_name(mv_string("CommandBuffer"));
    add_def(module, name, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def_internal(name, module);
    command_buffer_ty = e->value;

    typep = mk_proc_type(pia, 0, queue_ty);
    build_get_queue_fn(typep, ass, pia, &ra, &point);
    name = string_to_name(mv_string("get-queue"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, queue_ty, command_buffer_ty);
    build_start_recording_commands_fn(typep, ass, pia, &ra, &point);
    name = string_to_name(mv_string("start-recording"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 4,
                         queue_ty,
                         mk_type_app(pia, get_slice_type(), command_buffer_ty),
                         semaphore_ty,
                         mk_prim_type(pia, UInt_64),
                         mk_prim_type(pia, Unit));
    build_submit_commands_fn(typep, ass, pia, &ra, &point);
    name = string_to_name(mv_string("submit-commands"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    /**
     * Commands
     * ----------
     *
     */

    typep = mk_proc_type(pia, 2, command_buffer_ty, pipeline_ty, mk_prim_type(pia, Unit));
    build_set_pipeline_fn(typep, ass, pia, &ra, &point);
    name = string_to_name(mv_string("set-pipeline"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    PiType *wave_dims = mk_struct_type(pia, 3,
                                       "x", mk_prim_type(pia, UInt_32),
                                       "y", mk_prim_type(pia, UInt_32),
                                       "z", mk_prim_type(pia, UInt_32));
    typep = mk_proc_type(pia, 3, command_buffer_ty, mk_prim_type(pia, Address), wave_dims, mk_prim_type(pia, Unit));
    build_dispatch_fn(typep, ass, pia, &ra, &point);
    name = string_to_name(mv_string("dispatch"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
}

#endif
