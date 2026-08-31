#ifdef USE_VULKAN

#include "platform/signals.h"
#include "platform/machine_info.h"
#include "platform/hedron/hedron.h"
#include "platform/memory/std_allocator.h"

#include "components/pretty/string_printer.h"

#include "pico/values/ctypes.h"
#include "pico/codegen/codegen.h"
#include "pico/codegen/backend-direct/internal.h"

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

/*
  void build_create_shader_module_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 1, "code", mk_slice_ctype(pia), mk_voidptr_ctype(pia));
  convert_c_fn(create_shader_module, &fn_ctype, type, ass, a, point); 
  }

  void build_destroy_shader_module_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 1, "module", mk_voidptr_ctype(pia), (CType){.sort = CSVoid});
  convert_c_fn(destroy_shader_module, &fn_ctype, type, ass, a, point); 
  }

  void build_create_pipeline_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType arg_ctype = mk_struct_ctype(pia, 6,
  "resource_describe", mk_slice_ctype(pia),
  "binder_describe", mk_slice_ctype(pia),
  "attrib_describe", mk_slice_ctype(pia),
  "push_constants", mk_slice_ctype(pia),
  "shaders", mk_slice_ctype(pia),
  "surface", mk_voidptr_ctype(pia));
  CType fn_ctype = mk_fn_ctype(pia, 1, "info", arg_ctype, mk_voidptr_ctype(pia));
  convert_c_fn(create_pipeline, &fn_ctype, type, ass, a, point); 
  }

  void build_destroy_pipeline_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 1, "pipeline", mk_voidptr_ctype(pia), (CType){.sort = CSVoid});
  convert_c_fn(destroy_pipeline, &fn_ctype, type, ass, a, point); 
  }

  // ----------------------------------------------------------------------------
  //
  // Data contract (vertex/input formats, etc.)
  // 
  // ----------------------------------------------------------------------------


  void build_create_image_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 3, 
  "width", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt,}),
  "height", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt,}),
  "format", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CLongLong,}),
  mk_voidptr_ctype(pia));

  convert_c_fn(create_image, &fn_ctype, type, ass, a, point); 
  }

  void build_destroy_image_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 1, 
  "image", mk_voidptr_ctype(pia), 
  (CType){.sort = CSVoid});

  convert_c_fn(destroy_image, &fn_ctype, type, ass, a, point); 
  }

  void build_create_image_view_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 2, 
  "image", mk_voidptr_ctype(pia),
  "format", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CLongLong,}),
  mk_voidptr_ctype(pia));

  convert_c_fn(create_image_view, &fn_ctype, type, ass, a, point); 
  }

  void build_destroy_image_view_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 1, 
  "image", mk_voidptr_ctype(pia), 
  (CType){.sort = CSVoid});

  convert_c_fn(destroy_image_view, &fn_ctype, type, ass, a, point); 
  }

  void build_create_sampler_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 3, 
  "enable-anisotropy", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CChar,}),
  "min-filter", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CLongLong,}),
  "mag-filter", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CLongLong,}),
  mk_voidptr_ctype(pia));

  convert_c_fn(create_sampler, &fn_ctype, type, ass, a, point); 
  }

  void build_destroy_sampler_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 1, 
  "sampler", mk_voidptr_ctype(pia), 
  (CType){.sort = CSVoid});

  convert_c_fn(destroy_sampler, &fn_ctype, type, ass, a, point); 
  }

  // Descriptor Sets
  // ----------------------------------

  void build_create_descriptor_set_layout(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 1, "binding_descriptions", mk_slice_ctype(pia), mk_voidptr_ctype(pia));
  convert_c_fn(create_descriptor_set_layout, &fn_ctype, type, ass, a, point); 
  }

  void build_destroy_descriptor_set_layout(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 1, "layout", mk_voidptr_ctype(pia), (CType){.sort = CSVoid});
  convert_c_fn(destroy_descriptor_set_layout, &fn_ctype, type, ass, a, point); 
  }

  void build_create_descriptor_pool(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 2, "sizes", mk_slice_ctype(pia), "max-sets", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt}), mk_voidptr_ctype(pia));
  convert_c_fn(create_descriptor_pool, &fn_ctype, type, ass, a, point); 
  }

  void build_destroy_descriptor_pool(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 1, "pool", mk_voidptr_ctype(pia), (CType){.sort = CSVoid});
  convert_c_fn(destroy_descriptor_pool, &fn_ctype, type, ass, a, point); 
  }

  void build_alloc_descriptor_sets(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 3,
  "set_count", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt}),
  "descriptor_set_layouts", mk_voidptr_ctype(pia),
  "pool", mk_voidptr_ctype(pia),
  mk_slice_ctype(pia));
  convert_c_fn(alloc_descriptor_sets, &fn_ctype, type, ass, a, point); 
  }

  void build_update_descriptor_sets(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 2,
  "writes", mk_slice_ctype(pia),
  "copies", mk_slice_ctype(pia),
  (CType){.sort = CSVoid});
                                 
  convert_c_fn(update_descriptor_sets, &fn_ctype, type, ass, a, point); 
  }

  // -----------------------------------------------------------------------------
  //
  //                                      Commands
  //
  // -----------------------------------------------------------------------------

  void build_create_command_pool_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 0, mk_voidptr_ctype(pia));
  convert_c_fn(create_command_pool, &fn_ctype, type, ass, a, point); 
  }

  void build_destroy_command_pool_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 1, "pool", mk_voidptr_ctype(pia), (CType){.sort = CSVoid});
  convert_c_fn(destroy_command_pool, &fn_ctype, type, ass, a, point); 
  }

  void build_create_command_buffer_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 1, "pool", mk_voidptr_ctype(pia), mk_voidptr_ctype(pia));
  convert_c_fn(create_command_buffer, &fn_ctype, type, ass, a, point); 
  }

  void build_free_command_buffer_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 2,
  "pool", mk_voidptr_ctype(pia),
  "buffer",  mk_voidptr_ctype(pia),
  (CType){.sort = CSVoid});
  convert_c_fn(free_command_buffer, &fn_ctype, type, ass, a, point); 
  }

  void build_queue_submit_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType ptr_option = mk_struct_ctype(pia, 2,
  "optional", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
  "ptr", mk_voidptr_ctype(pia));
  CType fn_ctype = mk_fn_ctype(pia, 4, "buffer", mk_voidptr_ctype(pia),
  "fence", ptr_option,
  "wait", mk_slice_ctype(pia),
  "signal", mk_slice_ctype(pia),
  (CType){.sort = CSVoid});
  convert_c_fn(queue_submit, &fn_ctype, type, ass, a, point); 
  }

  void build_queue_present_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 3, "surface", mk_voidptr_ctype(pia),
  "wait", mk_voidptr_ctype(pia),
  "index", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt}),
  (CType){.sort = CSVoid});
  convert_c_fn(queue_present, &fn_ctype, type, ass, a, point); 
  }

  void build_queue_wait_idle_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 0, (CType){.sort = CSVoid});
  convert_c_fn(queue_wait_idle, &fn_ctype, type, ass, a, point); 
  }

  void build_command_begin_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 2, 
  "buffer", mk_voidptr_ctype(pia), 
  "usage", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
  (CType){.sort = CSVoid});
  convert_c_fn(command_begin, &fn_ctype, type, ass, a, point); 
  }

  void build_command_end_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 1, "buffer", mk_voidptr_ctype(pia), (CType){.sort = CSVoid});
  convert_c_fn(command_end, &fn_ctype, type, ass, a, point); 
  }

  void build_reset_command_buffer_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 1, "buffer", mk_voidptr_ctype(pia), (CType){.sort = CSVoid});
  convert_c_fn(reset_command_buffer, &fn_ctype, type, ass, a, point); 
  }

  void build_command_begin_renderpass_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 3, "buffer", mk_voidptr_ctype(pia),
  "surface", mk_voidptr_ctype(pia),
  "image_index", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt}), 
  (CType){.sort = CSVoid});
  convert_c_fn(command_begin_render_pass, &fn_ctype, type, ass, a, point); 
  }

  void build_command_end_renderpass_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 1, "buffer", mk_voidptr_ctype(pia), (CType){.sort = CSVoid});
  convert_c_fn(command_end_render_pass, &fn_ctype, type, ass, a, point); 
  }

  void build_command_pipeline_barrier_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 6, 
  "commands", mk_voidptr_ctype(pia), 
  "source_stage", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CLongLong}), 
  "dest_stage", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CLongLong}) ,
  "memory_barriers", mk_slice_ctype(pia), 
  "buffer_memory_barriers", mk_slice_ctype(pia), 
  "image_memory_barriers", mk_slice_ctype(pia), 
  (CType){.sort = CSVoid});
  convert_c_fn(command_pipeline_barrier, &fn_ctype, type, ass, a, point); 
  }

  void build_command_copy_buffer_to_image_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 5, 
  "commands", mk_voidptr_ctype(pia), 
  "buffer", mk_voidptr_ctype(pia), 
  "image", mk_voidptr_ctype(pia),
  "width", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt}), 
  "height", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt}), 
  (CType){.sort = CSVoid});
  convert_c_fn(command_copy_buffer_to_image, &fn_ctype, type, ass, a, point); 
  }

  void build_command_bind_pipeline_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 2, "command_buffer", mk_voidptr_ctype(pia),
  "pipeline", mk_voidptr_ctype(pia),
  (CType){.sort = CSVoid});
  convert_c_fn(command_bind_pipeline, &fn_ctype, type, ass, a, point); 
  }

  void build_command_push_constants_fn(PiType *type, Assembler *ass, PiAllocator *pia, Allocator *a, ErrorPoint *point) {
  CType fn_ctype = mk_fn_ctype(pia, 6, "command_buffer", mk_voidptr_ctype(pia),
  "pipeline", mk_voidptr_ctype(pia),
  "shader_stage", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CLongLong}), 
  "offset", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt}), 
  "size", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt}), 
  "value", mk_voidptr_ctype(pia),
  (CType){.sort = CSVoid});
  convert_c_fn(command_push_constants, &fn_ctype, type, ass, a, point); 
  }

  void build_command_bind_descriptor_set_fn(PiType *type, Assembler *ass, PiAllocator *pia, Allocator *a, ErrorPoint *point) {
  CType fn_ctype = mk_fn_ctype(pia, 3, "command_buffer", mk_voidptr_ctype(pia),
  "pipeline", mk_voidptr_ctype(pia),
  "descriptor_set", mk_voidptr_ctype(pia),
  (CType){.sort = CSVoid});
  convert_c_fn(command_bind_descriptor_set, &fn_ctype, type, ass, a, point); 
  }

  void build_command_bind_vertex_buffer_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 2, "command_buffer", mk_voidptr_ctype(pia),
  "buffer", mk_voidptr_ctype(pia),
  (CType){.sort = CSVoid});
  convert_c_fn(command_bind_vertex_buffer, &fn_ctype, type, ass, a, point); 
  }

  void build_command_bind_vertex_buffers_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 2, "command_buffer", mk_voidptr_ctype(pia),
  "buffers", mk_slice_ctype(pia),
  (CType){.sort = CSVoid});
  convert_c_fn(command_bind_vertex_buffers, &fn_ctype, type, ass, a, point); 
  }

  void build_command_bind_index_buffer_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 3, "command_buffer", mk_voidptr_ctype(pia),
  "buffer", mk_voidptr_ctype(pia),
  "datatype", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
  (CType){.sort = CSVoid});
  convert_c_fn(command_bind_index_buffer, &fn_ctype, type, ass, a, point); 
  }

  void build_command_set_surface_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 2, "buffer", mk_voidptr_ctype(pia),
  "surface", mk_voidptr_ctype(pia),
  (CType){.sort = CSVoid});
  convert_c_fn(command_set_surface, &fn_ctype, type, ass, a, point); 
  }

  void build_command_draw_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 5, "buffer", mk_voidptr_ctype(pia),
  "vertex-count", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt}),
  "instance-count", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt}),
  "first-vertex", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt}),
  "first-instance", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt}),
  (CType){.sort = CSVoid});
  convert_c_fn(command_draw, &fn_ctype, type, ass, a, point); 
  }

  void build_command_draw_indexed_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 6, "buffer", mk_voidptr_ctype(pia),
  "index-count", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt}),
  "instance-count", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt}),
  "first-index", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt}),
  "vertex-offset", mk_primint_ctype((CPrimInt){.is_signed = Signed, .prim = CInt}),
  "first-instance", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt}),
  (CType){.sort = CSVoid});
  convert_c_fn(command_draw_indexed, &fn_ctype, type, ass, a, point); 
  }

  // -----------------------------------------------------------------------------
  //
  //                                Syncrhonisation
  //
  // -----------------------------------------------------------------------------

  void build_create_semaphore_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 0, mk_voidptr_ctype(pia));
  convert_c_fn(create_semaphore, &fn_ctype, type, ass, a, point); 
  }

  void build_destroy_semaphore_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 1, "semaphore", mk_voidptr_ctype(pia), (CType){.sort = CSVoid});
  convert_c_fn(destroy_semaphore, &fn_ctype, type, ass, a, point); 
  }
  void build_create_fence_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 0, mk_voidptr_ctype(pia));
  convert_c_fn(create_fence, &fn_ctype, type, ass, a, point); 
  }

  void build_destroy_fence_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 1, "fence", mk_voidptr_ctype(pia), (CType){.sort = CSVoid});
  convert_c_fn(destroy_fence, &fn_ctype, type, ass, a, point); 
  }

  void build_wait_for_fence_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 1, "fence", mk_voidptr_ctype(pia),
  (CType){.sort = CSVoid});
  convert_c_fn(wait_for_fence, &fn_ctype, type, ass, a, point); 
  }

  void build_reset_fence_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 1, "fence", mk_voidptr_ctype(pia),
  (CType){.sort = CSVoid});
  convert_c_fn(reset_fence, &fn_ctype, type, ass, a, point); 
  }

  void build_wait_for_device_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(pia, 0, (CType){.sort = CSVoid});
  convert_c_fn(wait_for_device, &fn_ctype, type, ass, a, point); 
  }

  void build_acquire_next_image_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
  CType ret_type = mk_struct_ctype(pia, 2, "type", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CLongLong}),
  "image", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt}));
    
  CType fn_ctype = mk_fn_ctype(pia, 2, "surface", mk_voidptr_ctype(pia), "semaphore", mk_voidptr_ctype(pia), ret_type);
  convert_c_fn(acquire_next_image, &fn_ctype, type, ass, a, point); 
  }
*/

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

    /*
     * ------------------------------------------------------------
     *
     * The V2 API is here
     *
     * ------------------------------------------------------------
     */

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

    // struct GpuPipeline;
    // struct GpuTexture;
    // struct GpuDepthStencilState;
    // struct GpuBlendState;
    // struct GpuQueue;
    // struct GpuCommandBuffer;
    // struct GpuSemaphore;
    // - 

    /**
     * The V1 API Is here
     */

    /*
      typep = mk_opaque_type(pia, "Image", module, mk_prim_type(pia, Address));
      type = (PiType) {.sort = TType};
      name = string_to_name(mv_string("Image"));
      add_def(module, name, type, &typep, null_segments, NULL);
      clear_assembler(ass);
      e = get_def_internal(name, module);
      image_ty = e->value;

      typep = mk_enum_type(pia, 1, "r8-g8-b8-a8-srgb", 0);
      type = (PiType) {.sort = TType};
      name = string_to_name(mv_string("ImageFormat"));
      add_def(module, name, type, &typep, null_segments, NULL);
      clear_assembler(ass);
      e = get_def_internal(name, module);
      image_format_ty = e->value;

      typep = mk_opaque_type(pia, "CommandPool", module, mk_prim_type(pia, Address));
      type = (PiType) {.sort = TType};
      name = string_to_name(mv_string("CommandPool"));
      add_def(module, name, type, &typep, null_segments, NULL);
      clear_assembler(ass);
      e = get_def_internal(name, module);
      command_pool_ty = e->value;

      typep = mk_opaque_type(pia, "CommandBuffer", module, mk_prim_type(pia, Address));
      type = (PiType) {.sort = TType};
      name = string_to_name(mv_string("CommandBuffer"));
      add_def(module, name, type, &typep, null_segments, NULL);
      clear_assembler(ass);
      e = get_def_internal(name, module);
      command_buffer_ty = e->value;

      typep = mk_named_type(pia, "MemoryBarrier", mk_struct_type(pia, 0));
      type = (PiType) {.sort = TType};
      name = string_to_name(mv_string("MemoryBarrier"));
      add_def(module, name, type, &typep, null_segments, NULL);
      clear_assembler(ass);
      e = get_def_internal(name, module);
      memory_barrier_ty = e->value;

      typep = mk_named_type(pia, "BufferMemoryBarrier", mk_struct_type(pia, 0));
      type = (PiType) {.sort = TType};
      name = string_to_name(mv_string("BufferMemoryBarrier"));
      add_def(module, name, type, &typep, null_segments, NULL);
      clear_assembler(ass);
      e = get_def_internal(name, module);
      buffer_memory_barrier_ty = e->value;

      typep = mk_named_type(pia, "ImageMemoryBarrier",
      mk_struct_type(pia, 5,
      "old-layout", image_layout_ty,
      "new-layout", image_layout_ty,
      "source-access-mask", access_flag_ty,
      "destination-access-mask", access_flag_ty,
      "image", image_ty));
      type = (PiType) {.sort = TType};
      name = string_to_name(mv_string("ImageMemoryBarrier"));
      add_def(module, name, type, &typep, null_segments, NULL);
      clear_assembler(ass);
      e = get_def_internal(name, module);
      image_memory_barrier_ty = e->value;

      typep = mk_struct_type(pia, 3,
      "binding", mk_prim_type(pia, UInt_32),
      "stride", mk_prim_type(pia, UInt_32),
      "input-rate", input_rate_ty);
      type = (PiType) {.sort = TType};
      name = string_to_name(mv_string("BindingDescription"));
      add_def(module, name, type, &typep, null_segments, NULL);
      clear_assembler(ass);
      e = get_def_internal(name, module);
      binder_desc_ty = e->value;

      typep = mk_struct_type(pia, 3,
      "stage", shader_stage_ty,
      "offset", mk_prim_type(pia, UInt_32),
      "size", mk_prim_type(pia, UInt_32));
      type = (PiType) {.sort = TType};
      name = string_to_name(mv_string("PushConstantRange"));
      add_def(module, name, type, &typep, null_segments, NULL);
      clear_assembler(ass);
      e = get_def_internal(name, module);
      push_const_range_ty = e->value;

      typep = mk_opaque_type(pia, "Semaphore", module, mk_prim_type(pia, Address));
      type = (PiType) {.sort = TType};
      name = string_to_name(mv_string("Semaphore"));
      add_def(module, name, type, &typep, null_segments, NULL);
      clear_assembler(ass);
      e = get_def_internal(name, module);
      semaphore_ty = e->value;

      typep = mk_opaque_type(pia, "Fence", module, mk_prim_type(pia, Address));
      type = (PiType) {.sort = TType};
      name = string_to_name(mv_string("Fence"));
      add_def(module, name, type, &typep, null_segments, NULL);
      clear_assembler(ass);
      e = get_def_internal(name, module);
      fence_ty = e->value;


      // ------------------------------------------------------------------------
      //
      // Data contract (vertex/input formats, etc.)
      // 
      // ------------------------------------------------------------------------

      typep = mk_proc_type(pia, 3, mk_prim_type(pia, UInt_32), mk_prim_type(pia, UInt_32), image_format_ty, image_ty);
      build_create_image_fn(typep, ass, pia, &ra, &point);
      name = string_to_name(mv_string("create-image"));
      fn_segments.code = get_instructions(ass);
      prepped = prep_target(module, fn_segments, ass, NULL);
      add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
      clear_assembler(ass);

      typep = mk_proc_type(pia, 1, image_ty, mk_prim_type(pia, Unit));
      build_destroy_image_fn(typep, ass, pia, &ra, &point);
      name = string_to_name(mv_string("destroy-image"));
      fn_segments.code = get_instructions(ass);
      prepped = prep_target(module, fn_segments, ass, NULL);
      add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
      clear_assembler(ass);

      typep = mk_proc_type(pia, 2, image_ty, image_format_ty, image_view_ty);
      build_create_image_view_fn(typep, ass, pia, &ra, &point);
      name = string_to_name(mv_string("create-image-view"));
      fn_segments.code = get_instructions(ass);
      prepped = prep_target(module, fn_segments, ass, NULL);
      add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
      clear_assembler(ass);

      typep = mk_proc_type(pia, 1, image_view_ty, mk_prim_type(pia, Unit));
      build_destroy_image_view_fn(typep, ass, pia, &ra, &point);
      name = string_to_name(mv_string("destroy-image-view"));
      fn_segments.code = get_instructions(ass);
      prepped = prep_target(module, fn_segments, ass, NULL);
      add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
      clear_assembler(ass);


      typep = mk_proc_type(pia, 1, mk_type_app(pia, get_slice_type(), mk_prim_type(pia, UInt_8)), shader_module_ty);
      build_create_shader_module_fn(typep, ass, pia, &ra, &point);
      name = string_to_name(mv_string("create-shader-module"));
      fn_segments.code = get_instructions(ass);
      prepped = prep_target(module, fn_segments, ass, NULL);
      add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
      clear_assembler(ass);

      typep = mk_proc_type(pia, 1, shader_module_ty, mk_prim_type(pia, Unit));
      build_destroy_shader_module_fn(typep, ass, pia, &ra, &point);
      name = string_to_name(mv_string("destroy-shader-module"));
      fn_segments.code = get_instructions(ass);
      prepped = prep_target(module, fn_segments, ass, NULL);
      add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
      clear_assembler(ass);

      typep = mk_proc_type(pia, 0, command_pool_ty);
      build_create_command_pool_fn(typep, ass, pia, &ra, &point);
      name = string_to_name(mv_string("create-command-pool"));
      fn_segments.code = get_instructions(ass);
      prepped = prep_target(module, fn_segments, ass, NULL);
      add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
      clear_assembler(ass);

      typep = mk_proc_type(pia, 1, command_pool_ty, mk_prim_type(pia, Unit));
      build_destroy_command_pool_fn(typep, ass, pia, &ra, &point);
      name = string_to_name(mv_string("destroy-command-pool"));
      fn_segments.code = get_instructions(ass);
      prepped = prep_target(module, fn_segments, ass, NULL);
      add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
      clear_assembler(ass);

      typep = mk_proc_type(pia, 1, command_pool_ty, command_buffer_ty);
      build_create_command_buffer_fn(typep, ass, pia, &ra, &point);
      name = string_to_name(mv_string("create-command-buffer"));
      fn_segments.code = get_instructions(ass);
      prepped = prep_target(module, fn_segments, ass, NULL);
      add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
      clear_assembler(ass);

      typep = mk_proc_type(pia, 2, 
      command_pool_ty,
      command_buffer_ty,
      mk_prim_type(pia, Unit));
      build_free_command_buffer_fn(typep, ass, pia, &ra, &point);
      name = string_to_name(mv_string("free-command-buffer"));
      fn_segments.code = get_instructions(ass);
      prepped = prep_target(module, fn_segments, ass, NULL);
      add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
      clear_assembler(ass);

      typep = mk_proc_type(pia, 4, 
      command_buffer_ty,
      mk_type_app(pia, get_maybe_type(), fence_ty),
      mk_type_app(pia, get_slice_type(), mk_type_app(pia, get_pair_type(), semaphore_ty, pipeline_stage_ty)), 
      mk_type_app(pia, get_slice_type(), semaphore_ty),
      mk_prim_type(pia, Unit));
      build_queue_submit_fn(typep, ass, pia, &ra, &point);
      name = string_to_name(mv_string("queue-submit"));
      fn_segments.code = get_instructions(ass);
      prepped = prep_target(module, fn_segments, ass, NULL);
      add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
      clear_assembler(ass);

      typep = mk_proc_type(pia, 3, surface_ty,
      semaphore_ty, mk_prim_type(pia, UInt_32), mk_prim_type(pia, Unit));
      build_queue_present_fn(typep, ass, pia, &ra, &point);
      name = string_to_name(mv_string("queue-present"));
      fn_segments.code = get_instructions(ass);
      prepped = prep_target(module, fn_segments, ass, NULL);
      add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
      clear_assembler(ass);

      typep = mk_proc_type(pia, 0, mk_prim_type(pia, Unit));
      build_queue_wait_idle_fn(typep, ass, pia, &ra, &point);
      name = string_to_name(mv_string("queue-wait-idle"));
      fn_segments.code = get_instructions(ass);
      prepped = prep_target(module, fn_segments, ass, NULL);
      add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
      clear_assembler(ass);

      typep = mk_proc_type(pia, 2, command_buffer_ty, command_buffer_usage_ty, mk_prim_type(pia, Unit));
      build_command_begin_fn(typep, ass, pia, &ra, &point);
      name = string_to_name(mv_string("command-begin"));
      fn_segments.code = get_instructions(ass);
      prepped = prep_target(module, fn_segments, ass, NULL);
      add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
      clear_assembler(ass);

      typep = mk_proc_type(pia, 1, command_buffer_ty, mk_prim_type(pia, Unit));
      build_command_end_fn(typep, ass, pia, &ra, &point);
      name = string_to_name(mv_string("command-end"));
      fn_segments.code = get_instructions(ass);
      prepped = prep_target(module, fn_segments, ass, NULL);
      add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
      clear_assembler(ass);

      typep = mk_proc_type(pia, 1, command_buffer_ty, mk_prim_type(pia, Unit));
      build_reset_command_buffer_fn(typep, ass, pia, &ra, &point);
      name = string_to_name(mv_string("reset-command-buffer"));
      fn_segments.code = get_instructions(ass);
      prepped = prep_target(module, fn_segments, ass, NULL);
      add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
      clear_assembler(ass);

      typep = mk_proc_type(pia, 3, command_buffer_ty,
      surface_ty,
      mk_prim_type(pia, UInt_32),
      mk_prim_type(pia, Unit));
      build_command_begin_renderpass_fn(typep, ass, pia, &ra, &point);
      name = string_to_name(mv_string("command-begin-renderpass"));
      fn_segments.code = get_instructions(ass);
      prepped = prep_target(module, fn_segments, ass, NULL);
      add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
      clear_assembler(ass);

      typep = mk_proc_type(pia, 1, command_buffer_ty, mk_prim_type(pia, Unit));
      build_command_end_renderpass_fn(typep, ass, pia, &ra, &point);
      name = string_to_name(mv_string("command-end-renderpass"));
      fn_segments.code = get_instructions(ass);
      prepped = prep_target(module, fn_segments, ass, NULL);
      add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
      clear_assembler(ass);

      typep = mk_proc_type(pia, 6, 
      command_buffer_ty,
      pipeline_stage_ty,
      pipeline_stage_ty,
      mk_type_app(pia, get_slice_type(), memory_barrier_ty),
      mk_type_app(pia, get_slice_type(), buffer_memory_barrier_ty),
      mk_type_app(pia, get_slice_type(), image_memory_barrier_ty),
      mk_prim_type(pia, Unit));
      build_command_pipeline_barrier_fn(typep, ass, pia, &ra, &point);
      name = string_to_name(mv_string("command-pipeline-barrier"));
      fn_segments.code = get_instructions(ass);
      prepped = prep_target(module, fn_segments, ass, NULL);
      add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
      clear_assembler(ass);

      typep = mk_proc_type(pia, 5, 
      command_buffer_ty,
      buffer_ty,
      image_ty,
      mk_prim_type(pia, UInt_32),
      mk_prim_type(pia, UInt_32),
      mk_prim_type(pia, Unit));
      build_command_copy_buffer_to_image_fn(typep, ass, pia, &ra, &point);
      name = string_to_name(mv_string("command-copy-buffer-to-image"));
      fn_segments.code = get_instructions(ass);
      prepped = prep_target(module, fn_segments, ass, NULL);
      add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
      clear_assembler(ass);

      typep = mk_proc_type(pia, 2, command_buffer_ty, pipeline_ty, mk_prim_type(pia, Unit));
      build_command_bind_pipeline_fn(typep, ass, pia, &ra, &point);
      name = string_to_name(mv_string("command-bind-pipeline"));
      fn_segments.code = get_instructions(ass);
      prepped = prep_target(module, fn_segments, ass, NULL);
      add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
      clear_assembler(ass);

      typep = mk_proc_type(pia, 6, command_buffer_ty, pipeline_ty, shader_stage_ty,
      mk_prim_type(pia, UInt_32), mk_prim_type(pia, UInt_32),
      mk_prim_type(pia, Address), mk_prim_type(pia, Unit)); 
      build_command_push_constants_fn(typep, ass, pia, &ra, &point);
      name = string_to_name(mv_string("command-push-constants"));
      fn_segments.code = get_instructions(ass);
      prepped = prep_target(module, fn_segments, ass, NULL);
      add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
      clear_assembler(ass);

      typep = mk_proc_type(pia, 3, command_buffer_ty,
      pipeline_ty, 
      descriptor_set_ty, 
      mk_prim_type(pia, Unit));
      build_command_bind_descriptor_set_fn(typep, ass, pia, &ra, &point);
      name = string_to_name(mv_string("command-bind-descriptor-set"));
      fn_segments.code = get_instructions(ass);
      prepped = prep_target(module, fn_segments, ass, NULL);
      add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
      clear_assembler(ass);

      typep = mk_proc_type(pia, 2, command_buffer_ty, buffer_ty, mk_prim_type(pia, Unit));
      build_command_bind_vertex_buffer_fn(typep, ass, pia, &ra, &point);
      name = string_to_name(mv_string("command-bind-vertex-buffer"));
      fn_segments.code = get_instructions(ass);
      prepped = prep_target(module, fn_segments, ass, NULL);
      add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
      clear_assembler(ass);

      typep = mk_proc_type(pia, 2, command_buffer_ty, mk_type_app(pia, get_slice_type(), buffer_ty), mk_prim_type(pia, Unit));
      build_command_bind_vertex_buffers_fn(typep, ass, pia, &ra, &point);
      name = string_to_name(mv_string("command-bind-vertex-buffers"));
      fn_segments.code = get_instructions(ass);
      prepped = prep_target(module, fn_segments, ass, NULL);
      add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
      clear_assembler(ass);

      typep = mk_proc_type(pia, 3, command_buffer_ty, buffer_ty, index_format_ty, mk_prim_type(pia, Unit));
      build_command_bind_index_buffer_fn(typep, ass, pia, &ra, &point);
      name = string_to_name(mv_string("command-bind-index-buffer"));
      fn_segments.code = get_instructions(ass);
      prepped = prep_target(module, fn_segments, ass, NULL);
      add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
      clear_assembler(ass);

      typep = mk_proc_type(pia, 2, command_buffer_ty, surface_ty, mk_prim_type(pia, Unit));
      build_command_set_surface_fn(typep, ass, pia, &ra, &point);
      name = string_to_name(mv_string("command-set-surface"));
      fn_segments.code = get_instructions(ass);
      prepped = prep_target(module, fn_segments, ass, NULL);
      add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
      clear_assembler(ass);

      typep = mk_proc_type(pia, 5, command_buffer_ty,
      mk_prim_type(pia, UInt_32), mk_prim_type(pia, UInt_32),
      mk_prim_type(pia, UInt_32), mk_prim_type(pia, UInt_32),
      mk_prim_type(pia, Unit));
      build_command_draw_fn(typep, ass, pia, &ra, &point);
      name = string_to_name(mv_string("command-draw"));
      fn_segments.code = get_instructions(ass);
      prepped = prep_target(module, fn_segments, ass, NULL);
      add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
      clear_assembler(ass);

      typep = mk_proc_type(pia, 6, command_buffer_ty,
      mk_prim_type(pia, UInt_32), mk_prim_type(pia, UInt_32),
      mk_prim_type(pia, UInt_32), mk_prim_type(pia, Int_32),
      mk_prim_type(pia, UInt_32), mk_prim_type(pia, Unit));
      build_command_draw_indexed_fn(typep, ass, pia, &ra, &point);
      name = string_to_name(mv_string("command-draw-indexed"));
      fn_segments.code = get_instructions(ass);
      prepped = prep_target(module, fn_segments, ass, NULL);
      add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
      clear_assembler(ass);

      typep = mk_proc_type(pia, 0, semaphore_ty);
      build_create_semaphore_fn(typep, ass, pia, &ra, &point);
      name = string_to_name(mv_string("create-semaphore"));
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

      typep = mk_proc_type(pia, 0, fence_ty);
      build_create_fence_fn(typep, ass, pia, &ra, &point);
      name = string_to_name(mv_string("create-fence"));
      fn_segments.code = get_instructions(ass);
      prepped = prep_target(module, fn_segments, ass, NULL);
      add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
      clear_assembler(ass);

      typep = mk_proc_type(pia, 1, fence_ty, mk_prim_type(pia, Unit));
      build_destroy_fence_fn(typep, ass, pia, &ra, &point);
      name = string_to_name(mv_string("destroy-fence"));
      fn_segments.code = get_instructions(ass);
      prepped = prep_target(module, fn_segments, ass, NULL);
      add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
      clear_assembler(ass);

      typep = mk_proc_type(pia, 1, fence_ty, mk_prim_type(pia, Unit));
      build_wait_for_fence_fn(typep, ass, pia, &ra, &point);
      name = string_to_name(mv_string("wait-for-fence"));
      fn_segments.code = get_instructions(ass);
      prepped = prep_target(module, fn_segments, ass, NULL);
      add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
      clear_assembler(ass);

      typep = mk_proc_type(pia, 1, fence_ty, mk_prim_type(pia, Unit));
      build_reset_fence_fn(typep, ass, pia, &ra, &point);
      name = string_to_name(mv_string("reset-fence"));
      fn_segments.code = get_instructions(ass);
      prepped = prep_target(module, fn_segments, ass, NULL);
      add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
      clear_assembler(ass);

      typep = mk_proc_type(pia, 0, mk_prim_type(pia, Unit));
      build_wait_for_device_fn(typep, ass, pia, &ra, &point);
      name = string_to_name(mv_string("wait-for-device"));
      fn_segments.code = get_instructions(ass);
      prepped = prep_target(module, fn_segments, ass, NULL);
      add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
      clear_assembler(ass);

      typep = mk_proc_type(pia, 2, surface_ty, semaphore_ty,
      mk_enum_type(pia, 2,
      "image", 1, mk_prim_type(pia, UInt_32),
      "resized", 0));
      build_acquire_next_image_fn(typep, ass, pia, &ra, &point);
      name = string_to_name(mv_string("acquire-next-image"));
      fn_segments.code = get_instructions(ass);
      prepped = prep_target(module, fn_segments, ass, NULL);
      add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
      clear_assembler(ass);
    */
}

#endif
