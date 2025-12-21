#ifdef USE_VULKAN

#include "platform/signals.h"
#include "platform/machine_info.h"
#include "platform/hedron/hedron.h"

#include "components/pretty/string_printer.h"

#include "pico/values/ctypes.h"
#include "pico/codegen/codegen.h"
#include "pico/codegen/backend-direct/internal.h"

#include "pico/stdlib/core.h"
#include "pico/stdlib/platform/submodules.h"

static PiType* surface_ty;
static PiType* shader_module_ty;
static PiType* pipeline_ty;

static PiType* index_format_ty;
static PiType* input_rate_ty;
static PiType* input_format_ty;
static PiType* binder_desc_ty;
static PiType* attribute_desc_ty;
static PiType* stage_sort_ty;

static PiType* descriptor_binding_ty;

static PiType* descriptor_type_ty;
static PiType* shader_type_ty;
static PiType* descriptor_pool_size_ty;
static PiType* descriptor_pool_ty;

static PiType* descriptor_set_ty;
static PiType* descriptor_set_layout_ty;
static PiType* descriptor_buffer_info_ty;
static PiType* descriptor_write_ty;
static PiType* descriptor_copy_ty;

static PiType* buffer_ty;
static PiType* buffer_sort_ty;

static PiType* command_pool_ty;
static PiType* command_buffer_ty;

static PiType* semaphore_ty;
static PiType* fence_ty;

// ----------------------------------------------------------------------------
//
//   Context: Instances, Devices, Windows
// 
// ----------------------------------------------------------------------------

#ifdef WINDOW_SYSTEM
void build_create_window_surface_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "window", mk_voidptr_ctype(pia), mk_voidptr_ctype(pia));
    convert_c_fn(create_window_surface, &fn_ctype, type, ass, a, point); 
}

void build_resize_window_surface_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType extent_type = mk_struct_ctype(pia, 2, "width", mk_primint_ctype((CPrimInt){.prim = CInt, .is_signed = Unsigned}),
                                        "height", mk_primint_ctype((CPrimInt){.prim = CInt, .is_signed = Unsigned}));
    CType fn_ctype = mk_fn_ctype(pia, 2, "window", mk_voidptr_ctype(pia), "extent", extent_type, (CType){.sort = CSVoid});
    convert_c_fn(resize_window_surface, &fn_ctype, type, ass, a, point); 
}

void build_destroy_window_surface_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "surface", mk_voidptr_ctype(pia), (CType){.sort = CSVoid});
    convert_c_fn(destroy_window_surface, &fn_ctype, type, ass, a, point); 
}
#endif

void build_num_swapchain_images_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "surface", mk_voidptr_ctype(pia),
                                 mk_primint_ctype((CPrimInt){.prim = CInt, .is_signed = Unsigned}));
    convert_c_fn(num_swapchain_images, &fn_ctype, type, ass, a, point); 
}

void build_create_shader_module_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "code", mk_list_ctype(pia), mk_voidptr_ctype(pia));
    convert_c_fn(create_shader_module, &fn_ctype, type, ass, a, point); 
}

void build_destroy_shader_module_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "module", mk_voidptr_ctype(pia), (CType){.sort = CSVoid});
    convert_c_fn(destroy_shader_module, &fn_ctype, type, ass, a, point); 
}

void build_create_pipeline_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 5,
                                 "resource_describe", mk_list_ctype(pia),
                                 "binder_describe", mk_list_ctype(pia),
                                 "attrib_describe", mk_list_ctype(pia),
                                 "shaders", mk_list_ctype(pia),
                                 "surface", mk_voidptr_ctype(pia),
                                 mk_voidptr_ctype(pia));
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

void build_create_buffer_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(
                                 pia, 2, "type", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                                 "size", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                                 mk_voidptr_ctype(pia));
    convert_c_fn(create_buffer, &fn_ctype, type, ass, a, point); 
}

void build_destroy_buffer_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "size", mk_voidptr_ctype(pia), (CType){.sort = CSVoid});
    convert_c_fn(destroy_buffer, &fn_ctype, type, ass, a, point); 
}

PiType* build_buffer_set_fn_ty(PiAllocator* pia) {
    PiType* proc_ty  = mk_proc_type(pia, 2, copy_pi_type_p(buffer_ty, pia), mk_var_type(pia, "A"), mk_prim_type(pia, Unit));

    SymbolPiList types = mk_sym_list(1, pia);
    push_sym(string_to_symbol(mv_string("A")), &types);

    PiType* out_ty = call_alloc(sizeof(PiType), pia);
    *out_ty = (PiType) {.sort = TAll, .binder.vars = types, .binder.body = proc_ty};
    return out_ty;
}

void build_buffer_set_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    // The usual calling convention for polymorphic functions is assumed, hence
    // stack has the form:
    // RSP-24  | type size
    // RSP-16  | store address
    // RSP-8   | variable stack index (value/ptr)
    // RSP     | return address 

    build_unary_op(Pop, reg(RAX, sz_64), ass, a, point);

#if ABI == SYSTEM_V_64
    // memcpy (dest = rdi, src = rsi, size = rdx)
    build_unary_op(Pop, reg(RSI, sz_64), ass, a, point);
    build_unary_op(Pop, reg(RDI, sz_64), ass, a, point);

    build_binary_op(Mov, reg(RDX, sz_64), rref8(RSP, 0, sz_64), ass, a, point);
    build_binary_op(SHR, reg(RDX, sz_64), imm8(28), ass, a, point);
    build_binary_op(And, reg(RDX, sz_64), imm32(0xFFFFFFF), ass, a, point);

#elif ABI == WIN_64
    // memcpy (dest = rcx, src = rdx, size = r8)
    build_unary_op(Pop, reg(RDX, sz_64), ass, a, point);
    build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);

    build_binary_op(Mov, reg(R8, sz_64), rref8(RSP, 0, sz_64), ass, a, point);
    build_binary_op(SHR, reg(R8, sz_64), imm8(28), ass, a, point);
    build_binary_op(And, reg(R8, sz_64), imm32(0xFFFFFFF), ass, a, point);
#else
#error "Unknown calling convention"
#endif
    // Push return address
    build_unary_op(Push, reg(RAX, sz_64), ass, a, point);

    // copy memcpy into RCX & call
    generate_c_call(set_buffer_data, ass, a, point);

    build_unary_op(Pop, reg(RAX, sz_64), ass, a, point);

    // Stack size of & return
    build_unary_op(Pop, reg(R9, sz_64), ass, a, point);
    build_binary_op(And, reg(R9, sz_64), imm32(0xFFFFFFF), ass, a, point);
    build_binary_op(Add, reg(R14, sz_64), reg(R9, sz_64), ass, a, point);

    build_unary_op(Push, reg(RAX, sz_64), ass, a, point);

    build_nullary_op(Ret, ass, a, point);
}

void build_set_buffer_data_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 2, "buffer", mk_voidptr_ctype(pia), "data", mk_voidptr_ctype(pia), (CType){.sort = CSVoid});
    convert_c_fn(set_buffer_data, &fn_ctype, type, ass, a, point); 
}

// Descriptor Sets
// ----------------------------------

void build_create_descriptor_set_layout(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "binding_descriptions", mk_list_ctype(pia), mk_voidptr_ctype(pia));
    convert_c_fn(create_descriptor_set_layout, &fn_ctype, type, ass, a, point); 
}

void build_destroy_descriptor_set_layout(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "layout", mk_voidptr_ctype(pia), (CType){.sort = CSVoid});
    convert_c_fn(destroy_descriptor_set_layout, &fn_ctype, type, ass, a, point); 
}

void build_create_descriptor_pool(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 2, "sizes", mk_list_ctype(pia), "max-sets", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt}), mk_voidptr_ctype(pia));
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
                                 mk_list_ctype(pia));
    convert_c_fn(alloc_descriptor_sets, &fn_ctype, type, ass, a, point); 
}

void build_update_descriptor_sets(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 2,
                                 "writes", mk_list_ctype(pia),
                                 "copies", mk_list_ctype(pia),
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

void build_queue_submit_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 4, "buffer", mk_voidptr_ctype(pia),
                                 "fence", mk_voidptr_ctype(pia),
                                 "wait", mk_voidptr_ctype(pia),
                                 "signal", mk_voidptr_ctype(pia),
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

void build_command_begin_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "buffer", mk_voidptr_ctype(pia), (CType){.sort = CSVoid});
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

void build_command_bind_pipeline_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 2, "command_buffer", mk_voidptr_ctype(pia),
                                 "pipeline", mk_voidptr_ctype(pia),
                                 (CType){.sort = CSVoid});
    convert_c_fn(command_bind_pipeline, &fn_ctype, type, ass, a, point); 
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

void add_hedron_module(Assembler *ass, Module *platform, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);
    PiAllocator pico_region = convert_to_pallocator(&ra);
    PiAllocator* pia = &pico_region;
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(0, &ra),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, &ra),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("hedron")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(platform), NULL);
    Symbol sym;

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

    typep = mk_opaque_type(pia, module, mk_named_type(pia, "Surface",mk_prim_type(pia, Address)));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("Surface"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    surface_ty = e->value;

    typep = mk_opaque_type(pia, module, mk_named_type(pia, "ShaderModule",mk_prim_type(pia, Address)));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("ShaderModule"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    shader_module_ty = e->value;

    typep = mk_opaque_type(pia, module, mk_named_type(pia, "Pipeline",mk_prim_type(pia, Address)));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("Pipeline"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    pipeline_ty = e->value;

    typep = mk_enum_type(pia, 2, "vertex", 0, "instance", 0);
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("InputRate"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    input_rate_ty = e->value;

    typep = mk_opaque_type(pia, module, mk_named_type(pia, "Buffer", mk_prim_type(pia, Address)));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("Buffer"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    buffer_ty = e->value;

    typep = mk_opaque_type(pia, module, mk_named_type(pia, "CommandPool", mk_prim_type(pia, Address)));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("CommandPool"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    command_pool_ty = e->value;

    typep = mk_opaque_type(pia, module, mk_named_type(pia, "CommandBuffer", mk_prim_type(pia, Address)));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("CommandBuffer"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    command_buffer_ty = e->value;

    typep = mk_struct_type(pia, 3,
                           "binding", mk_prim_type(pia, UInt_32),
                           "stride", mk_prim_type(pia, UInt_32),
                           "input-rate", copy_pi_type_p(input_rate_ty, pia));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("BindingDescription"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    binder_desc_ty = e->value;

    typep = mk_enum_type(pia, 1, "vertex-shader", 0);
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("ShaderType"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    shader_type_ty = e->value;

    typep = mk_enum_type(pia, 1, "uniform-buffer", 0);
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("DecriptorType"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    descriptor_type_ty = e->value;

    typep = mk_struct_type(pia, 2,
                           "type", descriptor_type_ty,
                           "descriptor-count", mk_prim_type(pia, UInt_32));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("DescriptorPoolSize"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    descriptor_pool_size_ty = e->value;

    typep = mk_opaque_type(pia, module, mk_named_type(pia, "DescriptorPool",mk_prim_type(pia, Address)));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("DescriptorPool"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    descriptor_pool_ty = e->value;

    typep = mk_opaque_type(pia, module, mk_named_type(pia, "DescriptorSetLayout",mk_prim_type(pia, Address)));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("DescriptorSetLayout"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    descriptor_set_layout_ty = e->value;
    
    typep = mk_opaque_type(pia, module, mk_named_type(pia, "DescriptorSet",mk_prim_type(pia, Address)));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("DescriptorSet"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    descriptor_set_ty = e->value;

    typep = mk_struct_type(pia, 3,
                           "buffer", buffer_ty,
                           "offset", mk_prim_type(pia, UInt_32),
                           "range", mk_prim_type(pia, UInt_32));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("DescriptorBufferInfo"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    descriptor_buffer_info_ty = e->value;

    typep = mk_struct_type(pia, 3,
                           "buffer-info", descriptor_buffer_info_ty,
                           "descriptor-type", descriptor_type_ty,
                           "descriptor-set", descriptor_set_ty);
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("DescriptorWrite"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    descriptor_write_ty = e->value;

    typep = mk_struct_type(pia, 1,
                           "buffer-info", descriptor_buffer_info_ty);
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("DescriptorCopy"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    descriptor_copy_ty = e->value;

    typep = mk_enum_type(pia, 2,
                         "u16", 0,
                         "u32", 0);
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("IndexFormat"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    index_format_ty = e->value;

    typep = mk_enum_type(pia, 3,
                         "float-1", 0,
                         "float-2", 0,
                         "float-3", 0);
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("InputFormat"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    input_format_ty = e->value;

    typep = mk_enum_type(pia, 1, "vertex-shader", 0);
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("StageType"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    stage_sort_ty = e->value;

    typep = mk_struct_type(pia, 2,
                           "type", copy_pi_type_p(descriptor_type_ty, pia),
                           "stage-type", copy_pi_type_p(stage_sort_ty, pia));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("DescriptorBinding"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    descriptor_binding_ty = e->value;

    typep = mk_struct_type(pia, 4,
                           "binding", mk_prim_type(pia, UInt_32),
                           "location", mk_prim_type(pia, UInt_32),
                           "format", copy_pi_type_p(input_format_ty, pia),
                           "offset", mk_prim_type(pia, UInt_32));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("AttributeDescription"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    attribute_desc_ty = e->value;

    typep = mk_enum_type(pia, 3, "vertex", 0, "index", 0, "uniform", 0);
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("BufferSort"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    buffer_sort_ty = e->value;


    typep = mk_opaque_type(pia, module, mk_named_type(pia, "Semaphore", mk_prim_type(pia, Address)));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("Semaphore"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    semaphore_ty = e->value;

    typep = mk_opaque_type(pia, module, mk_named_type(pia, "Fence", mk_prim_type(pia, Address)));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("Fence"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    fence_ty = e->value;

#ifdef WINDOW_SYSTEM
    typep = mk_proc_type(pia, 1, copy_pi_type_p(get_window_ty(), pia), copy_pi_type_p(surface_ty, pia));
    build_create_window_surface_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("create-window-surface"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 2, copy_pi_type_p(surface_ty, pia), 
                         mk_app_type(pia, get_pair_type(), mk_prim_type(pia, UInt_32), mk_prim_type(pia, UInt_32)),
                         mk_prim_type(pia, Unit));
    build_resize_window_surface_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("resize-window-surface"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, copy_pi_type_p(surface_ty, pia), mk_prim_type(pia, Unit));
    build_destroy_window_surface_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("destroy-window-surface"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
#endif

    typep = mk_proc_type(pia, 1, copy_pi_type_p(surface_ty, pia), mk_prim_type(pia, UInt_32));
    build_num_swapchain_images_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("num-swapchain-images"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    // ------------------------------------------------------------------------
    //
    // Data contract (vertex/input formats, etc.)
    // 
    // ------------------------------------------------------------------------


    typep = mk_proc_type(pia, 2, copy_pi_type_p(buffer_sort_ty, pia), mk_prim_type(pia, UInt_64), copy_pi_type_p(buffer_ty, pia));
    build_create_buffer_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("create-buffer"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, copy_pi_type_p(buffer_ty, pia), mk_prim_type(pia, Unit));
    build_destroy_buffer_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("destroy-buffer"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = build_buffer_set_fn_ty(pia);
    build_buffer_set_fn(ass, &ra, &point);
    sym = string_to_symbol(mv_string("set-buffer-val"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 2, copy_pi_type_p(buffer_ty, pia), mk_prim_type(pia, Address), mk_prim_type(pia, Unit));
    build_set_buffer_data_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("set-buffer-data"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    // Descriptor Sets
    // ----------------------------------
    typep = mk_proc_type(pia, 1, mk_app_type(pia, get_list_type(), descriptor_binding_ty), descriptor_set_layout_ty);
    build_create_descriptor_set_layout(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("create-descriptor-set-layout"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, descriptor_set_layout_ty, mk_prim_type(pia, Unit));
    build_destroy_descriptor_set_layout(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("destroy-descriptor-set-layout"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 2, mk_app_type(pia, get_list_type(), descriptor_pool_size_ty), mk_prim_type(pia, UInt_32), descriptor_pool_ty);
    build_create_descriptor_pool(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("create-descriptor-pool"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, descriptor_pool_ty, mk_prim_type(pia, Unit));
    build_destroy_descriptor_pool(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("destroy-descriptor-pool"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 3,
                         mk_prim_type(pia, UInt_32),
                         descriptor_set_layout_ty,
                         descriptor_pool_ty,
                         mk_app_type(pia, get_list_type(), descriptor_set_ty));
    build_alloc_descriptor_sets(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("alloc-descriptor-sets"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 2,
                         mk_app_type(pia, get_list_type(), descriptor_write_ty),
                         mk_app_type(pia, get_list_type(), descriptor_copy_ty),
                         mk_prim_type(pia, Unit));
    build_update_descriptor_sets(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("update-descriptor-sets"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, mk_app_type(pia, get_list_type(), mk_prim_type(pia, UInt_8)), copy_pi_type_p(shader_module_ty, pia));
    build_create_shader_module_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("create-shader-module"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, copy_pi_type_p(shader_module_ty, pia), mk_prim_type(pia, Unit));
    build_destroy_shader_module_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("destroy-shader-module"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 5,
                         mk_app_type(pia, get_list_type(), descriptor_set_layout_ty),
                         mk_app_type(pia, get_list_type(), binder_desc_ty),
                         mk_app_type(pia, get_list_type(), attribute_desc_ty),
                         mk_app_type(pia, get_list_type(), shader_module_ty),
                         copy_pi_type_p(surface_ty, pia),
                         copy_pi_type_p(pipeline_ty, pia));
    build_create_pipeline_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("create-pipeline"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, copy_pi_type_p(pipeline_ty, pia), mk_prim_type(pia, Unit));
    build_destroy_pipeline_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("destroy-pipeline"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 0, copy_pi_type_p(command_pool_ty, pia));
    build_create_command_pool_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("create-command-pool"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, copy_pi_type_p(command_pool_ty, pia), mk_prim_type(pia, Unit));
    build_destroy_command_pool_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("destroy-command-pool"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, copy_pi_type_p(command_pool_ty, pia), copy_pi_type_p(command_buffer_ty, pia));
    build_create_command_buffer_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("create-command-buffer"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 4, copy_pi_type_p(command_buffer_ty, pia), copy_pi_type_p(fence_ty, pia),
                         copy_pi_type_p(semaphore_ty, pia), copy_pi_type_p(semaphore_ty, pia), mk_prim_type(pia, Unit));
    build_queue_submit_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("queue-submit"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 3, copy_pi_type_p(surface_ty, pia),
                         copy_pi_type_p(semaphore_ty, pia), mk_prim_type(pia, UInt_32), mk_prim_type(pia, Unit));
    build_queue_present_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("queue-present"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, copy_pi_type_p(command_buffer_ty, pia), mk_prim_type(pia, Unit));
    build_command_begin_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("command-begin"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, copy_pi_type_p(command_buffer_ty, pia), mk_prim_type(pia, Unit));
    build_command_end_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("command-end"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, copy_pi_type_p(command_buffer_ty, pia), mk_prim_type(pia, Unit));
    build_reset_command_buffer_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("reset-command-buffer"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 3, copy_pi_type_p(command_buffer_ty, pia),
                         copy_pi_type_p(surface_ty, pia),
                         mk_prim_type(pia, UInt_32),
                         mk_prim_type(pia, Unit));
    build_command_begin_renderpass_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("command-begin-renderpass"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, command_buffer_ty, mk_prim_type(pia, Unit));
    build_command_end_renderpass_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("command-end-renderpass"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 2, command_buffer_ty, pipeline_ty, mk_prim_type(pia, Unit));
    build_command_bind_pipeline_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("command-bind-pipeline"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 3, command_buffer_ty,
                         pipeline_ty, 
                         descriptor_set_ty, 
                         mk_prim_type(pia, Unit));
    build_command_bind_descriptor_set_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("command-bind-descriptor-set"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 2, copy_pi_type_p(command_buffer_ty, pia), copy_pi_type_p(buffer_ty, pia), mk_prim_type(pia, Unit));
    build_command_bind_vertex_buffer_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("command-bind-vertex-buffer"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 3, copy_pi_type_p(command_buffer_ty, pia), copy_pi_type_p(buffer_ty, pia), copy_pi_type_p(index_format_ty, pia), mk_prim_type(pia, Unit));
    build_command_bind_index_buffer_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("command-bind-index-buffer"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 2, copy_pi_type_p(command_buffer_ty, pia), copy_pi_type_p(surface_ty, pia),mk_prim_type(pia, Unit));
    build_command_set_surface_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("command-set-surface"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 5, copy_pi_type_p(command_buffer_ty, pia),
                         mk_prim_type(pia, UInt_32), mk_prim_type(pia, UInt_32),
                         mk_prim_type(pia, UInt_32), mk_prim_type(pia, UInt_32),
                         mk_prim_type(pia, Unit));
    build_command_draw_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("command-draw"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 6, copy_pi_type_p(command_buffer_ty, pia),
                         mk_prim_type(pia, UInt_32), mk_prim_type(pia, UInt_32),
                         mk_prim_type(pia, UInt_32), mk_prim_type(pia, Int_32),
                         mk_prim_type(pia, UInt_32), mk_prim_type(pia, Unit));
    build_command_draw_indexed_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("command-draw-indexed"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 0, copy_pi_type_p(semaphore_ty, pia));
    build_create_semaphore_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("create-semaphore"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, copy_pi_type_p(semaphore_ty, pia), mk_prim_type(pia, Unit));
    build_destroy_semaphore_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("destroy-semaphore"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 0, copy_pi_type_p(fence_ty, pia));
    build_create_fence_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("create-fence"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, copy_pi_type_p(fence_ty, pia), mk_prim_type(pia, Unit));
    build_destroy_fence_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("destroy-fence"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, copy_pi_type_p(fence_ty, pia), mk_prim_type(pia, Unit));
    build_wait_for_fence_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("wait-for-fence"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, copy_pi_type_p(fence_ty, pia), mk_prim_type(pia, Unit));
    build_reset_fence_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("reset-fence"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 0, mk_prim_type(pia, Unit));
    build_wait_for_device_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("wait-for-device"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 2, copy_pi_type_p(surface_ty, pia), copy_pi_type_p(semaphore_ty, pia),
                         mk_enum_type(pia, 2,
                                      "image", 1, mk_prim_type(pia, UInt_32),
                                      "resized", 0));
    build_acquire_next_image_fn(typep, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("acquire-next-image"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    Result r = add_module_def(platform, string_to_symbol(mv_string("hedron")), module);
    if (r.type == Err) panic(r.error_message);
}

#endif
