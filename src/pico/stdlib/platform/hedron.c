#include "platform/signals.h"
#include "platform/hedron/hedron.h"

#include "pico/values/ctypes.h"
#include "pico/codegen/codegen.h"
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

static PiType* buffer_ty;
static PiType* buffer_sort_ty;

static PiType* command_pool_ty;
static PiType* command_buffer_ty;

static PiType* semaphore_ty;
static PiType* fence_ty;


void build_create_window_surface_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "window", mk_voidptr_ctype(pia), mk_voidptr_ctype(pia));
    convert_c_fn(create_window_surface, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, pia);
}

void build_resize_window_surface_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType extent_type = mk_struct_ctype(pia, 2, "width", mk_primint_ctype((CPrimInt){.prim = CInt, .is_signed = Unsigned}),
                                        "height", mk_primint_ctype((CPrimInt){.prim = CInt, .is_signed = Unsigned}));
    CType fn_ctype = mk_fn_ctype(pia, 2, "window", mk_voidptr_ctype(pia), "extent", extent_type, (CType){.sort = CSVoid});
    convert_c_fn(resize_window_surface, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, pia);
}

void build_destroy_window_surface_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "surface", mk_voidptr_ctype(pia), (CType){.sort = CSVoid});
    convert_c_fn(destroy_window_surface, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, pia);
}

void build_num_swapchain_images_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "surface", mk_voidptr_ctype(pia),
                                 mk_primint_ctype((CPrimInt){.prim = CInt, .is_signed = Unsigned}));
    convert_c_fn(num_swapchain_images, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, pia);
}

void build_create_shader_module_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "code", mk_list_ctype(pia), mk_voidptr_ctype(pia));
    convert_c_fn(create_shader_module, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, pia);
}

void build_destroy_shader_module_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "module", mk_voidptr_ctype(pia), (CType){.sort = CSVoid});
    convert_c_fn(destroy_shader_module, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, pia);
}

void build_create_pipeline_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 4,
                                 "binder_describe", mk_list_ctype(pia),
                                 "attrib_describe", mk_list_ctype(pia),
                                 "shaders", mk_list_ctype(pia),
                                 "surface", mk_voidptr_ctype(pia),
                                 mk_voidptr_ctype(pia));
    convert_c_fn(create_pipeline, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, pia);
}

void build_destroy_pipeline_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "pipeline", mk_voidptr_ctype(pia), (CType){.sort = CSVoid});
    convert_c_fn(destroy_pipeline, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, pia);
}

// ------------------------------------------------
//
//    Data contract (vertex/input formats, etc.)
// 
// ------------------------------------------------

void build_create_buffer_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(
                                 pia, 2, "type", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                                 "size", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                                 mk_voidptr_ctype(pia));
    convert_c_fn(create_buffer, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, pia);
}

void build_destroy_buffer_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "size", mk_voidptr_ctype(pia), (CType){.sort = CSVoid});
    convert_c_fn(destroy_buffer, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, pia);
}

void build_set_buffer_data_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 2, "buffer", mk_voidptr_ctype(pia), "data", mk_voidptr_ctype(pia), (CType){.sort = CSVoid});
    convert_c_fn(set_buffer_data, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, pia);
}

// -----------------------------------------------------------------------------
//
//                                      Commands
//
// -----------------------------------------------------------------------------

void build_create_command_pool_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 0, mk_voidptr_ctype(pia));
    convert_c_fn(create_command_pool, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, pia);
}

void build_destroy_command_pool_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "pool", mk_voidptr_ctype(pia), (CType){.sort = CSVoid});
    convert_c_fn(destroy_command_pool, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, pia);
}

void build_create_command_buffer_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "pool", mk_voidptr_ctype(pia), mk_voidptr_ctype(pia));
    convert_c_fn(create_command_buffer, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, pia);
}

void build_queue_submit_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 4, "buffer", mk_voidptr_ctype(pia),
                                 "fence", mk_voidptr_ctype(pia),
                                 "wait", mk_voidptr_ctype(pia),
                                 "signal", mk_voidptr_ctype(pia),
                                 (CType){.sort = CSVoid});
    convert_c_fn(queue_submit, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, pia);
}

void build_queue_present_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 3, "surface", mk_voidptr_ctype(pia),
                                 "wait", mk_voidptr_ctype(pia),
                                 "index", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt}),
                                 (CType){.sort = CSVoid});
    convert_c_fn(queue_present, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, pia);
}

void build_command_begin_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "buffer", mk_voidptr_ctype(pia), (CType){.sort = CSVoid});
    convert_c_fn(command_begin, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, pia);
}

void build_command_end_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "buffer", mk_voidptr_ctype(pia), (CType){.sort = CSVoid});
    convert_c_fn(command_end, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, pia);
}

void build_reset_command_buffer_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "buffer", mk_voidptr_ctype(pia), (CType){.sort = CSVoid});
    convert_c_fn(reset_command_buffer, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, pia);
}

void build_command_begin_renderpass_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 3, "buffer", mk_voidptr_ctype(pia),
                                 "surface", mk_voidptr_ctype(pia),
                                 "image_index", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt}), 
                                 (CType){.sort = CSVoid});
    convert_c_fn(command_begin_render_pass, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, pia);
}

void build_command_end_renderpass_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "buffer", mk_voidptr_ctype(pia), (CType){.sort = CSVoid});
    convert_c_fn(command_end_render_pass, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, pia);
}

void build_command_bind_pipeline_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 2, "command_buffer", mk_voidptr_ctype(pia),
                                 "pipeline", mk_voidptr_ctype(pia),
                                 (CType){.sort = CSVoid});
    convert_c_fn(command_bind_pipeline, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, pia);
}

void build_command_bind_vertex_buffer_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 2, "command_buffer", mk_voidptr_ctype(pia),
                                 "buffer", mk_voidptr_ctype(pia),
                                 (CType){.sort = CSVoid});
    convert_c_fn(command_bind_vertex_buffer, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, pia);
}

void build_command_bind_index_buffer_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 3, "command_buffer", mk_voidptr_ctype(pia),
                                 "buffer", mk_voidptr_ctype(pia),
                                 "datatype", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                                 (CType){.sort = CSVoid});
    convert_c_fn(command_bind_index_buffer, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, pia);
}

void build_command_set_surface_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 2, "buffer", mk_voidptr_ctype(pia),
                                 "surface", mk_voidptr_ctype(pia),
                                 (CType){.sort = CSVoid});
    convert_c_fn(command_set_surface, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, pia);
}

void build_command_draw_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 5, "buffer", mk_voidptr_ctype(pia),
                                 "vertex-count", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt}),
                                 "instance-count", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt}),
                                 "first-vertex", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt}),
                                 "first-instance", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt}),
                                 (CType){.sort = CSVoid});
    convert_c_fn(command_draw, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, pia);
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
    delete_c_type(fn_ctype, pia);
}

// -----------------------------------------------------------------------------
//
//                                Syncrhonisation
//
// -----------------------------------------------------------------------------

void build_create_semaphore_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 0, mk_voidptr_ctype(pia));
    convert_c_fn(create_semaphore, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, pia);
}

void build_destroy_semaphore_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "semaphore", mk_voidptr_ctype(pia), (CType){.sort = CSVoid});
    convert_c_fn(destroy_semaphore, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, pia);
}
void build_create_fence_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 0, mk_voidptr_ctype(pia));
    convert_c_fn(create_fence, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, pia);
}

void build_destroy_fence_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "fence", mk_voidptr_ctype(pia), (CType){.sort = CSVoid});
    convert_c_fn(destroy_fence, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, pia);
}

void build_wait_for_fence_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "fence", mk_voidptr_ctype(pia),
                                 (CType){.sort = CSVoid});
    convert_c_fn(wait_for_fence, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, pia);
}

void build_reset_fence_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "fence", mk_voidptr_ctype(pia),
                                 (CType){.sort = CSVoid});
    convert_c_fn(reset_fence, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, pia);
}

void build_wait_for_device_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 0, (CType){.sort = CSVoid});
    convert_c_fn(wait_for_device, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, pia);
}

void build_acquire_next_image_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType ret_type = mk_struct_ctype(pia, 2, "type", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CLongLong}),
                                     "image", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt}));
    
    CType fn_ctype = mk_fn_ctype(pia, 2, "surface", mk_voidptr_ctype(pia), "semaphore", mk_voidptr_ctype(pia), ret_type);
    convert_c_fn(acquire_next_image, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, pia);
}

void add_hedron_module(Assembler *ass, Module *platform, Allocator *a) {
    PiAllocator pico_allocator = convert_to_pallocator(a);
    PiAllocator* pia = &pico_allocator;
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(0, a),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, a),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("hedron")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(platform), NULL, a);
    delete_module_header(header);
    Symbol sym;

    ModuleEntry* e;
    PiType type;
    PiType* typep;
    ErrorPoint point;
    if (catch_error(point)) {
        panic(point.error_message);
    }

    Segments prepped;
    Segments fn_segments = {.data = mk_u8_array(0, a),};
    Segments null_segments = (Segments) {
        .code = mk_u8_array(0, a),
        .data = mk_u8_array(0, a),
    };

    typep = mk_opaque_type(pia, module, mk_prim_type(pia, Address));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("Surface"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    surface_ty = e->value;
    delete_pi_type_p(typep, pia);

    typep = mk_opaque_type(pia, module, mk_prim_type(pia, Address));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("ShaderModule"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    shader_module_ty = e->value;
    delete_pi_type_p(typep, pia);

    typep = mk_opaque_type(pia, module, mk_prim_type(pia, Address));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("Pipeline"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    pipeline_ty = e->value;
    delete_pi_type_p(typep, pia);

    typep = mk_enum_type(pia, 2,
                         "vertex", 0,
                         "instance", 0);
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("InputRate"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    input_rate_ty = e->value;
    delete_pi_type_p(typep, pia);

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
    delete_pi_type_p(typep, pia);

    typep = mk_enum_type(pia, 2,
                         "u16", 0,
                         "u32", 0);
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("IndexFormat"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    index_format_ty = e->value;
    delete_pi_type_p(typep, pia);

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
    delete_pi_type_p(typep, pia);

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
    delete_pi_type_p(typep, pia);

    typep = mk_enum_type(pia, 2, "vertex", 0, "index", 0);
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("BufferSort"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    buffer_sort_ty = e->value;
    delete_pi_type_p(typep, pia);

    typep = mk_opaque_type(pia, module, mk_prim_type(pia, Address));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("Buffer"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    buffer_ty = e->value;
    delete_pi_type_p(typep, pia);

    typep = mk_opaque_type(pia, module, mk_prim_type(pia, Address));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("CommandPool"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    command_pool_ty = e->value;
    delete_pi_type_p(typep, pia);

    typep = mk_opaque_type(pia, module, mk_prim_type(pia, Address));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("CommandBuffer"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    command_buffer_ty = e->value;
    delete_pi_type_p(typep, pia);

    typep = mk_opaque_type(pia, module, mk_prim_type(pia, Address));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("Semaphore"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    semaphore_ty = e->value;
    delete_pi_type_p(typep, pia);

    typep = mk_opaque_type(pia, module, mk_prim_type(pia, Address));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("Fence"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    fence_ty = e->value;
    delete_pi_type_p(typep, pia);

    typep = mk_proc_type(pia, 1, copy_pi_type_p(get_window_ty(), pia), copy_pi_type_p(surface_ty, pia));
    build_create_window_surface_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("create-window-surface"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    PiType* prim = mk_prim_type(pia, UInt_32);
    typep = mk_proc_type(pia, 2, copy_pi_type_p(surface_ty, pia), 
                         mk_app_type(pia, get_pair_type(), prim, prim),
                         mk_prim_type(pia, Unit));
    build_resize_window_surface_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("resize-window-surface"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);
    delete_pi_type_p(prim, pia);

    typep = mk_proc_type(pia, 1, copy_pi_type_p(surface_ty, pia), mk_prim_type(pia, Unit));
    build_destroy_window_surface_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("destroy-window-surface"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    typep = mk_proc_type(pia, 1, copy_pi_type_p(surface_ty, pia), mk_prim_type(pia, UInt_32));
    build_num_swapchain_images_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("num-swapchain-images"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    prim = mk_prim_type(pia, UInt_8);
    typep = mk_proc_type(pia, 1, mk_app_type(pia, get_list_type(), prim), copy_pi_type_p(shader_module_ty, pia));
    build_create_shader_module_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("create-shader-module"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);
    delete_pi_type_p(prim, pia);

    typep = mk_proc_type(pia, 1, copy_pi_type_p(shader_module_ty, pia), mk_prim_type(pia, Unit));
    build_destroy_shader_module_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("destroy-shader-module"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    typep = mk_proc_type(pia, 4,
                         mk_app_type(pia, get_list_type(), binder_desc_ty),
                         mk_app_type(pia, get_list_type(), attribute_desc_ty),
                         mk_app_type(pia, get_list_type(), shader_module_ty),
                         copy_pi_type_p(surface_ty, pia),
                         copy_pi_type_p(pipeline_ty, pia));
    build_create_pipeline_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("create-pipeline"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    typep = mk_proc_type(pia, 1, copy_pi_type_p(pipeline_ty, pia), mk_prim_type(pia, Unit));
    build_destroy_pipeline_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("destroy-pipeline"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    typep = mk_proc_type(pia, 2, copy_pi_type_p(buffer_sort_ty, pia), mk_prim_type(pia, UInt_64), copy_pi_type_p(buffer_ty, pia));
    build_create_buffer_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("create-buffer"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    typep = mk_proc_type(pia, 1, copy_pi_type_p(buffer_ty, pia), mk_prim_type(pia, Unit));
    build_destroy_buffer_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("destroy-buffer"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    typep = mk_proc_type(pia, 2, copy_pi_type_p(buffer_ty, pia), mk_prim_type(pia, Address), mk_prim_type(pia, Unit));
    build_set_buffer_data_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("set-buffer-data"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    typep = mk_proc_type(pia, 0, copy_pi_type_p(command_pool_ty, pia));
    build_create_command_pool_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("create-command-pool"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    typep = mk_proc_type(pia, 1, copy_pi_type_p(command_pool_ty, pia), mk_prim_type(pia, Unit));
    build_destroy_command_pool_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("destroy-command-pool"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    typep = mk_proc_type(pia, 1, copy_pi_type_p(command_pool_ty, pia), copy_pi_type_p(command_buffer_ty, pia));
    build_create_command_buffer_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("create-command-buffer"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    typep = mk_proc_type(pia, 4, copy_pi_type_p(command_buffer_ty, pia), copy_pi_type_p(fence_ty, pia),
                         copy_pi_type_p(semaphore_ty, pia), copy_pi_type_p(semaphore_ty, pia), mk_prim_type(pia, Unit));
    build_queue_submit_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("queue-submit"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    typep = mk_proc_type(pia, 3, copy_pi_type_p(surface_ty, pia),
                         copy_pi_type_p(semaphore_ty, pia), mk_prim_type(pia, UInt_32), mk_prim_type(pia, Unit));
    build_queue_present_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("queue-present"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    typep = mk_proc_type(pia, 1, copy_pi_type_p(command_buffer_ty, pia), mk_prim_type(pia, Unit));
    build_command_begin_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("command-begin"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    typep = mk_proc_type(pia, 1, copy_pi_type_p(command_buffer_ty, pia), mk_prim_type(pia, Unit));
    build_command_end_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("command-end"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    typep = mk_proc_type(pia, 1, copy_pi_type_p(command_buffer_ty, pia), mk_prim_type(pia, Unit));
    build_reset_command_buffer_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("reset-command-buffer"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    typep = mk_proc_type(pia, 3, copy_pi_type_p(command_buffer_ty, pia),
                         copy_pi_type_p(surface_ty, pia),
                         mk_prim_type(pia, UInt_32),
                         mk_prim_type(pia, Unit));
    build_command_begin_renderpass_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("command-begin-renderpass"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    typep = mk_proc_type(pia, 1, copy_pi_type_p(command_buffer_ty, pia), mk_prim_type(pia, Unit));
    build_command_end_renderpass_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("command-end-renderpass"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    typep = mk_proc_type(pia, 2, copy_pi_type_p(command_buffer_ty, pia), copy_pi_type_p(pipeline_ty, pia),mk_prim_type(pia, Unit));
    build_command_bind_pipeline_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("command-bind-pipeline"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    typep = mk_proc_type(pia, 2, copy_pi_type_p(command_buffer_ty, pia), copy_pi_type_p(buffer_ty, pia), mk_prim_type(pia, Unit));
    build_command_bind_vertex_buffer_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("command-bind-vertex-buffer"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    typep = mk_proc_type(pia, 3, copy_pi_type_p(command_buffer_ty, pia), copy_pi_type_p(buffer_ty, pia), copy_pi_type_p(index_format_ty, pia), mk_prim_type(pia, Unit));
    build_command_bind_index_buffer_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("command-bind-index-buffer"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    typep = mk_proc_type(pia, 2, copy_pi_type_p(command_buffer_ty, pia), copy_pi_type_p(surface_ty, pia),mk_prim_type(pia, Unit));
    build_command_set_surface_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("command-set-surface"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    typep = mk_proc_type(pia, 5, copy_pi_type_p(command_buffer_ty, pia),
                         mk_prim_type(pia, UInt_32), mk_prim_type(pia, UInt_32),
                         mk_prim_type(pia, UInt_32), mk_prim_type(pia, UInt_32),
                         mk_prim_type(pia, Unit));
    build_command_draw_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("command-draw"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    typep = mk_proc_type(pia, 6, copy_pi_type_p(command_buffer_ty, pia),
                         mk_prim_type(pia, UInt_32), mk_prim_type(pia, UInt_32),
                         mk_prim_type(pia, UInt_32), mk_prim_type(pia, Int_32),
                         mk_prim_type(pia, UInt_32), mk_prim_type(pia, Unit));
    build_command_draw_indexed_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("command-draw-indexed"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    typep = mk_proc_type(pia, 0, copy_pi_type_p(semaphore_ty, pia));
    build_create_semaphore_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("create-semaphore"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    typep = mk_proc_type(pia, 1, copy_pi_type_p(semaphore_ty, pia), mk_prim_type(pia, Unit));
    build_destroy_semaphore_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("destroy-semaphore"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    typep = mk_proc_type(pia, 0, copy_pi_type_p(fence_ty, pia));
    build_create_fence_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("create-fence"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    typep = mk_proc_type(pia, 1, copy_pi_type_p(fence_ty, pia), mk_prim_type(pia, Unit));
    build_destroy_fence_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("destroy-fence"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    typep = mk_proc_type(pia, 1, copy_pi_type_p(fence_ty, pia), mk_prim_type(pia, Unit));
    build_wait_for_fence_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("wait-for-fence"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    typep = mk_proc_type(pia, 1, copy_pi_type_p(fence_ty, pia), mk_prim_type(pia, Unit));
    build_reset_fence_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("reset-fence"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    typep = mk_proc_type(pia, 0, mk_prim_type(pia, Unit));
    build_wait_for_device_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("wait-for-device"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    typep = mk_proc_type(pia, 2, copy_pi_type_p(surface_ty, pia), copy_pi_type_p(semaphore_ty, pia),
                         mk_enum_type(pia, 2,
                                      "image", 1, mk_prim_type(pia, UInt_32),
                                      "resized", 0));
    build_acquire_next_image_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("acquire-next-image"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    sdelete_u8_array(fn_segments.data);
    sdelete_u8_array(null_segments.data);
    sdelete_u8_array(null_segments.code);

    Result r = add_module_def(platform, string_to_symbol(mv_string("hedron")), module);
    if (r.type == Err) panic(r.error_message);
}
