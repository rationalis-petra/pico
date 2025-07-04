#include "platform/signals.h"
#include "platform/hedron/hedron.h"

#include "pico/values/ctypes.h"
#include "pico/codegen/foreign_adapters.h"
#include "pico/stdlib/core.h"
#include "pico/stdlib/platform/hedron.h"
#include "pico/stdlib/platform/window.h"

static PiType* surface_ty;
static PiType* shader_module_ty;
static PiType* pipeline_ty;

static PiType* command_pool_ty;
static PiType* command_buffer_ty;

static PiType* semaphore_ty;
static PiType* fence_ty;


void build_create_window_surface_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(a, 1, "window", mk_voidptr_ctype(a), mk_voidptr_ctype(a));
    convert_c_fn(create_window_surface, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, a);
}

void build_resize_window_surface_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType extent_type = mk_struct_ctype(a, 2, "width", mk_primint_ctype((CPrimInt){.prim = CInt, .is_signed = Unsigned}),
                                              "height", mk_primint_ctype((CPrimInt){.prim = CInt, .is_signed = Unsigned}));
    CType fn_ctype = mk_fn_ctype(a, 2, "window", mk_voidptr_ctype(a), "extent", extent_type, (CType){.sort = CSVoid});
    convert_c_fn(resize_window_surface, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, a);
}

void build_destroy_window_surface_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(a, 1, "surface", mk_voidptr_ctype(a), (CType){.sort = CSVoid});
    convert_c_fn(destroy_window_surface, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, a);
}

void build_num_swapchain_images_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(a, 1, "surface", mk_voidptr_ctype(a),
                               mk_primint_ctype((CPrimInt){.prim = CInt, .is_signed = Unsigned}));
    convert_c_fn(num_swapchain_images, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, a);
}

void build_create_shader_module_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(a, 1, "code", mk_list_ctype(a), mk_voidptr_ctype(a));
    convert_c_fn(create_shader_module, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, a);
}

void build_destroy_shader_module_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(a, 1, "module", mk_voidptr_ctype(a), (CType){.sort = CSVoid});
    convert_c_fn(destroy_shader_module, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, a);
}

void build_create_pipeline_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(a, 2, "shaders", mk_list_ctype(a),
                               "surface", mk_voidptr_ctype(a),
                               mk_voidptr_ctype(a));
    convert_c_fn(create_pipeline, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, a);
}

void build_destroy_pipeline_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(a, 1, "pipeline", mk_voidptr_ctype(a), (CType){.sort = CSVoid});
    convert_c_fn(destroy_pipeline, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, a);
}

// -----------------------------------------------------------------------------
//
//                                      Commands
//
// -----------------------------------------------------------------------------

void build_create_command_pool_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(a, 0, mk_voidptr_ctype(a));
    convert_c_fn(create_command_pool, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, a);
}

void build_destroy_command_pool_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(a, 1, "pool", mk_voidptr_ctype(a), (CType){.sort = CSVoid});
    convert_c_fn(destroy_command_pool, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, a);
}

void build_create_command_buffer_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(a, 1, "pool", mk_voidptr_ctype(a), mk_voidptr_ctype(a));
    convert_c_fn(create_command_buffer, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, a);
}

void build_queue_submit_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(a, 4, "buffer", mk_voidptr_ctype(a),
                                     "fence", mk_voidptr_ctype(a),
                                     "wait", mk_voidptr_ctype(a),
                                     "signal", mk_voidptr_ctype(a),
                               (CType){.sort = CSVoid});
    convert_c_fn(queue_submit, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, a);
}

void build_queue_present_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(a, 3, "surface", mk_voidptr_ctype(a),
                                 "wait", mk_voidptr_ctype(a),
                                 "index", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt}),
                                 (CType){.sort = CSVoid});
    convert_c_fn(queue_present, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, a);
}

void build_command_begin_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(a, 1, "buffer", mk_voidptr_ctype(a), (CType){.sort = CSVoid});
    convert_c_fn(command_begin, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, a);
}

void build_command_end_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(a, 1, "buffer", mk_voidptr_ctype(a), (CType){.sort = CSVoid});
    convert_c_fn(command_end, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, a);
}

void build_reset_command_buffer_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(a, 1, "buffer", mk_voidptr_ctype(a), (CType){.sort = CSVoid});
    convert_c_fn(reset_command_buffer, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, a);
}

void build_command_begin_renderpass_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(a, 3, "buffer", mk_voidptr_ctype(a),
                               "surface", mk_voidptr_ctype(a),
                               "image_index", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt}), 
                               (CType){.sort = CSVoid});
    convert_c_fn(command_begin_render_pass, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, a);
}

void build_command_end_renderpass_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(a, 1, "buffer", mk_voidptr_ctype(a), (CType){.sort = CSVoid});
    convert_c_fn(command_end_render_pass, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, a);
}

void build_command_bind_pipeline_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(a, 2, "buffer", mk_voidptr_ctype(a),
                               "pipeline", mk_voidptr_ctype(a),
                               (CType){.sort = CSVoid});
    convert_c_fn(command_bind_pipeline, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, a);
}

void build_command_set_surface_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(a, 2, "buffer", mk_voidptr_ctype(a),
                               "surface", mk_voidptr_ctype(a),
                               (CType){.sort = CSVoid});
    convert_c_fn(command_set_surface, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, a);
}

void build_command_draw_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(a, 5, "buffer", mk_voidptr_ctype(a),
                               "nvertices", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt}),
                               "ninstances", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt}),
                               "vertex0", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt}),
                               "instance0", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt}),
                               (CType){.sort = CSVoid});
    convert_c_fn(command_draw, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, a);
}

// -----------------------------------------------------------------------------
//
//                                Syncrhonisation
//
// -----------------------------------------------------------------------------

void build_create_semaphore_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(a, 0, mk_voidptr_ctype(a));
    convert_c_fn(create_semaphore, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, a);
}

void build_destroy_semaphore_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(a, 1, "semaphore", mk_voidptr_ctype(a), (CType){.sort = CSVoid});
    convert_c_fn(destroy_semaphore, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, a);
}
void build_create_fence_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(a, 0, mk_voidptr_ctype(a));
    convert_c_fn(create_fence, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, a);
}

void build_destroy_fence_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(a, 1, "fence", mk_voidptr_ctype(a), (CType){.sort = CSVoid});
    convert_c_fn(destroy_fence, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, a);
}

void build_wait_for_fence_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(a, 1, "fence", mk_voidptr_ctype(a),
                               (CType){.sort = CSVoid});
    convert_c_fn(wait_for_fence, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, a);
}

void build_reset_fence_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(a, 1, "fence", mk_voidptr_ctype(a),
                               (CType){.sort = CSVoid});
    convert_c_fn(reset_fence, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, a);
}

void build_wait_for_device_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(a, 0, (CType){.sort = CSVoid});
    convert_c_fn(wait_for_device, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, a);
}

void build_acquire_next_image_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType ret_type = mk_struct_ctype(a, 2, "type", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CLongLong}),
                                     "image", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt}));
    
    CType fn_ctype = mk_fn_ctype(a, 2, "surface", mk_voidptr_ctype(a), "semaphore", mk_voidptr_ctype(a), ret_type);
    convert_c_fn(acquire_next_image, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, a);
}

void add_hedron_module(Assembler *ass, Module *platform, Allocator *a) {
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

    typep = mk_opaque_type(a, module, mk_prim_type(a, Address));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("Surface"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    surface_ty = e->value;
    delete_pi_type_p(typep, a);

    typep = mk_opaque_type(a, module, mk_prim_type(a, Address));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("ShaderModule"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    shader_module_ty = e->value;
    delete_pi_type_p(typep, a);

    typep = mk_opaque_type(a, module, mk_prim_type(a, Address));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("Pipeline"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    pipeline_ty = e->value;
    delete_pi_type_p(typep, a);

    typep = mk_opaque_type(a, module, mk_prim_type(a, Address));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("CommandPool"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    command_pool_ty = e->value;
    delete_pi_type_p(typep, a);

    typep = mk_opaque_type(a, module, mk_prim_type(a, Address));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("CommandBuffer"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    command_buffer_ty = e->value;
    delete_pi_type_p(typep, a);

    typep = mk_opaque_type(a, module, mk_prim_type(a, Address));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("Semaphore"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    semaphore_ty = e->value;
    delete_pi_type_p(typep, a);

    typep = mk_opaque_type(a, module, mk_prim_type(a, Address));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("Fence"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    fence_ty = e->value;
    delete_pi_type_p(typep, a);

    typep = mk_proc_type(a, 1, copy_pi_type_p(get_window_ty(), a), copy_pi_type_p(surface_ty, a));
    build_create_window_surface_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("create-window-surface"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    PiType* prim = mk_prim_type(a, UInt_32);
    typep = mk_proc_type(a, 2, copy_pi_type_p(surface_ty, a), 
                         mk_app_type(a, get_pair_type(), prim, prim),
                         mk_prim_type(a, Unit));
    build_resize_window_surface_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("resize-window-surface"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);
    delete_pi_type_p(prim, a);

    typep = mk_proc_type(a, 1, copy_pi_type_p(surface_ty, a), mk_prim_type(a, Unit));
    build_destroy_window_surface_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("destroy-window-surface"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    typep = mk_proc_type(a, 1, copy_pi_type_p(surface_ty, a), mk_prim_type(a, UInt_32));
    build_num_swapchain_images_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("num-swapchain-images"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    prim = mk_prim_type(a, UInt_8);
    typep = mk_proc_type(a, 1, mk_app_type(a, get_list_type(), prim), copy_pi_type_p(shader_module_ty, a));
    build_create_shader_module_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("create-shader-module"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);
    delete_pi_type_p(prim, a);

    typep = mk_proc_type(a, 1, copy_pi_type_p(shader_module_ty, a), mk_prim_type(a, Unit));
    build_destroy_shader_module_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("destroy-shader-module"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    typep = mk_proc_type(a, 2, mk_app_type(a, get_list_type(), shader_module_ty),
                         copy_pi_type_p(surface_ty, a),
                         copy_pi_type_p(pipeline_ty, a));
    build_create_pipeline_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("create-pipeline"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    typep = mk_proc_type(a, 1, copy_pi_type_p(pipeline_ty, a), mk_prim_type(a, Unit));
    build_destroy_pipeline_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("destroy-pipeline"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    typep = mk_proc_type(a, 0, copy_pi_type_p(command_pool_ty, a));
    build_create_command_pool_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("create-command-pool"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    typep = mk_proc_type(a, 1, copy_pi_type_p(command_pool_ty, a), mk_prim_type(a, Unit));
    build_destroy_command_pool_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("destroy-command-pool"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    typep = mk_proc_type(a, 1, copy_pi_type_p(command_pool_ty, a), copy_pi_type_p(command_buffer_ty, a));
    build_create_command_buffer_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("create-command-buffer"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    typep = mk_proc_type(a, 4, copy_pi_type_p(command_buffer_ty, a), copy_pi_type_p(fence_ty, a),
                         copy_pi_type_p(semaphore_ty, a), copy_pi_type_p(semaphore_ty, a), mk_prim_type(a, Unit));
    build_queue_submit_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("queue-submit"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    typep = mk_proc_type(a, 3, copy_pi_type_p(surface_ty, a),
                         copy_pi_type_p(semaphore_ty, a), mk_prim_type(a, UInt_32), mk_prim_type(a, Unit));
    build_queue_present_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("queue-present"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    typep = mk_proc_type(a, 1, copy_pi_type_p(command_buffer_ty, a), mk_prim_type(a, Unit));
    build_command_begin_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("command-begin"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    typep = mk_proc_type(a, 1, copy_pi_type_p(command_buffer_ty, a), mk_prim_type(a, Unit));
    build_command_end_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("command-end"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    typep = mk_proc_type(a, 1, copy_pi_type_p(command_buffer_ty, a), mk_prim_type(a, Unit));
    build_reset_command_buffer_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("reset-command-buffer"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    typep = mk_proc_type(a, 3, copy_pi_type_p(command_buffer_ty, a),
                         copy_pi_type_p(surface_ty, a),
                         mk_prim_type(a, UInt_32),
                         mk_prim_type(a, Unit));
    build_command_begin_renderpass_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("command-begin-renderpass"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    typep = mk_proc_type(a, 1, copy_pi_type_p(command_buffer_ty, a), mk_prim_type(a, Unit));
    build_command_end_renderpass_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("command-end-renderpass"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    typep = mk_proc_type(a, 2, copy_pi_type_p(command_buffer_ty, a), copy_pi_type_p(pipeline_ty, a),mk_prim_type(a, Unit));
    build_command_bind_pipeline_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("command-bind-pipeline"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    typep = mk_proc_type(a, 2, copy_pi_type_p(command_buffer_ty, a), copy_pi_type_p(surface_ty, a),mk_prim_type(a, Unit));
    build_command_set_surface_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("command-set-surface"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    typep = mk_proc_type(a, 5, copy_pi_type_p(command_buffer_ty, a),
                         mk_prim_type(a, UInt_32), mk_prim_type(a, UInt_32),
                         mk_prim_type(a, UInt_32), mk_prim_type(a, UInt_32),
                         mk_prim_type(a, Unit));
    build_command_draw_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("command-draw"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    typep = mk_proc_type(a, 0, copy_pi_type_p(semaphore_ty, a));
    build_create_semaphore_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("create-semaphore"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    typep = mk_proc_type(a, 1, copy_pi_type_p(semaphore_ty, a), mk_prim_type(a, Unit));
    build_destroy_semaphore_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("destroy-semaphore"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    typep = mk_proc_type(a, 0, copy_pi_type_p(fence_ty, a));
    build_create_fence_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("create-fence"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    typep = mk_proc_type(a, 1, copy_pi_type_p(fence_ty, a), mk_prim_type(a, Unit));
    build_destroy_fence_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("destroy-fence"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    typep = mk_proc_type(a, 1, copy_pi_type_p(fence_ty, a), mk_prim_type(a, Unit));
    build_wait_for_fence_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("wait-for-fence"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    typep = mk_proc_type(a, 1, copy_pi_type_p(fence_ty, a), mk_prim_type(a, Unit));
    build_reset_fence_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("reset-fence"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    typep = mk_proc_type(a, 0, mk_prim_type(a, Unit));
    build_wait_for_device_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("wait-for-device"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    typep = mk_proc_type(a, 2, copy_pi_type_p(surface_ty, a), copy_pi_type_p(semaphore_ty, a),
                         mk_enum_type(a, 2,
                                      "image", 1, mk_prim_type(a, UInt_32),
                                      "resized", 0));
    build_acquire_next_image_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("acquire-next-image"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    sdelete_u8_array(fn_segments.data);
    sdelete_u8_array(null_segments.data);
    sdelete_u8_array(null_segments.code);

    Result r = add_module_def(platform, string_to_symbol(mv_string("hedron")), module);
    if (r.type == Err) panic(r.error_message);
}
