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

void build_create_window_surface_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(a, 1, "window", mk_voidptr_ctype(a), mk_voidptr_ctype(a));

    convert_c_fn(create_window_surface, &fn_ctype, type, ass, a, point); 

    delete_c_type(fn_ctype, a);
}

void build_destroy_window_surface_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(a, 1, "surface", mk_voidptr_ctype(a), (CType){.sort = CSVoid});

    convert_c_fn(destroy_window_surface, &fn_ctype, type, ass, a, point); 

    delete_c_type(fn_ctype, a);
}

void build_create_shader_module_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(a, 1, "code", mk_array_ctype(a), mk_voidptr_ctype(a));

    convert_c_fn(create_shader_module, &fn_ctype, type, ass, a, point); 

    delete_c_type(fn_ctype, a);
}

void build_destroy_shader_module_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(a, 1, "module", mk_voidptr_ctype(a), (CType){.sort = CSVoid});

    convert_c_fn(destroy_shader_module, &fn_ctype, type, ass, a, point); 

    delete_c_type(fn_ctype, a);
}

void build_create_pipeline_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
  CType fn_ctype = mk_fn_ctype(a, 2, "shaders", mk_array_ctype(a),
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

    typep = mk_proc_type(a, 1, copy_pi_type_p(get_window_ty(), a), copy_pi_type_p(surface_ty, a));
    build_create_window_surface_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("create-window-surface"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    typep = mk_proc_type(a, 1, copy_pi_type_p(surface_ty, a), mk_prim_type(a, Unit));
    build_destroy_window_surface_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("destroy-window-surface"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    PiType* prim = mk_prim_type(a, UInt_8);
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

    sdelete_u8_array(fn_segments.data);
    sdelete_u8_array(null_segments.data);
    sdelete_u8_array(null_segments.code);

    Result r = add_module_def(platform, string_to_symbol(mv_string("hedron")), module);
    if (r.type == Err) panic(r.error_message);
}
