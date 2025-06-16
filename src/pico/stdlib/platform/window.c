#include "platform/signals.h"
#include "platform/window/window.h"

#include "pico/values/ctypes.h"
#include "pico/codegen/foreign_adapters.h"
#include "pico/stdlib/platform/window.h"

static PiType* window_ty;

void build_create_window_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(a, 3, "name", mk_string_ctype(a),
                                       "width", mk_primint_ctype((CPrimInt){.prim = CInt, .is_signed = Unspecified}),
                                       "height", mk_primint_ctype((CPrimInt){.prim = CInt, .is_signed = Unspecified}),
                                    mk_voidptr_ctype(a));

    convert_c_fn(create_window, &fn_ctype, type, ass, a, point); 

    delete_c_type(fn_ctype, a);
}

void build_destroy_window_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(a, 1, mk_voidptr_ctype(a), (CType){.sort = CSVoid});

    convert_c_fn(destroy_window, &fn_ctype, type, ass, a, point); 

    delete_c_type(fn_ctype, a);
}

void build_window_should_close_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(a, 1, "window", mk_voidptr_ctype(a),
                                 mk_primint_ctype((CPrimInt){.prim = CChar, .is_signed = Unsigned}));

    convert_c_fn(window_should_close, &fn_ctype, type, ass, a, point);

    delete_c_type(fn_ctype, a);
}

void build_poll_events_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(a, 0, mk_primint_ctype((CPrimInt){.prim = CChar, .is_signed = Unsigned}));

    convert_c_fn(poll_events, &fn_ctype, type, ass, a, point);

    delete_c_type(fn_ctype, a);
}

void add_window_module(Assembler *ass, Module *platform, Allocator *a) {
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(0, a),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, a),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("window")),
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

    // The window type is simple an opaque pointer (address)
    typep = mk_opaque_type(a, module, mk_prim_type(a, Address));
    type = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("Window"));
    add_def(module, sym, type, &typep, null_segments, NULL);
    clear_assembler(ass);
    e = get_def(sym, module);
    window_ty = e->value;
    delete_pi_type_p(typep, a);

    typep = mk_proc_type(a, 3, mk_string_type(a), mk_prim_type(a, Int_32), mk_prim_type(a, Int_32), copy_pi_type_p(window_ty, a));
    build_create_window_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("create-window"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    typep = mk_proc_type(a, 1, copy_pi_type_p(window_ty, a), mk_prim_type(a, Unit));
    build_destroy_window_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("destroy-window"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    typep = mk_proc_type(a, 1, copy_pi_type_p(window_ty, a), mk_prim_type(a, Bool));
    build_window_should_close_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("should-close"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    typep = mk_proc_type(a, 0, mk_prim_type(a, Bool));
    build_poll_events_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("poll-events"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    sdelete_u8_array(fn_segments.data);
    sdelete_u8_array(null_segments.data);
    sdelete_u8_array(null_segments.code);

    Result r = add_module_def(platform, string_to_symbol(mv_string("window")), module);
    if (r.type == Err) panic(r.error_message);
}
