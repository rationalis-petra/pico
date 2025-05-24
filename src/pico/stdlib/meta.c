#include <string.h>

#include "platform/signals.h"
#include "platform/machine_info.h"
#include "platform/memory/arena.h"

#include "pico/codegen/foreign_adapters.h"
#include "pico/stdlib/core.h"

void build_mk_name_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    // Proc type
    CType string_ctype = mk_struct_ctype(a, 2,
                                         "memsize", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                                         "bytes", mk_voidptr_ctype(a));

    CType name_ctype = mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned});

    CType fn_ctype = mk_fn_ctype(a, 1, "name", string_ctype, name_ctype);

    convert_c_fn(string_to_name, &fn_ctype, type, ass, a, point); 
}

void build_mk_symbol_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    // Proc type
    CType string_ctype = mk_struct_ctype(a, 2,
                                         "memsize", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                                         "bytes", mk_voidptr_ctype(a));

    CType symbol_ctype = mk_struct_ctype(a, 2,
                                         "name", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                                         "did", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}));

    CType fn_ctype = mk_fn_ctype(a, 1, "symbol", string_ctype, symbol_ctype);

    convert_c_fn(string_to_symbol, &fn_ctype, type, ass, a, point); 
}

void build_mk_unique_symbol_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    // Proc type
    CType string_ctype = mk_struct_ctype(a, 2,
                                         "memsize", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                                         "bytes", mk_voidptr_ctype(a));

    CType symbol_ctype = mk_struct_ctype(a, 2,
                                         "name", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                                         "did", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}));

    CType fn_ctype = mk_fn_ctype(a, 1, "symbol", string_ctype, symbol_ctype);

    convert_c_fn(string_to_unique_symbol, &fn_ctype, type, ass, a, point); 
}

void add_meta_module(Assembler* ass, Package* base, Allocator* a) {
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(0, a),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, a),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("meta")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, base, NULL, a);
    delete_module_header(header);
    Symbol sym;

    PiType type;
    PiType* typep;
    PiType type_val;
    PiType* type_data = &type_val;
    ErrorPoint point;
    if (catch_error(point)) {
        panic(point.error_message);
    }

    // TODO: we use int64_t as it has the requisite size (8 bytes)
    // for pico values: currently don't support non-64 bit values 
    TermFormer former;
    // TermFormer former;
    type.sort = TPrim;
    type.prim = TFormer;

    Segments null_segments = (Segments) {
        .code = mk_u8_array(0, a),
        .data = mk_u8_array(0, a),
    };

    // Now that we have setup appropriately, override the allocator
    Allocator arena = mk_arena_allocator(4096, a);
    a = &arena;

    // ------------------------------------------------------------------------
    // Term Formers
    // ------------------------------------------------------------------------
    former = FTypeOf;
    sym = string_to_symbol(mv_string("type-of"));
    add_def(module, sym, type, &former, null_segments, NULL);

    /* former = FDefine; */
    /* sym = string_to_symbol(mv_string("declare")); */
    /* add_def(module, sym, type, &former, null_segments, NULL); */

    // ------------------------------------------------------------------------
    // Types 
    // ------------------------------------------------------------------------

    type = (PiType) {
        .sort = TKind,
        .kind.nargs = 0,
    };

    Segments fn_segments = (Segments) {.data = mk_u8_array(0, a),};
    Segments prepped;

    typep = mk_proc_type(a, 1, mk_string_type(a), mk_prim_type(a, UInt_64));
    build_mk_name_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("mk-name"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(a, 1, mk_string_type(a), copy_pi_type_p(get_symbol_type(), a));
    build_mk_symbol_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("mk-symbol"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(a, 1, mk_string_type(a), copy_pi_type_p(get_symbol_type(), a));
    build_mk_unique_symbol_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("mk-unique-symbol"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    add_module(string_to_symbol(mv_string("meta")), module, base);

    sdelete_u8_array(null_segments.code);
    sdelete_u8_array(null_segments.data);
    // Note: we do NOT delete the 'fn_segments.code' because it is the
    // assembler, and needs to be used later!
    sdelete_u8_array(fn_segments.data);
    release_arena_allocator(arena);
}

