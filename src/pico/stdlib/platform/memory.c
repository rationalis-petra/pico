#include "platform/signals.h"
#include "platform/memory/platform.h"

#include "pico/data/client/allocator.h"
#include "pico/values/ctypes.h"
#include "pico/codegen/codegen.h"
#include "pico/stdlib/platform/submodules.h"

void build_platform_alloc_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType memblk = mk_struct_ctype(pia, 2,
                                   "data", mk_voidptr_ctype(pia),
                                   "size", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CLongLong}));
    CType fn_ctype = mk_fn_ctype(pia, 2,
                                 "min_size", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CLongLong}),
                                 "flags", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CInt}),
                                 memblk);

    convert_c_fn(platform_allocate, &fn_ctype, type, ass, a, point); 

    delete_c_type(fn_ctype, pia);
}

void build_platform_free_fn(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType memblk = mk_struct_ctype(pia, 2,
                                   "data", mk_voidptr_ctype(pia),
                                   "size", mk_primint_ctype((CPrimInt){.is_signed = Unsigned, .prim = CLongLong}));
    CType fn_ctype = mk_fn_ctype(pia, 1, "block", memblk, (CType){.sort = CSVoid});

    convert_c_fn(platform_free, &fn_ctype, type, ass, a, point); 

    delete_c_type(fn_ctype, pia);
}

void add_memory_module(Assembler *ass, Module *platform, Allocator *a) {
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
        .name = string_to_symbol(mv_string("memory")),
        .imports = imports,
        .exports = exports,
    };
    PiAllocator pico_module_allocator = convert_to_pallocator(a);
    Module* module = mk_module(header, get_package(platform), NULL, pico_module_allocator);
    delete_module_header(header);
    Symbol sym;

    PiType kind;
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

    
    typep = mk_struct_type(pia, 2, "data", mk_prim_type(pia, Address), "size", mk_prim_type(pia, UInt_64));
    kind = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("MemBlock"));
    add_def(module, sym, kind, &typep, null_segments, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    uint32_t flag_val = 1; // ARead
    typep = mk_prim_type(pia, UInt_32);
    kind = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("read"));
    add_def(module, sym, *typep, &flag_val, null_segments, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    flag_val = 2; // AWrite
    typep = mk_prim_type(pia, UInt_32);
    kind = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("write"));
    add_def(module, sym, *typep, &flag_val, null_segments, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    flag_val = 4; // AExecute
    typep = mk_prim_type(pia, UInt_32);
    kind = (PiType) {.sort = TKind, .kind.nargs = 0};
    sym = string_to_symbol(mv_string("execute"));
    add_def(module, sym, *typep, &flag_val, null_segments, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    PiType* memblk = mk_struct_type(pia, 2, "data", mk_prim_type(pia, Address), "size", mk_prim_type(pia, UInt_64));
    typep = mk_proc_type(pia, 2, mk_prim_type(pia, UInt_64), mk_prim_type(pia, UInt_32), memblk);
    build_platform_alloc_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("allocate"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    memblk = mk_struct_type(pia, 2, "data", mk_prim_type(pia, Address), "size", mk_prim_type(pia, UInt_64));
    typep = mk_proc_type(pia, 1, memblk, mk_prim_type(pia, Unit));
    build_platform_free_fn(typep, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("free"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, pia);

    Result r = add_module_def(platform, string_to_symbol(mv_string("memory")), module);
    if (r.type == Err) panic(r.error_message);
    sdelete_u8_array(fn_segments.data);
    sdelete_u8_array(null_segments.data);
    sdelete_u8_array(null_segments.code);
}
