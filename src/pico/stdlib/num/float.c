#include <inttypes.h>
#include "platform/signals.h"
#include "data/stringify.h"
#include "data/float.h"

#include "components/pretty/string_printer.h"

#include "pico/stdlib/platform/submodules.h"
#include "pico/stdlib/num/submodules.h"
#include "pico/codegen/codegen.h"

static PiType* mk_binop_type(PiAllocator* pia, PrimType a1, PrimType a2, PrimType r) {
    return mk_proc_type(pia, 2, mk_prim_type(pia, a1), mk_prim_type(pia, a2), mk_prim_type(pia, r));
}

static PiType* mk_unop_type(PiAllocator* pia, PrimType arg, PrimType r) {
    return mk_proc_type(pia, 1, mk_prim_type(pia, arg), mk_prim_type(pia, r));
}

void build_binary_float_fn(Assembler* ass, BinaryOp op, LocationSize sz, Allocator* a, ErrorPoint* point) {
    BinaryOp mov_op = sz == sz_32 ? MovSS : MovSD;
    build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);
    build_binary_op(mov_op, reg(XMM1, sz), rref8(RSP, 0, sz), ass, a, point);
    build_binary_op(mov_op, reg(XMM0, sz), rref8(RSP, 8, sz), ass, a, point);
    build_binary_op(op, reg(XMM0, sz), reg(XMM1, sz), ass, a, point);
    build_binary_op(Add, reg(RSP, sz_64), imm8(8), ass, a, point);
    build_binary_op(mov_op, rref8(RSP, 0, sz), reg(XMM0, sz), ass, a, point);
    build_unary_op(Push, reg(RCX, sz_64), ass, a, point);
    build_nullary_op(Ret, ass, a, point);
}

void build_unary_float_fn(PiType* type, LocationSize sz, void* cfn, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType arg_type = sz == sz_64 ? (CType){.sort = CSDouble} : (CType){.sort = CSFloat};
    CType fn_ctype = mk_fn_ctype(pia, 1, "x", arg_type, arg_type);
    convert_c_fn(cfn, &fn_ctype, type, ass, a, point); 
}


String relic_f32_to_string(float32_t f32) {
    PiAllocator pia = get_std_current_allocator();
    Allocator a = convert_to_callocator(&pia);
    return string_f32(f32, &a);
}

String relic_f64_to_string(float64_t f64) {
    PiAllocator pia = get_std_current_allocator();
    Allocator a = convert_to_callocator(&pia);
    return string_f64(f64, &a);
}


static void build_to_string_fn(PiType* type, PrimType prim, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType argty;
    void* cfn;
    switch (prim) {
    case Float_32:
        argty = (CType){.sort = CSFloat};
        cfn = relic_f32_to_string;
        break;
    case Float_64:
        argty = (CType){.sort = CSDouble};
        cfn = relic_f64_to_string;
        break;
    default:
        panic(mv_string("pico/stdlib/num/float.c: unrecognized primitive to build_to_string_fn"));
    }

    CType c_type = mk_fn_ctype(pia, 1, "num", argty, mk_string_ctype(pia));

    convert_c_fn(cfn, &c_type, type, ass, a, point); 
}

void add_float_module(String name, PrimType prim, Assembler* ass, Module* num, Allocator* a) {
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
        .name = string_to_symbol(mv_string("core")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(num), NULL);
    Symbol sym;

    PiType* typep;
    ErrorPoint point;
    if (catch_error(point)) {
        panic(doc_to_str(point.error_message, 120, a));
    }

    Segments fn_segments = (Segments) {.data = mk_u8_array(0, a)};
    Segments prepped;
    LocationSize sz = prim == Float_64 ? sz_64 : sz_32;

    typep = mk_proc_type(pia, 1, mk_prim_type(pia, prim), mk_string_type(pia));
    build_to_string_fn(typep, prim, ass, pia, a, &point);
    sym = string_to_symbol(mv_string("to-string"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    BinaryOp add_op = prim == Float_64 ? AddSD : AddSS;
    BinaryOp sub_op = prim == Float_64 ? SubSD : SubSS;
    BinaryOp mul_op = prim == Float_64 ? MulSD : MulSS;
    BinaryOp div_op = prim == Float_64 ? DivSD : DivSS;

    typep = mk_binop_type(pia, prim, prim, prim);

    build_binary_float_fn(ass, add_op, sz, a, &point);
    sym = string_to_symbol(mv_string("+"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    build_binary_float_fn(ass, sub_op, sz, a, &point);
    sym = string_to_symbol(mv_string("-"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    build_binary_float_fn(ass, mul_op, sz, a, &point);
    sym = string_to_symbol(mv_string("*"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    build_binary_float_fn(ass, div_op, sz, a, &point);
    sym = string_to_symbol(mv_string("/"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, mk_prim_type(pia, prim), mk_prim_type(pia, prim));
    sym = string_to_symbol(mv_string("sin"));
    build_unary_float_fn(typep, sz, sz == sz_64 ? (void*)sin_f64 : (void*)sin_f32, ass, pia, a, &point);
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    sym = string_to_symbol(mv_string("cos"));
    build_unary_float_fn(typep, sz, sz == sz_64 ? (void*)cos_f64 : (void*)cos_f32, ass, pia, a, &point);
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    Result r = add_module_def(num, string_to_symbol(name), module);
    if (r.type == Err) panic(r.error_message);
}
