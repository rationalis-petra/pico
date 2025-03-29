#include "platform/signals.h"

#include "pico/stdlib/num.h"

PiType* mk_binop_type(Allocator* a, PrimType a1, PrimType a2, PrimType r) {
    return mk_proc_type(a, 2, mk_prim_type(a, a1), mk_prim_type(a, a2), mk_prim_type(a, r));
}

void build_binary_fn(Assembler* ass, BinaryOp op, LocationSize sz, Allocator* a, ErrorPoint* point) {
    build_unary_op (ass, Pop, reg(RCX, sz_64), a, point);
    build_unary_op (ass, Pop, reg(RDX, sz_64), a, point);
    build_unary_op (ass, Pop, reg(RAX, sz_64), a, point);
    build_binary_op (ass, op, reg(RAX, sz), reg(RDX, sz), a, point);
    build_unary_op (ass, Push, reg(RAX, sz_64), a, point);
    build_unary_op (ass, Push, reg(RCX, sz_64), a, point);
    build_nullary_op (ass, Ret, a, point);
}

void build_special_binary_fn(Assembler* ass, UnaryOp op, LocationSize sz, Allocator* a, ErrorPoint* point) {
    build_unary_op (ass, Pop, reg(RCX, sz_64), a, point);
    build_unary_op (ass, Pop, reg(RDI, sz_64), a, point);
    build_unary_op (ass, Pop, reg(RAX, sz_64), a, point);

    switch (sz) {
    case sz_64:
    case sz_32:
        build_binary_op (ass, Mov, reg(RDX, sz), imm32(0), a, point);
        break;
    case sz_16:
        build_binary_op (ass, Mov, reg(RDX, sz), imm16(0), a, point);
        break;
    case sz_8:
        build_binary_op (ass, Mov, reg(RDX, sz), imm8(0), a, point);
        break;
    }
    build_unary_op (ass, op, reg(RDI, sz), a, point);

    build_unary_op (ass, Push, reg(RAX, sz_64), a, point);
    build_unary_op (ass, Push, reg(RCX, sz_64), a, point);
    build_nullary_op (ass, Ret, a, point);
}

void build_comp_fn(Assembler* ass, UnaryOp op, LocationSize sz, Allocator* a, ErrorPoint* point) {
    build_unary_op (ass, Pop, reg(RCX, sz_64), a, point);
    build_unary_op (ass, Pop, reg(RDX, sz_64), a, point);
    build_unary_op (ass, Pop, reg(RAX, sz_64), a, point);
    build_binary_op (ass, Cmp, reg(RAX, sz), reg(RDX, sz), a, point);
    build_unary_op (ass, op, reg(RAX, sz_64), a, point);
    build_unary_op (ass, Push, reg(RAX, sz_64), a, point);
    build_unary_op (ass, Push, reg(RCX, sz_64), a, point);
    build_nullary_op (ass, Ret, a, point);
}

void add_primitive_module(String name, LocationSize sz, bool is_signed, Assembler* ass, Module* num, Allocator* a) {
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
    Module* module = mk_module(header, get_package(num), NULL, a);
    delete_module_header(header);
    Symbol sym;

    PiType* typep;
    ErrorPoint point;
    if (catch_error(point)) {
        panic(point.error_message);
    }

    PrimType prims[2][4] = {
        {UInt_8, UInt_16, UInt_32, UInt_64},
        {Int_8, Int_16, Int_32, Int_64},
    };
    PrimType prim = prims[is_signed][sz];

    Segments fn_segments = (Segments) {.data = mk_u8_array(0, a)};
    Segments prepped;

    build_binary_fn(ass, Add, sz, a, &point);
    typep = mk_binop_type(a, prim, prim, prim);
    sym = string_to_symbol(mv_string("+"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    build_binary_fn(ass, Sub, sz, a, &point);
    sym = string_to_symbol(mv_string("-"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    build_special_binary_fn(ass, is_signed ? IMul : Mul, sz, a, &point);
    sym = string_to_symbol(mv_string("*"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    build_special_binary_fn(ass, is_signed ? IDiv : Div, sz, a, &point);
    sym = string_to_symbol(mv_string("/"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    build_comp_fn(ass, is_signed ? SetL : SetB, sz, a, &point);
    typep = mk_binop_type(a, prim, prim, Bool);
    sym = string_to_symbol(mv_string("<"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    build_comp_fn(ass, is_signed ? SetG : SetA, sz, a, &point);
    sym = string_to_symbol(mv_string(">"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    build_comp_fn(ass, SetE, sz, a, &point);
    sym = string_to_symbol(mv_string("="));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    delete_pi_type_p(typep, a);
    sdelete_u8_array(fn_segments.data);

    Result r = add_module_def(num, string_to_symbol(name), module);
    if (r.type == Err) panic(r.error_message);

}

void add_num_module(Assembler* ass, Package* base, Allocator* a) {
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
    Module* module = mk_module(header, base, NULL, a);
    delete_module_header(header);

    add_primitive_module(mv_string("u8"), sz_8, false, ass, module, a);
    add_primitive_module(mv_string("u16"), sz_16, false, ass, module, a);
    add_primitive_module(mv_string("u32"), sz_32, false, ass, module, a);
    add_primitive_module(mv_string("u64"), sz_64, false, ass, module, a);

    add_primitive_module(mv_string("i8"), sz_8, true, ass, module, a);
    add_primitive_module(mv_string("i16"), sz_16, true, ass, module, a);
    add_primitive_module(mv_string("i32"), sz_32, true, ass, module, a);
    add_primitive_module(mv_string("i64"), sz_64, true, ass, module, a);

    add_module(string_to_symbol(mv_string("num")), module, base);
}

