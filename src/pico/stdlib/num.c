#include <stdio.h>
#include <inttypes.h>
#include "platform/memory/std_allocator.h"
#include "platform/signals.h"
#include "data/stringify.h"

#include "pico/stdlib/extra.h"
#include "pico/stdlib/num.h"
#include "pico/codegen/foreign_adapters.h"

PiType* mk_binop_type(Allocator* a, PrimType a1, PrimType a2, PrimType r) {
    return mk_proc_type(a, 2, mk_prim_type(a, a1), mk_prim_type(a, a2), mk_prim_type(a, r));
}

PiType* mk_unop_type(Allocator* a, PrimType arg, PrimType r) {
    return mk_proc_type(a, 1, mk_prim_type(a, arg), mk_prim_type(a, r));
}

void build_not_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    build_unary_op (ass, Pop, reg(RCX, sz_64), a, point);
    build_unary_op (ass, Pop, reg(RAX, sz_64), a, point);
    build_binary_op (ass, Xor, reg(RAX, sz_64), imm8(1), a, point);
    build_unary_op (ass, Push, reg(RAX, sz_64), a, point);
    build_unary_op (ass, Push, reg(RCX, sz_64), a, point);
    build_nullary_op (ass, Ret, a, point);
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

void build_special_binary_fn(Assembler* ass, UnaryOp op, Regname out, LocationSize sz, Allocator* a, ErrorPoint* point) {
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

    build_unary_op (ass, Push, reg(out, sz_64), a, point);
    build_unary_op (ass, Push, reg(RCX, sz_64), a, point);
    build_nullary_op (ass, Ret, a, point);
}

void build_comp_fn(Assembler* ass, UnaryOp op, LocationSize sz, Allocator* a, ErrorPoint* point) {
    build_unary_op (ass, Pop, reg(RCX, sz_64), a, point);
    build_unary_op (ass, Pop, reg(RDX, sz_64), a, point);
    build_unary_op (ass, Pop, reg(RAX, sz_64), a, point);
    build_binary_op (ass, Cmp, reg(RAX, sz), reg(RDX, sz), a, point);

    // TODO (BUG): most ops only work on sz_8 - the fact that the assembler
    // didn't complain is an issue!!
    build_unary_op (ass, op, reg(RAX, sz_64), a, point);
    build_binary_op (ass, And, reg(RAX, sz_64), imm32(0xff), a, point);
    build_unary_op (ass, Push, reg(RAX, sz_64), a, point);
    build_unary_op (ass, Push, reg(RCX, sz_64), a, point);
    build_nullary_op (ass, Ret, a, point);
}

String relic_u64_to_string(uint64_t u64) {
    Allocator a = get_std_current_allocator();
    return string_u64(u64, &a);
}

String relic_u32_to_string(uint32_t u32) {
    Allocator a = get_std_current_allocator();
    return string_u32(u32, &a);
}

String relic_u16_to_string(uint16_t u16) {
    Allocator a = get_std_current_allocator();
    return string_u16(u16, &a);
}

String relic_u8_to_string(uint8_t u8) {
    Allocator a = get_std_current_allocator();
    return string_u8(u8, &a);
}

String relic_i64_to_string(int64_t i64) {
    Allocator a = get_std_current_allocator();
    return string_i64(i64, &a);
}

String relic_i32_to_string(int32_t i32) {
    Allocator a = get_std_current_allocator();
    return string_i32(i32, &a);
}

String relic_i16_to_string(int16_t i16) {
    Allocator a = get_std_current_allocator();
    return string_i16(i16, &a);
}

String relic_i8_to_string(int8_t i8) {
    Allocator a = get_std_current_allocator();
    return string_u8(i8, &a);
}

void build_to_string_fn(PiType* type, PrimType prim, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CPrimInt cint;
    void* cfn;
    switch (prim) {
    case UInt_64:
        cint = (CPrimInt){.prim = CLongLong, .is_signed = Unsigned};
        cfn = relic_u64_to_string;
        break;
    case Int_64:
        cint = (CPrimInt){.prim = CLongLong, .is_signed = Signed};
        cfn = relic_i64_to_string;
        break;
    case UInt_32:
        cint = (CPrimInt){.prim = CInt, .is_signed = Unsigned};
        cfn = relic_u32_to_string;
        break;
    case Int_32:
        cint = (CPrimInt){.prim = CInt, .is_signed = Signed};
        cfn = relic_i32_to_string;
        break;
    case UInt_16:
        cint = (CPrimInt){.prim = CShort, .is_signed = Unsigned};
        cfn = relic_u16_to_string;
        break;
    case Int_16:
        cint = (CPrimInt){.prim = CShort, .is_signed = Signed};
        cfn = relic_i16_to_string;
        break;
    case UInt_8:
        cint = (CPrimInt){.prim = CChar, .is_signed = Unsigned};
        cfn = relic_u8_to_string;
        break;
    case Int_8:
        cint = (CPrimInt){.prim = CChar, .is_signed = Signed};
        cfn = relic_i8_to_string;
        break;
    default:
        panic(mv_string("num.c: unrecognized primitive to build_to_string_fn"));
    }

    CType c_type = mk_fn_ctype(a, 1,
                               "num", mk_primint_ctype(cint),
                               mk_string_ctype(a));

    convert_c_fn(cfn, &c_type, type, ass, a, point); 

    delete_c_type(c_type, a);
}

void add_integral_module(String name, LocationSize sz, bool is_signed, Assembler* ass, Module* num, Allocator* a) {
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

    build_special_binary_fn(ass, is_signed ? IMul : Mul, RAX, sz, a, &point);
    sym = string_to_symbol(mv_string("*"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    build_special_binary_fn(ass, is_signed ? IDiv : Div, RAX, sz, a, &point);
    sym = string_to_symbol(mv_string("/"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    build_special_binary_fn(ass, is_signed ? IDiv : Div, RDX, sz, a, &point);
    sym = string_to_symbol(mv_string("mod"));
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

    build_comp_fn(ass, is_signed ? SetLE : SetBE, sz, a, &point);
    sym = string_to_symbol(mv_string("<="));
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

    build_comp_fn(ass, is_signed ? SetGE : SetAE, sz, a, &point);
    sym = string_to_symbol(mv_string(">="));
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

    build_comp_fn(ass, SetNE, sz, a, &point);
    sym = string_to_symbol(mv_string("!="));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    typep = mk_proc_type(a, 1, mk_prim_type(a, prim), mk_string_type(a));
    build_to_string_fn(typep, prim, ass, a, &point);
    sym = string_to_symbol(mv_string("to-string"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    delete_pi_type_p(typep, a);
    sdelete_u8_array(fn_segments.data);

    Result r = add_module_def(num, string_to_symbol(name), module);
    if (r.type == Err) panic(r.error_message);
}

void add_bool_module(Assembler *ass, Module *num, Allocator *a) {
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

    Segments fn_segments = (Segments) {.data = mk_u8_array(0, a)};
    Segments prepped;

    typep = mk_binop_type(a, Bool, Bool, Bool);

    build_binary_fn(ass, And, sz_8, a, &point);
    sym = string_to_symbol(mv_string("and"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    build_binary_fn(ass, Or, sz_8, a, &point);
    sym = string_to_symbol(mv_string("or"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    typep = mk_unop_type(a, Bool, Bool);
    build_not_fn(ass, a, &point);
    sym = string_to_symbol(mv_string("not"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
    delete_pi_type_p(typep, a);

    sdelete_u8_array(fn_segments.data);

    Result r = add_module_def(num, string_to_symbol(mv_string("bool")), module);
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

    add_bool_module(ass, module, a);

    add_integral_module(mv_string("u8"), sz_8, false, ass, module, a);
    add_integral_module(mv_string("u16"), sz_16, false, ass, module, a);
    add_integral_module(mv_string("u32"), sz_32, false, ass, module, a);
    add_integral_module(mv_string("u64"), sz_64, false, ass, module, a);

    add_integral_module(mv_string("i8"), sz_8, true, ass, module, a);
    add_integral_module(mv_string("i16"), sz_16, true, ass, module, a);
    add_integral_module(mv_string("i32"), sz_32, true, ass, module, a);
    add_integral_module(mv_string("i64"), sz_64, true, ass, module, a);

    add_module(string_to_symbol(mv_string("num")), module, base);
}
