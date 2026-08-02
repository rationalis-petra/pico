#include <inttypes.h>
#include "platform/signals.h"
#include "data/stringify.h"
#include "data/float.h"

#include "components/pretty/string_printer.h"

#include "pico/stdlib/platform/submodules.h"
#include "pico/stdlib/num/submodules.h"
#include "pico/codegen/codegen.h"
#include "pico/stdlib/helpers.h"

static PiType* mk_binop_type(PiAllocator* pia, PrimType a1, PrimType a2, PrimType r) {
    return mk_proc_type(pia, 2, mk_prim_type(pia, a1), mk_prim_type(pia, a2), mk_prim_type(pia, r));
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

void add_float_module(PrimType prim, Assembler* ass, Target target, Module* num, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);
    Allocator* a = &ra;
    PiAllocator pico_allocator = convert_to_pallocator(a);
    PiAllocator* pia = &pico_allocator;

    String name_lower = mv_string(prim == Float_32 ? "f32" : "f64");
    String name_upper = mv_string(prim == Float_32 ? "F32" : "F64");

    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(8, a),
    };
    add_import_all(&imports.clauses, a, 2, "lang", "relic");
    add_import_all(&imports.clauses, a, 2, "abs", "show");
    add_import_all(&imports.clauses, a, 2, "abs", "equality");
    add_import_all(&imports.clauses, a, 2, "abs", "order");
    add_import_all(&imports.clauses, a, 2, "abs", "numeric");
    ReExports re_exports = (ReExports) {
        .clauses = mk_import_clause_array(0, a),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, a),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_name(name_lower),
        .imports = imports,
        .re_exports = re_exports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(num), num);
    Name name;

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
    name = string_to_name(mv_string("to-string"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    BinaryOp add_op = prim == Float_64 ? AddSD : AddSS;
    BinaryOp sub_op = prim == Float_64 ? SubSD : SubSS;
    BinaryOp mul_op = prim == Float_64 ? MulSD : MulSS;
    BinaryOp div_op = prim == Float_64 ? DivSD : DivSS;

    typep = mk_binop_type(pia, prim, prim, prim);

    build_binary_float_fn(ass, add_op, sz, a, &point);
    name = string_to_name(mv_string("+"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    build_binary_float_fn(ass, sub_op, sz, a, &point);
    name = string_to_name(mv_string("-"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    build_binary_float_fn(ass, mul_op, sz, a, &point);
    name = string_to_name(mv_string("*"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    build_binary_float_fn(ass, div_op, sz, a, &point);
    name = string_to_name(mv_string("/"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, mk_prim_type(pia, prim), mk_prim_type(pia, prim));
    name = string_to_name(mv_string("sin"));
    build_unary_float_fn(typep, sz, sz == sz_64 ? (void*)sin_f64 : (void*)sin_f32, ass, pia, a, &point);
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    name = string_to_name(mv_string("cos"));
    build_unary_float_fn(typep, sz, sz == sz_64 ? (void*)cos_f64 : (void*)cos_f32, ass, pia, a, &point);
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);


    PiErrorPoint pi_point;
    if (catch_error(pi_point)) {
        panic(mv_string("pico error in pico/stdlib/abs/order.c"));
    }


    String show_instance = string_ncat(a, 5,
                mv_string("(def show-"),
                name_lower,
                mv_string(" instance (Show "),
                name_upper,
                mv_string(")  \n  [.show to-string])"));
    compile_str_toplevel(show_instance, module, target, &point, &pi_point, region);

    /*
    String eq_instance = string_ncat(a, 5, 
                mv_string("(def eq-"),
                name_lower,
                mv_string(" instance (Eq "),
                name_upper,
                mv_string(")  [.= =] [.!= !=])"));
    compile_str_toplevel(eq_instance, module, target, &point, &pi_point, region);

    String ord_instance = string_ncat(a, 5, 
                mv_string("(def ord-"),
                name_lower,
                mv_string(" instance (Ord "),
                name_upper,
                mv_string(")  [.< <] [.<= <=] [.> >] [.>= >=])"));
    compile_str_toplevel(ord_instance, module, target, &point, &pi_point, region);
    */

    String num_instance = string_ncat(a, 5, 
                mv_string("(def num-"),
                name_lower,
                mv_string(" instance (Num "),
                name_upper,
                mv_string(")  [.+ +] [.- -] [.* *] [./ /] [.zero 0.0] [.one 1.0])"));
    compile_str_toplevel(num_instance, module, target, &point, &pi_point, region);
}
