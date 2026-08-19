#include <inttypes.h>
#include "platform/signals.h"
#include "data/stringify.h"

#include "components/pretty/string_printer.h"

#include "pico/codegen/codegen.h"
#include "pico/stdlib/platform/submodules.h"
#include "pico/stdlib/num/submodules.h"
#include "pico/stdlib/helpers.h"

static PiType* mk_binop_type(PiAllocator* pia, PrimType a1, PrimType a2, PrimType r) {
    return mk_proc_type(pia, 2, mk_prim_type(pia, a1), mk_prim_type(pia, a2), mk_prim_type(pia, r));
}

static PiType* mk_unop_type(PiAllocator* pia, PrimType arg, PrimType r) {
    return mk_proc_type(pia, 1, mk_prim_type(pia, arg), mk_prim_type(pia, r));
}

static void build_binary_fn(Assembler* ass, BinaryOp op, LocationSize sz, Allocator* a, ErrorPoint* point) {
    build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);
    build_unary_op(Pop, reg(RDX, sz_64), ass, a, point);
    build_unary_op(Pop, reg(RAX, sz_64), ass, a, point);
    build_binary_op(op, reg(RAX, sz), reg(RDX, sz), ass, a, point);
    build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
    build_unary_op(Push, reg(RCX, sz_64), ass, a, point);
    build_nullary_op(Ret, ass, a, point);
}

static void build_not_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);
    build_unary_op(Pop, reg(RAX, sz_64), ass, a, point);
    build_binary_op(Xor, reg(RAX, sz_64), imm8(1), ass, a, point);
    build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
    build_unary_op(Push, reg(RCX, sz_64), ass, a, point);
    build_nullary_op(Ret, ass, a, point);
}


static void build_comp_fn(Assembler* ass, UnaryOp op, LocationSize sz, Allocator* a, ErrorPoint* point) {
    build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);
    build_unary_op(Pop, reg(RDX, sz_64), ass, a, point);
    build_unary_op(Pop, reg(RAX, sz_64), ass, a, point);
    build_binary_op(Cmp, reg(RAX, sz), reg(RDX, sz), ass, a, point);

    // TODO (BUG): most ops only work on sz_8 - the fact that the assembler
    // didn't complain is an issue!!
    build_unary_op(op, reg(RAX, sz_64), ass, a, point);
    build_binary_op(And, reg(RAX, sz_64), imm32(0xff), ass, a, point);
    build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
    build_unary_op(Push, reg(RCX, sz_64), ass, a, point);
    build_nullary_op(Ret, ass, a, point);
}

String relic_bool_to_string(uint64_t u64) {
    PiAllocator pia = get_std_current_allocator();
    Allocator a = convert_to_callocator(&pia);
    return string_bool(u64, &a);
}

static void build_to_string_fn(PiType* type, PrimType prim, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType argty = mk_primint_ctype((CPrimInt){.prim = CChar, .is_signed = Unsigned});
    void* cfn = relic_bool_to_string;
    CType c_type = mk_fn_ctype(pia, 1, "num", argty, mk_string_ctype(pia));
    convert_c_fn(cfn, &c_type, type, ass, a, point); 
}

void add_prim_bool_module(Assembler *ass, Target target, Module *num, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);
    Allocator* a = &ra;
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
        .name = string_to_name(mv_string("bool")),
        .imports = imports,
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

    PiType type;
    PiType type_val;
    PiType* type_data = &type_val;
    type.sort = TPrim;
    type.prim = TFormer;

    Segments null_segments = (Segments) {
        .code = mk_u8_array(0, &ra),
        .data = mk_u8_array(0, &ra),
    };

    type_val = (PiType) {.sort = TPrim, .prim = Bool};
    name = string_to_name(mv_string("Bool"));
    add_def(module, name, type, &type_data, null_segments, NULL);

    typep = mk_binop_type(pia, Bool, Bool, Bool);

    build_comp_fn(ass, SetE, sz_8, a, &point);
    name = string_to_name(mv_string("="));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    build_comp_fn(ass, SetNE, sz_8, a, &point);
    name = string_to_name(mv_string("!="));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    build_binary_fn(ass, And, sz_8, a, &point);
    name = string_to_name(mv_string("and"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    build_binary_fn(ass, Or, sz_8, a, &point);
    name = string_to_name(mv_string("or"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_unop_type(pia, Bool, Bool);
    build_not_fn(ass, a, &point);
    name = string_to_name(mv_string("not"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(pia, 1, mk_prim_type(pia, Bool), mk_string_type(pia));
    build_to_string_fn(typep, Bool, ass, pia, a, &point);
    name = string_to_name(mv_string("to-string"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
}
