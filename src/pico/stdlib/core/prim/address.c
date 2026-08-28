#include <string.h>

#include "platform/signals.h"
#include "platform/machine_info.h"

#include "components/pretty/string_printer.h"

#include "pico/codegen/backend-direct/internal.h"
#include "pico/stdlib/core/prim/submodules.h"

PiType build_store_fn_ty(PiAllocator* pia) {
    PiType* proc_ty  = mk_proc_type(pia, 2, mk_prim_type(pia, Address), mk_var_type(pia, "A"), mk_prim_type(pia, Unit));

    SymAddrPiAMap types = mk_sym_addr_piamap(1, pia);
    sym_addr_insert(string_to_symbol(mv_string("A")), mk_type_type(pia), &types);

    return (PiType) {.sort = TAll, .binder.vars = types, .binder.body = proc_ty};
}

void build_store_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    /**
     * The usual calling convention for polymorphic functions is assumed, hence
     * stack has the form:
     * [RSP-0x28] | return dest
     * [RSP-0x20] | R14 output value
     * [RSP-0x18] | type size
     * [RSP-0x10  | store address
     * [RSP-0x8]  | variable stack index (value/ptr)
     * [RSP]      | return address 
     */

#if ABI == SYSTEM_V_64
    // memcpy (dest = rdi, src = rsi, size = rdx)
    build_binary_op(Mov, reg(RSI, sz_64), rref8(RSP, 0x8, sz_64), ass, a, point);
    build_binary_op(Mov, reg(RDI, sz_64), rref8(RSP, 0x10, sz_64), ass, a, point);

    build_binary_op(Mov, reg(RDX, sz_64), rref8(RSP, 0x18, sz_64), ass, a, point);
    build_binary_op(SHR, reg(RDX, sz_64), imm8(28), ass, a, point);
    build_binary_op(And, reg(RDX, sz_64), imm32(0xFFFFFFF), ass, a, point);

#elif ABI == WIN_64
    // memcpy (dest = rcx, src = rdx, size = r8)
    build_binary_op(Mov, reg(RDX, sz_64), rref8(RSP, 0x8, sz_64), ass, a, point);
    build_binary_op(Mov, reg(RCX, sz_64), rref8(RSP, 0x10, sz_64), ass, a, point);

    build_binary_op(Mov, reg(R8, sz_64), rref8(RSP, 0x18, sz_64), ass, a, point);
    build_binary_op(SHR, reg(R8, sz_64), imm8(28), ass, a, point);
    build_binary_op(And, reg(R8, sz_64), imm32(0xFFFFFFF), ass, a, point);
#elif ABI == SYSTEM_V_AARCH64
    panic(mv_string("Not implemented: build_store_fn for aarch64"));
#else
#error "Unknown calling convention"
#endif

    // copy memcpy into RCX & call
    generate_c_call(memcpy, ass, a, point);

    // Restore R14
    build_binary_op(Mov, reg(R14, sz_64), rref8(RSP, 0x20, sz_64), ass, a, point);

    // Pop all values from stack (except return address, then return) 
    build_binary_op(Mov, reg(RAX, sz_64), rref8(RSP, 0x0, sz_64), ass, a, point);
    build_binary_op(Add, reg(RSP, sz_64), imm8(0x28), ass, a, point);
    build_binary_op(Mov, rref8(RSP, 0x0, sz_64), reg(RAX, sz_64), ass, a, point);
    build_nullary_op(Ret, ass, a, point);
}

PiType build_load_fn_ty(PiAllocator* pia) {
    PiType* proc_ty = mk_proc_type(pia, 1, mk_prim_type(pia, Address), mk_var_type(pia, "A"));

    SymAddrPiAMap types = mk_sym_addr_piamap(1, pia);
    sym_addr_insert(string_to_symbol(mv_string("A")), mk_type_type(pia), &types);

    return (PiType) {.sort = TAll, .binder.vars = types, .binder.body = proc_ty};
}

void relic_memcpy(char *dest, char *src, size_t size) {
    for (size_t i = 0; i < size; i++) {
        dest[i] = src[i];
    }
}

void build_load_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    /** 
     * The usual calling convention for polymorphic functions is assumed, hence
     * stack has the form:
     * [RSP-0x20] | return dest
     * [RSP-0x18] | R14 output value
     * [RSP-x010] | type size
     * [RSP-0x8]  | load address
     * [RSP]      | return address 
     */

#if ABI == SYSTEM_V_64
    // memcpy (dest = rdi, src = rsi, size = rdx)
    build_binary_op(Mov, reg(RSI, sz_64), rref8(RSP, 0x8, sz_64), ass, a, point);

    // Store size in RDX
    build_binary_op(Mov, reg(RDX, sz_64), rref8(RSP, 0x10, sz_64), ass, a, point);
    build_binary_op(SHR, reg(RDX, sz_64), imm8(28), ass, a, point);
    build_binary_op(And, reg(RDX, sz_64), imm32(0xFFFFFFF), ass, a, point);

    build_binary_op(Mov, reg(RDI, sz_64), rref8(RSP, 0x20, sz_64), ass, a, point);

#elif ABI == WIN_64
    // memcpy (dest = rcx, src = rdx, size = r8)
    build_binary_op(Mov, reg(RDX, sz_64), rref8(RSP, 0x8, sz_64), ass, a, point);

    // Store size in R8
    build_binary_op(Mov, reg(R8, sz_64), rref8(RSP, 0x10, sz_64), ass, a, point);
    build_binary_op(SHR, reg(R8, sz_64), imm8(28), ass, a, point);
    build_binary_op(And, reg(R8, sz_64), imm32(0xFFFFFFF), ass, a, point);

    // Store the output address value in RCX
    build_binary_op(Mov, reg(RCX, sz_64), rref8(RSP, 0x20, sz_64), ass, a, point);

#elif ABI == SYSTEM_V_AARCH64
    panic(mv_string("Not implemented: build_load_fn for aarch64"));
#else
#error "Unknown calling convention"
#endif

    // Do the load
    generate_c_call(relic_memcpy, ass, a, point);

    // Restore R14
    build_binary_op(Mov, reg(R14, sz_64), rref8(RSP, 0x18, sz_64), ass, a, point);

    // Pop all values from stack (except return address, then return) 
    build_binary_op(Mov, reg(RAX, sz_64), rref8(RSP, 0x0, sz_64), ass, a, point);
    build_binary_op(Add, reg(RSP, sz_64), imm8(0x20), ass, a, point);
    build_binary_op(Mov, rref8(RSP, 0x0, sz_64), reg(RAX, sz_64), ass, a, point);
    build_nullary_op(Ret, ass, a, point);
}

void build_nop_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    build_nullary_op(Ret, ass, a, point);
}

void add_prim_address_module(Assembler* ass, Module* prim, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(0, &ra),
    };
    ReExports re_exports = (ReExports) {
        .clauses = mk_import_clause_array(0, &ra),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, &ra),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_name(mv_string("address")),
        .imports = imports,
        .re_exports = re_exports,
        .exports = exports,
    };
    Package* base = get_package(prim);
    Module* module = mk_module(header, base, prim);
    Name name;

    PiType type;
    PiType* typep;
    PiType type_val;
    PiType* type_data = &type_val;
    ErrorPoint point;
    PiAllocator pia = convert_to_pallocator(&ra);
    if (catch_error(point)) {
        panic(doc_to_str(point.error_message, 120, &ra));
    }

    type.sort = TPrim;
    type.prim = TFormer;

    Segments null_segments = (Segments) {
        .code = mk_u8_array(0, &ra),
        .data = mk_u8_array(0, &ra),
    };

    type_val = (PiType) {.sort = TPrim, .prim = Address};
    name = string_to_name(mv_string("Address"));
    add_def(module, name, type, &type_data, null_segments, NULL);

    // Unit value

    Segments fn_segments = (Segments) {.data = mk_u8_array(0, &ra),};
    Segments prepped;

    type = build_store_fn_ty(&pia);
    build_store_fn(ass, &ra, &point);
    name = string_to_name(mv_string("store"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, type, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    type = build_load_fn_ty(&pia);
    build_load_fn(ass, &ra, &point);
    name = string_to_name(mv_string("load"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, type, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(&pia, 1, mk_prim_type(&pia, Address), mk_prim_type(&pia, UInt_64));
    build_nop_fn(ass, &ra, &point);
    name = string_to_name(mv_string("address-to-num"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_proc_type(&pia, 1, mk_prim_type(&pia, UInt_64), mk_prim_type(&pia, Address));
    build_nop_fn(ass, &ra, &point);
    name = string_to_name(mv_string("num-to-address"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, name, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);
}
