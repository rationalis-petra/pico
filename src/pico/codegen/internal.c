#include "pico/codegen/internal.h"

#include "platform/machine_info.h"
#include "data/meta/array_impl.h"
#include "pico/values/stdlib.h"

int compare_to_generate(ToGenerate lhs, ToGenerate rhs) {
    int diff_1 = lhs.offset - rhs.offset;
    if (diff_1) return diff_1;
    return lhs.expr - rhs.expr;
}

ARRAY_CMP_IMPL(ToGenerate, compare_to_generate, to_gen, ToGen);

void backlink_global(Symbol sym, size_t offset, LinkData* links, Allocator* a) {
    // Step 1: Try lookup or else create & insert 
    SizeArray* sarr = sym_sarr_lookup(sym, links->backlinks);

    if (!sarr) {
        // Create & Insert
        sym_sarr_insert(sym, mk_size_array(4, a), &links->backlinks);
        sarr = sym_sarr_lookup(sym, links->backlinks);
    }

    // Step 2: insert offset into array
    push_size(offset, sarr);
}

void backlink_goto(Symbol sym, size_t offset, LinkData* links, Allocator* a) {
    // Step 1: Try lookup or else create & insert 
    SizeArray* sarr = sym_sarr_lookup(sym, links->gotolinks);

    if (!sarr) {
        // Create & Insert
        sym_sarr_insert(sym, mk_size_array(4, a), &links->gotolinks);
        sarr = sym_sarr_lookup(sym, links->gotolinks);
    }

    // Step 2: insert offset into array
    push_size(offset, sarr);
}

void generate_monomorphic_copy(Regname dest, Regname src, size_t size, Assembler* ass, Allocator* a, ErrorPoint* point) {
    // First, assert that size_t is divisible by 8 ( we use rax for copies )
    if (size % 8 != 0)  {
        throw_error(point, mv_string("Error in generate_stack_copy: expected copy size to be divisible by 8"));
    };

    if (size > 255)  {
        throw_error(point, mv_string("Error in generate_copy: copy size must be smaller than 255!"));
    };

    if (src == RAX || dest == RAX)  {
        throw_error(point, mv_string("Error in generate_copy: cannoy copy from/to RAX"));
    };

    for (size_t i = 0; i < size / 8; i++) {
        build_binary_op(ass, Mov, reg(RAX), rref8(src, i * 8), a, point);
        build_binary_op(ass, Mov, rref8(dest, i * 8), reg(RAX), a, point);
    }
}

void generate_monomorphic_swap(Regname loc1, Regname loc2, size_t size, Assembler* ass, Allocator* a, ErrorPoint* point) {
    // First, assert that size_t is divisible by 8 ( we use rax for copies )
    if (size % 8 != 0)  {
        throw_error(point, mv_string("Error in generate_monomorphic_swap expected copy size to be divisible by 8"));
    };

    if (size > 255)  {
        throw_error(point, mv_string("Error in generate_monomorphic_swap copy size must be smaller than 255!"));
    };

    if (loc1 == RDI || loc2 == RDI || loc1 == RSI || loc2 == RSI)  {
        throw_error(point, mv_string("Error in generate_monomorphic_swap cannot swap with RDI or RSI"));
    };

    for (size_t i = 0; i < size / 8; i++) {
        build_binary_op(ass, Mov, reg(RDI), rref8(loc1, i * 8), a, point);
        build_binary_op(ass, Mov, reg(RSI), rref8(loc2, i * 8), a, point);
        build_binary_op(ass, Mov, rref8(loc1, i * 8), reg(RSI), a, point);
        build_binary_op(ass, Mov, rref8(loc2, i * 8), reg(RDI), a, point);
    }
}

void* tmp_malloc(uint64_t memsize) {
    return mem_alloc(memsize, get_std_tmp_allocator());
}

void generate_tmp_malloc(Location dest, Location mem_size, Assembler* ass, Allocator* a, ErrorPoint* point) {
#if ABI == SYSTEM_V_64
    build_binary_op(ass, Mov, reg(RDI), mem_size, a, point);
#elif ABI == WIN_64
    build_binary_op(ass, Mov, reg(RCX), mem_size, a, point);
    build_binary_op(ass, Sub, reg(RSP), , imm32(32), a, point);
#else 
    #error "Unknown calling convention"
#endif

    build_binary_op(ass, Mov, reg(RAX), imm64((uint64_t)&tmp_malloc), a, point);
    build_unary_op(ass, Call, reg(RAX), a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP), imm32(32), a, point);
#endif 

    if (dest.type != Register && dest.reg != RAX) {
        build_binary_op(ass, Mov, dest, reg(RAX), a, point);
    }
}

void* mk_struct_ty(size_t len, void* data) {
    Allocator* a = get_std_tmp_allocator();

    PiType* ty = mem_alloc(sizeof(PiType), a);
    *ty = (PiType) {
        .sort = TStruct,
        .structure.fields.data = data,
        .structure.fields.len = len,
        .structure.fields.capacity = len,
        .structure.fields.gpa = a,
    };
    return ty;
}

void gen_mk_struct_ty(Location dest, Location nfields, Location data, Assembler* ass, Allocator* a, ErrorPoint* point) {
#if ABI == SYSTEM_V_64
    build_binary_op(ass, Mov, reg(RDI), nfields, a, point);
    build_binary_op(ass, Mov, reg(RSI), data, a, point);
#elif ABI == WIN_64
    build_binary_op(ass, Mov, reg(RCX), nfields, a, point);
    build_binary_op(ass, Mov, reg(RDX), data, a, point);
    build_binary_op(ass, Sub, reg(RSP), imm32(32), a, point);
#else 
    #error "Unknown calling convention"
#endif

    build_binary_op(ass, Mov, reg(RAX), imm64((uint64_t)&mk_struct_ty), a, point);
    build_unary_op(ass, Call, reg(RAX), a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP), imm32(32), a, point);
#endif 

    if (dest.type != Register && dest.reg != RAX) {
        build_binary_op(ass, Mov, dest, reg(RAX), a, point);
    }
}
