#include "pico/codegen/internal.h"

#include "data/meta/array_impl.h"

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
