#include "pico/codegen/internal.h"

void backlink_global(Symbol sym, size_t offset, SymSArrAMap* links, Allocator* a) {
    // Step 1: Try lookup or else create & insert 
    SizeArray* sarr = NULL;
    sarr = sym_sarr_lookup(sym, *links);

    if (!sarr) {
        // create & insert
        sym_sarr_insert(sym, mk_size_array(4, a), links);
        sarr = sym_sarr_lookup(sym, *links);
    }

    // Step 2: insert offset into array
    push_size(offset, sarr);
}

AsmResult generate_monomorphic_copy(Regname dest, Regname src, size_t size, Assembler* ass, Allocator* a) {
    AsmResult out;

    // first, assert that size_t is divisible by 8 ( we use rax for copies )
    if (size % 8 != 0)  {
        out.type = Err;
        out.error_message = mv_string("Error in generate_stack_copy: expected copy size to be divisible by 8");
        return out;
    };

    if (size > 255)  {
        out.type = Err;
        out.error_message = mv_string("Error in generate_copy: copy size must be smaller than 255!");
        return out;
    };

    if (src == RAX || dest == RAX)  {
        out.type = Err;
        out.error_message = mv_string("Error in generate_copy: offsets must be smaller than 255!");
        return out;
    };

    for (size_t i = 0; i < size / 8; i++) {
        out = build_binary_op(ass, Mov, reg(RAX), rref(src, i * 8), a);
        if (out.type == Err) return out;

        out = build_binary_op(ass, Mov, rref(dest, i * 8), reg(RAX), a);
        if (out.type == Err) return out;
    }

    return out;
}
