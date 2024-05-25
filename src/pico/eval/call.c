#include <stdio.h>
#include "pico/eval/call.h"

eval_result pico_run_toplevel(toplevel top, assembler* ass, pi_module* module, allocator a) {
    eval_result res;
    switch (top.type) {
    case TLExpr:
        res.val = pico_run_expr(ass, a);
        break;
    case TLDef:
        // copy into module
        switch (top.def.value->ptype->sort) {
        case TProc:
            add_fn_def(module, top.def.bind, *top.def.value->ptype, ass);
            res.val = 0;
        case TPrim: {
            // assume int64 for now!
            int64_t val = pico_run_expr(ass, a);
            res.val = val;
            add_def(module, top.def.bind, *top.def.value->ptype, &val);
            break;
        }
        case TUVar:
            res.val = 0;
            break;
        }
    }

    return res;
}

// Note: destructively modifies assembler! probaly want a better solution in the future
int64_t pico_run_expr(assembler* ass, allocator a) {
    /* result generated; */
    /* generated.type = Ok; */

    // code which will return to C
    build_unary_op(ass, Pop, reg(RAX));
    build_nullary_op(ass, Ret);
    u8_array instructions = get_instructions(ass);

    int64_t out;
    __asm__ __volatile__(
        "push %%rbp       \n\t"
        "call %1       \n\t"
        "movq %%RAX, %0  \n\t"
        "pop %%rbp       \n\t"
        : "=r" (out)
        : "r"(instructions.data));
    return out;
}

