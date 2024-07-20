#include <stdio.h>

#include "pico/eval/call.h"
#include "pico/values/types.h"
#include "pretty/standard_types.h"

eval_result pico_run_toplevel(toplevel top, assembler* ass, sym_sarr_amap* backlinks, pi_module* module, allocator a) {
    eval_result res;
    switch (top.type) {
    case TLExpr:
        if (top.expr.ptype->sort != TPrim) {
            res.type = ERFail;
            res.error_message = mv_string("Can only evaluate primitive values");
            return res;
        }

        switch (top.expr.ptype->prim) {
        case Int_64:
            res.type = ERInt;
            res.val = pico_run_expr(ass, a);
            break;
        case Bool:
            res.type = ERBool;
            res.val = pico_run_expr(ass, a);
            break;
        case TType:
            res.type = ERType;
            res.val = pico_run_expr(ass, a);
            break;
        default:
            res.type = ERFail;
            res.error_message = mv_string("Unrecognized primitive to evaluate");
            break;
        }
        break;
    case TLDef:
        // copy into module
        switch (top.def.value->ptype->sort) {
        case TProc:
            res.type = ERSucc;
            add_fn_def(module, top.def.bind, *top.def.value->ptype, ass, backlinks);
            res.val = 0;
            break;
        case TPrim: {
            res.type = ERSucc;
            // assume int64 for now!
            int64_t val = pico_run_expr(ass, a);
            res.val = val;
            add_def(module, top.def.bind, *top.def.value->ptype, &val);
            break;
        }
        default:
            res.type = ERFail;
            res.error_message = mv_string("Unrecognized type to define.");
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
    build_unary_op(ass, Pop, reg(RAX),a );
    build_nullary_op(ass, Ret, a);
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

document* pretty_res(eval_result res, allocator a) {
    document* out = NULL;
    switch (res.type) {
    case ERSucc:
        out = mk_str_doc(mv_string("Success"), a);
        break;
    case ERInt:
        out = pretty_i64(res.val, a);
        break;
    case ERBool:
        if (res.val == 0) 
            out = mk_str_doc(mv_string(":false"), a);
        else 
            out = mk_str_doc(mv_string(":true"), a);
        break;
    case ERType: {
        pi_type* pt = (pi_type*)res.val;
        out = pretty_type(pt, a);
        break;
    }
    default:
            out = mk_str_doc(mv_string("Invalid Result!"), a);
    }
    return out;
}
