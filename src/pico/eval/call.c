#include <stdio.h>
#include <string.h> // for memcpy

#include "pico/eval/call.h"
#include "pico/values/types.h"
#include "pretty/standard_types.h"

eval_result pico_run_toplevel(toplevel top, assembler* ass, sym_sarr_amap* backlinks, pi_module* module, allocator a) {
    eval_result res;
    switch (top.type) {
    case TLExpr: {
        size_t sz = pi_size_of(*top.expr.ptype);

        if (top.expr.ptype->sort == TPrim) {
            res.type = ERValue;
            res.val.type = top.expr.ptype;
            res.val.val = pico_run_expr(ass, sz, a);
        }
        else if (top.expr.ptype->sort == TStruct) {
            res.type = ERValue;
            res.val.type = top.expr.ptype;
            res.val.val = pico_run_expr(ass, sz, a);
        }
        else if (top.expr.ptype->sort == TEnum) {
            res.type = ERValue;
            res.val.type = top.expr.ptype;
            res.val.val = pico_run_expr(ass, sz, a);
        }
        else  {
            res.type = ERFail;
            res.error_message = mv_string("Cannot evaluate values of this type.");
            return res;
        }
        break;
    }
    case TLDef:
        // copy into module
        switch (top.def.value->ptype->sort) {
        case TProc:
            res.type = ERSucc;
            add_fn_def(module, top.def.bind, *top.def.value->ptype, ass, backlinks);
            break;
        case TEnum:
        case TStruct:
        case TPrim: {
            res.type = ERValue;
            // assume int64 for now!
            res.val.type = top.def.value->ptype;
            res.val.val = pico_run_expr(ass, pi_size_of(*top.def.value->ptype), a);
            add_def(module, top.def.bind, *top.def.value->ptype, res.val.val);
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
void* pico_run_expr(assembler* ass, size_t rsize, allocator a) {
    /* result generated; */
    /* generated.type = Ok; */
    void* value = mem_alloc(rsize, a);

    // Generate Code which will: 
    //  1. Copy the final value (on stack) into value
    //  2. Return to C
    // 
    // memcpy (dest = rdi, src = rsi, size = rdx)
    // retval = rax
    build_binary_op(ass, Mov, reg(RDI), imm64((int64_t)value), a);
    build_binary_op(ass, Mov, reg(RSI), reg(RSP), a);
    //build_binary_op(ass, Sub, reg(RSI), imm32(rsize), a);
    build_binary_op(ass, Mov, reg(RDX), imm64((int64_t)rsize), a);

    build_binary_op(ass, Mov, reg(RCX), imm64((int64_t)&memcpy), a);
    build_unary_op(ass, Call, reg(RCX),a);
    // pop value from stack
    build_binary_op(ass, Add, reg(RSP), imm32(rsize), a);
    build_nullary_op(ass, Ret, a);
    u8_array instructions = get_instructions(ass);

    int64_t out;
    __asm__ __volatile__(
        "push %%rbp       \n\t"
        "call %1       \n\t"
        "pop %%rbp       \n\t"
        : "=r" (out)
        : "r"(instructions.data));

    return value;
}

document* pretty_res(eval_result res, allocator a) {
    document* out = NULL;
    switch (res.type) {
    case ERFail:
        out = mk_str_doc(res.error_message, a);
        break;
    case ERSucc:
        out = mk_str_doc(mv_string("Success"), a);
        break;
    case ERValue:
        out = pretty_pi_value(res.val.val, res.val.type, a);
        break;
    default:
        out = mk_str_doc(mv_string("Invalid Result!"), a);
    }
    return out;
}
