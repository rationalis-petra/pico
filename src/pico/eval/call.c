#include <stdio.h>
#include <string.h> // for memcpy

#include "platform/calling_convention.h"
#include "pico/eval/call.h"
#include "pico/values/types.h"

EvalResult pico_run_toplevel(TopLevel top, Assembler* ass, SymSArrAMap* backlinks, Module* module, Allocator* a) {
    EvalResult res;
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
        else if (top.expr.ptype->sort == TKind) {
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
        case TKind:
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

// Note: destructively modifies Assembler! probaly want a better solution in the future
void* pico_run_expr(Assembler* ass, size_t rsize, Allocator* a) {
    /* result generated; */
    /* generated.type = Ok; */
    void* value = mem_alloc(rsize, a);

    // Generate Code which will: 
    //  1. Copy the final value (on stack) into value
    //  2. Return to C
#ifdef ABI_SYSTEM_V_64
    // memcpy (dest = rdi, src = rsi, size = rdx)
    // retval = rax
    build_binary_op(ass, Mov, reg(RDI), imm64((int64_t)value), a);
    build_binary_op(ass, Mov, reg(RSI), reg(RSP), a);
    build_binary_op(ass, Mov, reg(RDX), imm64((int64_t)rsize), a);

    build_binary_op(ass, Mov, reg(RCX), imm64((int64_t)&memcpy), a);
    build_unary_op(ass, Call, reg(RCX),a);
    // pop value from stack
    build_binary_op(ass, Add, reg(RSP), imm32(rsize), a);
    build_nullary_op(ass, Ret, a);

#elif ABI_WIN_64
    // memcpy (dest = rcx, src = rdx, size = r8)
    // retval = rax
    build_binary_op(ass, Mov, reg(RCX), imm64((int64_t)value), a);
    build_binary_op(ass, Mov, reg(RDX), reg(RSP), a);
    build_binary_op(ass, Mov, reg(R8), imm64((int64_t)rsize), a);
    build_binary_op(ass, Sub, reg(RSP), imm32(32), a);

    build_binary_op(ass, Mov, reg(RAX), imm64((int64_t)&memcpy), a);
    build_unary_op(ass, Call, reg(RAX),a);
    // pop value from stack
    build_binary_op(ass, Add, reg(RSP), imm32(rsize + 32), a);
    build_nullary_op(ass, Ret, a);
#else
#error "Unknown calling convention"
#endif
    U8Array instructions = get_instructions(ass);

    int64_t out;
    __asm__ __volatile__(
        "push %%rbp       \n\t"
        "mov %%rsp, %%rbp \n\t"
        "sub $0x8, %%rbp     \n\t"
        "call %1          \n\t"
        "pop %%rbp        \n\t"
        : "=r" (out)
        : "r"(instructions.data));

    return value;
}

Document* pretty_res(EvalResult res, Allocator* a) {
    Document* out = NULL;
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
