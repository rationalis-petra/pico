#include <stdio.h>
#include <string.h> // for memcpy

#include "platform/machine_info.h"
#include "pico/eval/call.h"
#include "pico/values/types.h"

EvalResult pico_run_toplevel(TopLevel top, Assembler* ass, SymSArrAMap* backlinks, Module* module, Allocator* a, ErrorPoint* point) {
    EvalResult res;
    switch (top.type) {
    case TLExpr: {
        size_t sz = pi_size_of(*top.expr->ptype);

        if (top.expr->ptype->sort == TPrim
            || top.expr->ptype->sort == TStruct
            || top.expr->ptype->sort == TEnum
            || top.expr->ptype->sort == TDynamic
            || top.expr->ptype->sort == TKind) {
            res.type = ERValue;
            res.val.type = top.expr->ptype;
            res.val.val = pico_run_expr(ass, sz, a, point);
        }
        else  {
            throw_error(point, mv_string("Cannot evaluate values of this type."));
        }
        break;
    }
    case TLDef:
        // copy into module
        res = (EvalResult) {
            .type = ERDef,
            .def.name = top.def.bind,
            .def.type = top.def.value->ptype,
        };
        switch (top.def.value->ptype->sort) {
        case TAll:
        case TProc:
            add_fn_def(module, top.def.bind, *top.def.value->ptype, ass, backlinks);
            break;
        case TPrim:
        case TEnum:
        case TStruct:
        case TDynamic:
        case TKind: {
            // assume int64 for now!
            void* val = pico_run_expr(ass, pi_size_of(*top.def.value->ptype), a, point);
            add_def(module, top.def.bind, *top.def.value->ptype, val);
            break;
        }
        default:
            throw_error(point, mv_string("Unrecognized type to define."));
            break;
        }
    }

    return res;
}

// Note: destructively modifies Assembler! probaly want a better solution in the future
void* pico_run_expr(Assembler* ass, size_t rsize, Allocator* a, ErrorPoint* point) {
    /* result generated; */
    /* generated.type = Ok; */
    void* value = mem_alloc(rsize, a);

    // Generate Code which will: 
    //  1. Copy the final value (on stack) into value
    //  2. Return to C
#if ABI == SYSTEM_V_64
    // memcpy (dest = rdi, src = rsi, size = rdx)
    // retval = rax
    if (rsize != 0) {
        build_binary_op(ass, Mov, reg(RDI), imm64((int64_t)value), a, point);
        build_binary_op(ass, Mov, reg(RSI), reg(RSP), a, point);
        build_binary_op(ass, Mov, reg(RDX), imm64((int64_t)rsize), a, point);

        build_binary_op(ass, Mov, reg(RCX), imm64((int64_t)&memcpy), a, point);
        build_unary_op(ass, Call, reg(RCX), a, point);
        // pop value from stack
        build_binary_op(ass, Add, reg(RSP), imm32(rsize), a, point);
    }
    build_nullary_op(ass, Ret, a, point);

#elif ABI == WIN_64
    // memcpy (dest = rcx, src = rdx, size = r8)
    // retval = rax
    if (rsize != 0) {
        build_binary_op(ass, Mov, reg(RCX), imm64((int64_t)value), a, point);
        build_binary_op(ass, Mov, reg(RDX), reg(RSP), a, point);
        build_binary_op(ass, Mov, reg(R8), imm64((int64_t)rsize), a, point);
        build_binary_op(ass, Sub, reg(RSP), imm32(32), a, point);

        build_binary_op(ass, Mov, reg(RAX), imm64((int64_t)&memcpy), a, point);
        build_unary_op(ass, Call, reg(RAX), a, point);
        // pop value from stack
        build_binary_op(ass, Add, reg(RSP), imm32(rsize + 32), a, point);
    }
    build_nullary_op(ass, Ret, a, point);
#else
#error "Unknown calling convention"
#endif

    U8Array instructions = get_instructions(ass);

    int64_t out;
    __asm__ __volatile__(
        "push %%rbp       \n\t"
        "mov %%rsp, %%rbp \n\t"
        "sub $0x8, %%rbp  \n\t"
        "call *%1         \n\t"
        "pop %%rbp        \n\t"
        : "=r" (out)
        : "r"(instructions.data));

    return value;
}

Document* pretty_res(EvalResult res, Allocator* a) {
    Document* out = NULL;
    switch (res.type) {
    case ERDef: {
        PtrArray docs = mk_ptr_array(4, a);
        push_ptr(mk_str_doc(mv_string("Defined "), a), &docs);
        push_ptr(mk_str_doc(*symbol_to_string(res.def.name), a), &docs);
        push_ptr(mk_str_doc(mv_string(" : "), a), &docs);
        push_ptr(pretty_type(res.def.type, a), &docs);
        out = mv_cat_doc(docs, a);
        break;
    }
    case ERValue:
        out = pretty_pi_value(res.val.val, res.val.type, a);
        break;
    default:
        out = mk_str_doc(mv_string("Invalid Result!"), a);
    }
    return out;
}
