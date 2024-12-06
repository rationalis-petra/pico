#include <stdio.h>
#include <string.h> // for memcpy

#include "platform/machine_info.h"
#include "platform/memory/arena.h"

#include "pico/eval/call.h"
#include "pico/values/types.h"
#include "pico/values/stdlib.h"

EvalResult pico_run_toplevel(TopLevel top, Assembler* ass, SymSArrAMap* backlinks, Module* module, Allocator* a, ErrorPoint* point) {
    EvalResult res;
    switch (top.type) {
    case TLExpr: {
        PiType indistinct_type = *top.expr->ptype;
        while (indistinct_type.sort == TDistinct) { indistinct_type = *indistinct_type.distinct.type; }
        size_t sz = pi_size_of(indistinct_type);

        if (indistinct_type.sort == TPrim
            || indistinct_type.sort == TStruct
            || indistinct_type.sort == TEnum
            || indistinct_type.sort == TDynamic
            || indistinct_type.sort == TKind) {
            res.type = ERValue;
            res.val.type = top.expr->ptype;
            res.val.val = pico_run_expr(ass, sz, a, point);
        }
        else  {
            throw_error(point, mv_string("Cannot evaluate values of this type."));
        }
        break;
    }
    case TLDef: {
        PiType indistinct_type = *top.def.value->ptype;
        while (indistinct_type.sort == TDistinct) { indistinct_type = *indistinct_type.distinct.type; }
        // copy into module
        res = (EvalResult) {
            .type = ERDef,
            .def.name = top.def.bind,
            .def.type = top.def.value->ptype,
        };
        switch (indistinct_type.sort) {
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

    void* dvars = get_dynamic_memory();
    void* dynamic_memory_space = mem_alloc(4096, a);

    Allocator* old_tmp_alloc = get_std_tmp_allocator();
    bind_std_tmp_allocator(a);

    int64_t out;
    __asm__ __volatile__(
        "push %%rbp       \n"
        "push %%r15       \n"
        "push %%r14       \n"
        "mov %3, %%r14    \n"
        "mov %2, %%r15    \n"
        "mov %%rsp, %%rbp \n"
        "sub $0x8, %%rbp  \n" // Do this to align RSP & RBP?
        "call *%1         \n"
        "pop %%r14        \n"
        "pop %%r15        \n"
        "pop %%rbp        \n"
        : "=r" (out)

        : "r" (instructions.data)
        , "r" (dvars)
        , "r"(dynamic_memory_space)) ;

    release_std_tmp_allocator(old_tmp_alloc);
    mem_free(dynamic_memory_space, a);
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
