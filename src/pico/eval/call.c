#include <stdio.h>
#include <string.h> // for memcpy

#include "platform/machine_info.h"

#include "pico/eval/call.h"
#include "pico/values/types.h"
#include "pico/stdlib/extra.h"
#include "pico/codegen/codegen.h"

EvalResult pico_run_toplevel(TopLevel top, Target target, LinkData links, Module* module, Allocator* a, ErrorPoint* point) {
    EvalResult res;
    switch (top.type) {
    case TLExpr: {
        PiType indistinct_type = *top.expr->ptype;
        while (indistinct_type.sort == TDistinct) { indistinct_type = *indistinct_type.distinct.type; }
        size_t sz = pi_size_of(indistinct_type);

        res.type = ERValue;
        res.val.type = top.expr->ptype;
        res.val.val = pico_run_expr(target, sz, a, point);
        break;
    }
    case TLImport: {
        res.type = EROpen;
        res.opened = top.open.paths;
        for (size_t i = 0; i < top.open.paths.len; i++) {
            ImportClause clause = (ImportClause) {
                .type = Import,
                .path = *(SymbolArray*)top.open.paths.data[i],
            };
            add_import_clause(clause, module);
        }
        break;
    }
    case TLDecl: {
        res.type = ERDecl;
        for (size_t i = 0; i < top.decl.decls.len; i++) {
            ModuleDecl* decl = top.decl.decls.data[i];
            add_decl(module, top.decl.bind, *decl); 
        }
        break;
    }
    case TLDef: {
        // copy into module
        res = (EvalResult) {
            .type = ERDef,
            .def.name = top.def.bind,
            .def.type = top.def.value->ptype,
        };

        Segments def_segments = (Segments) {
            .code = get_instructions(target.code_aux),
            .data = *target.data_aux,
        };

        def_segments = prep_target(module, def_segments, target.target, &links);
        void* val = pico_run_expr(target, pi_size_of(*top.def.value->ptype), a, point);
        add_def(module, top.def.bind, *top.def.value->ptype, val, def_segments, &links);
    }
    }

    return res;
}

// TODO: current implementation destructively modifies the assembly of the expression. probaly want a better solution in the future
void* pico_run_expr(Target target, size_t rsize, Allocator* a, ErrorPoint* point) {
    Assembler* ass = target.target;
    /* result generated; */
    /* generated.type = Ok; */
    void* value = mem_alloc(rsize, a);

    // Generate Code which will: 
    //  1. Copy the final value (on stack) into the memory allocated into the
    //     'value' variable (above)
    //  2. Return to C
#if ABI == SYSTEM_V_64
    // memcpy (dest = rdi, src = rsi, size = rdx)
    // retval = rax
    if (rsize != 0) {
        build_binary_op(ass, Mov, reg(RDI, sz_64), imm64((int64_t)value), a, point);
        build_binary_op(ass, Mov, reg(RSI, sz_64), reg(RSP, sz_64), a, point);
        build_binary_op(ass, Mov, reg(RDX, sz_64), imm64((int64_t)rsize), a, point);

        build_binary_op(ass, Mov, reg(RCX, sz_64), imm64((int64_t)&memcpy), a, point);
        build_unary_op(ass, Call, reg(RCX, sz_64), a, point);
        // pop value from stack
        build_binary_op(ass, Add, reg(RSP, sz_64), imm32(pi_stack_align(rsize)), a, point);
    }
    build_nullary_op(ass, Ret, a, point);

#elif ABI == WIN_64
    // memcpy (dest = rcx, src = rdx, size = r8)
    // retval = rax
    if (rsize != 0) {
        build_binary_op(ass, Mov, reg(RCX, sz_64), imm64((int64_t)value), a, point);
        build_binary_op(ass, Mov, reg(RDX, sz_64), reg(RSP, sz_64), a, point);
        build_binary_op(ass, Mov, reg(R8, sz_64), imm64((int64_t)rsize), a, point);
        build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(32), a, point);

        build_binary_op(ass, Mov, reg(RAX, sz_64), imm64((int64_t)&memcpy), a, point);
        build_unary_op(ass, Call, reg(RAX, sz_64), a, point);
        // pop value from stack
        build_binary_op(ass, Add, reg(RSP, sz_64), imm32(pi_stack_align(rsize) + 32), a, point);
    }
    build_nullary_op(ass, Ret, a, point);
#else
#error "Unknown calling convention"
#endif

    U8Array instructions = get_instructions(ass);

    void* dvars = get_dynamic_memory();
    void* dynamic_memory_space = mem_alloc(4096, a);
    void* offset_memory_space = mem_alloc(1024, a);

    Allocator old_temp_alloc = set_std_temp_allocator(*a);

    int64_t out;
    __asm__ __volatile__(
        // NOTE: When updating to push more registers, make sure to also update assembly
        //       in abstraction.c
        "push %%rbp       \n" // Nonvolatile on System V + Win64
        "push %%rbx       \n" // Nonvolatile on System V + Win64
        "push %%rdi       \n" // Nonvolatile on Win 64
        "push %%rsi       \n" // Nonvolatile on Win 64
        "push %%r15       \n" // for dynamic vars
        "push %%r14       \n" // for dynamic memory space
        "push %%r13       \n" // for control/indexing memory space
        "push %%r12       \n" // Nonvolatile on System V + Win64
        "mov %2, %%r15    \n"
        "mov %3, %%r14    \n"
        "mov %4, %%r13    \n"
        "mov %%rsp, %%rbp \n"
        "sub $0x8, %%rbp  \n" // Do this to align RSP & RBP? Possibly to account
                              // for value on stack?
        "call *%1         \n"
        "pop %%r12        \n"
        "pop %%r13        \n"
        "pop %%r14        \n"
        "pop %%r15        \n"
        "pop %%rsi        \n"
        "pop %%rdi        \n"
        "pop %%rbx        \n"
        "pop %%rbp        \n"
        : "=r"(out)

        : "r"(instructions.data), "r"(dvars), "r"(dynamic_memory_space), "r"(offset_memory_space));

    set_std_temp_allocator(old_temp_alloc);
    mem_free(dynamic_memory_space, a);
    return value;
}

Document* pretty_res(EvalResult res, Allocator* a) {
    Document* out = NULL;
    switch (res.type) {
    case ERDef: {
        PtrArray docs = mk_ptr_array(4, a);
        push_ptr(mk_str_doc(mv_string("Defined "), a), &docs);
        push_ptr(mk_str_doc(symbol_to_string(res.def.name, a), a), &docs);
        push_ptr(mk_str_doc(mv_string(" : "), a), &docs);
        push_ptr(pretty_type(res.def.type, a), &docs);
        out = mv_cat_doc(docs, a);
        break;
    }
    case EROpen: {
        PtrArray docs = mk_ptr_array(res.opened.len + 1, a);
        push_ptr(mk_str_doc(mv_string("Opened:"), a), &docs);
        for (size_t i = 0; i < res.opened.len; i++) {
            SymbolArray* syms = res.opened.data[i];
            PtrArray elts = mk_ptr_array(2 * syms->len, a);
            for (size_t j = 0; j < syms->len; j++) {
                push_ptr(mk_str_doc(symbol_to_string(syms->data[j], a), a), &elts);
                if (j + 1 != syms->len) {
                    push_ptr(mk_str_doc(mv_string("."), a), &elts);
                }
            }
              
            push_ptr(mv_cat_doc(elts, a), &docs);
        }
        out = mv_sep_doc(docs, a);
        break;
    }
    case ERValue:
        out = pretty_pi_value(res.val.val, res.val.type, a);
        break;
    case ERDecl:
        out = mk_str_doc(mv_string("Declared: "), a);
        break;
    default:
        out = mk_str_doc(mv_string("Invalid Result!"), a);
    }
    return out;
}
