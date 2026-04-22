#include "platform/machine_info.h"
#include "platform/signals.h"

#include "pico/eval/call.h"
#include "pico/values/types.h"
#include "pico/stdlib/platform/submodules.h"
#include "pico/codegen/codegen.h"

EvalResult pico_run_toplevel(TopLevel top, EvalCtx ctx) {
    EvalResult res;
    switch (top.type) {
    case TLExpr: {
        size_t sz = pi_size_of(*get_type(top.expr, ctx.tape));
        res.type = ERValue;
        res.val.type = get_type(top.expr, ctx.tape);
        res.val.val = pico_run_expr(ctx.target, sz, ctx.a, ctx.point);
        break;
    }
    case TLImport: {
        res.type = ERImport;
        res.imported = top.import.clauses;
        for (size_t i = 0; i < top.import.clauses.len; i++) {
            add_import_clause(top.import.clauses.data[i], ctx.module);
        }
        break;
    }
    case TLDecl: {
        res.type = ERDecl;
        for (size_t i = 0; i < top.decl.decls.len; i++) {
            ModuleDecl* decl = top.decl.decls.data[i];
            add_decl(ctx.module, top.decl.bind, *decl); 
        }
        break;
    }
    case TLDef: {
        // copy into module
        res = (EvalResult) {
            .type = ERDef,
            .def.name = top.def.bind,
            .def.type = get_type(top.def.value, ctx.tape),
        };

        Segments def_segments = (Segments) {
            .code = get_instructions(ctx.target.code_aux),
            .data = *ctx.target.data_aux,
        };

        PiType* type = get_type(top.def.value, ctx.tape);
        def_segments = prep_target(ctx.module, def_segments, ctx.target.target, &ctx.links);
        void* val = pico_run_expr(ctx.target, pi_size_of(*type), ctx.a, ctx.point);
        add_def(ctx.module, top.def.bind, *type, val, def_segments, &ctx.links);
    }
    }

    return res;
}

void* pico_run_expr(Target target, size_t rsize, Allocator* a, ErrorPoint* point) {
    void* dvars = get_dynamic_memory();
    void* dynamic_memory = mem_alloc(4096, a);
    void* vstack_memory_space = mem_alloc(4096, a);
    void* vstack_memory_ptr = vstack_memory_space + 4096; 

    PiAllocator new_temp_alloc = convert_to_pallocator(a);
    PiAllocator old_temp_alloc = set_std_temp_allocator(new_temp_alloc);
    typedef void*(*RunExpression)(void*, void*, void*, void*); 
    RunExpression run = (RunExpression)get_instructions(target.target).data;

    void* out = mem_alloc(rsize, a);

#ifdef DEBUG_ASSERT
    void* new_dmp = run(out, vstack_memory_ptr, dvars, dynamic_memory);
    if (new_dmp != vstack_memory_ptr) {
        panic(mv_string("Variable Stack Head (R14) register constraint violated."));
    }
#else
    run(out, vstack_memory_ptr, dvars, dynamic_memory);
#endif

    mem_free(vstack_memory_space, a);
    mem_free(dynamic_memory, a);
    set_std_temp_allocator(old_temp_alloc);

    return out;
}

void call_unit_fn(void *function, Allocator *a) {
    void* dvars = get_dynamic_memory();
    void* dynamic_memory = mem_alloc(4096, a);
    void* vstack_memory = mem_alloc(4096, a);
    void* vstack_memory_ptr = vstack_memory + 4095; 

    // TODO: swap so this is backend independent (use foreign_adapters to call) 
#if ARCH == AMD64
    int64_t out;
    __asm__ __volatile__(
                         // save nonvolatile registers
                         "push %%rbp       \n" // Nonvolatile on System V + Win64
                         "push %%rbx       \n" // Nonvolatile on System V + Win64
                         "push %%rdi       \n" // Nonvolatile on Win 64
                         "push %%rsi       \n" // Nonvolatile on Win 64
                         "push %%r15       \n" // for dynamic vars
                         "push %%r14       \n" // for dynamic memory space
                         "push %%r13       \n" // for control/indexing memory space
                         "push %%r12       \n" // Nonvolatile on System V + Win64

                         "mov %4, %%r13    \n"
                         "mov %3, %%r12    \n"
                         "mov %2, %%r14    \n"
                         "mov %2, %%r15    \n"

                         // First store the function to call in RAX, in case
                         // the compiler has stored it as an offset to rbp
                         // then we can set rbp and call
                         "mov %1, %%rax \n"
                         "mov %%rsp, %%rbp \n"

                         // Call function, this should consume 'Array' from the stack and push
                         // 'Raw Syntax' onto the stack
                         "call *%%rax         \n"

                         "pop %%r12        \n"
                         "pop %%r13        \n"
                         "pop %%r14        \n"
                         "pop %%r15        \n"
                         "pop %%rsi        \n" 
                         "pop %%rdi        \n" 
                         "pop %%rbx        \n"
                         "pop %%rbp        \n"
                         : "=r" (out)

                         : "r" (function)
                           , "r" (vstack_memory_ptr)
                           , "r" (dynamic_memory)
                           , "r" (dvars)
                           // Clobbers are either registers we change (output cannot be trusted)
                           // or registers we don't want compiler to assign to input values
                         : "rax", "r13", "r14", "r15");
#elif ARCH == AARCH64
    panic(mv_string("not implemented: unit call for aarch64"));
#else
    #error "Unsupported ARCH"
#endif
}

Document* pretty_res(EvalResult res, Allocator* a) {
    Document* out = NULL;
    switch (res.type) {
    case ERDef: {
        PtrArray docs = mk_ptr_array(4, a);
        push_ptr(mk_str_doc(mv_string("Defined "), a), &docs);
        push_ptr(mk_str_doc(symbol_to_string(res.def.name, a), a), &docs);
        push_ptr(mk_str_doc(mv_string(" : "), a), &docs);
        PrettyTypeParams ptp = default_ptp; 
        ptp.show_named = true;
        push_ptr(pretty_type(res.def.type, ptp, a), &docs);
        out = mv_cat_doc(docs, a);
        break;
    }
    case ERImport: {
        PtrArray docs = mk_ptr_array(res.imported.len + 1, a);
        push_ptr(mk_str_doc(mv_string("Opened:"), a), &docs);
        for (size_t i = 0; i < res.imported.len; i++) {
            ImportClause clause = res.imported.data[i];
            push_ptr(pretty_import_clause(clause, a), &docs);
        }
        out = mv_sep_doc(docs, a);
        break;
    }
    case ERValue: {
        PrettyValParams pvp = default_pvp;
        pvp.type.show_named = true;
        out = pretty_pi_value(res.val.val, res.val.type, pvp, a);
        break;
    }
    case ERDecl:
        out = mk_str_doc(mv_string("Declared: "), a);
        break;
    default:
        out = mk_str_doc(mv_string("Invalid Result!"), a);
    }
    return out;
}
