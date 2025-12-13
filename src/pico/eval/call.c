#include "platform/signals.h"

#include "pico/eval/call.h"
#include "pico/values/types.h"
#include "pico/stdlib/platform/submodules.h"
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
        res.type = ERImport;
        res.imported = top.import.clauses;
        for (size_t i = 0; i < top.import.clauses.len; i++) {
            add_import_clause(top.import.clauses.data[i], module);
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

void* pico_run_expr(Target target, size_t rsize, Allocator* a, ErrorPoint* point) {
    void* dvars = get_dynamic_memory();
    void* dynamic_memory_space = mem_alloc(4096, a);
    void* dynamic_memory_ptr = dynamic_memory_space + 4096; 

    PiAllocator new_temp_alloc = convert_to_pallocator(a);
    PiAllocator old_temp_alloc = set_std_temp_allocator(new_temp_alloc);
    typedef void*(*RunExpression)(void*, void*, void*); 
    RunExpression run = (RunExpression)get_instructions(target.target).data;

    void* out = mem_alloc(rsize, a);

#ifdef DEBUG_ASSERT
    void* new_dmp = run(out, dynamic_memory_ptr, dvars);
    if (new_dmp != dynamic_memory_ptr) {
        panic(mv_string("Variable Stack Head (R14) register constraint violated."));
    }
#else
    run(out, dynamic_memory_ptr, dvars);
#endif

    mem_free(dynamic_memory_space, a);
    set_std_temp_allocator(old_temp_alloc);

    return out;
}

void call_unit_fn(void *function, Allocator *a) {
    void* dvars = get_dynamic_memory();
    void* dynamic_memory = mem_alloc(4096, a);
    void* dynamic_memory_ptr = dynamic_memory + 4095; 

    // TODO: swap so this is backend independent (use foreign_adapters to call) 
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

                         "mov %3, %%r13    \n"
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
                           , "r" (dynamic_memory_ptr)
                           , "r" (dvars)
                           // Clobbers are either registers we change (output cannot be trusted)
                           // or registers we don't want compiler to assign to input values
                         : "rax", "r13", "r14", "r15");
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
