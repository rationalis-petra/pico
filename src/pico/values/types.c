#include "pico/values/types.h"
#include "pico/values/values.h"

void delete_pi_ptr(void* t, allocator a) {
    delete_pi_type(*(pi_type*)t, a);
    mem_free(t, a);
}

void delete_pi_type(pi_type t, allocator a) {
    switch(t.sort) {
    case TProc:
        delete_pi_ptr(t.proc.ret, a);
        delete_ptr_array(t.proc.args, &delete_pi_ptr, a);
        break;

    case TUVar:
        if (t.uvar->subst != NULL) {
            delete_pi_type(*t.uvar->subst, a);
        }
        mem_free(t.uvar, a);
        break;
    case TPrim:
        // Do nothing!
        break;
            
    }
}

document* pretty_type(pi_type* type, allocator a) {
    document* out = NULL;
    switch (type->sort) {
    case TProc: {

        ptr_array nodes = mk_ptr_array(4 + type->proc.args.len, a);
        push_ptr(mv_str_doc((mk_string("(Proc (", a)), a), &nodes, a);
        for (size_t i = 0; i < type->proc.args.len; i++) {
            document* arg = pretty_type(aref_ptr(i, type->proc.args), a);
            push_ptr(arg, &nodes, a);
        }
        push_ptr(mv_str_doc((mk_string(")", a)), a), &nodes, a);
        push_ptr(pretty_type(type->proc.ret, a), &nodes, a);
        push_ptr(mv_str_doc((mk_string(")", a)), a), &nodes, a);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case TUVar:
        out = mv_str_doc(mk_string("No Print UVar!", a), a);
        break;
    case TPrim:
        switch (type->prim) {
        case Int_64: 
            out = mv_str_doc(mk_string("I64", a), a);
            break;
        case TFormer: 
            out = mv_str_doc(mk_string("Former", a), a);
            break;
            break;
        }
        break;
    }
    return out;
}

size_t pi_size_of(pi_type type) {
    switch (type.sort) {
    case TPrim:
        switch (type.prim) {
        case Int_64:
            return sizeof(uint64_t);
        case TFormer:
            return sizeof(pi_term_former_t);
        }
        return sizeof(uint64_t);
    case TProc:
        return 0;
    case TUVar:
        return 0;
    default:
        return 0;
    }
}

struct uvar_generator {
    uint64_t counter;
};

pi_type* mk_uvar(uvar_generator* gen, allocator a) {
    pi_type* uvar = mem_alloc(sizeof(pi_type), a);
    uvar->sort = TUVar;
    uvar->uvar = mem_alloc(sizeof(uvar_type), a) ;
    uvar->uvar->id = gen->counter++;
    
    return uvar;
}

uvar_generator* mk_gen(allocator a) {
    uvar_generator* gen = mem_alloc(sizeof(uvar_generator), a);
    gen->counter = 0;
    return gen;
}

void delete_gen(uvar_generator* gen, allocator a) {
    mem_free(gen, a);
}
