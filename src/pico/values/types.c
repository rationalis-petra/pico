#include "pico/values/types.h"
#include "pico/values/values.h"

void delete_pi_ptr(void* t, allocator a) {
    delete_pi_type(*(pi_type*)t, a);
    mem_free(t, a);
}

void symbol_nod(pi_symbol s, allocator a) { }

void delete_pi_type_p(pi_type* t, allocator a) {
    delete_pi_type(*t, a);
    mem_free(t, a);
}

void delete_pi_type(pi_type t, allocator a) {
    switch(t.sort) {
    case TProc:
        delete_pi_ptr(t.proc.ret, a);
        delete_ptr_array(t.proc.args, &delete_pi_ptr, a);
        break;
    case TStruct:
        delete_sym_ptr_amap(t.structure.fields, &symbol_nod, &delete_pi_ptr, a);
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

pi_type* copy_pi_type_p(pi_type* t, allocator a)  {
    pi_type* out = mem_alloc(sizeof(pi_type), a);
    *out = copy_pi_type(*t, a);
    return out;
}

pi_symbol symbol_id(pi_symbol s, allocator a) {
    return s;
}

pi_type copy_pi_type(pi_type t, allocator a) {
    pi_type out;
    out.sort = t.sort;
    switch(t.sort) {
    case TProc:
        out.proc.ret = copy_pi_type_p(t.proc.ret, a);
        out.proc.args = copy_ptr_array(t.proc.args, (void*(*)(void*, allocator)) copy_pi_type_p, a);
        break;
    case TStruct:
        out.structure.fields = copy_sym_ptr_amap(t.structure.fields, symbol_id, (void*(*)(void*, allocator)) copy_pi_type_p, a);
        break;

    case TUVar:
        out.uvar = mem_alloc(sizeof(uvar_type), a);
        out.uvar->id = t.uvar->id; 
        if (t.uvar->subst) {
            out.uvar->subst = copy_pi_type_p(t.uvar->subst, a);
        } else {
            out.uvar->subst = NULL;
        }
        break;
    case TPrim:
        out.prim = t.prim;
        break;
            
    }

    return out;
}

document* pretty_type(pi_type* type, allocator a) {
    document* out = NULL;
    switch (type->sort) {
    case TProc: {
        ptr_array nodes = mk_ptr_array(4 + type->proc.args.len, a);
        push_ptr(mv_str_doc((mk_string("(Proc (", a)), a), &nodes, a);
        for (size_t i = 0; i < type->proc.args.len; i++) {
            document* arg = pretty_type(type->proc.args.data[i], a);
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
        case Bool: 
            out = mv_str_doc(mk_string("Bool", a), a);
            break;
        case Int_64: 
            out = mv_str_doc(mk_string("I64", a), a);
            break;
        case TFormer: 
            out = mv_str_doc(mk_string("Former", a), a);
            break;
        case TType: 
            out = mv_str_doc(mk_string("Type", a), a);
            break;
        }
        break;
    case TStruct: {
        ptr_array nodes = mk_ptr_array(2 + type->structure.fields.len, a);
        push_ptr(mv_str_doc((mk_string("(Struct ", a)), a), &nodes, a);
        for (size_t i = 0; i < type->structure.fields.len; i++) {
            ptr_array fd_nodes = mk_ptr_array(4, a);
            document* pre = mk_str_doc(mv_string("[."), a);
            document* fname = mk_str_doc(*symbol_to_string(type->structure.fields.data[i].key), a);
            document* arg = pretty_type(type->structure.fields.data[i].val, a);
            document* post = mk_str_doc(mv_string("]"), a);

            push_ptr(pre,   &fd_nodes, a);
            push_ptr(fname, &fd_nodes, a);
            push_ptr(arg,   &fd_nodes, a);
            push_ptr(post,  &fd_nodes, a);
            document* fd_doc = mv_sep_doc(fd_nodes, a);

            push_ptr(fd_doc, &nodes, a);
        }
        push_ptr(mv_str_doc((mk_string(")", a)), a), &nodes, a);
        out = mv_sep_doc(nodes, a);
        break;
    }
    default:
        out = mk_str_doc(mv_string("Error printing type: unrecognised sort."), a);
        break;
    }
    return out;
}

size_t pi_size_of(pi_type type) {
    switch (type.sort) {
    case TPrim:
        switch (type.prim) {
        case Bool:
            return sizeof(uint64_t);
        case Int_64:
            return sizeof(uint64_t);
        case TFormer:
            return sizeof(pi_term_former_t);
        case TType:
            return sizeof(pi_type);
        }
        return sizeof(uint64_t);
    case TProc:
        return sizeof(uint64_t);
    case TUVar:
        return 0;
    default:
        return 0;
    }
}

pi_type mk_prim_type(prim_type t) {
    pi_type type;
    type.sort = TPrim;
    type.prim = t;
    return type;
}

struct uvar_generator {
    uint64_t counter;
};

pi_type* mk_uvar(uvar_generator* gen, allocator a) {
    pi_type* uvar = mem_alloc(sizeof(pi_type), a);
    uvar->sort = TUVar;
    uvar->uvar = mem_alloc(sizeof(uvar_type), a) ;
    uvar->uvar->subst = NULL;
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

