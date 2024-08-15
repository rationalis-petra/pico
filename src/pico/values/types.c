#include "pretty/standard_types.h"
#include "pico/values/types.h"
#include "pico/values/values.h"


void symbol_nod(pi_symbol s, allocator a) { }

void delete_pi_type_p(pi_type* t, allocator a) {
    delete_pi_type(*t, a);
    mem_free(t, a);
}

void delete_pi_type(pi_type t, allocator a) {
    typedef void(*deleter)(void*, allocator a);
    switch(t.sort) {
    case TProc:
        delete_pi_type_p(t.proc.ret, a);
        delete_ptr_array(t.proc.args, (deleter)&delete_pi_type_p, a);
        break;
    case TStruct:
        delete_sym_ptr_amap(t.structure.fields, &symbol_nod, (deleter)&delete_pi_type_p, a);
        break;
    case TEnum:
        delete_sym_ptr_amap(t.structure.fields, &symbol_nod, (deleter)&delete_pi_type_p, a);
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

void* copy_enum_variant(void* v, allocator a) {
    ptr_array* out = mem_alloc(sizeof(ptr_array), a);
    *out = copy_ptr_array(*(ptr_array*)v, (void*(*)(void*, allocator))&copy_pi_type_p, a);
    return out;
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
    case TEnum:
        out.enumeration.variants = copy_sym_ptr_amap(t.enumeration.variants, symbol_id, (void*(*)(void*, allocator)) copy_enum_variant, a);
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

document* pretty_pi_value(void* val, pi_type* type, allocator a) {
    document* out = NULL;
    switch (type->sort) {
    case TProc: {
        out = mk_str_doc(mv_string("#<proc>"), a);
        break;
    }
    case TUVar:
        out = mk_str_doc(mv_string("No Print UVar!"), a);
        break;
    case TPrim:
        switch (type->prim) {
        case Bool:  {
            uint64_t* uival = (uint64_t*) val;
            if (uival == 0) {
                out = mk_str_doc(mv_string(":false"), a);
            } else {
                out = mk_str_doc(mv_string(":true"), a);
            }
            break;
        }
        case Int_64: {
            int64_t* uival = (int64_t*) val;
            out =  pretty_i64(*uival, a);
            break;
        }
        case TFormer:  {
            pi_term_former_t* pformer = (pi_term_former_t*) val;
            out = pretty_former(*pformer, a);
            break;
        }
        case TType:  {
            pi_type** ptype = (pi_type**) val;
            out = pretty_type(*ptype, a);
            break;
        }
        }
        break;
    case TStruct: {
        size_t current_offset = 0;

        ptr_array nodes = mk_ptr_array(2 + type->structure.fields.len, a);
        push_ptr(mv_str_doc((mk_string("(struct ", a)), a), &nodes, a);
        for (size_t i = 0; i < type->structure.fields.len; i++) {
            ptr_array fd_nodes = mk_ptr_array(4, a);
            document* pre = mk_str_doc(mv_string("[."), a);
            document* fname = mk_str_doc(*symbol_to_string(type->structure.fields.data[i].key), a);
            pi_type* ftype = type->structure.fields.data[i].val;

            document* arg = pretty_pi_value(val + current_offset, ftype, a);
            document* post = mk_str_doc(mv_string("]"), a);

            push_ptr(pre,   &fd_nodes, a);
            push_ptr(fname, &fd_nodes, a);
            push_ptr(arg,   &fd_nodes, a);
            push_ptr(post,  &fd_nodes, a);
            document* fd_doc = mv_sep_doc(fd_nodes, a);

            push_ptr(fd_doc, &nodes, a);
            current_offset += pi_size_of(*ftype);
        }
        push_ptr(mv_str_doc((mk_string(")", a)), a), &nodes, a);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case TEnum: {
        size_t current_offset = 0;

        ptr_array nodes = mk_ptr_array(2 + type->structure.fields.len, a);
        push_ptr(mv_str_doc((mk_string("(struct ", a)), a), &nodes, a);
        for (size_t i = 0; i < type->structure.fields.len; i++) {
            ptr_array fd_nodes = mk_ptr_array(4, a);
            document* pre = mk_str_doc(mv_string("[."), a);
            document* fname = mk_str_doc(*symbol_to_string(type->structure.fields.data[i].key), a);
            pi_type* ftype = type->structure.fields.data[i].val;

            document* arg = pretty_pi_value(val + current_offset, ftype, a);
            document* post = mk_str_doc(mv_string("]"), a);

            push_ptr(pre,   &fd_nodes, a);
            push_ptr(fname, &fd_nodes, a);
            push_ptr(arg,   &fd_nodes, a);
            push_ptr(post,  &fd_nodes, a);
            document* fd_doc = mv_sep_doc(fd_nodes, a);

            push_ptr(fd_doc, &nodes, a);
            current_offset += pi_size_of(*ftype);
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
    case TEnum: {
        ptr_array nodes = mk_ptr_array(2 + type->enumeration.variants.len, a);
        push_ptr(mv_str_doc((mk_string("(Enum ", a)), a), &nodes, a);
        for (size_t i = 0; i < type->structure.fields.len; i++) {
            ptr_array var_nodes = mk_ptr_array(4, a);
            document* pre = mk_str_doc(mv_string("[:"), a);
            document* fname = mk_str_doc(*symbol_to_string(type->enumeration.variants.data[i].key), a);

            ptr_array* types = type->structure.fields.data[i].val;
            ptr_array ty_nodes = mk_ptr_array(types->len, a);
            for (size_t j = 0; j < types->len; j++) {
                document* arg = pretty_type((pi_type*)types->data[j], a);
                push_ptr(arg, &ty_nodes, a);
            }
            document* ptypes = mv_sep_doc(ty_nodes, a);
            document* post = mk_str_doc(mv_string("]"), a);

            push_ptr(pre,   &var_nodes, a);
            push_ptr(fname, &var_nodes, a);
            push_ptr(ptypes,   &var_nodes, a);
            push_ptr(post,  &var_nodes, a);
            document* var_doc = mv_sep_doc(var_nodes, a);

            push_ptr(var_doc, &nodes, a);
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
            return sizeof(uint64_t); //sizeof(pi_term_former_t);
        case TType:
            //return sizeof(pi_type);
            return sizeof(void*);
        }
        return sizeof(uint64_t);
    case TProc:
        return sizeof(uint64_t);
    case TStruct: {
        size_t total = 0; 
        for (size_t i = 0; i < type.structure.fields.len; i++)
            total += pi_size_of(*(pi_type*)type.structure.fields.data[i].val);
        return total;
    }
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

