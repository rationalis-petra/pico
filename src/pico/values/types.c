#include "pretty/standard_types.h"
#include "pico/values/types.h"
#include "pico/values/values.h"

struct UVarGenerator {
    uint64_t counter;
};

void symbol_nod(Symbol s, Allocator a) { }

void delete_pi_type_p(PiType* t, Allocator* a) {
    delete_pi_type(*t, a);
    mem_free(t, a);
}

void delete_enum_variant_p(PtrArray* t, Allocator* a) {
    for (size_t i = 0; i < t->len; i++)
        delete_pi_type_p(t->data[i], a);
    sdelete_ptr_array(*t);
}

void delete_pi_type(PiType t, Allocator* a) {
    typedef void(*deleter)(void*, Allocator* a);
    switch(t.sort) {
    case TProc: {
        delete_pi_type_p(t.proc.ret, a);
        for (size_t i = 0; i < t.proc.args.len; i++)
            delete_pi_type_p(t.proc.args.data[i], a);
        sdelete_ptr_array(t.proc.args);
        break;
    }
    case TStruct: {
        for (size_t i = 0; i < t.structure.fields.len; i++)
            delete_pi_type_p(t.structure.fields.data[i].val, a);
        sdelete_sym_ptr_amap(t.structure.fields);
        break;
    }
    case TEnum:
        for (size_t i = 0; i < t.enumeration.variants.len; i++)
            delete_enum_variant_p(t.enumeration.variants.data[i].val, a);
        sdelete_sym_ptr_amap(t.enumeration.variants);
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

PiType* copy_pi_type_p(PiType* t, Allocator* a)  {
    PiType* out = mem_alloc(sizeof(PiType), a);
    *out = copy_pi_type(*t, a);
    return out;
}

Symbol symbol_id(Symbol s, Allocator* a) {
    return s;
}

void* copy_enum_variant(void* v, Allocator* a) {
    PtrArray* out = mem_alloc(sizeof(PtrArray), a);
    *out = copy_ptr_array(*(PtrArray*)v, (void*(*)(void*, Allocator*))&copy_pi_type_p, a);
    return out;
}

PiType copy_pi_type(PiType t, Allocator* a) {
    typedef void* (*TyCopier)(void*, Allocator*);

    PiType out;
    out.sort = t.sort;
    switch(t.sort) {
    case TProc:
        out.proc.ret = copy_pi_type_p(t.proc.ret, a);
        out.proc.args = copy_ptr_array(t.proc.args,  (TyCopier)copy_pi_type_p, a);
        break;
    case TStruct:
        out.structure.fields = copy_sym_ptr_amap(t.structure.fields, symbol_id, (TyCopier)copy_pi_type_p, a);
        break;
    case TEnum:
        out.enumeration.variants = copy_sym_ptr_amap(t.enumeration.variants, symbol_id, (TyCopier)copy_enum_variant, a);
        break;

    case TUVar:
        out.uvar = mem_alloc(sizeof(UVarType), a);
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

Document* pretty_pi_value(void* val, PiType* type, Allocator* a) {
    Document* out = NULL;
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
            if (*uival == 0) {
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
            TermFormer* pformer = (TermFormer*) val;
            out = pretty_former(*pformer, a);
            break;
        }
        case TType:  {
            PiType** ptype = (PiType**) val;
            out = pretty_type(*ptype, a);
            break;
        }
        }
        break;
    case TStruct: {
        size_t current_offset = 0;

        PtrArray nodes = mk_ptr_array(2 + type->structure.fields.len, a);
        push_ptr(mv_str_doc((mk_string("(struct ", a)), a), &nodes);
        for (size_t i = 0; i < type->structure.fields.len; i++) {
            PtrArray fd_nodes = mk_ptr_array(4, a);
            Document* pre = mk_str_doc(mv_string("[."), a);
            Document* fname = mk_str_doc(*symbol_to_string(type->structure.fields.data[i].key), a);
            PiType* ftype = type->structure.fields.data[i].val;

            Document* arg = pretty_pi_value(val + current_offset, ftype, a);
            Document* post = mk_str_doc(mv_string("]"), a);

            push_ptr(pre,   &fd_nodes);
            push_ptr(fname, &fd_nodes);
            push_ptr(arg,   &fd_nodes);
            push_ptr(post,  &fd_nodes);
            Document* fd_doc = mv_sep_doc(fd_nodes, a);

            push_ptr(fd_doc, &nodes);
            current_offset += pi_size_of(*ftype);
        }
        push_ptr(mv_str_doc((mk_string(")", a)), a), &nodes);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case TEnum: {
        uint64_t tagidx = *(uint64_t*)val;

        PtrArray variant_types = *(PtrArray*)type->enumeration.variants.data[tagidx].val;
        PtrArray nodes = mk_ptr_array(2 + variant_types.len, a);

        // Symbol 
        Symbol tagname = type->enumeration.variants.data[tagidx].key;
        push_ptr(mk_str_doc(string_cat(mv_string("[:"), *symbol_to_string(tagname), a), a), &nodes);

        size_t current_offset = sizeof(uint64_t); // Start after current tag
        for (size_t i = 0; i < variant_types.len; i++) {
            PiType* ftype = variant_types.data[i];
            Document* arg = pretty_pi_value(val + current_offset, ftype, a);
            push_ptr(arg, &nodes);
            current_offset += pi_size_of(*ftype);
        }
        push_ptr(mk_str_doc(mv_string("]"), a), &nodes);
        out = mv_sep_doc(nodes, a);
        break;
    }
    default:
        out = mk_str_doc(mv_string("Error printing type: unrecognised sort."), a);
        break;
    }
    return out;
}

Document* pretty_type(PiType* type, Allocator* a) {
    Document* out = NULL;
    switch (type->sort) {
    case TProc: {
        PtrArray nodes = mk_ptr_array(4 + type->proc.args.len, a);
        push_ptr(mv_str_doc((mk_string("(Proc (", a)), a), &nodes);
        for (size_t i = 0; i < type->proc.args.len; i++) {
            Document* arg = pretty_type(type->proc.args.data[i], a);
            push_ptr(arg, &nodes);
        }
        push_ptr(mv_str_doc((mk_string(")", a)), a), &nodes);
        push_ptr(pretty_type(type->proc.ret, a), &nodes);
        push_ptr(mv_str_doc((mk_string(")", a)), a), &nodes);
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
        PtrArray nodes = mk_ptr_array(2 + type->structure.fields.len, a);
        push_ptr(mv_str_doc((mk_string("(Struct ", a)), a), &nodes);
        for (size_t i = 0; i < type->structure.fields.len; i++) {
            PtrArray fd_nodes = mk_ptr_array(4, a);
            Document* pre = mk_str_doc(mv_string("[."), a);
            Document* fname = mk_str_doc(*symbol_to_string(type->structure.fields.data[i].key), a);
            Document* arg = pretty_type(type->structure.fields.data[i].val, a);
            Document* post = mk_str_doc(mv_string("]"), a);

            push_ptr(pre,   &fd_nodes);
            push_ptr(fname, &fd_nodes);
            push_ptr(arg,   &fd_nodes);
            push_ptr(post,  &fd_nodes);
            Document* fd_doc = mv_sep_doc(fd_nodes, a);

            push_ptr(fd_doc, &nodes);
        }
        push_ptr(mv_str_doc((mk_string(")", a)), a), &nodes);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case TEnum: {
        PtrArray nodes = mk_ptr_array(2 + type->enumeration.variants.len, a);
        push_ptr(mv_str_doc((mk_string("(Enum ", a)), a), &nodes);
        for (size_t i = 0; i < type->structure.fields.len; i++) {
            PtrArray var_nodes = mk_ptr_array(4, a);
            Document* pre = mk_str_doc(mv_string("[:"), a);
            Document* fname = mk_str_doc(*symbol_to_string(type->enumeration.variants.data[i].key), a);

            PtrArray* types = type->structure.fields.data[i].val;
            PtrArray ty_nodes = mk_ptr_array(types->len, a);
            for (size_t j = 0; j < types->len; j++) {
                Document* arg = pretty_type((PiType*)types->data[j], a);
                push_ptr(arg, &ty_nodes);
            }
            Document* ptypes = mv_sep_doc(ty_nodes, a);
            Document* post = mk_str_doc(mv_string("]"), a);

            push_ptr(pre,   &var_nodes);
            push_ptr(fname, &var_nodes);
            push_ptr(ptypes,   &var_nodes);
            push_ptr(post,  &var_nodes);
            Document* var_doc = mv_sep_doc(var_nodes, a);

            push_ptr(var_doc, &nodes);
        }
        push_ptr(mv_str_doc((mk_string(")", a)), a), &nodes);
        out = mv_sep_doc(nodes, a);
        break;
    }
    default:
        out = mk_str_doc(mv_string("Error printing type: unrecognised sort."), a);
        break;
    }
    return out;
}

size_t pi_size_of(PiType type) {
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
            total += pi_size_of(*(PiType*)type.structure.fields.data[i].val);
        return total;
    }
    case TEnum: {
        size_t max = 0; 
        for (size_t i = 0; i < type.enumeration.variants.len; i++) {
            size_t total = 0;
            PtrArray types = *(PtrArray*)type.enumeration.variants.data[i].val;
            for (size_t i = 0; i < types.len; i++)
                total += pi_size_of(*(PiType*)types.data[i]);

            if (total > max) {
                max = total;
            }
        }
        // Add 1 for tag!
        return max + sizeof(uint64_t);
    }
    case TUVar:
        return 0;
    default:
        return 0;
    }
}

PiType mk_prim_type(PrimType t) {
    return (PiType) {
      .sort = TPrim,
      .prim = t,
    };
}

PiType* mk_uvar(UVarGenerator* gen, Allocator* a) {
    PiType* uvar = mem_alloc(sizeof(PiType), a);
    uvar->sort = TUVar;
    uvar->uvar = mem_alloc(sizeof(UVarType), a) ;
    uvar->uvar->subst = NULL;
    uvar->uvar->id = gen->counter++;
    
    return uvar;
}

UVarGenerator* mk_gen(Allocator* a) {
    UVarGenerator* gen = mem_alloc(sizeof(UVarGenerator), a);
    gen->counter = 0;
    return gen;
}

void delete_gen(UVarGenerator* gen, Allocator* a) {
    mem_free(gen, a);
}

