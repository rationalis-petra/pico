#include "pretty/standard_types.h"
#include "pico/values/types.h"
#include "pico/values/values.h"

struct UVarGenerator {
    uint64_t counter;
};

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
void symbol_nod(Symbol s, Allocator a) { }
#pragma GCC diagnostic pop

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
    case TEnum: {
        for (size_t i = 0; i < t.enumeration.variants.len; i++)
            delete_enum_variant_p(t.enumeration.variants.data[i].val, a);
        sdelete_sym_ptr_amap(t.enumeration.variants);
        break;
    }
    case TAll:
    case TExists:
    case TCLam: {
        delete_pi_type_p(t.binder.body, a);
        break;
    }

    case TCApp: {
        delete_pi_type_p(t.app.fam, a);
        for (size_t i = 0; i < t.app.args.len; i++) {
            delete_pi_type_p(t.app.args.data[i], a);
        }
        sdelete_ptr_array(t.app.args);
        break;
    }
    case TVar: {
        // Do nothing; 
        break;
    }

    case TUVar:
        if (t.uvar->subst != NULL) {
            delete_pi_type(*t.uvar->subst, a);
        }
        mem_free(t.uvar, a);
        break;
    case TUVarDefaulted:
        if (t.uvar->subst != NULL) {
            delete_pi_type(*t.uvar->subst, a);
        }
        mem_free(t.uvar, a);
        break;
    case TPrim: break;

    case TKind: break;
    }
}

PiType* copy_pi_type_p(PiType* t, Allocator* a)  {
    PiType* out = mem_alloc(sizeof(PiType), a);
    *out = copy_pi_type(*t, a);
    return out;
}


#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
Symbol symbol_id(Symbol s, Allocator* a) {
    return s;
}
#pragma GCC diagnostic pop

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
    case TVar:
        out.var = t.var;
        break;
    case TExists:
    case TAll:
    case TCLam: {
        break;
    }
    case TCApp:
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
    case TUVarDefaulted:
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
    case TKind:
        out.kind = t.kind;
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
        case Unit:  {
            out = mk_str_doc(mv_string(":unit"), a);
            break;
        }
        case Bool:  {
            uint64_t* uival = (uint64_t*) val;
            if (*uival == 0) {
                out = mk_str_doc(mv_string(":false"), a);
            } else {
                out = mk_str_doc(mv_string(":true"), a);
            }
            break;
        }
        case Address: {
            void** addr = (void**) val;
            out = pretty_ptr(*addr, a);
            break;
        }
        case Int_64: {
            int64_t* uival = (int64_t*) val;
            out =  pretty_i64(*uival, a);
            break;
        }
        case Int_32: {
            int32_t* uival = (int32_t*) val;
            out =  pretty_i32(*uival, a);
            break;
        }
        case Int_16: {
            int16_t* uival = (int16_t*) val;
            out =  pretty_i16(*uival, a);
            break;
        }
        case Int_8: {
            int8_t* uival = (int8_t*) val;
            out =  pretty_i8(*uival, a);
            break;
        }
        case TFormer:  {
            TermFormer* pformer = (TermFormer*) val;
            out = pretty_former(*pformer, a);
            break;
        }
        default: {
            out = mk_str_doc(mv_string("Error printing Pico Value: unrecognized primitive"), a);
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
    case TVar: {
        out = mk_str_doc(*symbol_to_string(type->var), a);
        break;
    }
    case TAll:
    case TExists:
    case TCLam: {
        PtrArray nodes = mk_ptr_array(3, a);
        if (type->sort == TAll) push_ptr(mk_str_doc(mv_string("(All "), a), &nodes);
        else if (type->sort == TExists) push_ptr(mk_str_doc(mv_string("(Exists "), a), &nodes);
        else push_ptr(mk_str_doc(mv_string("(Fam "), a), &nodes);

        PtrArray args = mk_ptr_array(type->binder.vars.len + 2, a);
        push_ptr(mk_str_doc(mv_string("["), a), &args);
        for (size_t i = 0; i < type->binder.vars.len; i++) {
            push_ptr(mk_str_doc(*symbol_to_string(type->binder.vars.data[i]), a), &args);
        }
        push_ptr(mk_str_doc(mv_string("]"), a), &args);

        push_ptr(mv_sep_doc(args, a), &nodes);
        push_ptr(pretty_type(type->binder.body, a), &nodes);

        out = mv_sep_doc(nodes, a);
        break;
    }
    case TCApp: {
        PtrArray nodes = mk_ptr_array(3 + type->app.args.len, a);
        push_ptr(mk_str_doc(mv_string("("), a), &nodes);
        push_ptr(pretty_type(type->app.fam, a), &nodes);
        for (size_t i = 0; i < type->app.args.len; i++) {
            push_ptr(pretty_type(type->app.args.data[i], a), &nodes);
        }
        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case TKind:  {
        PiType** ptype = (PiType**) val;
        out = pretty_type(*ptype, a);
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
        case Unit: 
            out = mv_str_doc(mk_string("Unit", a), a);
            break;
        case Bool: 
            out = mv_str_doc(mk_string("Bool", a), a);
            break;
        case Address: 
            out = mv_str_doc(mk_string("Address", a), a);
            break;
        case Int_64: 
            out = mv_str_doc(mk_string("I64", a), a);
            break;
        case Int_32: 
            out = mv_str_doc(mk_string("I32", a), a);
            break;
        case Int_16: 
            out = mv_str_doc(mk_string("I16", a), a);
            break;
        case Int_8: 
            out = mv_str_doc(mk_string("I8", a), a);
            break;
        case TFormer: 
            out = mv_str_doc(mk_string("Former", a), a);
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
        for (size_t i = 0; i < type->enumeration.variants.len; i++) {
            PtrArray var_nodes = mk_ptr_array(4, a);
            Document* pre = mk_str_doc(mv_string("[:"), a);
            Document* fname = mk_str_doc(*symbol_to_string(type->enumeration.variants.data[i].key), a);

            PtrArray* types = type->enumeration.variants.data[i].val;
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

    case TVar: {
        out = mk_str_doc(*symbol_to_string(type->var), a);
        break;
    }
    case TAll: {
        PtrArray nodes = mk_ptr_array(type->binder.vars.len + 3, a);
        push_ptr(mk_str_doc(mv_string("All [" ), a), &nodes);
        for (size_t i = 0; i < type->binder.vars.len; i++) {
            push_ptr(mk_str_doc(*symbol_to_string(type->binder.vars.data[i]), a), &nodes);
        }
        push_ptr(mk_str_doc(mv_string("]" ), a), &nodes);
        push_ptr(pretty_type(type->binder.body, a), &nodes);

        out = mv_sep_doc(nodes, a);
        break;
    }
    case TKind: {
        size_t nargs = type->kind.nargs;
        if (nargs == 0) {
            out = mk_str_doc(mv_string("Type"), a);
        } else {
            PtrArray nodes = mk_ptr_array(nargs + 2, a);
            push_ptr(mk_str_doc(mv_string("Fam ("), a), &nodes);
            for (size_t i = 0; i < nargs; i++) {
                push_ptr(mk_str_doc(mv_string("Type"), a), &nodes);
            }
            push_ptr(mk_str_doc(mv_string(") Type"), a), &nodes);
            out = mv_sep_doc(nodes, a);
        }
        break;
    }
    default:
        out = mk_str_doc(mv_string("Error printing type: unrecognised sort."), a);
        break;
    }
    return out;
}

size_t pi_mono_size_of(PiType type) {
    return pi_size_of(type);
}

size_t pi_size_of(PiType type) {
    switch (type.sort) {
    case TPrim:
        switch (type.prim) {
        case Unit:
            return 0;
        case Bool:
            return sizeof(uint8_t);
        case Address:
        case Int_64:
            return sizeof(int64_t);
        case Int_32:
            return sizeof(int32_t);
        case Int_16:
            return sizeof(int16_t);
        case Int_8:
            return sizeof(int8_t);
        case TFormer:
            return sizeof(TermFormer); // sizeof(pi_term_former_t);
        }
        return sizeof(uint64_t);
    case TProc:
        return sizeof(uint64_t);
    case TStruct: {
        size_t total = 0; 
        for (size_t i = 0; i < type.structure.fields.len; i++) {
            // Note: Data is padded to be 8-byte aligned!
            // TODO (TAGS: FEAT): Make generic on padding size?
            size_t field_size = pi_size_of(*(PiType*)type.structure.fields.data[i].val);
            size_t padding = field_size % 8 == 0 ? 0 : 8 - (field_size % 8);
            total += field_size + padding;
        }
        return total;
    }
    case TEnum: {
        size_t max = 0; 
        for (size_t i = 0; i < type.enumeration.variants.len; i++) {
            size_t total = 0;
            PtrArray types = *(PtrArray*)type.enumeration.variants.data[i].val;
            for (size_t i = 0; i < types.len; i++) {
                // Note: Data is padded to be 8-byte aligned!
                // TODO (TAGS: FEAT): Make generic on padding size?
                size_t field_size = pi_size_of(*(PiType*)types.data[i]);
                size_t padding = field_size % 8 == 0 ? 0 : 8 - (field_size % 8);
                total += field_size + padding;
            }

            if (total > max) {
                max = total;
            }
        }
        // Add 1 for tag!
        return max + sizeof(uint64_t);
    }
    case TKind: 
        return sizeof(void*);
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

    uvar->uvar = mem_alloc(sizeof(UVarType), a);
    *uvar->uvar = (UVarType) {.subst = NULL, .id = gen->counter++,};
    
    return uvar;
}

PiType* mk_uvar_with_default(UVarGenerator* gen, Allocator* a) {
    PiType* uvar = mem_alloc(sizeof(PiType), a);
    uvar->sort = TUVarDefaulted; 

    uvar->uvar = mem_alloc(sizeof(UVarType), a);
    *uvar->uvar = (UVarType) {.subst = NULL, .id = gen->counter++,};
    
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


