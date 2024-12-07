#include <stdarg.h>
#include "platform/machine_info.h"
#include "platform/signals.h"

#include "data/string.h"

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
    case TReset: {
        delete_pi_type_p(t.reset.in, a);
        delete_pi_type_p(t.reset.out, a);
        break;
    }
    case TDynamic: {
        delete_pi_type_p(t.dynamic, a);
        break;
    }
    case TDistinct: {
        delete_pi_type_p(t.distinct.type, a);
        break;
    }

    case TAll:
    case TExists:
    case TFam: {
        sdelete_u64_array(t.binder.vars);
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

    default:
        panic(mv_string("In delete_pi_type: unrecognized sort."));
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
    case TReset:
        out.reset.in = copy_pi_type_p(t.reset.in, a);
        out.reset.out = copy_pi_type_p(t.reset.out, a);
        break;
    case TDynamic:
        out.dynamic = copy_pi_type_p(t.dynamic, a);
        break;
    case TResumeMark:
        break;
    case TDistinct:
        out.distinct.type = copy_pi_type_p(t.distinct.type, a);
        out.distinct.id = t.distinct.id;
        out.distinct.source_module = t.distinct.source_module;
        break;
    case TVar:
        out.var = t.var;
        break;
    case TExists:
    case TAll:
    case TFam: {
        out.binder.vars = scopy_u64_array(t.binder.vars, a);
        out.binder.body = copy_pi_type_p(t.binder.body, a);
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
            int64_t* ival = (int64_t*) val;
            out =  pretty_i64(*ival, a);
            break;
        }
        case Int_32: {
            int32_t* ival = (int32_t*) val;
            out =  pretty_i32(*ival, a);
            break;
        }
        case Int_16: {
            int16_t* ival = (int16_t*) val;
            out =  pretty_i16(*ival, a);
            break;
        }
        case Int_8: {
            int8_t* ival = (int8_t*) val;
            out =  pretty_i8(*ival, a);
            break;
        }
        case UInt_64: {
            uint64_t* uival = (uint64_t*) val;
            out =  pretty_u64(*uival, a);
            break;
        }
        case UInt_32: {
            uint32_t* uival = (uint32_t*) val;
            out =  pretty_u32(*uival, a);
            break;
        }
        case UInt_16: {
            uint16_t* uival = (uint16_t*) val;
            out =  pretty_i16(*uival, a);
            break;
        }
        case UInt_8: {
            uint8_t* uival = (uint8_t*) val;
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
    case TReset: {
        out = mk_str_doc(mv_string("#reset-point"), a);
        break;
    }
    case TDynamic: {
        uint64_t dvar = *(uint64_t*)val;
        PtrArray nodes = mk_ptr_array(5, a);
        push_ptr(mk_str_doc(mv_string("(dynamic #"), a), &nodes);
        push_ptr(pretty_u64(dvar, a), &nodes);
        push_ptr(mk_str_doc(mv_string(": "), a), &nodes);
        push_ptr(pretty_pi_value(get_dynamic_val(dvar), type->dynamic, a), &nodes);
        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);
        out = mv_cat_doc(nodes, a);
        break;
    }
    case TVar: {
        out = mk_str_doc(*symbol_to_string(type->var), a);
        break;
    }
    case TAll:
    case TExists:
    case TFam: {
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
    case TDistinct:  {
        out = pretty_pi_value(val, type->distinct.type, a);
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
    case TUVarDefaulted:
        out = mv_str_doc(mk_string("No Print UVar Defaulted!", a), a);
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
        case UInt_64: 
            out = mv_str_doc(mk_string("U64", a), a);
            break;
        case UInt_32: 
            out = mv_str_doc(mk_string("U32", a), a);
            break;
        case UInt_16: 
            out = mv_str_doc(mk_string("U16", a), a);
            break;
        case UInt_8: 
            out = mv_str_doc(mk_string("U8", a), a);
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
    case TReset: {
        PtrArray nodes = mk_ptr_array(4, a);
        push_ptr(mk_str_doc(mv_string("(Reset"), a), &nodes);
        push_ptr(pretty_type(type->reset.in, a), &nodes);
        push_ptr(pretty_type(type->reset.out, a), &nodes);
        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case TDynamic: {
        PtrArray nodes = mk_ptr_array(4, a);
        push_ptr(mk_str_doc(mv_string("(Dynamic "), a), &nodes);
        push_ptr(pretty_type(type->dynamic, a), &nodes);
        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);
        out = mv_cat_doc(nodes, a);
        break;
    }
    case TDistinct:  {
        PtrArray nodes = mk_ptr_array(5, a);
        if (type->distinct.source_module) {
            push_ptr(mk_str_doc(mv_string("Opaque #" ), a), &nodes);
        } else {
            push_ptr(mk_str_doc(mv_string("Distinct #" ), a), &nodes);
        }
        push_ptr(pretty_u64(type->distinct.id, a), &nodes);
        push_ptr(mk_str_doc(mv_string(" " ), a), &nodes);
        push_ptr(pretty_type(type->distinct.type, a), &nodes);
        out = mv_cat_doc(nodes, a);
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
    case TFam: {
        PtrArray nodes = mk_ptr_array(type->binder.vars.len + 3, a);
        push_ptr(mk_str_doc(mv_string("Family [" ), a), &nodes);
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
            push_ptr(mk_str_doc(mv_string("Kind ("), a), &nodes);
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


PiType* pi_type_subst_i(PiType* type, SymPtrAssoc binds, SymbolArray* shadow, Allocator* a) {
    // Replace all (free) type variables in type with the variable given by binds
    PiType* out = mem_alloc(sizeof(PiType), a);
    switch (type->sort) {
    case TPrim: {
        *out = *type;
        return out;
    }
        
    case TProc: {
        PtrArray args = mk_ptr_array(type->proc.args.len, a);
        for (size_t i = 0; i < type->proc.args.len; i++) {
            push_ptr(pi_type_subst_i(type->proc.args.data[i], binds, shadow, a), &args);
        }
        *out = (PiType) {
            .sort = TProc,
            .proc.args = args,
            .proc.ret = pi_type_subst_i(type->proc.ret, binds, shadow, a),
        };
        return out;
    }
    case TStruct: {
        panic(mv_string("pi_type subst: struct not implemented"));
    }
    case TEnum: {
        panic(mv_string("pi_type subst: enum not implemented"));
    }
    // Quantified Types
    case TVar: {
        if (find_u64(type->var, *shadow) == shadow->len) {
            PiType** result = (PiType**)sym_ptr_alookup(type->var, binds);
            *out = result ? **result : *type;
        }
        return out;
    }
    case TAll: {
        panic(mv_string("pi_type subst: all not implemented"));
    }
    case TExists: {
        panic(mv_string("pi_type subst: exists not implemented"));
    }
    // Used by Sytem-Fω (type constructors)
    case TCApp: {
        panic(mv_string("pi_type subst: app not implemented"));
    }
    case TFam: {
        panic(mv_string("pi_type subst: lam not implemented"));
    }
    // Kinds (higher kinds not supported)
    case TKind: {
        panic(mv_string("pi_type subst: kind not implemented"));
    }
    // Used only during unification
    case TUVar: {
        panic(mv_string("pi_type subst: uvar not implemented"));
    }
    case TUVarDefaulted: {
        panic(mv_string("pi_type subst: uvar-defaulted not implemented"));
    }
    default:
        panic(mv_string("Unrecognized sort of type provided to pi_type_susbt_i"));
    }
}

PiType* pi_type_subst(PiType* type, SymPtrAssoc binds, Allocator* a) {
    SymbolArray shadow = mk_u64_array(16, a);
    return pi_type_subst_i(type, binds, &shadow, a);
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
        /*
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
        */
        default:
            return sizeof(uint64_t);
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

    case TReset: {
    case TResumeMark: 
    case TDynamic:
        return ADDRESS_SIZE;
    }
    case TDistinct: {
        return pi_size_of(*type.distinct.type);
    }
    case TAll: {
        return sizeof(void*);
    }
    case TFam: {
        panic(mv_string("pi_size_of received invalid type: Family."));
    }
    case TKind: 
        return sizeof(void*);
    case TUVar:
        panic(mv_string("pi_size_of received invalid type: UVar."));
    case TUVarDefaulted:
        panic(mv_string("pi_size_of received invalid type: UVar with Default."));
    default:
        panic(mv_string("pi_size_of received invalid type."));
    }
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

// TODO (UB): make this thread safe
static int id_counter = 0;
uint64_t distinct_id() {return id_counter++;}

void type_app_subst(PiType* body, SymPtrAMap subst, Allocator* a) {
    switch (body->sort) {
    case TPrim: break;
    case TProc: 
        for (size_t i = 0; i < body->proc.args.len; i++) {
            type_app_subst(body->proc.args.data[i], subst, a);
        }
        type_app_subst(body->proc.ret, subst, a);
        break;
    case TStruct:
        for (size_t i = 0; i < body->structure.fields.len; i++) {
            type_app_subst(body->structure.fields.data[i].val, subst, a);
        }
        break;
    case TEnum:
        for (size_t i = 0; i < body->enumeration.variants.len; i++) {
            PtrArray* variant = body->structure.fields.data[i].val;
            for (size_t j = 0; j < variant->len; j++) {
                type_app_subst(variant->data[i], subst, a);
            }
        }
        break;
    case TReset:
        type_app_subst(body->reset.in, subst, a);
        type_app_subst(body->reset.out, subst, a);
        break;
    case TResumeMark:
        panic(mv_string("not implemetned type-app for ResumeMark"));
        break;
    case TDynamic:
        type_app_subst(body->dynamic, subst, a);
        break;

    // Quantified Types
    case TVar: {
        PiType** val = (PiType**)sym_ptr_lookup(body->var, subst);
        if (val) {*body = **val;}
        break;
    }
    case TAll:
        panic(mv_string("Not implemetned type-app for All"));
        break;
    case TExists:
        panic(mv_string("Not implemetned type-app for Exists"));
        break;

    // Used by Sytem-Fω (type constructors)
    case TCApp:
        panic(mv_string("Not implemetned type-app for App"));
        break;
    case TFam:
        panic(mv_string("Not implemetned type-app for Fam"));
        break;

    // Kinds (higher kinds not supported)
    case TKind: break;
    default:
        panic(mv_string("not implemetned type-app for this type of unknown sort!"));
        break;
    }
}

PiType* type_app (PiType family, PtrArray args, Allocator* a) {
    if (family.sort != TFam || family.binder.vars.len != args.len) {
        panic(mv_string("Invalid type_app!"));
    }
    SymPtrAMap subst = mk_sym_ptr_amap(args.len, a);;
    for (size_t i = 0; i < args.len; i++) {
        Symbol var = family.binder.vars.data[i];
        PiType* tipe = args.data[i];
        sym_ptr_insert(var, tipe, &subst);
    }

    PiType* new_type = copy_pi_type_p(family.binder.body, a);
    type_app_subst (new_type, subst, a);
    return new_type;
}

PiType mk_prim_type(PrimType t) {
    return (PiType) {
      .sort = TPrim,
      .prim = t,
    };
}

PiType mk_dynamic_type(Allocator* a, PiType t) {
    PiType* dyn = mem_alloc(sizeof(PiType), a);
    *dyn = t;
    return (PiType){.sort = TDynamic, .dynamic = dyn};
}

PiType mk_proc_type(Allocator* a, size_t nargs, ...) {
    va_list args;
    va_start(args, nargs);
    
    PtrArray ty_args = mk_ptr_array(nargs, a);
    for (size_t i = 0; i < nargs ; i++) {
        PiType* arg = mem_alloc(sizeof(PiType), a);
        *arg = va_arg(args, PiType);
        push_ptr(arg, &ty_args);
    }

    PiType* ret = mem_alloc(sizeof(PiType), a);
    *ret = va_arg(args, PiType);
    va_end(args);

    return (PiType) {.sort = TProc, .proc.args = ty_args, .proc.ret = ret};
}

PiType mk_struct_type(Allocator* a, size_t nfields, ...) {
    va_list args;
    va_start(args, nfields);
    
    SymPtrAMap fields = mk_sym_ptr_amap(nfields, a);
    for (size_t i = 0; i < nfields ; i++) {
        Symbol name = string_to_symbol(mv_string(va_arg(args, char*)));
        PiType* arg = mem_alloc(sizeof(PiType), a);

        *arg = va_arg(args, PiType);
        sym_ptr_insert(name, arg, &fields);
    }
    va_end(args);

    return (PiType) {.sort = TStruct, .structure.fields = fields,};
}

PiType mk_string_type(Allocator* a) {
    // Struct [.memsize U64] [.bytes Address]

    PiType* memsize_type = mem_alloc(sizeof(PiType), a);
    PiType* bytes_type = mem_alloc(sizeof(PiType), a);

    *memsize_type = (PiType) {.sort = TPrim, .prim = UInt_64};
    *bytes_type = (PiType) {.sort = TPrim, .prim = Address};

    SymPtrAMap fields = mk_sym_ptr_amap(2, a);
    sym_ptr_insert(string_to_symbol(mv_string("memsize")), memsize_type, &fields);
    sym_ptr_insert(string_to_symbol(mv_string("bytes")), bytes_type, &fields);
    
    return (PiType) {
        .sort = TStruct,
        .structure.fields = fields
    };
}

