#include <stdarg.h>
#include "platform/machine_info.h"
#include "platform/signals.h"

#include "data/string.h"

#include "pretty/standard_types.h"
#include "pretty/string_printer.h"
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
        for (size_t i = 0; i < t.proc.implicits.len; i++)
            delete_pi_type_p(t.proc.implicits.data[i], a);
        sdelete_ptr_array(t.proc.implicits);
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
        if (t.distinct.args) {
            for (size_t i = 0; i < t.distinct.args->len; i++)
                delete_pi_type_p(t.distinct.args->data[i], a);
            sdelete_ptr_array(*t.distinct.args);
        }
        break;
    }
    case TTrait: {
        sdelete_u64_array(t.trait.vars);
        for (size_t i = 0; i < t.trait.fields.len; i++)
            delete_pi_type_p(t.trait.fields.data[i].val, a);
        sdelete_sym_ptr_amap(t.trait.fields);
        break;
    }
    case TTraitInstance: {
        for (size_t i = 0; i < t.instance.args.len; i++)
            delete_pi_type_p(t.instance.args.data[i], a);
        sdelete_ptr_array(t.instance.args);

        for (size_t i = 0; i < t.instance.fields.len; i++)
            delete_pi_type_p(t.instance.fields.data[i].val, a);
        sdelete_sym_ptr_amap(t.instance.fields);
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
    case TConstraint: break;

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
        out.proc.implicits = copy_ptr_array(t.proc.implicits,  (TyCopier)copy_pi_type_p, a);
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
    case TTrait:
        out.trait.id = t.trait.id;
        out.trait.vars = scopy_u64_array(t.trait.vars, a);
        out.trait.fields = copy_sym_ptr_amap(t.trait.fields, symbol_id, (TyCopier)copy_pi_type_p, a);
        break;
    case TTraitInstance:
        out.instance.instance_of = t.instance.instance_of;
        out.instance.args = copy_ptr_array(t.instance.args,  (TyCopier)copy_pi_type_p, a);
        out.instance.fields = copy_sym_ptr_amap(t.instance.fields, symbol_id, (TyCopier)copy_pi_type_p, a);
        break;
    case TResumeMark:
        break;
    case TDistinct:
        out.distinct.type = copy_pi_type_p(t.distinct.type, a);
        out.distinct.id = t.distinct.id;
        out.distinct.source_module = t.distinct.source_module;
        out.distinct.args = mem_alloc(sizeof(PtrArray), a);
        *out.distinct.args = copy_ptr_array(*t.distinct.args,  (TyCopier)copy_pi_type_p, a);
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
    case TConstraint:
        out.constraint = t.constraint;
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
            uint8_t* uival = (uint8_t*) val;
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
    case TTraitInstance: {
        void* data = *(void**)val;
        PtrArray nodes = mk_ptr_array(5 + type->instance.fields.len, a);
        push_ptr(mk_str_doc(mv_string("(instance <TYPE>"), a), &nodes);
        for (size_t i = 0; i < type->instance.fields.len; i++) {
            SymPtrCell cell = type->instance.fields.data[i];
            PtrArray lcl_nodes = mk_ptr_array(2, a);
            push_ptr(mk_str_doc(*symbol_to_string(cell.key), a), &lcl_nodes);
            push_ptr(pretty_pi_value(data, (PiType*)cell.val, a), &lcl_nodes);

            data += pi_size_of(*(PiType*)cell.val);
            push_ptr(mk_paren_doc("[.", "]", mv_sep_doc(lcl_nodes, a), a), &nodes);
        }
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
    case TKind:
    case TConstraint: {
        PiType** ptype = (PiType**) val;
        out = pretty_type(*ptype, a);
        break;
    }
    default:
        out = mk_str_doc(mv_string("Error printing value: it's type has unrecognised sort."), a);
        break;
    }
    return out;
}

Document* pretty_type(PiType* type, Allocator* a) {
    Document* out = NULL;
    switch (type->sort) {
    case TProc: {
        PtrArray nodes = mk_ptr_array(4, a);
        push_ptr(mv_str_doc((mk_string("Proc", a)), a), &nodes);
        if (type->proc.implicits.len != 0) {
            PtrArray arg_nodes = mk_ptr_array(type->proc.implicits.len, a);
            for (size_t i = 0; i < type->proc.implicits.len; i++) {
                push_ptr(pretty_type(type->proc.implicits.data[i], a), &arg_nodes);
            }
            push_ptr(mk_paren_doc("{", "}", mv_sep_doc(arg_nodes, a), a), &nodes);
        }

        PtrArray arg_nodes = mk_ptr_array(type->proc.args.len, a);
        for (size_t i = 0; i < type->proc.args.len; i++) {
            push_ptr(pretty_type(type->proc.args.data[i], a), &arg_nodes);
        }
        push_ptr(mk_paren_doc("[", "]", mv_sep_doc(arg_nodes, a), a), &nodes);
        push_ptr(pretty_type(type->proc.ret, a), &nodes);
        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
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
        PtrArray nodes = mk_ptr_array(1 + type->structure.fields.len, a);
        push_ptr(mv_str_doc((mk_string("Struct ", a)), a), &nodes);
        for (size_t i = 0; i < type->structure.fields.len; i++) {
            PtrArray fd_nodes = mk_ptr_array(2, a);
            Document* fname = mk_str_doc(*symbol_to_string(type->structure.fields.data[i].key), a);
            Document* arg = pretty_type(type->structure.fields.data[i].val, a);

            push_ptr(fname, &fd_nodes);
            push_ptr(arg,   &fd_nodes);
            Document* fd_doc = mk_paren_doc("[.", "]", mv_sep_doc(fd_nodes, a), a);

            push_ptr(fd_doc, &nodes);
        }
        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case TEnum: {
        PtrArray nodes = mk_ptr_array(1 + type->enumeration.variants.len, a);
        push_ptr(mv_str_doc((mk_string("Enum ", a)), a), &nodes);
        for (size_t i = 0; i < type->enumeration.variants.len; i++) {
            PtrArray var_nodes = mk_ptr_array(2, a);
            Document* fname = mk_str_doc(*symbol_to_string(type->enumeration.variants.data[i].key), a);

            PtrArray* types = type->enumeration.variants.data[i].val;
            PtrArray ty_nodes = mk_ptr_array(types->len, a);
            for (size_t j = 0; j < types->len; j++) {
                Document* arg = pretty_type((PiType*)types->data[j], a);
                push_ptr(arg, &ty_nodes);
            }
            Document* ptypes = mv_sep_doc(ty_nodes, a);

            push_ptr(fname, &var_nodes);
            push_ptr(ptypes,   &var_nodes);
            Document* var_doc = mk_paren_doc("[:", "]",mv_sep_doc(var_nodes, a), a);

            push_ptr(var_doc, &nodes);
        }
        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case TReset: {
        PtrArray nodes = mk_ptr_array(3, a);
        push_ptr(mk_str_doc(mv_string("Reset"), a), &nodes);
        push_ptr(pretty_type(type->reset.in, a), &nodes);
        push_ptr(pretty_type(type->reset.out, a), &nodes);
        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case TDynamic: {
        PtrArray nodes = mk_ptr_array(2, a);
        push_ptr(mk_str_doc(mv_string("Dynamic "), a), &nodes);
        push_ptr(pretty_type(type->dynamic, a), &nodes);
        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case TDistinct:  {
        PtrArray nodes = mk_ptr_array(6, a);
        if (type->distinct.source_module) {
            push_ptr(mk_str_doc(mv_string("Opaque #" ), a), &nodes);
        } else {
            push_ptr(mk_str_doc(mv_string("Distinct #" ), a), &nodes);
        }
        if (type->distinct.args) {
            PtrArray args = mk_ptr_array(type->distinct.args->len, a);
            for (size_t i = 0; i < type->distinct.args->len; i++) {
                push_ptr(pretty_type(type->distinct.args->data[i], a), &args);
            }
            push_ptr(mk_paren_doc("(", ")", mv_sep_doc(args, a), a), &nodes);
        }
        push_ptr(pretty_u64(type->distinct.id, a), &nodes);
        push_ptr(mk_str_doc(mv_string(" " ), a), &nodes);
        push_ptr(pretty_type(type->distinct.type, a), &nodes);
        out = mk_paren_doc("(", ")", mv_cat_doc(nodes, a), a);
        break;
    }
    case TTrait:  {
        PtrArray nodes = mk_ptr_array(2 + type->trait.fields.len, a);
        push_ptr(mk_str_doc(mv_string("Trait" ), a), &nodes);

        PtrArray vars = mk_ptr_array(type->trait.vars.len, a);
        for (size_t i = 0; i < type->trait.vars.len; i++) {
            push_ptr(mk_str_doc(*symbol_to_string(type->trait.vars.data[i]), a), &vars);
        }
        push_ptr(mk_paren_doc("[", "]", mv_sep_doc(vars, a), a), &nodes);

        for (size_t i = 0; i < type->trait.fields.len; i++) {
            PtrArray fd_nodes = mk_ptr_array(2, a);
            Document* fname = mk_str_doc(*symbol_to_string(type->trait.fields.data[i].key), a);
            Document* arg = pretty_type(type->trait.fields.data[i].val, a);

            push_ptr(fname, &fd_nodes);
            push_ptr(arg,   &fd_nodes);
            Document* fd_doc = mk_paren_doc("[.", "]", mv_sep_doc(fd_nodes, a), a);

            push_ptr(fd_doc, &nodes);
        }
        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case TTraitInstance: {
        PtrArray nodes = mk_ptr_array(2 + type->instance.fields.len, a);
        push_ptr(mk_str_doc(mv_string("Instance" ), a), &nodes);
        push_ptr(pretty_u64(type->instance.instance_of, a), &nodes);

        for (size_t i = 0; i < type->instance.fields.len; i++) {
            PtrArray fd_nodes = mk_ptr_array(2, a);
            Document* fname = mk_str_doc(*symbol_to_string(type->instance.fields.data[i].key), a);
            Document* arg = pretty_type(type->instance.fields.data[i].val, a);

            push_ptr(fname, &fd_nodes);
            push_ptr(arg,   &fd_nodes);
            Document* fd_doc = mk_paren_doc("[.", "]", mv_sep_doc(fd_nodes, a), a);

            push_ptr(fd_doc, &nodes);
        }

        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
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
            push_ptr(mk_str_doc(mv_string("Kind ["), a), &nodes);
            for (size_t i = 0; i < nargs; i++) {
                push_ptr(mk_str_doc(mv_string("Type"), a), &nodes);
            }
            push_ptr(mk_str_doc(mv_string("] Type"), a), &nodes);
            out = mv_sep_doc(nodes, a);
        }
        break;
    }
    case TConstraint: {
        size_t nargs = type->constraint.nargs;
        if (nargs == 0) {
            out = mk_str_doc(mv_string("Constraint"), a);
        } else {
            PtrArray nodes = mk_ptr_array(nargs + 2, a);
            push_ptr(mk_str_doc(mv_string("Kind ["), a), &nodes);
            for (size_t i = 0; i < nargs; i++) {
                push_ptr(mk_str_doc(mv_string("Type"), a), &nodes);
            }
            push_ptr(mk_str_doc(mv_string("] Constraint"), a), &nodes);
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

size_t pi_size_align(size_t size, size_t align) {
    size_t rem = size % align;
    size_t pad = rem == 0 ? 0 : align - rem;
    return size + pad;
}

size_t pi_stack_align(size_t in) {
    return pi_size_align(in, 8);
}

size_t pi_stack_size_of(PiType type) {
    return pi_stack_align(pi_size_of(type));
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
        case UInt_64:
            return sizeof(int64_t);
        case Int_32:
        case UInt_32:
            return sizeof(int32_t);
        case Int_16:
        case UInt_16:
            return sizeof(int16_t);
        case Int_8:
        case UInt_8:
            return sizeof(int8_t);
        case TFormer:
            return sizeof(TermFormer);
        default:
            panic(mv_string("pi-size-of: unrecognized primitive."));
        }
        break;
    case TProc:
        return sizeof(uint64_t);
    case TStruct: {
        size_t total = 0; 
        for (size_t i = 0; i < type.structure.fields.len; i++) {
            // Note: Data is padded to be 8-byte aligned!
            // TODO (FEAT): Make generic on padding size?
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
                // TODO (FEAT): Make generic on padding size?
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
    case TTrait:
        panic(mv_string("pi_size_of received invalid type: Trait."));
        break;
    case TTraitInstance:
        return ADDRESS_SIZE;
        break;
    case TAll: {
        return sizeof(void*);
    }
    case TFam: {
        panic(mv_string("pi_size_of received invalid type: Family."));
    }
    case TKind: 
    case TConstraint: 
        return sizeof(void*);
    case TUVar:
        panic(mv_string("pi_size_of received invalid type: UVar."));
    case TUVarDefaulted:
        panic(mv_string("pi_size_of received invalid type: UVar with Default."));
    default:
        panic(mv_string("pi_size_of received invalid type."));
    }
}

size_t pi_align_of(PiType type) {
    switch (type.sort) {
    case TPrim:
        switch (type.prim) {
        case Unit:
            return 0;
        case Bool:
            return sizeof(uint8_t);
        case Address:
        case Int_64:
        case UInt_64:
            return sizeof(int64_t);
        case Int_32:
        case UInt_32:
            return sizeof(int32_t);
        case Int_16:
        case UInt_16:
            return sizeof(int16_t);
        case Int_8:
        case UInt_8:
            return sizeof(int8_t);
        case TFormer:
            return sizeof(TermFormer);
        default:
            panic(mv_string("pi_align_of received invalid primitive"));
        }
        return sizeof(uint64_t);
    case TProc:
        return sizeof(uint64_t);
    case TStruct: {
        size_t align = 0; 
        for (size_t i = 0; i < type.structure.fields.len; i++) {
            size_t field_align = pi_align_of(*(PiType*)type.structure.fields.data[i].val);
            // align = max(align, field_align)
            align = align > field_align ? align : field_align;
        }
        return align;
    }
    case TEnum: {
        size_t align = 0; 
        for (size_t i = 0; i < type.enumeration.variants.len; i++) {
            PtrArray types = *(PtrArray*)type.enumeration.variants.data[i].val;
            for (size_t i = 0; i < types.len; i++) {
                // Note: Data is padded to be 8-byte aligned!
                // TODO (FEAT): Make generic on padding size?
                size_t field_align = pi_align_of(*(PiType*)types.data[i]);
                // accumulate max
                // size_t padding = field_size % 8 == 0 ? 0 : 8 - (field_size % 8);
                align += field_align > align ? field_align : align;
            }
        }
        // Note: this will set it to max, we should shrink the tag size (maybe 16 bits? variable bits?)
        return 8 > align ? 8 : align;
    }

    case TReset: {
    case TResumeMark: 
    case TDynamic:
        return ADDRESS_ALIGN;
    }
    case TDistinct: {
        return pi_align_of(*type.distinct.type);
    }
    case TTrait:
        panic(mv_string("pi_align_of received invalid type: Trait."));
        break;
    case TTraitInstance:
        return ADDRESS_ALIGN;
        break;
    case TAll: {
        return sizeof(void*);
    }
    case TFam: {
        panic(mv_string("pi_align_of received invalid type: Family."));
    }
    case TKind: 
    case TConstraint: 
        return sizeof(void*);
    case TUVar:
        panic(mv_string("pi_align_of received invalid type: UVar."));
    case TUVarDefaulted:
        panic(mv_string("pi_align_of received invalid type: UVar with Default."));
    default:
        panic(mv_string("pi_align_of received invalid type."));
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

void type_app_subst(PiType* body, SymPtrAssoc subst, Allocator* a) {
    switch (body->sort) {
    case TPrim: break;
    case TProc: 
        for (size_t i = 0; i < body->proc.implicits.len; i++) {
            type_app_subst(body->proc.implicits.data[i], subst, a);
        }
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
    case TDistinct:
        panic(mv_string("not implemetned type-app for distinct"));
        break;
    case TTrait:
        for (size_t i = 0; i < body->trait.fields.len; i++) {
            type_app_subst(body->trait.fields.data[i].val, subst, a);
        }
        break;
    case TTraitInstance: // note: not a "real" type in the theory
        for (size_t i = 0; i < body->instance.args.len; i++) {
            type_app_subst(body->instance.args.data[i], subst, a);
        }
        for (size_t i = 0; i < body->instance.fields.len; i++) {
            type_app_subst(body->instance.fields.data[i].val, subst, a);
        }
        break;

    // Quantified Types
    case TVar: {
        PiType** val = (PiType**)sym_ptr_alookup(body->var, subst);
        if (val) {*body = **val;}
        break;
    }
    case TAll:
        // Note: when implementing, consider shadowing
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
        // Note: when implementing, consider shadowing
        panic(mv_string("Not implemetned type-app for Fam"));
        break;

    // Kinds (higher kinds not supported)
    case TKind: break;
    default: {
        PtrArray nodes = mk_ptr_array(4, a);
        push_ptr(mv_str_doc(mv_string("Unrecognized type to type-app:"), a), &nodes);
        push_ptr(pretty_type(body, a), &nodes);
        Document* message = mk_sep_doc(nodes, a);
        panic(doc_to_str(message, a));
        break;
    }
    }
}

PiType* type_app (PiType family, PtrArray args, Allocator* a) {
    if (family.sort == TTrait) {
        if (family.trait.vars.len != args.len) {
            panic(mv_string("Invalid type_app!"));
        }
        
        SymPtrAssoc subst = mk_sym_ptr_assoc(args.len, a);;
        for (size_t i = 0; i < args.len; i++) {
            Symbol var = family.trait.vars.data[i];
            PiType* tipe = args.data[i];
            sym_ptr_bind(var, tipe, &subst);
        }

        SymPtrAMap new_fields = mk_sym_ptr_amap(family.trait.fields.len, a);
        for (size_t i = 0; i < family.trait.fields.len; i++) {
            SymPtrCell cell = family.trait.fields.data[i];
            PiType* new_type = copy_pi_type_p(cell.val, a);
            type_app_subst (new_type, subst, a);
            sym_ptr_insert(cell.key, new_type, &new_fields);
        }

        PiType* out_ty = mem_alloc(sizeof(PiType), a);
        *out_ty = (PiType) {
            .sort = TTraitInstance,
            .instance.instance_of = family.trait.id,
            .instance.args = args, // TODO (INVESTIGATE): is the non-copying of args a bug?
            .instance.fields = new_fields,
        };
        return out_ty;
    } else if (family.sort == TDistinct) {
        PiType* new_type = mem_alloc(sizeof(PiType), a);
        *new_type = family;
        new_type->distinct.type = type_app(*family.distinct.type, args, a);
        return new_type;
    } else {
        if (family.sort != TFam || family.binder.vars.len != args.len) {
            panic(mv_string("Invalid type_app!"));
        }
        SymPtrAssoc subst = mk_sym_ptr_assoc(args.len, a);;
        for (size_t i = 0; i < args.len; i++) {
            Symbol var = family.binder.vars.data[i];
            PiType* tipe = args.data[i];
            sym_ptr_bind(var, tipe, &subst);
        }

        PiType* new_type = copy_pi_type_p(family.binder.body, a);
        type_app_subst (new_type, subst, a);
        return new_type;
    }
}

PiType* pi_type_subst(PiType* type, SymPtrAssoc binds, Allocator* a) {
    PiType* new_type = copy_pi_type_p(type, a);
    type_app_subst(new_type, binds, a);
    return new_type;
}

bool pi_type_eql(PiType* lhs, PiType* rhs) {
    if (lhs->sort != rhs->sort) return false;

    switch (lhs->sort) {
    case TPrim:
        return lhs->prim == rhs->prim;
        break;
    case TProc:
        if (lhs->proc.implicits.len != rhs->proc.implicits.len) return false;
        for (size_t i = 0; i < lhs->proc.args.len; i++) {
            if (!pi_type_eql(lhs->proc.args.data[i], rhs->proc.args.data[i])) return false;
        }

        if (lhs->proc.args.len != rhs->proc.args.len) return false;
        for (size_t i = 0; i < lhs->proc.args.len; i++) {
            if (!pi_type_eql(lhs->proc.args.data[i], rhs->proc.args.data[i])) return false;
        }
        return pi_type_eql(lhs->proc.ret, rhs->proc.ret);
        break;
    case TStruct:
        panic(mv_string("pi_type_eql not implemented for structs"));
    case TEnum:
        panic(mv_string("pi_type_eql not implemented for enums"));
    case TReset:
        panic(mv_string("pi_type_eql not implemented for resets"));
    case TResumeMark:
        panic(mv_string("pi_type_eql not implemented for resume marks"));
    case TDynamic:
        panic(mv_string("pi_type_eql not implemented for dynamics"));

    // 'Special'
    case TDistinct:
        panic(mv_string("pi_type_eql not implemented for distinct types"));
    case TTrait:
        panic(mv_string("pi_type_eql not implemented for trait types"));
    case TTraitInstance: // note: not a "real" type in the theory
        panic(mv_string("pi_type_eql not implemented for trait instance types"));

    // Quantified Types
    case TVar: // note: not a "real" type in the theory
        panic(mv_string("pi_type_eql not implemented for type vars"));
    case TAll:
        panic(mv_string("pi_type_eql not implemented for all types"));
    case TExists:
        panic(mv_string("pi_type_eql not implemented for existential types"));

    // Used by Sytem-Fω (type constructors)
    case TCApp:
        panic(mv_string("pi_type_eql not implemented for type applications"));
    case TFam:
        panic(mv_string("pi_type_eql not implemented for type families"));

    // Kinds (higher kinds not supported)
    case TKind:
        panic(mv_string("pi_type_eql not implemented for kinds"));
    case TConstraint:
        panic(mv_string("pi_type_eql not implemented for constraints"));
    default:
        panic(mv_string("pi_type_eql provided invalid sort!"));
    }
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

    return (PiType) {
        .sort = TProc,
        .proc.implicits = mk_ptr_array(0, a),
        .proc.args = ty_args,
        .proc.ret = ret
    };
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

