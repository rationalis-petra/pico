#include "platform/signals.h"
#include "pretty/standard_types.h"

#include "pico/syntax/syntax.h"

typedef struct SyntaxCall {
    Symbol field;
    Syntax* fnc;
    Syntax* arg;
} SyntaxCall;


Syntax* mk_lit_untyped_int_syn(const int64_t value, Allocator* a) {
    Syntax* out = (Syntax*)mem_alloc(sizeof(Syntax), a);
    *out = (Syntax) {
        .type = SLitUntypedIntegral,
        .integral.value = value,
    };
    return out;
}

Syntax* mk_lit_typed_int_syn(const int64_t value, PrimType prim, Allocator* a) {
    Syntax* out = (Syntax*)mem_alloc(sizeof(Syntax), a);
    *out = (Syntax) {
        .type = SLitTypedIntegral,
        .integral.value = value,
        .integral.type = prim,
    };
    return out;
}

Document* pretty_syntax(Syntax* syntax, Allocator* a) {
    Document* out = NULL;
    switch (syntax->type) {
    case SLitUntypedIntegral: {
    case SLitTypedIntegral: 
        out = pretty_i64(syntax->integral.value, a);
        break;
    }
    case SLitBool: {
        if (syntax->integral.value == 0) {
            out = mk_str_doc(mv_string(":false"), a);
        } else {
            out = mk_str_doc(mv_string(":true"), a);
        }
        break;
    }
    case SLitString: {
        Document* delimiter = mk_str_doc(mv_string("\""), a);
        PtrArray nodes = mk_ptr_array(3, a);
        push_ptr(delimiter, &nodes);
        push_ptr(mv_str_doc(syntax->string, a), &nodes);
        push_ptr(delimiter, &nodes);
        out = mk_cat_doc(nodes, a);
        break;
    }
    case SVariable: {
        out = mk_str_doc(*symbol_to_string(syntax->variable), a);
        break;
    }
    case SProcedure: {
        PtrArray nodes = mk_ptr_array(4 + syntax->procedure.args.len, a);
        push_ptr(mv_str_doc((mk_string("(proc (", a)), a), &nodes);
        for (size_t i = 0; i < syntax->procedure.args.len; i++) {
            Document* arg = mk_str_doc(*symbol_to_string(syntax->procedure.args.data[i].key), a);
            push_ptr(arg, &nodes);
        }
        push_ptr(mv_str_doc((mk_string(")", a)), a), &nodes);
        push_ptr(pretty_syntax(syntax->procedure.body, a), &nodes);
        push_ptr(mv_str_doc((mk_string(")", a)), a), &nodes);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case SAll: {
        PtrArray nodes = mk_ptr_array(4 + syntax->all.args.len, a);
        push_ptr(mv_str_doc((mk_string("(all [", a)), a), &nodes);
        for (size_t i = 0; i < syntax->all.args.len; i++) {
            Document* arg = mk_str_doc(*symbol_to_string(syntax->all.args.data[i]), a);
            push_ptr(arg, &nodes);
        }
        push_ptr(mv_str_doc((mk_string("]", a)), a), &nodes);
        push_ptr(pretty_syntax(syntax->all.body, a), &nodes);
        push_ptr(mv_str_doc((mk_string(")", a)), a), &nodes);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case SApplication: {
        Document* head = pretty_syntax(syntax->application.function, a);
        PtrArray nodes = mk_ptr_array(1 + syntax->application.args.len, a);
        push_ptr(head, &nodes);
        for (size_t i = 0; i < syntax->application.args.len; i++) {
            Document* node = pretty_syntax(syntax->application.args.data[i], a);
            push_ptr(node, &nodes);
        }
        out = mv_sep_doc(nodes, a);
        break;
    }
    case SAllApplication: {
        Document* head = pretty_syntax(syntax->all_application.function, a);
        bool print_types = syntax->all_application.types.len == 0;
        PtrArray nodes = mk_ptr_array((print_types ? 5 : 3) + syntax->all_application.types.len + syntax->all_application.args.len, a);
        push_ptr(mk_str_doc(mv_string("("), a), &nodes);
        push_ptr(head, &nodes);

        if (print_types) push_ptr(mk_str_doc(mv_string("{"), a), &nodes);
        for (size_t i = 0; i < syntax->all_application.types.len; i++) {
            Document* node = pretty_syntax(syntax->all_application.types.data[i], a);
            push_ptr(node, &nodes);
        }
        if (print_types) push_ptr(mk_str_doc(mv_string("}"), a), &nodes);

        for (size_t i = 0; i < syntax->all_application.args.len; i++) {
            Document* node = pretty_syntax(syntax->all_application.args.data[i], a);
            push_ptr(node, &nodes);
        }
        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case SConstructor: {
        PtrArray nodes = mk_ptr_array(3, a);
        push_ptr(pretty_syntax(syntax->constructor.enum_type, a), &nodes);
        push_ptr(mk_str_doc(mv_string(":"), a), &nodes);
        push_ptr(mk_str_doc(*symbol_to_string(syntax->variant.tagname), a), &nodes);
        out = mv_cat_doc(nodes, a);
        break;
    }
    case SVariant: {
        PtrArray nodes = mk_ptr_array(6, a);

        push_ptr(mk_str_doc(mv_string("("), a), &nodes);
        push_ptr(pretty_syntax(syntax->variant.enum_type, a), &nodes);
        push_ptr(mk_str_doc(mv_string(":"), a), &nodes);
        push_ptr(mk_str_doc(*symbol_to_string(syntax->variant.tagname), a), &nodes);

        PtrArray pretty_args = mk_ptr_array(syntax->variant.args.len, a);
        for (size_t i = 0; i < syntax->variant.args.len; i++) {
            push_ptr(pretty_syntax(syntax->variant.args.data[i], a), &pretty_args);
        }
        push_ptr(mv_sep_doc(pretty_args, a), &nodes);

        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);

        out = mv_cat_doc(nodes, a);
        break;
    }
    case SMatch: {
        // Nodes = (match¹ val² expr expr ... )³, hence len + 3
        PtrArray nodes = mk_ptr_array(syntax->match.clauses.len + 3, a);

        push_ptr(mk_str_doc(mv_string("(match"), a), &nodes);
        push_ptr(pretty_syntax(syntax->match.val, a), &nodes);

        for (size_t i = 0; i < syntax->match.clauses.len; i++) {
            SynClause* clause = syntax->match.clauses.data[i];
            
            // the 4 comes from (, pattern, body, )
            PtrArray clause_nodes = mk_ptr_array(4, a);
            push_ptr(mk_str_doc(mv_string("("), a), &clause_nodes);
            
            PtrArray ptn_nodes = mk_ptr_array(4 + clause->vars.len, a);
            push_ptr(mk_str_doc(mv_string("[:"), a), &ptn_nodes);
            push_ptr(mk_str_doc(*symbol_to_string(clause->tagname), a), &ptn_nodes);
            for (size_t j = 0; j < clause->vars.len; j++) {
                push_ptr(mk_str_doc(*symbol_to_string(clause->vars.data[j]), a), &ptn_nodes);
            }
            push_ptr(mk_str_doc(mv_string("]"), a), &ptn_nodes);

            push_ptr(mv_sep_doc(ptn_nodes, a), &clause_nodes);
            push_ptr(pretty_syntax(clause->body, a), &clause_nodes);
            push_ptr(mk_str_doc(mv_string(")"), a), &clause_nodes);

            push_ptr(mv_cat_doc(clause_nodes, a), &nodes);
        }

        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case SStructure: {
        PtrArray nodes = mk_ptr_array(2 + syntax->structure.fields.len, a);
        push_ptr(mv_str_doc(mk_string("(struct", a), a), &nodes);


        for (size_t i = 0; i < syntax->structure.fields.len; i++) {
            PtrArray field_nodes = mk_ptr_array(4, a);
            Document* temp;
            temp = mk_str_doc(mv_string("[."), a);
            push_ptr(temp, &field_nodes);

            temp = mk_str_doc(*symbol_to_string(syntax->structure.fields.data[i].key), a);
            push_ptr(temp, &field_nodes);

            temp = pretty_syntax(syntax->structure.fields.data[i].val, a);
            push_ptr(temp, &field_nodes);

            temp = mk_str_doc(mv_string("]"), a);
            push_ptr(temp, &field_nodes);

            Document* fdoc = mv_sep_doc(field_nodes, a);
            push_ptr(fdoc, &nodes);
        }
        push_ptr(mv_str_doc(mk_string(")", a), a), &nodes);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case SProjector: {
        PtrArray nodes = mk_ptr_array(3, a);

        push_ptr(pretty_syntax(syntax->projector.val, a), &nodes);
        push_ptr(mk_str_doc(mv_string("."), a), &nodes);
        push_ptr(mk_str_doc(*symbol_to_string(syntax->projector.field), a), &nodes);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case SDynamic: {
        PtrArray nodes = mk_ptr_array(3, a);

        push_ptr(mk_str_doc(mv_string("(dynamic "), a), &nodes);
        push_ptr(pretty_syntax(syntax->dynamic, a), &nodes);
        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);
        out = mv_cat_doc(nodes, a);
        break;
    }
    case SDynamicUse: {
        PtrArray nodes = mk_ptr_array(3, a);

        push_ptr(mk_str_doc(mv_string("(use "), a), &nodes);
        push_ptr(pretty_syntax(syntax->use, a), &nodes);
        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);
        out = mv_cat_doc(nodes, a);
        break;
    }
    case SLet: {
        PtrArray nodes = mk_ptr_array(3 + syntax->let_expr.bindings.len, a);
        push_ptr(mk_str_doc(mv_string("(let"), a), &nodes);
        for (size_t i = 0; i < syntax->let_expr.bindings.len; i++) {
            Symbol name = syntax->let_expr.bindings.data[i].key;
            Syntax* expr = syntax->let_expr.bindings.data[i].val;
            PtrArray let_nodes = mk_ptr_array(4, a);
            push_ptr(mk_str_doc(mv_string("["), a), &let_nodes);
            push_ptr(mk_str_doc(*symbol_to_string(name), a), &let_nodes);
            push_ptr(pretty_syntax(expr, a), &nodes);
            push_ptr(mk_str_doc(mv_string("]"), a), &let_nodes);

            push_ptr(mv_sep_doc(let_nodes, a), &nodes);
        }
        push_ptr(pretty_syntax(syntax->let_expr.body, a), &nodes);
        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);

        out = mv_sep_doc(nodes, a);
        break;
        break;
    }
    case SDynamicLet: {
        PtrArray nodes = mk_ptr_array(3 + syntax->let_expr.bindings.len, a);
        push_ptr(mk_str_doc(mv_string("(bind"), a), &nodes);
        for (size_t i = 0; i < syntax->dyn_let_expr.bindings.len; i++) {
            DynBinding* bind = syntax->dyn_let_expr.bindings.data[i];
            PtrArray let_nodes = mk_ptr_array(4, a);
            push_ptr(mk_str_doc(mv_string("["), a), &let_nodes);
            push_ptr(pretty_syntax(bind->var, a), &let_nodes);
            push_ptr(pretty_syntax(bind->expr, a), &let_nodes);
            push_ptr(mk_str_doc(mv_string("]"), a), &let_nodes);

            push_ptr(mv_sep_doc(let_nodes, a), &nodes);
        }
        push_ptr(pretty_syntax(syntax->let_expr.body, a), &nodes);
        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);

        out = mv_sep_doc(nodes, a);
        break;
    }
    case SIf: {
        PtrArray nodes = mk_ptr_array(6, a);
        push_ptr(mk_str_doc(mv_string("(if"), a), &nodes);
        push_ptr(pretty_syntax(syntax->if_expr.condition, a), &nodes);
        push_ptr(pretty_syntax(syntax->if_expr.true_branch, a), &nodes);
        push_ptr(pretty_syntax(syntax->if_expr.false_branch, a), &nodes);
        push_ptr(mv_str_doc(mk_string(")", a), a), &nodes);
        out = mv_sep_doc(nodes, a);
        break;
    }

    case SIs: {
        PtrArray nodes = mk_ptr_array(4, a);
        push_ptr(mk_str_doc(mv_string("(is "), a), &nodes);
        push_ptr(pretty_syntax(syntax->is.val, a), &nodes);
        push_ptr(pretty_syntax(syntax->is.type, a), &nodes);
        push_ptr(mv_str_doc(mk_string(")", a), a), &nodes);
        out = mv_cat_doc(nodes, a);
        break;
    }
    case SInTo: {
        PtrArray nodes = mk_ptr_array(4, a);
        push_ptr(mk_str_doc(mv_string("(into "), a), &nodes);
        push_ptr(pretty_syntax(syntax->is.val, a), &nodes);
        push_ptr(pretty_syntax(syntax->is.type, a), &nodes);
        push_ptr(mv_str_doc(mk_string(")", a), a), &nodes);
        out = mv_cat_doc(nodes, a);
        break;
    }
    case SOutOf: {
        PtrArray nodes = mk_ptr_array(4, a);
        push_ptr(mk_str_doc(mv_string("(out-of "), a), &nodes);
        push_ptr(pretty_syntax(syntax->is.val, a), &nodes);
        push_ptr(pretty_syntax(syntax->is.type, a), &nodes);
        push_ptr(mv_str_doc(mk_string(")", a), a), &nodes);
        out = mv_cat_doc(nodes, a);
        break;
    }
    case SDynAlloc: {
        PtrArray nodes = mk_ptr_array(4, a);
        push_ptr(mk_str_doc(mv_string("(dyn-alloc "), a), &nodes);
        push_ptr(pretty_syntax(syntax->size, a), &nodes);
        push_ptr(mv_str_doc(mk_string(")", a), a), &nodes);
        out = mv_cat_doc(nodes, a);
        break;
    }
    case SModule: {
        out = mv_str_doc(mv_string("Pretty syntax not implemented for modules!"), a);
        break;
    }
    case SLabels: {
        PtrArray nodes = mk_ptr_array(2 + syntax->labels.terms.len, a);
        push_ptr(mk_str_doc(mv_string("(labels "), a), &nodes);
        push_ptr(pretty_syntax(syntax->labels.entry, a), &nodes);

        for (size_t i = 0; i < syntax->labels.terms.len; i++) {
            PtrArray label_nodes = mk_ptr_array(4, a);
            SymPtrACell cell = syntax->labels.terms.data[i];

            push_ptr(mk_str_doc(mv_string("["), a), &label_nodes);
            push_ptr(mk_str_doc(*symbol_to_string(cell.key), a), &label_nodes);
            push_ptr(pretty_syntax(cell.val, a), &label_nodes);
            push_ptr(mk_str_doc(mv_string("]"), a), &label_nodes);

            push_ptr(mv_sep_doc(label_nodes, a), &nodes);
        }

        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case SGoTo: {
        PtrArray nodes = mk_ptr_array(3, a);
        push_ptr(mk_str_doc(mv_string("(go-to "), a), &nodes);
        push_ptr(mk_str_doc(*symbol_to_string(syntax->go_to.label), a), &nodes);
        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);
        out = mv_cat_doc(nodes, a);
        break;
    }
    case SWithReset: {
        PtrArray nodes = mk_ptr_array(3, a);

        push_ptr(mk_str_doc(mv_string("(with-reset ["), a), &nodes);
        push_ptr(mk_str_doc(*symbol_to_string(syntax->with_reset.point_sym), a), &nodes);
        push_ptr(mk_str_doc(mv_string("]"), a), &nodes);

        push_ptr(pretty_syntax(syntax->with_reset.expr, a), &nodes);

        push_ptr(mk_str_doc(mv_string("[handler"), a), &nodes);
        push_ptr(mk_str_doc(*symbol_to_string(syntax->with_reset.in_sym), a), &nodes);
        push_ptr(mk_str_doc(*symbol_to_string(syntax->with_reset.cont_sym), a), &nodes);
        push_ptr(mk_str_doc(mv_string("]"), a), &nodes);

        push_ptr(pretty_syntax(syntax->with_reset.handler, a), &nodes);
        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case SResetTo: {
        PtrArray nodes = mk_ptr_array(3, a);
        push_ptr(mk_str_doc(mv_string("(reset-to "), a), &nodes);
        push_ptr(pretty_syntax(syntax->reset_to.point, a), &nodes);
        push_ptr(pretty_syntax(syntax->reset_to.arg, a), &nodes);
        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case SSequence: {
        PtrArray nodes = mk_ptr_array(2 + syntax->sequence.elements.len, a);
        push_ptr(mk_str_doc(mv_string("(seq"), a), &nodes);
        for (size_t i = 0; i < syntax->sequence.elements.len; i++) {
            SeqElt* elt = syntax->sequence.elements.data[i];
            if (elt->is_binding) {
                PtrArray let_nodes = mk_ptr_array(4, a);
                push_ptr(mk_str_doc(mv_string("[let!"), a), &let_nodes);
                push_ptr(mk_str_doc(*symbol_to_string(elt->symbol), a), &let_nodes);
                push_ptr(pretty_syntax(elt->expr, a), &nodes);
                push_ptr(mk_str_doc(mv_string("]"), a), &let_nodes);

                push_ptr(mv_sep_doc(let_nodes, a), &nodes);
            } else {
                push_ptr(pretty_syntax(elt->expr, a), &nodes);
            }
        }
        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case SProcType: {
        PtrArray nodes = mk_ptr_array(syntax->proc_type.args.len + 4, a) ;
        push_ptr(mv_str_doc(mk_string("(Proc (", a), a), &nodes);
        
        for (size_t i = 0; i < syntax->proc_type.args.len ; i++)  {
            push_ptr(pretty_syntax(syntax->proc_type.args.data[i], a), &nodes);
        }

        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);
        push_ptr(pretty_syntax(syntax->proc_type.return_type, a), &nodes);
        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case SStructType: {
        PtrArray nodes = mk_ptr_array(syntax->struct_type.fields.len + 2, a) ;
        push_ptr(mv_str_doc(mk_string("(Struct", a), a), &nodes);
        
        for (size_t i = 0; i < syntax->struct_type.fields.len ; i++)  {
            PtrArray fnodes = mk_ptr_array(4, a);
            push_ptr(mk_str_doc(mv_string("[."), a), &fnodes);
            push_ptr(mk_str_doc(*symbol_to_string(syntax->struct_type.fields.data[i].key), a), &fnodes);
            push_ptr(pretty_syntax(syntax->struct_type.fields.data[i].val, a), &fnodes);
            push_ptr(mk_str_doc(mv_string("]"), a), &fnodes);

            push_ptr(mv_sep_doc(fnodes, a), &nodes);
        }

        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case SEnumType: {
        PtrArray nodes = mk_ptr_array(syntax->enum_type.variants.len + 2, a) ;
        push_ptr(mv_str_doc(mk_string("(Enum", a), a), &nodes);
        
        for (size_t i = 0; i < syntax->enum_type.variants.len ; i++)  {
            PtrArray fnodes = mk_ptr_array(4, a);
            push_ptr(mk_str_doc(mv_string("[:"), a), &fnodes);
            push_ptr(mk_str_doc(*symbol_to_string(syntax->struct_type.fields.data[i].key), a), &fnodes);

            PtrArray* args = syntax->enum_type.variants.data[i].val;
            PtrArray anodes = mk_ptr_array(args->len, a);
            for (size_t j = 0; j < args->len; j++) {
                push_ptr(pretty_syntax(args->data[j], a), &anodes);
            }

            push_ptr(mv_sep_doc(anodes, a), &fnodes);
            push_ptr(mk_str_doc(mv_string("]"), a), &fnodes);

            push_ptr(mv_sep_doc(fnodes, a), &nodes);
        }

        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case SResetType: {
        PtrArray nodes = mk_ptr_array(4, a) ;
        push_ptr(mv_str_doc(mk_string("(Reset ", a), a), &nodes);
        push_ptr(pretty_syntax(syntax->reset_type.in, a), &nodes);
        push_ptr(pretty_syntax(syntax->reset_type.out, a), &nodes);
        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case SDynamicType: {
        PtrArray nodes = mk_ptr_array(3, a) ;
        push_ptr(mk_str_doc(mv_string("(Dynamic "), a), &nodes);
        push_ptr(pretty_syntax(syntax->dynamic_type, a), &nodes);
        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case SDistinctType: {
        PtrArray nodes = mk_ptr_array(3, a) ;
        push_ptr(mk_str_doc(mv_string("(Distinct "), a), &nodes);
        push_ptr(pretty_syntax(syntax->distinct_type, a), &nodes);
        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);
        out = mv_cat_doc(nodes, a);
        break;
    }
    case SAllType: {
        PtrArray nodes = mk_ptr_array(5, a) ;
        push_ptr(mk_str_doc(mv_string("(All ["), a), &nodes);

        SymbolArray arr = syntax->bind_type.bindings;
        PtrArray arg_nodes = mk_ptr_array(arr.len, a);
        for (size_t i = 0; i < arr.len; i++) {
            push_ptr(mk_str_doc(*symbol_to_string(arr.data[i]), a), &arg_nodes);
        }
        push_ptr(mv_sep_doc(arg_nodes, a), &nodes);
        push_ptr(mk_str_doc(mv_string("]"), a), &nodes);
        push_ptr(pretty_syntax(syntax->bind_type.body, a), &nodes);
        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case SExistsType: {
        out = mv_str_doc(mk_string("Pretty syntax not implemented for existential type", a), a);
        break;
    }
    case STypeFamily: {
        PtrArray nodes = mk_ptr_array(5, a) ;
        push_ptr(mk_str_doc(mv_string("(Family ["), a), &nodes);

        SymbolArray arr = syntax->bind_type.bindings;
        PtrArray arg_nodes = mk_ptr_array(arr.len, a);
        for (size_t i = 0; i < arr.len; i++) {
            push_ptr(mk_str_doc(*symbol_to_string(arr.data[i]), a), &arg_nodes);
        }
        push_ptr(mv_sep_doc(arg_nodes, a), &nodes);
        push_ptr(mk_str_doc(mv_string("]"), a), &nodes);
        push_ptr(pretty_syntax(syntax->bind_type.body, a), &nodes);
        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case SCheckedType: {
        out = pretty_type(syntax->type_val, a);
        break;
    }
    case SAnnotation: {
        out = mv_str_doc(mk_string("Pretty syntax not implemented for annotation!", a), a);
        break;
    }
    default:
        panic(mv_string("Internal Error in pretty_syntax: Unknown syntax Type"));
    }

    return out;
}

Document* pretty_def(Definition* def, Allocator* a) {
    PtrArray nodes = mk_ptr_array(4, a);
    push_ptr(mv_str_doc(mk_string("(def", a), a), &nodes);
    push_ptr(mk_str_doc(*symbol_to_string(def->bind), a), &nodes);
    push_ptr(pretty_syntax(def->value, a), &nodes);
    push_ptr(mv_str_doc(mk_string(")", a), a), &nodes);
    return mv_sep_doc(nodes, a);
}

Document* pretty_toplevel(TopLevel* toplevel, Allocator* a) {
    Document* out = NULL;
    switch (toplevel->type) {
    case TLDef:
        out = pretty_def(&toplevel->def, a);
        break;
    case TLExpr:
        out = pretty_syntax(toplevel->expr, a);
        break;
    }
    return out;
}

PiType* toplevel_type(TopLevel top) {
    PiType* out = NULL;
    switch (top.type) {
    case TLExpr:
        out = top.expr->ptype;
        break;
    case TLDef:
        out = top.def.value->ptype;
        break;
    }
    return out;
}
