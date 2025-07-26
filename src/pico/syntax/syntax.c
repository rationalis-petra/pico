#include "data/string.h"
#include "platform/signals.h"
#include "pretty/standard_types.h"

#include "pico/values/array.h"
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

Document* pretty_syntax_internal(Syntax* syntax, Allocator* a);
Document* pretty_syntax_callback(Syntax* syntax, void* ctx, Allocator* a) {
    return pretty_syntax_internal(syntax, a);
}

Document* pretty_syntax_internal(Syntax* syntax, Allocator* a) {
    Document* out = NULL;
    DocStyle former_style = scolour(colour(60, 190, 24), dstyle);
    DocStyle field_style = scolour(colour(237, 218, 50), dstyle);
    DocStyle var_style = scolour(colour(212, 130, 42), dstyle);
    //DocStyle fstyle = scolour(colour(149, 187, 191), dstyle);

    switch (syntax->type) {
    case SLitUntypedIntegral: {
    case SLitTypedIntegral: 
        out = pretty_i64(syntax->integral.value, a);
        break;
    }
    case SLitUntypedFloating: {
    case SLitTypedFloating: 
        out = pretty_f64(syntax->floating.value, a);
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
    case SLitUnit: {
        out = mk_str_doc(mv_string(":unit"), a);
        break;
    }
    case SLitArray: {
        PrettyElem pelem = (PrettyElem) {
            .print_elem = (print_element)pretty_syntax_callback,
            .context = NULL,
        };
        Array array = (Array) {
            .shape.len = syntax->array_lit.shape.len,
            .shape.data = syntax->array_lit.shape.data,
            .data = syntax->array_lit.subterms.data,
        };
        size_t index_size = sizeof(void*);
        return pretty_array(array, index_size, pelem, a);
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
    case SAbsVariable: {
        PtrArray nodes = mk_ptr_array(4, a);
        push_ptr(mk_str_doc(mv_string("#abv"), a), &nodes);
        // TODO (BUG LOGIC): replace with pretty_size.
        push_ptr(mk_str_doc(*symbol_to_string(syntax->abvar.symbol), a), &nodes);
        push_ptr(pretty_u64(syntax->abvar.index, a), &nodes);
        push_ptr(pretty_ptr(syntax->abvar.value, a), &nodes);
        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SProcedure: {
        PtrArray nodes = mk_ptr_array(4, a);
        push_ptr(mv_style_doc(former_style, mv_str_doc((mk_string("proc", a)), a), a), &nodes);

        if (syntax->procedure.implicits.len != 0) {
            PtrArray impl_nodes = mk_ptr_array(syntax->procedure.implicits.len, a);
            for (size_t i = 0; i < syntax->procedure.implicits.len; i++) {
                Document* arg = mk_str_doc(*symbol_to_string(syntax->procedure.implicits.data[i].key), a);
                push_ptr(mv_nest_doc(2, arg, a), &impl_nodes);
            }
            push_ptr(mk_paren_doc("{", "}", mv_sep_doc(impl_nodes, a), a), &nodes);
        }

        PtrArray arg_nodes = mk_ptr_array(syntax->procedure.args.len, a);
        for (size_t i = 0; i < syntax->procedure.args.len; i++) {
            Document* arg = mk_str_doc(*symbol_to_string(syntax->procedure.args.data[i].key), a);
            push_ptr(mv_nest_doc(2, arg, a), &arg_nodes);
        }
        push_ptr(mk_paren_doc("[", "]", mv_style_doc(var_style, mv_sep_doc(arg_nodes, a), a), a), &nodes);

        push_ptr(mv_nest_doc(2, pretty_syntax_internal(syntax->procedure.body, a), a), &nodes);
        out = mk_paren_doc("(", ")", mv_hsep_doc(nodes, a), a);
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
        push_ptr(pretty_syntax_internal(syntax->all.body, a), &nodes);
        push_ptr(mv_str_doc((mk_string(")", a)), a), &nodes);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case SMacro: {
        PtrArray nodes = mk_ptr_array(2, a);
        push_ptr(mv_str_doc((mk_string("macro", a)), a), &nodes);
        push_ptr(mv_nest_doc(2, pretty_syntax_internal(syntax->transformer, a), a), &nodes);
        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SApplication: {
        Document* head = pretty_syntax_internal(syntax->application.function, a);
        PtrArray nodes = mk_ptr_array(2, a);
        push_ptr(head, &nodes);
        PtrArray args = mk_ptr_array(syntax->application.args.len, a);
        for (size_t i = 0; i < syntax->application.args.len; i++) {
            Document* node = pretty_syntax_internal(syntax->application.args.data[i], a);
            push_ptr(node, &args);
        }
        push_ptr(mv_nest_doc(2, mv_sep_doc(args, a), a), &nodes);
        out = mk_paren_doc("(", ")", mv_hsep_doc(nodes, a), a);
        break;
    }
    case SAllApplication: {
        Document* head = pretty_syntax_internal(syntax->all_application.function, a);
        bool print_types = syntax->all_application.types.len != 0;
        PtrArray nodes = mk_ptr_array((print_types ? 3 : 1) + syntax->all_application.types.len + syntax->all_application.args.len, a);
        push_ptr(head, &nodes);

        if (print_types) push_ptr(mk_str_doc(mv_string("{"), a), &nodes);
        for (size_t i = 0; i < syntax->all_application.types.len; i++) {
            Document* node = pretty_syntax_internal(syntax->all_application.types.data[i], a);
            push_ptr(node, &nodes);
        }
        if (print_types) push_ptr(mk_str_doc(mv_string("}"), a), &nodes);

        for (size_t i = 0; i < syntax->all_application.args.len; i++) {
            Document* node = pretty_syntax_internal(syntax->all_application.args.data[i], a);
            push_ptr(node, &nodes);
        }
        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SConstructor: {
        PtrArray nodes = mk_ptr_array(3, a);
        if (syntax->constructor.enum_type) {
            push_ptr(pretty_syntax_internal(syntax->constructor.enum_type, a), &nodes);
        }
        push_ptr(mk_str_doc(mv_string(":"), a), &nodes);
        push_ptr(mk_str_doc(*symbol_to_string(syntax->variant.tagname), a), &nodes);
        out = mv_cat_doc(nodes, a);
        break;
    }
    case SVariant: {
        PtrArray nodes = mk_ptr_array(syntax->variant.args.len + 1, a);
        PtrArray ctor_parts = mk_ptr_array(3, a);

        if (syntax->variant.enum_type) {
            push_ptr(pretty_syntax_internal(syntax->variant.enum_type, a), &ctor_parts);
        }
        push_ptr(mk_str_doc(mv_string(":") , a), &ctor_parts);
        push_ptr(mk_str_doc(*symbol_to_string(syntax->variant.tagname), a), &ctor_parts);
        push_ptr(mv_cat_doc(ctor_parts, a), &nodes);

        for (size_t i = 0; i < syntax->variant.args.len; i++) {
            push_ptr(pretty_syntax_internal(syntax->variant.args.data[i], a), &nodes);
        }
        
        if (syntax->variant.args.len > 0) {
            out = mk_paren_doc("(", ")",  mv_sep_doc(nodes, a), a);
        } else {
            out = mv_sep_doc(nodes, a);
        }
        break;
    }
    case SMatch: {
        // Nodes = (match¹ val² expr expr ... )³, hence len + 3
        PtrArray nodes = mk_ptr_array(syntax->match.clauses.len + 3, a);

        push_ptr(mk_str_doc(mv_string("(match"), a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->match.val, a), &nodes);

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
            push_ptr(pretty_syntax_internal(clause->body, a), &clause_nodes);
            push_ptr(mk_str_doc(mv_string(")"), a), &clause_nodes);

            push_ptr(mv_cat_doc(clause_nodes, a), &nodes);
        }

        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case SStructure: {
        PtrArray nodes = mk_ptr_array(2 + syntax->structure.fields.len, a);
        push_ptr(mv_style_doc(former_style, mv_cstr_doc("struct", a), a), &nodes);

        for (size_t i = 0; i < syntax->structure.fields.len; i++) {
            PtrArray field_nodes = mk_ptr_array(2, a);

            push_ptr(mv_style_doc(field_style, mk_str_doc(*symbol_to_string(syntax->structure.fields.data[i].key), a), a), &field_nodes);
            push_ptr(pretty_syntax_internal(syntax->structure.fields.data[i].val, a), &field_nodes);

            Document* fdoc = mv_nest_doc(2, mv_group_doc(mk_paren_doc("[.", "]", mv_sep_doc(field_nodes, a), a), a), a);
            push_ptr(fdoc, &nodes);
        }
        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SProjector: {
        PtrArray nodes = mk_ptr_array(3, a);

        push_ptr(pretty_syntax_internal(syntax->projector.val, a), &nodes);
        push_ptr(mk_str_doc(mv_string("."), a), &nodes);
        push_ptr(mk_str_doc(*symbol_to_string(syntax->projector.field), a), &nodes);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case SInstance: {
        PtrArray nodes = mk_ptr_array(4 + syntax->instance.fields.len, a);

        push_ptr(mk_str_doc(mv_string("instance "), a), &nodes);

        if (syntax->instance.params.len > 0) {
            const SymbolArray params = syntax->instance.params;
            PtrArray pnodes = mk_ptr_array(params.len, a);
            for (size_t i = 0; i < params.len; i++) {
                push_ptr(mk_str_doc(*symbol_to_string(params.data[i]), a), &pnodes);
            }
            push_ptr(mk_paren_doc("[", "]", mv_sep_doc(pnodes, a), a), &nodes);
        }

        if (syntax->instance.implicits.len > 0) {
            const SymPtrAssoc implicits = syntax->instance.implicits;
            PtrArray inodes = mk_ptr_array(implicits.len, a);
            for (size_t i = 0; i < implicits.len; i++) {
                if (implicits.data[i].val) {
                    PtrArray anodes = mk_ptr_array(2, a);
                    push_ptr(mk_str_doc(*symbol_to_string(implicits.data[i].key), a), &anodes);
                    push_ptr(pretty_syntax_internal(implicits.data[i].val, a), &anodes);
                    push_ptr(mk_paren_doc("(", ")", mv_sep_doc(anodes, a), a), &inodes);
                } else {
                    push_ptr(mk_str_doc(*symbol_to_string(implicits.data[i].key), a), &inodes);
                }
            }
            push_ptr(mk_paren_doc("{", "}", mv_sep_doc(inodes, a), a), &nodes);
        }

        push_ptr(pretty_syntax_internal(syntax->instance.constraint, a), &nodes);

        for (size_t i = 0; i < syntax->instance.fields.len; i++) {
            PtrArray fnodes = mk_ptr_array(2, a);
            push_ptr(mk_str_doc(*symbol_to_string(syntax->instance.fields.data[i].key), a), &fnodes);
            push_ptr(pretty_syntax_internal(syntax->instance.fields.data[i].val, a), &fnodes);
            push_ptr(mk_paren_doc("[.", "]", mv_sep_doc(fnodes, a), a), &nodes);
        }

        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SDynamic: {
        PtrArray nodes = mk_ptr_array(3, a);

        push_ptr(mk_str_doc(mv_string("(dynamic "), a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->dynamic, a), &nodes);
        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);
        out = mv_cat_doc(nodes, a);
        break;
    }
    case SDynamicUse: {
        PtrArray nodes = mk_ptr_array(2, a);
        push_ptr(mk_str_doc(mv_string("use "), a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->use, a), &nodes);
        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
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
            push_ptr(pretty_syntax_internal(expr, a), &nodes);
            push_ptr(mk_str_doc(mv_string("]"), a), &let_nodes);

            push_ptr(mv_sep_doc(let_nodes, a), &nodes);
        }
        push_ptr(pretty_syntax_internal(syntax->let_expr.body, a), &nodes);
        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);

        out = mv_sep_doc(nodes, a);
        break;
    }
    case SDynamicLet: {
        PtrArray nodes = mk_ptr_array(3 + syntax->let_expr.bindings.len, a);
        push_ptr(mk_str_doc(mv_string("(bind"), a), &nodes);
        for (size_t i = 0; i < syntax->dyn_let_expr.bindings.len; i++) {
            DynBinding* bind = syntax->dyn_let_expr.bindings.data[i];
            PtrArray let_nodes = mk_ptr_array(4, a);
            push_ptr(mk_str_doc(mv_string("["), a), &let_nodes);
            push_ptr(pretty_syntax_internal(bind->var, a), &let_nodes);
            push_ptr(pretty_syntax_internal(bind->expr, a), &let_nodes);
            push_ptr(mk_str_doc(mv_string("]"), a), &let_nodes);

            push_ptr(mv_sep_doc(let_nodes, a), &nodes);
        }
        push_ptr(pretty_syntax_internal(syntax->let_expr.body, a), &nodes);
        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);

        out = mv_sep_doc(nodes, a);
        break;
    }
    case SIf: {
        PtrArray nodes = mk_ptr_array(3, a);
        push_ptr(pretty_syntax_internal(syntax->if_expr.condition, a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->if_expr.true_branch, a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->if_expr.false_branch, a), &nodes);
        out = mk_paren_doc("(if ", ")", mv_sep_doc(nodes, a), a);
        break;
    }

    case SIs: {
        PtrArray nodes = mk_ptr_array(2, a);
        push_ptr(pretty_syntax_internal(syntax->is.val, a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->is.type, a), &nodes);
        out = mk_paren_doc("(is ", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SInTo: {
        PtrArray nodes = mk_ptr_array(2, a);
        push_ptr(pretty_syntax_internal(syntax->into.type, a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->into.val, a), &nodes);
        out = mk_paren_doc("(into ", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SOutOf: {
        PtrArray nodes = mk_ptr_array(2, a);
        push_ptr(pretty_syntax_internal(syntax->out_of.type, a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->out_of.val, a), &nodes);
        out = mk_paren_doc("(out-of ", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SName: {
        PtrArray nodes = mk_ptr_array(2, a);
        push_ptr(pretty_syntax_internal(syntax->name.type, a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->name.val, a), &nodes);
        out = mk_paren_doc("(name ", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SUnName: {
        out = mk_paren_doc("(unname ", ")", pretty_syntax_internal(syntax->unname, a), a);
        break;
    }
    case SWiden: {
        PtrArray nodes = mk_ptr_array(2, a);
        push_ptr(pretty_syntax_internal(syntax->widen.val, a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->widen.type, a), &nodes);
        out = mk_paren_doc("(widen ", ")", mv_nest_doc(2, mv_sep_doc(nodes, a), a), a);
        break;
    }
    case SNarrow: {
        PtrArray nodes = mk_ptr_array(2, a);
        push_ptr(pretty_syntax_internal(syntax->narrow.val, a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->narrow.type, a), &nodes);
        out = mk_paren_doc("(narrow ", ")", mv_nest_doc(2, mv_sep_doc(nodes, a), a), a);
        break;
    }
    case SDynAlloc: {
        out = mk_paren_doc("(dyn-alloc ", ")", pretty_syntax_internal(syntax->size, a), a);
        break;
    }
    case SSizeOf: {
        out = mk_paren_doc("(size-of ", ")", pretty_syntax_internal(syntax->size, a), a);
        break;
    }
    case SAlignOf: {
        out = mk_paren_doc("(align-of ", ")", pretty_syntax_internal(syntax->size, a), a);
        break;
    }
    case SOffsetOf: {
        PtrArray nodes = mk_ptr_array(2, a);
        push_ptr(mk_str_doc(*symbol_to_string(syntax->offset_of.field), a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->offset_of.body, a), &nodes);
        out = mk_paren_doc("(offset-of ", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SModule: {
        out = mv_str_doc(mv_string("Pretty syntax not implemented for modules!"), a);
        break;
    }
    case SLabels: {
        PtrArray nodes = mk_ptr_array(3, a);
        push_ptr(mk_str_doc(mv_string("labels"), a), &nodes);
        push_ptr(mv_nest_doc(2, pretty_syntax_internal(syntax->labels.entry, a), a), &nodes);

        PtrArray label_terms = mk_ptr_array(syntax->labels.terms.len, a);
        for (size_t i = 0; i < syntax->labels.terms.len; i++) {
            PtrArray label_nodes = mk_ptr_array(3, a);
            SymPtrACell cell = syntax->labels.terms.data[i];
            SynLabelBranch* branch = cell.val;

            PtrArray arg_nodes = mk_ptr_array(branch->args.len, a);
            for (size_t i = 0; i < branch->args.len; i++) {
                Document* arg = mk_str_doc(*symbol_to_string(branch->args.data[i].key), a);
                push_ptr(arg, &arg_nodes);
            }

            push_ptr(mk_str_doc(*symbol_to_string(cell.key), a), &label_nodes);
            if (arg_nodes.len > 0) {
                push_ptr(mv_nest_doc(2, mk_paren_doc("[", "]", mv_sep_doc(arg_nodes, a), a), a), &label_nodes);
            }
            push_ptr(mv_nest_doc(2, pretty_syntax_internal(branch->body, a), a), &label_nodes);

            push_ptr(mk_paren_doc("[", "]", mv_hsep_doc(label_nodes, a), a), &label_terms);
        }
        push_ptr(mv_nest_doc(2, mv_vsep_doc(label_terms, a), a), &nodes);
        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SGoTo: {
        PtrArray nodes = mk_ptr_array(3, a);
        push_ptr(mk_str_doc(mv_string("go-to"), a), &nodes);
        push_ptr(mk_str_doc(*symbol_to_string(syntax->go_to.label), a), &nodes);

        PtrArray args = mk_ptr_array(syntax->go_to.args.len, a);
        for (size_t i = 0; i < syntax->go_to.args.len; i++) {
            push_ptr(mv_nest_doc(2, pretty_syntax_internal(syntax->go_to.args.data[i], a), a), &args);
        }
        if (args.len > 0) {
            push_ptr(mv_sep_doc(args, a), &nodes);
        }
        out = mk_paren_doc("(", ")", mv_hsep_doc(nodes, a), a);
        break;
    }
    case SWithReset: {
        PtrArray nodes = mk_ptr_array(3, a);

        push_ptr(mk_str_doc(mv_string("(with-reset ["), a), &nodes);
        push_ptr(mk_str_doc(*symbol_to_string(syntax->with_reset.point_sym), a), &nodes);
        push_ptr(mk_str_doc(mv_string("]"), a), &nodes);

        push_ptr(pretty_syntax_internal(syntax->with_reset.expr, a), &nodes);

        push_ptr(mk_str_doc(mv_string("[handler"), a), &nodes);
        push_ptr(mk_str_doc(*symbol_to_string(syntax->with_reset.in_sym), a), &nodes);
        push_ptr(mk_str_doc(*symbol_to_string(syntax->with_reset.cont_sym), a), &nodes);
        push_ptr(mk_str_doc(mv_string("]"), a), &nodes);

        push_ptr(pretty_syntax_internal(syntax->with_reset.handler, a), &nodes);
        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case SResetTo: {
        PtrArray nodes = mk_ptr_array(3, a);
        push_ptr(mk_str_doc(mv_string("(reset-to "), a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->reset_to.point, a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->reset_to.arg, a), &nodes);
        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case SSequence: {
        PtrArray nodes = mk_ptr_array(1 + syntax->sequence.elements.len, a);
        push_ptr(mv_style_doc(former_style, mv_cstr_doc("seq", a), a), &nodes);
        for (size_t i = 0; i < syntax->sequence.elements.len; i++) {
            SeqElt* elt = syntax->sequence.elements.data[i];
            if (elt->is_binding) {
                PtrArray let_nodes = mk_ptr_array(3, a);
                push_ptr(mv_style_doc(former_style, mv_cstr_doc("let!", a), a), &let_nodes);
                push_ptr(mv_style_doc(var_style, mk_str_doc(*symbol_to_string(elt->symbol), a), a), &let_nodes);
                push_ptr(pretty_syntax_internal(elt->expr, a), &let_nodes);

                push_ptr(mv_group_doc(mk_paren_doc("[", "]", mv_nest_doc(2, mv_sep_doc(let_nodes, a), a), a), a), &nodes);
            } else {
                push_ptr(pretty_syntax_internal(elt->expr, a), &nodes);
            }
        }
        out = mk_paren_doc("(", ")", mv_nest_doc(2, mv_sep_doc(nodes, a), a), a);
        break;
    }
    case SProcType: {
        PtrArray nodes = mk_ptr_array(syntax->proc_type.args.len + 4, a) ;
        push_ptr(mv_str_doc(mk_string("(Proc (", a), a), &nodes);
        
        for (size_t i = 0; i < syntax->proc_type.args.len ; i++)  {
            push_ptr(pretty_syntax_internal(syntax->proc_type.args.data[i], a), &nodes);
        }

        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->proc_type.return_type, a), &nodes);
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
            push_ptr(pretty_syntax_internal(syntax->struct_type.fields.data[i].val, a), &fnodes);
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
                push_ptr(pretty_syntax_internal(args->data[j], a), &anodes);
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
        push_ptr(pretty_syntax_internal(syntax->reset_type.in, a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->reset_type.out, a), &nodes);
        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case SDynamicType: {
        PtrArray nodes = mk_ptr_array(2, a) ;
        push_ptr(mk_str_doc(mv_string("Dynamic"), a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->dynamic_type, a), &nodes);
        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SNamedType: {
        PtrArray nodes = mk_ptr_array(2, a) ;
        push_ptr(mk_str_doc(mv_string("Named"), a), &nodes);
        push_ptr(mk_str_doc(*symbol_to_string(syntax->named_type.name), a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->named_type.body, a), &nodes);
        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SDistinctType: {
        PtrArray nodes = mk_ptr_array(2, a) ;
        push_ptr(mk_str_doc(mv_string("Distinct"), a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->distinct_type, a), &nodes);
        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SOpaqueType: {
        PtrArray nodes = mk_ptr_array(2, a) ;
        push_ptr(mk_str_doc(mv_string("Opaque"), a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->opaque_type, a), &nodes);
        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case STraitType: {
        PtrArray nodes = mk_ptr_array(2 + syntax->trait.fields.len, a) ;
        push_ptr(mk_str_doc(mv_string("Trait "), a), &nodes);

        PtrArray pnodes = mk_ptr_array(syntax->trait.vars.len, a) ;
        for (size_t i = 0; i < syntax->trait.vars.len; i++) {
            push_ptr(mk_str_doc(*symbol_to_string(syntax->trait.vars.data[i]), a), &pnodes);
        }
        push_ptr(mk_paren_doc("[", "]", mv_sep_doc(pnodes, a), a), &nodes);

        for (size_t i = 0; i < syntax->trait.fields.len ; i++)  {
            PtrArray fnodes = mk_ptr_array(2, a);
            push_ptr(mk_str_doc(*symbol_to_string(syntax->trait.fields.data[i].key), a), &fnodes);
            push_ptr(pretty_syntax_internal(syntax->trait.fields.data[i].val, a), &fnodes);
            push_ptr(mk_paren_doc("[.", "]", mv_sep_doc(fnodes, a), a), &nodes);
        }

        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
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
        push_ptr(pretty_syntax_internal(syntax->bind_type.body, a), &nodes);
        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case SExistsType: {
        out = mv_str_doc(mk_string("Pretty syntax not implemented for existential type", a), a);
        break;
    }
    case STypeFamily: {
        PtrArray nodes = mk_ptr_array(4, a) ;
        push_ptr(mk_str_doc(mv_string("Family"), a), &nodes);

        SymbolArray arr = syntax->bind_type.bindings;
        PtrArray arg_nodes = mk_ptr_array(arr.len, a);
        for (size_t i = 0; i < arr.len; i++) {
            push_ptr(mk_str_doc(*symbol_to_string(arr.data[i]), a), &arg_nodes);
        }
        push_ptr(mk_paren_doc("[", "]", mv_sep_doc(arg_nodes, a), a), &nodes);

        push_ptr(pretty_syntax_internal(syntax->bind_type.body, a), &nodes);
        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SLiftCType: {
        PtrArray nodes = mk_ptr_array(2, a) ;
        push_ptr(mk_str_doc(mv_string("C-Type"), a), &nodes);

        push_ptr(pretty_syntax_internal(syntax->c_type, a), &nodes);
        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
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
    case SReinterpret: {
        PtrArray nodes = mk_ptr_array(4, a);
        if (syntax->reinterpret.from_native) {
            push_ptr(mk_str_doc(mv_string("reinterpret-native"), a), &nodes);
        } else {
            push_ptr(mk_str_doc(mv_string("reinterpret-relic"), a), &nodes);
        }

        push_ptr(pretty_syntax_internal(syntax->reinterpret.type, a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->reinterpret.body, a), &nodes);

        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SConvert: {
        PtrArray nodes = mk_ptr_array(4, a);
        if (syntax->convert.from_native) {
            push_ptr(mk_str_doc(mv_string("convert-native"), a), &nodes);
        } else {
            push_ptr(mk_str_doc(mv_string("convert-relic"), a), &nodes);
        }

        push_ptr(pretty_syntax_internal(syntax->convert.type, a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->convert.body, a), &nodes);

        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case STypeOf: {
        PtrArray nodes = mk_ptr_array(4, a);
        push_ptr(mk_str_doc(mv_string("type-of"), a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->type_of, a), &nodes);
        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SDescribe: {
        PtrArray nodes = mk_ptr_array(4, a);
        push_ptr(mk_str_doc(mv_string("describe"), a), &nodes);
        push_ptr(mk_str_doc(*symbol_to_string(syntax->to_describe), a), &nodes);
        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SQuote: {
        Document* raw = pretty_rawtree(syntax->quoted, a);
        out = mk_paren_doc("(quote ", ")", mv_nest_doc(2, raw, a), a);
        break;
    }
    case SCapture: {
        RawTree raw = (RawTree) {
          .type = RawAtom,
          .atom = (Atom) {
              .type = ACapture,
              .capture = (Capture) {
                  .type = syntax->capture.type,
                  .value = syntax->capture.value,
              }
          },
        };
        Document* raw_doc = pretty_rawtree(raw, a);
        out = mk_paren_doc("(capture ", ")", mv_nest_doc(2, raw_doc, a), a);
        break;
    }
    }

    if (out == NULL)  {
        panic(mv_string("Internal Error in pretty_syntax_internal: Unknown syntax Type"));
    }
    out = mv_group_doc(out, a);
    return out;
}

Document* pretty_syntax(Syntax* syntax, Allocator* a) {
    DocStyle text_style = scolour(colour(195, 195, 209), dstyle);
    return mv_style_doc(text_style, pretty_syntax_internal(syntax, a), a);
}

Document* pretty_def(Definition* def, Allocator* a) {
    PtrArray nodes = mk_ptr_array(4, a);
    push_ptr(mv_str_doc(mk_string("def", a), a), &nodes);
    push_ptr(mk_str_doc(*symbol_to_string(def->bind), a), &nodes);
    push_ptr(mv_nest_doc(2, pretty_syntax(def->value, a), a), &nodes);
    return mk_paren_doc("(", ")", mv_hsep_doc(nodes, a), a);
}

Document* pretty_toplevel(TopLevel* toplevel, Allocator* a) {
    Document* out = NULL;
    switch (toplevel->type) {
    case TLDef:
        out = pretty_def(&toplevel->def, a);
        break;
    case TLOpen: {
        PtrArray docs = mk_ptr_array(toplevel->open.paths.len, a);
        for (size_t i = 0; i < toplevel->open.paths.len; i++) {
            SymbolArray* syms = toplevel->open.paths.data[i];
            PtrArray elts = mk_ptr_array(2 * syms->len, a);
            for (size_t j = 0; j < syms->len; j++) {
                push_ptr(mk_str_doc(*symbol_to_string(syms->data[j]), a), &elts);
                if (j + 1 != syms->len) {
                    push_ptr(mk_str_doc(mv_string("."), a), &elts);
                }
            }
              
            push_ptr(mv_cat_doc(elts, a), &docs);
        }
        out = mk_paren_doc("(open ", ")", mv_sep_doc(docs, a), a);
        break;
    }
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
    case TLOpen:
        out = NULL;
        break;
    case TLDef:
        out = top.def.value->ptype;
        break;
    }
    return out;
}
