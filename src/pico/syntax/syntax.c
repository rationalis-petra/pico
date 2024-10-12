#include "pico/syntax/syntax.h"
#include "pretty/standard_types.h"

typedef struct SyntaxCall {
    Symbol field;
    Syntax* fnc;
    Syntax* arg;
} SyntaxCall;


Syntax* mk_lit_i64_syn(const int64_t value, Allocator* a) {
    Syntax* out = (Syntax*)mem_alloc(sizeof(Syntax), a);
    out->type = SLitI64;
    out->lit_i64 = value; return out;
}

/* The Syntax Destructor */
void delete_syntax(Syntax syntax, Allocator* a) {
    switch (syntax.type) {
        case SLitI64: 
        case SLitBool: 
            // Nothing 
            break;
        case SApplication: {
            delete_syntax_pointer(syntax.application.function, a);
            for (size_t i = 0; i < syntax.application.args.len; i++) {
                delete_syntax_pointer(aref_ptr(i, syntax.application.args), a);
            }
            sdelete_ptr_array(syntax.application.args);
            break;
        }
        case SIf: {
            delete_syntax_pointer(syntax.if_expr.condition, a);
            delete_syntax_pointer(syntax.if_expr.true_branch, a);
            delete_syntax_pointer(syntax.if_expr.false_branch, a);
            break;
        }
        default:
            break;
        }
}

void delete_syntax_pointer(Syntax* syntax, Allocator* a) {
    delete_syntax(*syntax, a);
    mem_free(syntax, a);
}

Document* pretty_syntax(Syntax* syntax, Allocator* a) {
    Document* out = NULL;
    switch (syntax->type) {
    case SLitI64: {
        out = pretty_u64(syntax->lit_i64, a);
        break;
    }
    case SLitBool: {
        if (syntax->lit_i64 == 0) {
            out = mk_str_doc(mv_string(":false"), a);
        } else {
            out = mk_str_doc(mv_string(":true"), a);
        }
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
            Document* arg = mk_str_doc(*symbol_to_string(aref_u64(i, syntax->all.args)), a);
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
            Document* node = pretty_syntax(aref_ptr(i, syntax->application.args), a);
            push_ptr(node, &nodes);
        }
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
    case SLet: {
        out = mv_str_doc(mk_string("pretty_syntax not implemented on let", a), a);
        break;
    }
    case SIf: {
        PtrArray nodes = mk_ptr_array(6, a);
        push_ptr(mv_str_doc(mk_string("(if", a), a), &nodes);
        push_ptr(pretty_syntax(syntax->if_expr.condition, a), &nodes);
        push_ptr(pretty_syntax(syntax->if_expr.true_branch, a), &nodes);
        push_ptr(pretty_syntax(syntax->if_expr.false_branch, a), &nodes);
        push_ptr(mv_str_doc(mk_string(")", a), a), &nodes);
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
    case SCheckedType: {
        out = pretty_type(syntax->type_val, a);
        break;
    }
    default: {
        out = mv_str_doc(mk_string("Internal Error in pretty_syntax: Unknown syntax Type", a), a);
    }
    }
    return out;
}

void delete_def(Definition def, Allocator* a) {
    delete_syntax_pointer(def.value, a);
}

void delete_toplevel(TopLevel top, Allocator* a) {
    switch(top.type) {
    case TLExpr:
        delete_syntax_pointer(top.expr, a);
        break;
    case TLDef:
        delete_def(top.def, a);
        break;
    }
}

Document* pretty_def(Definition* def, Allocator* a) {
    PtrArray nodes = mk_ptr_array(4, a);
    push_ptr(mv_str_doc(mk_string("( def", a), a), &nodes);
    push_ptr(mk_str_doc(*symbol_to_string(def->bind), a), &nodes);
    push_ptr(pretty_syntax(def->value, a), &nodes);
    push_ptr(mv_str_doc(mk_string(")", a), a), &nodes);
    return mv_sep_doc(nodes, a);
}

Document* pretty_toplevel(TopLevel* toplevel, Allocator* a) {
    Document* out = NULL;
    switch (toplevel->type) {
    case TLExpr:
        out = pretty_syntax(toplevel->expr, a);
        break;
    case TLDef:
        out = pretty_def(&toplevel->def, a);
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
