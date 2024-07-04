#include "pico/syntax/syntax.h"
#include "pretty/standard_types.h"

typedef struct syntax_call {
    pi_symbol field;
    syntax* fnc;
    syntax* arg;
} syntax_call;


syntax* mk_lit_i64_syn(const int64_t value, allocator a) {
    //value_root(value, lcl);
    syntax* out = (syntax*)mem_alloc(sizeof(syntax), a);
    out->type = SLitI64;
    out->lit_i64 = value; return out;
}

/* The Syntax Destructor */
void delete_syntax(syntax syntax, allocator a) {
    switch (syntax.type)
        {
        case SLitI64: 
        case SLitBool: 
            // Nothing 
            break;
        case SApplication: {
            delete_syntax_pointer(syntax.application.function, a);
            for (size_t i = 0; i < syntax.application.args.len; i++) {
                delete_syntax_pointer(aref_ptr(i, syntax.application.args), a);
            }
            sdelete_ptr_array(syntax.application.args, a);
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
            //unrecognised_branch("in delete_syntax");
        }
}

void delete_syntax_pointer(syntax* syntax, allocator a) {
    delete_syntax(*syntax, a);
    mem_free(syntax, a);
}

document* pretty_syntax(syntax* syntax, allocator a) {
    document* out = NULL;
    switch (syntax->type) {
    case SLitI64: {
        out = pretty_u64(syntax->lit_i64, a);
        break;
    }
    case SLitBool: {
        if (syntax->lit_i64 == 0) {
            out = mk_str_doc(mv_string("false"), a);
        } else {
            out = mk_str_doc(mv_string("true"), a);
        }
        break;
    }
    case SVariable: {
        out = mk_str_doc(*symbol_to_string(syntax->variable), a);
        break;
    }
    case SProcedure: {
        ptr_array nodes = mk_ptr_array(4 + syntax->procedure.args.len, a);
        push_ptr(mv_str_doc((mk_string("(proc (", a)), a), &nodes, a);
        for (size_t i = 0; i < syntax->procedure.args.len; i++) {
            document* arg = mk_str_doc(*symbol_to_string(aref_u64(i, syntax->procedure.args)), a);
            push_ptr(arg, &nodes, a);
        }
        push_ptr(mv_str_doc((mk_string(")", a)), a), &nodes, a);
        push_ptr(pretty_syntax(syntax->procedure.body, a), &nodes, a);
        push_ptr(mv_str_doc((mk_string(")", a)), a), &nodes, a);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case SApplication: {
        document* head = pretty_syntax(syntax->application.function, a);
        ptr_array nodes = mk_ptr_array(1 + syntax->application.args.len, a);
        push_ptr(head, &nodes, a);
        for (size_t i = 0; i < syntax->application.args.len; i++) {
            document* node = pretty_syntax(aref_ptr(i, syntax->application.args), a);
            push_ptr(node, &nodes, a);
        }
        out = mv_sep_doc(nodes, a);
        break;
    }
    case SConstructor: {
        out = mv_str_doc(mk_string("pretty_syntax not implemented on constructor", a), a);
        break;
    }
    case SRecursor: {
        out = mv_str_doc(mk_string("pretty_syntax not implemented on recursor", a), a);
        break;
    }
    case SStructure:
    case SProjector:

    case SLet:
    case SIf: {
        ptr_array nodes = mk_ptr_array(6, a);
        push_ptr(mv_str_doc(mk_string("(if", a), a), &nodes, a);
        push_ptr(pretty_syntax(syntax->if_expr.condition, a), &nodes, a);
        push_ptr(pretty_syntax(syntax->if_expr.true_branch, a), &nodes, a);
        push_ptr(pretty_syntax(syntax->if_expr.false_branch, a), &nodes, a);
        push_ptr(mv_str_doc(mk_string(")", a), a), &nodes, a);
        out = mv_sep_doc(nodes, a);
        break;
    }
    default: {
        out = mv_str_doc(mk_string("Internal Error in pretty_syntax: Unknown Syntax Type", a), a);
    }
    }
    return out;
}

void delete_def(definition def, allocator a) {
    delete_syntax_pointer(def.value, a);
}

void delete_toplevel(toplevel top, allocator a) {
    switch(top.type) {
    case TLExpr:
        delete_syntax(top.expr, a);
        break;
    case TLDef:
        delete_def(top.def, a);
        break;
    }
}

document* pretty_def(definition* def, allocator a) {
    ptr_array nodes = mk_ptr_array(4, a);
    push_ptr(mv_str_doc(mk_string("( def", a), a), &nodes, a);
    push_ptr(mk_str_doc(*symbol_to_string(def->bind), a), &nodes, a);
    push_ptr(pretty_syntax(def->value, a), &nodes, a);
    push_ptr(mv_str_doc(mk_string(")", a), a), &nodes, a);
    return mv_sep_doc(nodes, a);
}

document* pretty_toplevel(toplevel* toplevel, allocator a) {
    document* out = NULL;
    switch (toplevel->type) {
    case TLExpr:
        out = pretty_syntax(&toplevel->expr, a);
        break;
    case TLDef:
        out = pretty_def(&toplevel->def, a);
        break;
    }
    return out;
}

pi_type* toplevel_type(toplevel top) {
    pi_type* out = NULL;
    switch (top.type) {
    case TLExpr:
        out = top.expr.ptype;
        break;
    case TLDef:
        out = top.def.value->ptype;
        break;
    }
    return out;
}
