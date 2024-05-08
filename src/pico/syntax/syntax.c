#include "pico/syntax/syntax.h"

typedef struct syntax_call {
    ob_symbol field;
    syntax* fnc;
    syntax* arg;
} syntax_call;


syntax* mv_val_syn(const ob_value value, allocator a) {
    //value_root(value, lcl);
    syntax* out = (syntax*)mem_alloc(sizeof(syntax), a);
    out->type = SValue;
    out->data.val = value;
    return out;
}
//syntax* mk_val_doc(const ob_value value, allocator a);

/* The Syntax Destructor */
void delete_syntax(syntax syntax, allocator a) {
    switch (syntax.type)
        {
        case SValue: 
            //value_unroot(syntax.data.val);
            break;
        case SApplication: {
            delete_syntax_pointer(syntax.data.application.function, a);
            for (size_t i = 0; i < syntax.data.application.args.len; i++) {
                delete_syntax_pointer(aref_ptr(i, syntax.data.application.args), a);
            }
            sdelete_ptr_array(syntax.data.application.args, a);
            break;
        }
        case SDestructor: {
            delete_syntax_pointer(syntax.data.destructor.value, a);
            break;
        }
        case SIf: {
            delete_syntax_pointer(syntax.data.if_expr.condition, a);
            delete_syntax_pointer(syntax.data.if_expr.true_branch, a);
            delete_syntax_pointer(syntax.data.if_expr.false_branch, a);
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
    case SValue: {
        out = pretty_value(syntax->data.val, a);
        break;
    }
    case SVariable: {
        out = mk_str_doc(*symbol_to_string(syntax->data.variable), a);
        break;
    }
    case SFunction: {
        ptr_array nodes = mk_ptr_array(4 + syntax->data.function.args.len, a);
        push_ptr(mv_str_doc((mk_string("(fn (", a)), a), &nodes, a);
        for (size_t i = 0; i < syntax->data.function.args.len; i++) {
            document* arg = mk_str_doc(*symbol_to_string(aref_u64(i, syntax->data.function.args)), a);
            push_ptr(arg, &nodes, a);
        }
        push_ptr(mv_str_doc((mk_string(")", a)), a), &nodes, a);
        push_ptr(pretty_syntax(syntax->data.function.body, a), &nodes, a);
        push_ptr(mv_str_doc((mk_string(")", a)), a), &nodes, a);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case SApplication: {
        document* head = pretty_syntax(syntax->data.application.function, a);
        ptr_array nodes = mk_ptr_array(1 + syntax->data.application.args.len, a);
        push_ptr(head, &nodes, a);
        for (size_t i = 0; i < syntax->data.application.args.len; i++) {
            document* node = pretty_syntax(aref_ptr(i, syntax->data.application.args), a);
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
    case SDestructor: {
        string* co_name = symbol_to_string(syntax->data.destructor.name);
        document* head = mv_str_doc(string_cat(mv_string(";"), *co_name, a), a);

        document* codata = pretty_syntax(syntax->data.destructor.value, a);
        ptr_array final_doc = mk_ptr_array(4, a);
        push_ptr(mv_str_doc(mk_string("(", a), a), &final_doc, a);
        push_ptr(head, &final_doc, a);
        push_ptr(codata, &final_doc, a);
        push_ptr(mv_str_doc(mk_string(")", a), a), &final_doc, a);
        out = mv_sep_doc(final_doc, a);
        break;
    }
    case SCorecursor:
    case SStructure:
    case SProjector:

    case SLet:
    case SIf: {
        ptr_array nodes = mk_ptr_array(6, a);
        push_ptr(mv_str_doc(mk_string("(if", a), a), &nodes, a);
        push_ptr(pretty_syntax(syntax->data.if_expr.condition, a), &nodes, a);
        push_ptr(pretty_syntax(syntax->data.if_expr.true_branch, a), &nodes, a);
        push_ptr(pretty_syntax(syntax->data.if_expr.false_branch, a), &nodes, a);
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
