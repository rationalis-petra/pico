#include "pico/analysis/abstraction.h"

#include "data/result.h"
#include "data/string.h"
#include "pico/values/values.h"
#include "pico/syntax/concrete.h"
#include "pico/syntax/syntax.h"
#include "pico/binding/shadow_env.h"


// Declarations for mutual recursion
// ---
// "Internal" implementation of resolve_dynamic 

abs_expr_result abstract_expr_i(pi_rawtree raw, shadow_env* env, allocator a);

bool is_symbol(pi_rawtree* raw) {
    return (raw->type == RawAtom && raw->data.atom.type == ASymbol);
}

bool get_symbol_list(symbol_array* arr, pi_rawtree nodes, allocator a) {
    if (nodes.type == RawAtom) { return false; }

    for (size_t i = 0; i < nodes.data.nodes.len; i++) {
        pi_rawtree* node = aref_ptr(i, nodes.data.nodes);
        if (node->type != RawAtom) { return false; }
        if (node->data.atom.type != ASymbol) { return false;  }
        push_u64(node->data.atom.symbol, arr, a);
    }
    return true;
}

abs_result to_toplevel(abs_expr_result res) {
    abs_result out;
    switch (res.type) {
    case Ok: {
        out.type = Ok;
        out.out.type = TLExpr;
        out.out.expr = res.out;
        break;
    }
    case Err:
        out.type = Err;
        out.error_message = res.error_message;
    }
    return out;
}


abs_expr_result mk_application(pi_rawtree raw, shadow_env* env, allocator a) {
    abs_expr_result fn_res = abstract_expr_i(*(pi_rawtree*)(aref_ptr(0, raw.data.nodes)), env, a);
    if (fn_res.type == Err) {
        return fn_res;
    }

    abs_expr_result res;
    res.type = Ok;
    res.out.type = SApplication;
    res.out.application.function = mem_alloc(sizeof(syntax), a);
    *res.out.application.function = fn_res.out;
    res.out.application.args = mk_ptr_array(raw.data.nodes.len - 1, a);

    for (size_t i = 1; i < raw.data.nodes.len; i++) {
        abs_expr_result arg_res = abstract_expr_i(*(pi_rawtree*)(aref_ptr(i, raw.data.nodes)), env, a);
        if (arg_res.type == Ok) {
            syntax* arg = mem_alloc(sizeof(syntax), a);
            *arg = arg_res.out;
            push_ptr(arg, &res.out.application.args, a);
        }
        else {
            delete_syntax(res.out, a);
            res = arg_res;
            break;
        }
    }

    return res;
}

abs_expr_result mk_term(pi_term_former_t former, pi_rawtree raw, shadow_env* env, allocator a) {
    abs_expr_result res;
    switch (former) {
    case FProcedure: {
        if (raw.data.nodes.len != 3) {
            res.type = Err;
            res.error_message = mk_string("Procedure term former requires 2 arguments!", a);
            return res;
        }

        symbol_array arguments = mk_u64_array(2, a);
        if (!get_symbol_list(&arguments, *(pi_rawtree*)aref_ptr(1, raw.data.nodes), a)) {
            res.type = Err;
            res.error_message = mk_string("Procedure term former requires first arguments to be a symbol-list!", a);
            return res;
        }

        shadow_vars(arguments, env, a);
        abs_expr_result rbody = abstract_expr_i(*(pi_rawtree*)(aref_ptr(2, raw.data.nodes)), env, a);
        if (rbody.type == Err) {
            return rbody;
        }
        else 
            res.type = Ok;
        res.out.type = SProcedure;
        res.out.procedure.args = arguments;
        res.out.procedure.body = mem_alloc(sizeof(syntax), a);
        *res.out.procedure.body = rbody.out;
        break;
    }
    case FApplication: {
        res = mk_application(raw, env, a);
        break;
    }
    case FDestructor: {
        res.type = Err;
        res.error_message = mk_string("Destructor term former not implemented!", a);
        break;
    }
    case FCorecursor: {
        res.type = Err;
        res.error_message = mk_string("Corecursor term former not implemented!", a);
        break;
    }
    case FConstructor: {
        res.type = Err;
        res.error_message = mk_string("Constructor term former not implemented!", a);
        break;
    }
    case FRecursor: {
        res.type = Err;
        res.error_message = mk_string("Recursor term former not implemented!", a);
        break;
    }
    case FProjector: {
        res.type = Err;
        res.error_message = mk_string("Projector term former not implemented!", a);
        break;
    }
    case FStructure: {
        res.type = Err;
        res.error_message = mk_string("Structure term former not implemented!", a);
        break;
    }
    case FIf: {
        if (raw.data.nodes.len != 4) {
            res.type = Err;
            res.error_message = mk_string("If term former expects precisely 3 arguments!", a);
            return res;
        }
        syn_array terms = mk_ptr_array(3, a);
        for (size_t i = 1; i < raw.data.nodes.len; i++) {
            pi_rawtree* rptr = aref_ptr(i, raw.data.nodes);
            abs_expr_result rterm = abstract_expr_i(*rptr, env, a);
            if (rterm.type == Err) {
                delete_ptr_array(terms, (void(*)(void*, allocator))delete_syntax_pointer, a);
                return rterm;
            }
            else {
                syntax* term = mem_alloc(sizeof(syntax), a);
                *term = rterm.out;
                push_ptr(term, &terms, a);
            }
        }

        res.type = Ok;
        res.out.type = SIf;
        res.out.if_expr.condition = aref_ptr(0, terms);
        res.out.if_expr.true_branch = aref_ptr(1, terms);
        res.out.if_expr.false_branch = aref_ptr(2, terms);
        sdelete_ptr_array(terms, a);
        break;
    }
    case FLet: {
        res.type = Err;
        res.error_message = mk_string("Let term former not implemented!", a);
        break;
    }
    default:
        res.type = Err;
        res.error_message = mk_string("Internal Error: Invalid term former!", a);
    }
    return res;
}

// Convert to AST at runtime
abs_expr_result abstract_expr_i(pi_rawtree raw, shadow_env* env, allocator a) {
    abs_expr_result res;
    // Resolution does not perform type analysis, so we set the type pointer to NULL
    // This is then resolved in the typechecking stage;
    res.out.ptype = NULL;
    switch (raw.type) {
    case RawAtom: {
        if (raw.data.atom.type == ASymbol) {
            res.type = Ok;
            res.out.type = SVariable;
            res.out.variable = raw.data.atom.symbol;
        }
        else if (raw.data.atom.type == AI64) {
            res.type = Ok;
            res.out.type = SLitI64;
            res.out.lit_i64 = raw.data.atom.int_64;
        }
        else if (raw.data.atom.type == ABool) {
            res.type = Ok;
            res.out.type = SLitBool;
            res.out.lit_i64 = raw.data.atom.int_64;
        } else  {
            res.type = Err;
            res.error_message = mk_string("Currently, can only make literal values out of type i64." , a);
        }
        break;
    }
    case RawList: {
        // Currently, can only have function calls, so all Rawaists compile down to an application
        if (raw.data.nodes.len < 1) {
            res.type = Err;
            res.error_message = mk_string("Raw Syntax must have at least one element!", a);
            return res;
        }
        if (is_symbol(aref_ptr(0, raw.data.nodes))) {
            pi_symbol sym = ((pi_rawtree*)aref_ptr(0, raw.data.nodes))->data.atom.symbol;
            shadow_entry entry = shadow_env_lookup(sym, env);
            switch (entry.type) {
            case SErr:
                res.type = Err;
                string str1 = mk_string("Can't find symbol! ", a);
                string* str2 = symbol_to_string(sym);
                res.error_message = string_cat(str1, *str2, a);
                delete_string(str1, a);
                break;
            case SShadowed: 
                return mk_application(raw, env, a);
                break;
            case SGlobal:
                if (entry.vtype->sort == TPrim && entry.vtype->prim == TFormer) {
                    return mk_term(*((pi_term_former_t*)entry.value), raw, env, a);
                } else {
                    return mk_application(raw, env, a);
                }
            }
        }
        else {
            return mk_application(raw, env, a);
        }
        break;
    }
    default: {
        res.type = Err;
        res.error_message = mk_string("Name Resolution Received an invalid Raw Syntax Tree", a);
    }
    }
    return res;
}


abs_result mk_toplevel(pi_term_former_t former, pi_rawtree raw, shadow_env* env, allocator a) {
    abs_result res;
    switch (former) {
    case FDefine: {
        if (raw.data.nodes.len != 3) {
            res.type = Err;
            res.error_message = mk_string("Definitions expect exactly 2 terms", a);
            return res;
        }

        if (!is_symbol(raw.data.nodes.data[1])) {
            res.type = Err;
            res.error_message = mk_string("First argument to definitions should be a symbol", a);
            return res;
        }

        pi_symbol sym = ((pi_rawtree*)aref_ptr(1, raw.data.nodes))->data.atom.symbol;
        
        pi_rawtree* raw_term = (pi_rawtree*)aref_ptr(2, raw.data.nodes);
        abs_expr_result inter = abstract_expr_i(*raw_term, env, a);
        if (inter.type == Err) {
            res.type = Err;
            res.error_message = inter.error_message;
            return res;
        }

        syntax* term = (syntax*) mem_alloc(sizeof(syntax), a);
        *term = inter.out;
        // res.out;

        res.type = Ok;
        res.out.type = TLDef;
        res.out.def.bind = sym;
        res.out.def.value = term;

        break;
    }
    default:
        // fallback: make term. 
        res = to_toplevel(mk_term(former, raw, env, a));
        break;
    }
    return res;
}

abs_result abstract_i(pi_rawtree raw, shadow_env* env, allocator a) {
    // first: try a unique toplevel-form
    bool unique_toplevel = false;

    if (raw.type == RawList && raw.data.nodes.len > 1) {
        if (is_symbol(aref_ptr(0, raw.data.nodes))) {
            pi_symbol sym = ((pi_rawtree*)aref_ptr(0, raw.data.nodes))->data.atom.symbol;
            shadow_entry entry = shadow_env_lookup(sym, env);
            switch (entry.type) {
            case SGlobal:
                if (entry.vtype->sort == TPrim && entry.vtype->prim == TFormer) {
                    unique_toplevel = true;
                    return mk_toplevel(*((pi_term_former_t*)entry.value), raw, env, a);
                }
            default:
                break;
            }
        }
    } 

    if (!unique_toplevel) {
        return to_toplevel(abstract_expr_i(raw, env, a));
    }

    abs_result out; 
    out.type = Err;
    out.error_message = mk_string("Logic error in abstract_i: reached unreachable area.", a);
    return out;
}

abs_expr_result abstract_expr(pi_rawtree raw, environment* env, allocator a) {
    shadow_env* s_env = mk_shadow_env(a, env);
    abs_expr_result out = abstract_expr_i(raw, s_env, a);
    delete_shadow_env(s_env, a);
    return out; 
}

abs_result abstract(pi_rawtree raw, environment* env, allocator a) {
    shadow_env* s_env = mk_shadow_env(a, env);
    abs_result out = abstract_i(raw, s_env, a);
    delete_shadow_env(s_env, a);
    return out;
}
