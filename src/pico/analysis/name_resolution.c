#include "pico/analysis/name_resolution.h"

#include "data/result.h"
#include "data/string.h"
#include "pico/values/values.h"
#include "pico/syntax/concrete.h"
#include "pico/syntax/syntax.h"

typedef u64_array sym_array;
typedef struct resolve_env {
    environment* env;
    sym_array shadowed;
} resolve_env;
resolve_result resolve_dynamic_i(ob_rawtree raw, resolve_env env, allocator a);

bool is_symbol(ob_rawtree* raw) {
    return (raw->type == RawAtom && raw->data.value.type == VSymbol);
}

bool get_symbol_list(symbol_array* arr, ob_rawtree nodes, allocator a) {
    if (nodes.type == RawAtom) { return false; }

    for (size_t i = 0; i < nodes.data.nodes.len; i++) {
        ob_rawtree* node = aref_ptr(i, nodes.data.nodes);
        if (node->type != RawAtom) { return false; }
        if (node->data.value.type != VSymbol) { return false;  }
        push_u64(node->data.value.term.symbol, arr, a);
    }
    return true;
}

resolve_result mk_application(ob_rawtree raw, resolve_env env, allocator a) {
    resolve_result fn_res = resolve_dynamic_i(*(ob_rawtree*)(aref_ptr(0, raw.data.nodes)), env, a);
    if (fn_res.type == Err) {
        return fn_res;
    }

    resolve_result res;
    res.type = Ok;
    res.data.out.type = SApplication;
    res.data.out.data.application.function = mem_alloc(sizeof(syntax), a);
    *res.data.out.data.application.function = fn_res.data.out;
    res.data.out.data.application.args = mk_ptr_array(raw.data.nodes.len - 1, a);

    for (size_t i = 1; i < raw.data.nodes.len; i++) {
        resolve_result arg_res = resolve_dynamic_i(*(ob_rawtree*)(aref_ptr(i, raw.data.nodes)), env, a);
        if (arg_res.type == Ok) {
            syntax* arg = mem_alloc(sizeof(syntax), a);
            *arg = arg_res.data.out;
            push_ptr(arg, &res.data.out.data.application.args, a);
        }
        else {
            delete_syntax(res.data.out, a);
            res = arg_res;
            break;
        }
    }

    return res;
}

resolve_result mk_term(ob_term_former_t former, ob_rawtree raw, resolve_env env, allocator a) {
    resolve_result res;
    switch (former) {
    case FProcedure: {
        if (raw.data.nodes.len != 3) {
            res.type = Err;
            res.data.error_message = mk_string("Function term former requires 2 arguments!", a);
            return res;
        }

        symbol_array arguments = mk_u64_array(2, a);
        if (!get_symbol_list(&arguments, *(ob_rawtree*)aref_ptr(1, raw.data.nodes), a)) {
            res.type = Err;
            res.data.error_message = mk_string("Function term former requires first arguments to be a symbol-list!", a);
            return res;
        }

        for (size_t i = 0; i < arguments.len; i++) {
            push_u64(aref_u64(i, arguments), &env.shadowed, a);
        }
        resolve_result rbody = resolve_dynamic_i(*(ob_rawtree*)(aref_ptr(2, raw.data.nodes)), env, a);
        if (rbody.type == Err) {
            return rbody;
        }
        else 
            res.type = Ok;
        res.data.out.type = SFunction;
        res.data.out.data.function.args = arguments;
        res.data.out.data.function.body = mem_alloc(sizeof(syntax), a);
        *res.data.out.data.function.body = rbody.data.out;
        break;
    }
    case FApplication: {
        res = mk_application(raw, env, a);
        break;
    }
    case FDestructor: {
        res.type = Err;
        res.data.error_message = mk_string("Destructor term former not implemented!", a);
        break;
    }
    case FCorecursor: {
        res.type = Err;
        res.data.error_message = mk_string("Corecursor term former not implemented!", a);
        break;
    }
    case FConstructor: {
        res.type = Err;
        res.data.error_message = mk_string("Constructor term former not implemented!", a);
        break;
    }
    case FRecursor: {
        res.type = Err;
        res.data.error_message = mk_string("Recursor term former not implemented!", a);
        break;
    }
    case FProjector: {
        res.type = Err;
        res.data.error_message = mk_string("Projector term former not implemented!", a);
        break;
    }
    case FStructure: {
        res.type = Err;
        res.data.error_message = mk_string("Structure term former not implemented!", a);
        break;
    }
    case FIf: {
        if (raw.data.nodes.len != 4) {
            res.type = Err;
            res.data.error_message = mk_string("If term former expects precisely 3 arguments!", a);
            return res;
        }
        syn_array terms = mk_ptr_array(3, a);
        for (size_t i = 1; i < raw.data.nodes.len; i++) {
            ob_rawtree* rptr = aref_ptr(i, raw.data.nodes);
            resolve_result rterm = resolve_dynamic_i(*rptr, env, a);
            if (rterm.type == Err) {
                delete_ptr_array(terms, (void(*)(void*, allocator))delete_syntax_pointer, a);
                return rterm;
            }
            else {
                syntax* term = mem_alloc(sizeof(syntax), a);
                *term = rterm.data.out;
                push_ptr(term, &terms, a);
            }
        }

        res.type = Ok;
        res.data.out.type = SIf;
        res.data.out.data.if_expr.condition = aref_ptr(0, terms);
        res.data.out.data.if_expr.true_branch = aref_ptr(1, terms);
        res.data.out.data.if_expr.false_branch = aref_ptr(2, terms);
        sdelete_ptr_array(terms, a);
        break;
    }
    case FLet: {
        res.type = Err;
        res.data.error_message = mk_string("Let term former not implemented!", a);
        break;
    }
    default:
        res.type = Err;
        res.data.error_message = mk_string("Internal Error: Invalid term former!", a);
    }
    return res;
}

// Convert to AST at runtime
resolve_result resolve_dynamic_i(ob_rawtree raw, resolve_env env, allocator a) {
    resolve_result res;
    switch (raw.type) {
    case RawAtom: {
        res.type = Ok;
        if (raw.data.value.type == VSymbol) {
            res.data.out.type = SVariable;
            res.data.out.data.variable = raw.data.value.term.symbol;
        }
        else {
            res.data.out.type = SValue;
            res.data.out.data.val = raw.data.value;
        }
        break;
    }
    case RawList: {
        // Currently, can only have function calls, so all RawLists compile down to an application
        if (raw.data.nodes.len < 1) {
            res.type = Err;
            res.data.error_message = mk_string("Raw Syntax must have at least one element!", a);
            return res;
        }
        if (is_symbol(aref_ptr(0, raw.data.nodes))) {
            ob_symbol sym = ((ob_rawtree*)aref_ptr(0, raw.data.nodes))->data.value.term.symbol;
            ob_value* val = env_lookup(sym, env.env);
            if (!val) {
                res.type = Err;
                res.data.error_message = mk_string("Can't find symbol!", a);
            }
            else if (val->type == VFormer) {
                return mk_term(val->term.former, raw, env, a);
            }
            else {
                return mk_application(raw, env, a);
            }
        }
        else {
            return mk_application(raw, env, a);
        }
        break;
    }
    default: {
        res.type = Err;
        res.data.error_message = mk_string("Name Resolution Received an invalid Raw Syntax Tree", a);
    }
    }
    return res;
}

resolve_result resolve_dynamic(ob_rawtree raw, environment* env, allocator a) {
    resolve_env r_env;
    r_env.env = env;
    r_env.shadowed = mk_u64_array(32, a);
    resolve_result out = resolve_dynamic_i(raw, r_env, a);
    sdelete_u64_array(r_env.shadowed, a);
    return out;
}
