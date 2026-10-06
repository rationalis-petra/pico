#include <string.h>
#include "platform/signals.h"

#include "components/pretty/standard_types.h"
#include "pico/data/name_ptr_amap.h"

#include "rune/eval/eval.h"

/**
 * In addition to continuations set by the host,
 * there is another type of continuation we must deal with: 
 * if we encounter a 'thunk' (variable bound to an expression) 
 */

typedef struct {
    bool is_continuation;
    bool is_error;
    // Extra tag - not present in the available results.
    // Thunks ought to be evalated immediately.
    bool is_thunk;
} InternalTag;

typedef struct {
    InternalTag tag;
    union {
        ValRef value;
        Document* error_message;
    };
} InternalResult;

typedef struct {
    RuneEnv* env;
    String filename;
    NameValAssoc* locals;
    RegionAllocator* region;
    size_t depth;
} RuneEvalCtx;

struct RuneEnv {
    NamePtrAMap values;
    ExprPool* exprs;
    ValueHeap* vals;
    Allocator* gpa;
};

static RuneEvalResult get_value_internal(Name name, RuneEnv* env, size_t depth, RegionAllocator* region);

RuneEnv* mk_rune_env(Allocator* a) {
    RuneEnv* env = mem_alloc(sizeof(RuneEnv), a);
    *env = (RuneEnv) {
        .values = mk_name_ptr_amap(64, a),
        .exprs = mk_expr_pool(a),
        .vals = mk_value_heap(a),
        .gpa = a,
    };
    return env;
}

void delete_rune_env(RuneEnv* env) {
    sdelete_name_ptr_amap(env->values);
    delete_expr_pool(env->exprs);
    delete_value_heap(env->vals);
    mem_free(env, env->gpa);
}

Pools get_pools(RuneEnv* env) {
    return (Pools) {
        .expr = env->exprs,
        .value = env->vals,
    };
}

void rune_add_def(Name name, ExprRef expr, RuneEnv* env) {
    RuneClosureEnv ce = {};
    ValRef ref = mk_rune_closure(ce, expr, env->vals); 
    // TODO: detect duplicate definitions and produce error.
    name_ptr_insert(name, (void*)ref.ref, &env->values);
}

void rune_add_val_def(Name name, ValRef ref, RuneEnv* env) {
    // TODO: detect duplicate definitions and produce error.
    name_ptr_insert(name, (void*)ref.ref, &env->values);
}

RuneEvalResult eval_rune_internal(ExprRef expression, RuneEvalCtx ctx) {
    ctx.depth++;
    Allocator a = ra_to_gpa(ctx.region);
    Pools pools = get_pools(ctx.env);
    Expr expr = get_expr(expression, pools.expr);
    switch (expr.type) {
    case EVar: {
        ValRef* val = name_val_alookup(expr.var, *ctx.locals);
        if (val) {
            return (RuneEvalResult) {
                .type = AValue,
                .value = *val,
            };
        }

        RuneEvalResult result = get_value_internal(expr.var, ctx.env, ctx.depth, ctx.region);
        return result;
    }
    case EFn: {
        // TODO: capture variables!
        NameArray captures = free_vars(expression, pools.expr, &a);
        for (size_t i = 0; i < captures.len; i++) {
            if (!name_val_alookup(captures.data[i], *ctx.locals)) {
                // Not a local variabe: delete from free vars.
                captures.len--;
                captures.data[i] = captures.data[captures.len];
                i--;
            }
        }
        RuneClosureEnv env = {
            .local_vars = mk_name_val_assoc(captures.len, &a),
        };
        for (size_t i = 0; i < captures.len; i++) {
            ValRef* val = name_val_alookup(captures.data[i], *ctx.locals);
            name_val_bind(captures.data[i], *val, &env.local_vars);
        }
        ValRef closure = mk_rune_closure(env, expression, pools.value);
        return (RuneEvalResult) {
            .type = AValue,
            .value = closure,
        };
        break;
    }
    case EApp: {
        RuneEvalResult result = eval_rune_internal(expr.app.fn, ctx);
        if (result.type == AError) return result;
        size_t args_consumed = 0;
        while (args_consumed < expr.app.args.len) {
            ValRef fn = result.value;
            ValueSort sort = get_sort(fn);
            if (sort == ValClosure) {
                RuneClosure func = get_rune_closure(fn);
                if (get_expr(func.expr, pools.expr).type != EFn) {
                    return (RuneEvalResult) {
                        .type = AError,
                        .error_message = mv_cstr_doc("Non-function used as a function.", &a),
                    };
                }
                Expr fn_expr = get_expr(func.expr, pools.expr);
                const size_t req_args = fn_expr.fn.args.len - func.num_curried;
                const size_t avail_args = expr.app.args.len - args_consumed;
                const size_t actual_args = req_args <= avail_args ? req_args : avail_args;
                U64Array done_args = mk_u64_array(actual_args, &a);
                for (size_t i = 0; i < actual_args; i++) {
                    ExprRef arg = get_expr_elt(expr.app.args, i + args_consumed);
                    RuneEvalResult result = eval_rune_internal(arg, ctx);
                    if (result.type != AValue) return result;
                    push_u64(result.value.ref, &done_args);
                }
                args_consumed += actual_args;
                if (actual_args < req_args) {
                    // Form a a new function (curried)
                    ValRef new = mk_rune_curried_closure(func.env, func.expr, func.num_curried + actual_args, pools.value);
                    RuneClosure newc = get_rune_closure(new);
                    for (size_t i = 0; i < func.num_curried; i++) {
                        newc.curried[i] = func.curried[i];
                    }
                    for (size_t i = 0; i < actual_args; i++) {
                        newc.curried[func.num_curried + i] = (ValRef){done_args.data[i]};
                    }
                    return (RuneEvalResult) {
                        .type = AValue,
                        .value = new,
                    };
                } else {
                    // Evaluate the function
                    NameValAssoc* new_locals = region_alloc(sizeof(NameValAssoc), ctx.region);
                    *new_locals = mk_name_val_assoc(actual_args + func.env.local_vars.len, &a);
                    RuneEvalCtx new_ctx = ctx;
                    new_ctx.locals = new_locals;
                    for (size_t i = 0; i < func.num_curried; i++) {
                        name_val_bind(fn_expr.fn.args.data[i], func.curried[i], new_locals);
                    }
                    for (size_t i = 0; i < actual_args; i++) {
                        name_val_bind(fn_expr.fn.args.data[i + func.num_curried], (ValRef){done_args.data[i]}, new_locals);
                    }
                    for (size_t i = 0; i < func.env.local_vars.len; i++) {
                        NameValACell cell = func.env.local_vars.data[i];
                        name_val_bind(cell.key, cell.val, new_locals);
                    }
                    result = eval_rune_internal(fn_expr.fn.body, new_ctx);
                    if (result.type == AError) return result;
                }
            } else if (sort == ValData) {
                const size_t avail_args = expr.app.args.len - args_consumed;
                U64Array done_args = mk_u64_array(avail_args, &a);
                for (size_t i = 0; i < avail_args; i++) {
                    ExprRef arg = get_expr_elt(expr.app.args, i + args_consumed);
                    RuneEvalResult result = eval_rune_internal(arg, ctx);
                    if (result.type != AValue) return result;
                    push_u64(result.value.ref, &done_args);
                }
                RuneData* old = get_rune_data(fn);
                ValRef out = mk_rune_data(old->name, old->type, old->len + avail_args, pools.value);
                RuneData* new = get_rune_data(out);
                for (size_t i = 0; i < old->len; i++) {
                    new->values[i] = old->values[i];
                }
                for (size_t i = 0; i < avail_args; i++) {
                    new->values[i + old->len] = (ValRef){done_args.data[i]};
                }
                return (RuneEvalResult) {
                    .type = AValue,
                    .value = out,
                };
            } else if (sort == ValHost) {
                Bridge bridge = get_host_bridge(fn);
                if (bridge.type != BFn) {
                    return (RuneEvalResult) {
                        .type = AError,
                        .error_message = mv_cstr_doc("Non-function used as a function.", &a),
                    };
                }

                RuneEvalResult (*callee)(ValSlice values, RuneEnv* env, RegionAllocator* region);
                get_host_val(fn, &callee);
                if (bridge.fn.variadic) {
                    U64Array done_args = mk_u64_array(expr.app.args.len, &a);
                    for (size_t i = 0; i < expr.app.args.len; i++) {
                        ExprRef arg = get_expr_elt(expr.app.args, i + args_consumed);
                        RuneEvalResult result = eval_rune_internal(arg, ctx);
                        if (result.type != AValue) return result;
                        push_u64(result.value.ref, &done_args);
                    }
                    ValSlice args = {
                        .len = done_args.len,
                        .data = (void*)done_args.data,
                    };
                    return callee(args, ctx.env, ctx.region);
                } else {
                    panic(mv_string("TODO: implement currying for non-variadic builtins."));
                }
            } else {
                return (RuneEvalResult) {
                    .type = AError,
                    .error_message = mv_cstr_doc("Non-function used as a function.", &a),
                };
            }
        }
        return result;
    }
    case ECtor: {
        ValRefOption type = {.type = None};
        if (expr.ctor.type.type == Some) {
            RuneEvalResult result = eval_rune_internal(expr.ctor.type.val, ctx);
            if (result.type != AValue) return result;
            type = (ValRefOption) {.type = Some, .val = result.value};
        }
        ValRef out = mk_rune_data(expr.ctor.name, type, 0, pools.value); 
        return (RuneEvalResult) {
            .type = AValue,
            .value = out,
        };
    };
    case ERecord: {
        ValRefOption type = {.type = None};
        if (expr.record.type.type == Some) {
            RuneEvalResult result = eval_rune_internal(expr.record.type.val, ctx);
            if (result.type != AValue) return result;
            type = (ValRefOption) {.type = Some, .val = result.value};
        }
        ValRef out = mk_rune_record(type, expr.record.fields.len,  pools.value); 
        RuneRecord* record = get_rune_record(out);
        for (size_t i = 0; i < record->len; i++) {
            NameExprCell cell = get_expr_map_elt(expr.record.fields, i, pools.expr);
            RuneEvalResult result = eval_rune_internal(cell.val, ctx);
            if (result.type != AValue) return result;
            record->values[i] = (NameValPr) {
                .name = cell.name,
                .val = result.value,
            };
        };
        return (RuneEvalResult) {
            .type = AValue,
            .value = out,
        };
    };
    case EInt: {
        ValRef int_ref = mk_rune_int(expr.num, pools.value);
        return (RuneEvalResult) {
            .type = AValue,
            .value = int_ref,
        };
    }
    case EString: {
        ValRef string_ref = mk_rune_string(expr.string.memsize, pools.value);
        String string = get_rune_string(string_ref);
        memcpy(string.bytes, expr.string.bytes, expr.string.memsize);
        return (RuneEvalResult) {
            .type = AValue,
            .value = string_ref,
        };
    }
    case EList: {
        ValRef listref = mk_rune_list(expr.list.len, pools.value); 
        RuneList list = get_rune_list(listref);
        for (size_t i = 0; i < expr.list.len; i++) {
            ExprRef elt = get_expr_elt(expr.list, i);
            RuneEvalResult result = eval_rune_internal(elt, ctx);
            if (result.type == AError) return result;
            list.data[i] = result.value;
        }
        return (RuneEvalResult) {
            .type = AValue,
            .value = listref,
        };
    }
    }
    panic(mv_string("Invalid rune expression provided to eval_rune_internal"));
}

/**
 * Evaluate the given expression in the environment.
 */
RuneEvalResult eval_rune(ExprRef expression, RuneEnv* env, RegionAllocator* region) {
    Allocator a = ra_to_gpa(region);
    RuneEvalCtx ctx = {
        .locals = region_alloc(sizeof(NameValAssoc), region),
        //.filename = val.closure.env.file_path,
        .env = env,
        .region = region,
        .depth = 0,
    };
    *ctx.locals = mk_name_val_assoc(2, &a);
    return eval_rune_internal(expression, ctx);
}


static RuneEvalResult get_value_internal(Name name, RuneEnv* env, size_t depth, RegionAllocator* region) {
    Allocator a = ra_to_gpa(region);
    ValRef* idx = (ValRef*)name_ptr_lookup(name, env->values);
    if (!idx) {
        PtrArray nodes = mk_ptr_array(2, &a);
        push_ptr(mv_cstr_doc("Definition not found:", &a), &nodes);
        push_ptr(mk_str_doc(view_name_string(name), &a), &nodes);
        return (RuneEvalResult) {
            .type = AError,
            .error_message = mv_sep_doc(nodes, &a),
        };
    }
    ValRef ref = *idx;
    ValueSort sort = get_sort(ref);
    if (sort == ValClosure) {
        // Check for thunk - if expression type is function, then is closure,
        // otherwise is thunk that needs evaluation.

        Pools pools = get_pools(env);
        RuneClosure func = get_rune_closure(ref);
        Expr expr = get_expr(func.expr, pools.expr);
        if (expr.type != EFn) {
            // Is a thunk - evaluate.
            RuneEvalCtx ctx = {
                .locals = region_alloc(sizeof(NameValAssoc), region),
                .filename = func.env.file_path,
                .env = env,
                .region = region,
                .depth = depth
            };
            *ctx.locals = scopy_name_val_assoc(func.env.local_vars, &a);
            return eval_rune_internal(func.expr, ctx);
        }
        // if we fall out here, expression was a closure, so continue on to
        // return it as a value.
    }
    return (RuneEvalResult) {
        .value = ref,
    };
}
/**
 * Get the value of a definition that was previously added to the environment.
 * Note that definitions are not evaluated until 
 */
RuneEvalResult get_value(Name name, RuneEnv* env, RegionAllocator* region) {
    return get_value_internal(name, env, 0, region);
}


Document* pretty_rune_value(ValRef ref, RuneEnv* env, Allocator* a) {
    ValueSort sort = get_sort(ref);
    switch (sort) {
    case ValClosure: {
        RuneClosure closure = get_rune_closure(ref);
        return pretty_rune_expr(closure.expr, env->exprs, a);
      break;
    }
    case ValData: {
        RuneData* data = get_rune_data(ref);
        PtrArray nodes = mk_ptr_array(1 + data->len, a);
        PtrArray head =  mk_ptr_array(2, a);
        push_ptr(mv_cstr_doc(":", a), &head);
        push_ptr(mk_str_doc(view_name_string(data->name), a), &head);
        push_ptr(mv_cat_doc(head, a), &nodes);
        for (size_t i = 0; i < data->len; i++) {
            push_ptr(pretty_rune_value(data->values[i], env, a), &nodes);
        }
        return mv_sep_doc(nodes, a);
    }
    case ValRecord: {
        RuneRecord* record = get_rune_record(ref);
        PtrArray nodes = mk_ptr_array(1 + record->len, a);
        push_ptr(mv_cstr_doc("record", a), &nodes);
        for (size_t i = 0; i < record->len; i++) {
            PtrArray fnodes = mk_ptr_array(2, a);
            PtrArray head =  mk_ptr_array(2, a);
            push_ptr(mv_cstr_doc(".", a), &head);
            push_ptr(mk_str_doc(view_name_string(record->values[i].name), a), &head);
            push_ptr(mv_cat_doc(head, a), &fnodes);
            push_ptr(pretty_rune_value(record->values[i].val, env, a), &fnodes);
            push_ptr(mv_group_doc(mk_paren_doc("[", "]", mv_sep_doc(fnodes, a), a), a), &nodes);
        }
        return mv_sep_doc(nodes, a);
    }
    case ValInt:
        return pretty_i64(get_int(ref), a);
      break;
    case ValList: {
        RuneList list = get_rune_list(ref);
        PtrArray nodes = mk_ptr_array(list.len + 1, a);
        push_ptr(mv_cstr_doc("list", a), &nodes);
        for (size_t i = 0; i < list.len; i++) {
            push_ptr(pretty_rune_value(list.data[i], env, a), &nodes);
        }
        return mv_group_doc(mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a), a);
    }
    case ValString:
        return mk_paren_doc("\"", "\"", mv_str_doc(get_rune_string(ref), a), a);
        break;
    };
    panic(mv_string("Trying to produce document for invalid pretty value"));
}
