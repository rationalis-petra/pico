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

RuneEvalResult rune_add_def(Name name, ExprRef expr, RuneEnv* env, RegionAllocator* region) {
    RuneEvalResult result = eval_rune(expr, env, region);
    if (result.type != AValue) return result;
    
    // TODO: detect duplicate definitions and produce error.
    name_ptr_insert(name, (void*)result.value.ref, &env->values);
    return result;
}

void rune_add_val_def(Name name, ValRef ref, RuneEnv* env) {
    // TODO: detect duplicate definitions and produce error.
    name_ptr_insert(name, (void*)ref.ref, &env->values);
}

RuneEvalResult eval_rune_internal(Expr expr, RuneEvalCtx ctx) {
    ctx.depth++;
    Allocator a = ra_to_gpa(ctx.region);
    Pools pools = get_pools(ctx.env);
    ExprPool* pool = pools.expr;
    ValueHeap* heap = pools.value;
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
        NameArray captures = free_vars(expr, pool, &a);
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
        ValRef closure = mk_rune_closure(env, expr, heap);
        return (RuneEvalResult) {
            .type = AValue,
            .value = closure,
        };
        break;
    }
    case EApp: {
        RuneEvalResult result = eval_rune_internal(get_expr(expr.app.fn, pool), ctx);
        if (result.type == AError) return result;
        size_t args_consumed = 0;
        while (args_consumed < expr.app.args.len) {
            ValRef fn = result.value;
            ValueSort sort = get_sort(fn);
            if (sort == ValClosure) {
                RuneClosure func = get_rune_closure(fn);
                if (func.expr.type != EFn) {
                    return (RuneEvalResult) {
                        .type = AError,
                        .error_message = mv_cstr_doc("Non-function used as a function.", &a),
                    };
                }
                const size_t req_args = func.expr.fn.args.len - func.num_curried;
                const size_t avail_args = expr.app.args.len - args_consumed;
                const size_t actual_args = req_args <= avail_args ? req_args : avail_args;
                U64Array done_args = mk_u64_array(actual_args, &a);
                for (size_t i = 0; i < actual_args; i++) {
                    ExprRef arg = get_expr_elt(expr.app.args, i + args_consumed);
                    RuneEvalResult result = eval_rune_internal(get_expr(arg, pool), ctx);
                    if (result.type != AValue) return result;
                    push_u64(result.value.ref, &done_args);
                }
                args_consumed += actual_args;
                if (actual_args < req_args) {
                    // Form a a new function (curried)
                    ValRef new = mk_rune_curried_closure(func.env, func.expr, func.num_curried + actual_args, heap);
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
                        name_val_bind(func.expr.fn.args.data[i], func.curried[i], new_locals);
                    }
                    for (size_t i = 0; i < actual_args; i++) {
                        name_val_bind(func.expr.fn.args.data[i + func.num_curried], (ValRef){done_args.data[i]}, new_locals);
                    }
                    for (size_t i = 0; i < func.env.local_vars.len; i++) {
                        NameValACell cell = func.env.local_vars.data[i];
                        name_val_bind(cell.key, cell.val, new_locals);
                    }
                    result = eval_rune_internal(get_expr(func.expr.fn.body, pool), new_ctx);
                    if (result.type == AError) return result;
                }
            } else if (sort == ValData) {
                const size_t avail_args = expr.app.args.len - args_consumed;
                U64Array done_args = mk_u64_array(avail_args, &a);
                for (size_t i = 0; i < avail_args; i++) {
                    ExprRef arg = get_expr_elt(expr.app.args, i + args_consumed);
                    RuneEvalResult result = eval_rune_internal(get_expr(arg, pool), ctx);
                    if (result.type != AValue) return result;
                    push_u64(result.value.ref, &done_args);
                }
                RuneData* old = get_rune_data(fn);
                ValRef out = mk_rune_data(old->name, old->type, old->len + avail_args, heap);
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
                        RuneEvalResult result = eval_rune_internal(get_expr(arg, pool), ctx);
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
    case EPr: {
      panic(mv_string("TODO: implement pair expressions."));
    }
    case EElt: {
      panic(mv_string("TODO: implement pair element."));
    }
    case ECtor: {
        ValRefOption type = {.type = None};
        if (expr.ctor.type.type == Some) {
            RuneEvalResult result = eval_rune_internal(get_expr(expr.ctor.type.val, pool), ctx);
            if (result.type != AValue) return result;
            type = (ValRefOption) {.type = Some, .val = result.value};
        }
        ValRef out = mk_rune_data(expr.ctor.name, type, 0, heap); 
        return (RuneEvalResult) {
            .type = AValue,
            .value = out,
        };
    };
    case ERecord: {
      RuneEvalResult result = eval_rune_internal(get_expr(expr.record.type, pool), ctx);
      if (result.type != AValue) return result;
      ValRef type_val = result.value;
      ValueSort sort = get_sort(type_val);
      RuneEvalResult bad_sort = {
        .type = AError,
        .error_message = mv_cstr_doc("Creating a record, but the base type is not a record type.", &a),
      };
      if (sort != ValDataType) {
        return bad_sort;
      }
      RuneDataType* type = get_rune_data_type(type_val);
      if (!type->is_record) {
        return bad_sort;
      }
      ValRef out = mk_rune_data(string_to_name(mv_string("record")),
                                (ValRefOption){.type = Some,.val = type_val},
                                type->names.len,
                                heap);
      RuneData* data_val = get_rune_data(out);
      bool* found = mem_alloc(sizeof(bool) * type->names.len, &a);
      memset(found, 0, sizeof(bool) * type->names.len);
      for (size_t i = 0; i < type->names.len; i++) {
        for (size_t j = 0; j < expr.record.fields.len; j++) {
          ExprCell cell = get_expr_map_elt(expr.record.fields, j, pool);
          if (type->names.data[i] == cell.name) {
            found[i] = true;
            RuneEvalResult result = eval_rune_internal(cell.expr, ctx);
            if (result.type != AValue) return result;
            ValRef type_val = result.value;
            data_val->values[i] = type_val;
            break;
          }
        }
      }
      for (size_t i = 0; i < type->names.len; i++) {
        if (!found[i]) {
          Name name = type->names.data[i];
          PtrArray nodes = mk_ptr_array(2, &a);
          push_ptr(mv_cstr_doc("Missing a record field:", &a), &nodes);
          push_ptr(mk_str_doc(view_name_string(name), &a), &nodes);

          return (RuneEvalResult) {
            .type = AError,
            .error_message = mv_sep_doc(nodes, &a),
          };
        }
      }
      return (RuneEvalResult) {
        .type = AValue,
        .value = out,
      };
    };
    case ECase:
        panic(mv_string("TODO: implement eval-match"));
    case ECoCase: {
        ValRefOption type = {.type = None};
        if (expr.cocase.type.type == Some) {
          RuneEvalResult result = eval_rune_internal(get_expr(expr.cocase.type.val, pool), ctx);
            if (result.type != AValue) return result;
            type = (ValRefOption) {.type = Some, .val = result.value};
        }
        NameArray captures = free_vars(expr, pool, &a);
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

        ValRef out = mk_rune_cocase(type, env, expr.cocase.fields, heap); 
        return (RuneEvalResult) {
            .type = AValue,
            .value = out,
        };
    };
    case ERecur:
        panic(mv_string("TODO: implement eval-recur"));
    case EProject: {
        RuneEvalResult result = eval_rune_internal(get_expr(expr.project.from, pool), ctx);
        if (result.type != AValue) return result;
        ValueSort sort = get_sort(result.value);
        switch (sort) {
        case ValCoCase: {
            //RuneCoCase* cocase = get_rune_cocase(result.value); 
            panic(mv_string("TODO: implement project out of corecord"));
            break;
        }
        case ValData: {
            //RuneData* data = get_rune_data(result.value);
            panic(mv_string("TODO: implement project out of record"));
            break;
        }
        default:
            return (RuneEvalResult) {
                .type = AError,
                .error_message = mv_cstr_doc("Projecting out of non-cocase/record used as a function.", &a),
            };
        }
    }

    case EDataType: {
        ValRef data_sort;
        if (expr.data_ty.type.type == Some) {
            RuneEvalResult result = eval_rune_internal(get_expr(expr.project.from, pool), ctx);
            if (result.type != AValue) return result;
            data_sort = result.value;
        } else {
            data_sort = mk_rune_universe(0, heap);
        }
        ValRef data_ref =
            mk_rune_data_type(data_sort, expr.data_ty.name,
                              expr.data_ty.is_record, expr.data_ty.names.len,
                              expr.data_ty.constructors.len, heap);
        RuneDataType* type = get_rune_data_type(data_ref);
        for (size_t i = 0; i < expr.data_ty.names.len; i++) {
            type->names.data[i] = get_expr_name(expr.data_ty.names, i, pool);
        }

        //typedef struct {
        //    Name name;
        //    ExprOption type;
        //    bool is_record;
        //    ExprNames names;
        //    AlgebraicTypeSpec constructors;
        //} ExprDataType;
        for (size_t i = 0; i < expr.data_ty.constructors.len; i++) {
            //typedef struct {
            //  Name name;
            //  ExprMap args;
            //  ExprRef type;
            //} AlgebraOp;
            AlgebraOp op = get_alg_spec_op(expr.data_ty.constructors, i, pool);
            for (size_t j = 0; j < op.args.len; j++) {
            }
            //
            //type->names.data[i] = expr.data_ty.constructors, i, pool;
        }
        return (RuneEvalResult) {
            .type = AValue,
            .value = data_ref,
        };
        break;
    }
    case ECoDataType:
        panic(mv_string("TODO: add eval to CoData Type"));
        break;

    case EVal: {
        return (RuneEvalResult) {
            .type = AValue,
            .value = expr.value,
        };
    }
    case EInt: {
        ValRef int_ref = mk_rune_int(expr.num, heap);
        return (RuneEvalResult) {
            .type = AValue,
            .value = int_ref,
        };
    }
    case EString: {
        ValRef string_ref = mk_rune_string(expr.string.memsize, heap);
        String string = get_rune_string(string_ref);
        memcpy(string.bytes, expr.string.bytes, expr.string.memsize);
        return (RuneEvalResult) {
            .type = AValue,
            .value = string_ref,
        };
    }
    case EList: {
        ValRef listref = mk_rune_list(expr.list.len, heap); 
        RuneList list = get_rune_list(listref);
        for (size_t i = 0; i < expr.list.len; i++) {
            ExprRef elt = get_expr_elt(expr.list, i);
            RuneEvalResult result = eval_rune_internal(get_expr(elt, pool), ctx);
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
    return eval_rune_internal(get_expr(expression, get_pools(env).expr), ctx);
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
    ExprPool* pool = get_pools(env).expr;
    ValueSort sort = get_sort(ref);
    switch (sort) {
    case ValUniverse: {
        uint64_t level = get_rune_universe(ref);
        if (level != 0) {
          PtrArray nodes = mk_ptr_array(2, a);
          return mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        } else {
          return mv_cstr_doc("𝒰", a);
        }
    }
    case ValClosure: {
        RuneClosure closure = get_rune_closure(ref);
        return pretty_rune_expr(closure.expr, env->exprs, a);
    }
    case ValData: {
        RuneData* data = get_rune_data(ref);
        PtrArray nodes = mk_ptr_array(1 + data->len, a);
        PtrArray head =  mk_ptr_array(2, a);
        push_ptr(mv_cstr_doc(":", a), &head);
        push_ptr(mk_str_doc(view_name_string(data->name), a), &head);
        push_ptr(mv_cat_doc(head, a), &nodes);
        for (size_t i = 0; i < data->len; i++) {
            push_ptr(mv_nest_doc(2, pretty_rune_value(data->values[i], env, a), a), &nodes);
        }
        return (data->len > 0)
            ? mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a)
            : mv_sep_doc(nodes, a);
    }
    case ValCoCase: {
        RuneCoCase* cocase = get_rune_cocase(ref);
        PtrArray nodes = mk_ptr_array(1 + cocase->fields.len, a);
        push_ptr(mv_cstr_doc("cocase", a), &nodes);
        for (size_t i = 0; i < cocase->fields.len; i++) {
            PatternCell cell = get_expr_ptn_map_elt(cocase->fields, i, env->exprs);
            PtrArray fnodes = mk_ptr_array(2, a);
            PtrArray head =  mk_ptr_array(2, a);
            push_ptr(mv_cstr_doc(".", a), &head);
            push_ptr(mk_str_doc(view_name_string(cell.name), a), &head);
            push_ptr(mv_cat_doc(head, a), &fnodes);
            push_ptr(mv_nest_doc(2, pretty_rune_expr(get_expr(cell.pattern.body, pool), env->exprs, a), a), &fnodes);
            push_ptr(mv_nest_doc(2, mv_group_doc(mk_paren_doc("[", "]", mv_sep_doc(fnodes, a), a), a), a), &nodes);
        }
        return mv_sep_doc(nodes, a);
    }
    case ValFnType:
        panic(mv_string("TODO: pretty fn type"));
    case ValDataType:
        panic(mv_string("TODO: pretty data type"));
    case ValCoRecordType:
        panic(mv_string("TODO: pretty corecord type"));

    // Constants
    case ValInt:
        return pretty_i64(get_rune_int(ref), a);
      break;
    case ValList: {
        RuneList list = get_rune_list(ref);
        PtrArray nodes = mk_ptr_array(list.len + 1, a);
        push_ptr(mv_cstr_doc("list", a), &nodes);
        for (size_t i = 0; i < list.len; i++) {
            push_ptr(mv_nest_doc(2, pretty_rune_value(list.data[i], env, a), a), &nodes);
        }
        return mv_group_doc(mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a), a);
    }
    case ValString:
        return mk_paren_doc("\"", "\"", mv_str_doc(get_rune_string(ref), a), a);
        break;

    case ValHost:
        panic(mv_string("TODO: pretty host value"));
    };
    panic(mv_string("Trying to produce document for invalid pretty value"));
}

bool rune_value_eql(ValRef actual, ValRef expected, RuneEnv* env, Allocator* a) {
    ValueSort actual_sort = get_sort(actual);
    ValueSort expected_sort = get_sort(expected);

    if (actual_sort != expected_sort) {
        return false;
    }

    switch (actual_sort) {
    case ValClosure:
        return actual.ref == expected.ref;
    case ValData: {
        RuneData* actual_data = get_rune_data(actual);
        RuneData* expected_data = get_rune_data(expected);
        if (actual_data->len != expected_data->len) return false;
        if (actual_data->type.type != expected_data->type.type) return false;
        if (actual_data->type.type == Some) {
            if (!rune_value_eql(actual_data->type.val, expected_data->type.val, env, a)) {
                return false;
            }
        }
        for (size_t i = 0; i < actual_data->len; i++) {
            if (!rune_value_eql(actual_data->values[i], expected_data->values[i], env, a)) {
                return false;
            }
        }
        return true;
    }
    case ValCoCase: {
        RuneCoCase* actual_cocase = get_rune_cocase(actual);
        RuneCoCase* expected_cocase = get_rune_cocase(expected);
        if (actual_cocase->fields.len != expected_cocase->fields.len) return false;
        if (actual_cocase->type.type != expected_cocase->type.type) return false;
        if (actual_cocase->type.type == Some) {
            if (!rune_value_eql(actual_cocase->type.val, expected_cocase->type.val, env, a)) {
                return false;
            }
        }
        for (size_t i = 0; i < actual_cocase->fields.len; i++) {
            PatternCell actual_cell = get_expr_ptn_map_elt(actual_cocase->fields, i, env->exprs);
            PatternCell expected_cell = get_expr_ptn_map_elt(expected_cocase->fields, i, env->exprs);
            if (actual_cell.name != expected_cell.name) {
                return false;
            }
            panic(mv_string("TODO: cocase equivalence."));
        }
        return true;
    }
    case ValInt:
        return get_rune_int(actual) == get_rune_int(expected);
    case ValString: {

        String actual_string = get_rune_string(actual);
        String expected_string = get_rune_string(expected);
        return string_eq(actual_string, expected_string);
    }
    case ValList: {
        RuneList actual_list = get_rune_list(actual);
        RuneList expected_list = get_rune_list(expected);
        if (actual_list.len != expected_list.len) {
            return false;
        }
        for (size_t i = 0; i < actual_list.len; i++) {
            if (!rune_value_eql(actual_list.data[i],
                                expected_list.data[i],
                                env, a)) {
                return false;
            }
        }
        return true;
    }
    default:
        return actual.ref == expected.ref;
    }
}
