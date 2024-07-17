#include "pico/analysis/typecheck.h"

#include "data/string.h"
#include "pretty/string_printer.h"

#include "pico/binding/environment.h"
#include "pico/binding/type_env.h"
#include "pico/analysis/unify.h"

// forward declarations
result type_check_expr(syntax* untyped, pi_type type, type_env* env, uvar_generator* gen, allocator a);
result type_infer_expr(syntax* untyped, type_env* env, uvar_generator* gen, allocator a);

// Check a toplevel expression

result type_check(toplevel* top, environment* env, allocator a) {
    // If this is a definition, lookup the type to check against 
    result out;
    type_env *t_env = mk_type_env(env, a);
    uvar_generator* gen = mk_gen(a);

    switch (top->type) {
    case TLDef: {
        pi_type* check_against;
        env_entry e = env_lookup(top->def.bind, env);
        syntax* term = top->def.value;

        if (e.success == Ok) {
            check_against = e.type;
        } else {
            check_against = mk_uvar(gen, a);
        }
        type_var(top->def.bind, check_against, t_env, a);
        out = type_check_expr(term, *check_against, t_env, gen, a);
        pop_type(t_env);
        break;
    }
    case TLExpr: {
        syntax* term = &top->expr;
        out = type_infer_expr(term, t_env, gen, a);
        break;
    }
    }

    // TODO: delete_type_env (ok for now as we arena allocate!)
    delete_gen(gen, a);
    return out;
}

// Forward declarations for implementation
result type_infer_i(syntax* untyped, type_env* env, uvar_generator* gen, allocator a);
result type_check_i(syntax* untyped, pi_type* type, type_env* env, uvar_generator* gen, allocator a);
result squash_types(syntax* untyped, allocator a);

// -----------------------------------------------------------------------------
// Interface
// -----------------------------------------------------------------------------
result type_infer_expr(syntax* untyped, type_env* env, uvar_generator* gen, allocator a) {
    // Arena allocator to place types
    result impl = type_infer_i (untyped, env, gen, a);

    result out;
    if (impl.type == Ok) {
        out.type = Ok;
    } else {
        out.type = Err;
        // The error message is in the arena allocator's memory, so we need to
        // copy it to grand extra persistance.
        out.error_message = copy_string(impl.error_message, a);
    }

    // now, squash all types
    if (out.type == Ok) {
        result sqres = squash_types(untyped, a);
        if (sqres.type == Err) {
            out.type = Err;
            // The error message is in the arena allocator's memory, so we need to
            // copy it to grant extra persistance.
            out.error_message = copy_string(sqres.error_message, a);
        }
    }

    return out;
}

result type_check_expr(syntax* untyped, pi_type type, type_env* env, uvar_generator* gen, allocator a) {
    // TODO copy type, use in type_check_i
    result impl = type_check_i (untyped, &type, env, gen, a);

    result out;
    out.type = Ok;
    if (impl.type == Err) {
        out.type = Err;
        // The error message is in the arena allocator's memory, so we need to
        // copy it to grant extra persistance.
        out.error_message = copy_string(impl.error_message, a);
    }

    // now, squash all types
    if (out.type == Ok) {
        result sqres = squash_types(untyped, a);
        if (sqres.type == Err) {
            out.type = Err;
            // The error message is in the arena allocator's memory, so we need to
            // copy it to grant extra persistance.
            out.error_message = copy_string(sqres.error_message, a);
        }
    }

    return out;
}

// -----------------------------------------------------------------------------
// Implementation
// -----------------------------------------------------------------------------

result type_check_i(syntax* untyped, pi_type* type, type_env* env, uvar_generator* gen, allocator a) {
    result out;
    out = type_infer_i(untyped, env, gen, a);
    if (out.type == Err) return out;
    out = unify(type, untyped->ptype, a);
    return out;
}

// "internal" type inference. Destructively mutates types
result type_infer_i(syntax* untyped, type_env* env, uvar_generator* gen, allocator a) {
    result out;
    switch (untyped->type) {
    case SLitI64:
        out.type = Ok;
        untyped->ptype = mem_alloc(sizeof(pi_type),a);
        untyped->ptype->sort = TPrim; 
        untyped->ptype->prim = Int_64;
        break;
    case SLitBool:
        out.type = Ok;
        untyped->ptype = mem_alloc(sizeof(pi_type),a);
        untyped->ptype->sort = TPrim; 
        untyped->ptype->prim = Bool;
        break;
    case SVariable: {
        type_entry te = type_env_lookup(untyped->variable, env);
        if (te.type == TELocal || te.type == TEGlobal) {
            untyped->ptype = te.ptype;
            out.type = Ok;
        } else {
            out.type = Err;
            string* sym = symbol_to_string(untyped->variable);
            string msg = mv_string("Couldn't find type of variable: ");
            out.error_message = string_cat(msg, *sym, a);
        }
        break;
    }
    case SProcedure: {
        // give each arg a unification variable type. 
        pi_type* proc_ty = mem_alloc(sizeof(pi_type), a);
        proc_ty->sort = TProc;
        proc_ty->proc.args = mk_ptr_array(untyped->procedure.args.len, a);
        untyped->ptype = proc_ty;

        for (size_t i = 0; i < untyped->procedure.args.len; i++) {
            pi_symbol arg = untyped->procedure.args.data[i];
            pi_type* aty = mk_uvar(gen, a);
            type_var(arg, aty, env, a);
            push_ptr(aty, &proc_ty->proc.args, a);
        }

        out = type_infer_i(untyped->procedure.body, env, gen, a); 
        pop_types(env, untyped->procedure.args.len);
        proc_ty->proc.ret = untyped->procedure.body->ptype;
        break;
    }
    case SApplication: {
        out = type_infer_i(untyped->application.function, env, gen, a);
        if (out.type == Err) return out;
        pi_type fn_type = *untyped->application.function->ptype;
        if (fn_type.sort == TUVar) {
            // fill in structure 
            pi_type* ret = mk_uvar(gen, a);
            ptr_array args = mk_ptr_array(16, a);
            for (size_t i = 0; i < untyped->application.args.len; i++) {
                push_ptr(mk_uvar(gen, a), &args, a);
            };

            fn_type.sort = TProc;
            fn_type.proc.args = args;
            fn_type.proc.ret = ret;
            *untyped->application.function->ptype = fn_type;
        }
        else if (fn_type.sort != TProc) {
            out.type = Err;
            out.error_message = mk_string("Expected LHS of application to be a proc", a);
            return out;
        }
        
        if (fn_type.proc.args.len != untyped->application.args.len) {
            out.type = Err;
            out.error_message = mk_string("Incorrect number of function arguments", a);
            return out;
        }

        for (size_t i = 0; i < fn_type.proc.args.len; i++) {
            out = type_check_i(untyped->application.args.data[i],
                               (pi_type*)fn_type.proc.args.data[i],
                               env, gen, a);
            if (out.type == Err) return out;
        }

        untyped->ptype = fn_type.proc.ret;
        break;
    }
    case SConstructor:
    case SRecursor:
    case SStructure:
    case SProjector:
    case SLet:
        out.type = Err;
        out.error_message = mk_string("Type inference not implemented for this syntactic form", a);
        break;
    case SIf: {
        pi_type* t = mem_alloc(sizeof(pi_type), a);
        t->sort = TPrim;
        t->prim = Bool;
        out = type_check_i(untyped->if_expr.condition,
                           t,
                           env, gen, a);
        if (out.type == Err) return out;

        out = type_infer_i(untyped->if_expr.true_branch, env, gen, a);

        if (out.type == Err) return out;
        out = type_check_i(untyped->if_expr.false_branch,
                           untyped->if_expr.true_branch->ptype,
                           env, gen, a);
        untyped->ptype = untyped->if_expr.false_branch->ptype;
        break;
    }
    case SStructType: {
        pi_type* ty = mem_alloc(sizeof(pi_type), a);
        ty->sort = TPrim;
        ty->prim = TType; 
        untyped->ptype = ty;

        for (size_t i = 0; i < untyped->structure.fields.len; i++) {
            syntax* syn = untyped->structure.fields.data[i].val;
            out = type_check_i(syn, ty, env, gen, a);
            if (out.type == Err) return out;
        }
        out.type = Ok;
        break;
    }
    default:
        out.type = Err;
        out.error_message = mk_string("Internal Error: unrecognized syntactic form", a);
        break;
    }
    return out;
}

result squash_types(syntax* typed, allocator a) {
    result out;
    switch (typed->type) {
    case SLitI64:
    case SLitBool:
    case SVariable:
        out.type = Ok;
        break;
    case SProcedure: {
        // squash body
        out = squash_types(typed->procedure.body, a);
        break;
    }
    case SApplication: {
        out = squash_types(typed->application.function, a);
        if (out.type == Err) return out;
        
        for (size_t i = 0; i < typed->procedure.args.len; i++) {
            out = squash_types(typed->application.args.data[i], a);
            if (out.type == Err) return out;
        }
        break;
    }
    case SConstructor:
    case SRecursor:
    case SStructure:
    case SProjector:
    case SLet:
        out.type = Err;
        out.error_message = mk_string("squash_types not implemented for this syntactic form", a);
        break;
    case SIf: {
        out = squash_types(typed->if_expr.condition, a);
        if (out.type == Err) return out;
        out = squash_types(typed->if_expr.true_branch, a);
        if (out.type == Err) return out;
        out = squash_types(typed->if_expr.false_branch, a);
        break;
    }
    case SStructType: {
        for (size_t i = 0; i < typed->structure.fields.len; i++) {
            syntax* syn = typed->structure.fields.data[i].val;
            out = squash_types(syn, a);
            if (out.type == Err) return out;
        }
        break;
    }
    default:
        out.type = Err;
        out.error_message = mk_string("Internal Error: unrecognized syntactic form", a);
        break;
    }

    if (out.type == Ok) {
        if (!has_unification_vars_p(*typed->ptype)) {
            squash_type(typed->ptype);
        }
        else {
            out.type = Err;
            squash_type(typed->ptype);
            document* doc = pretty_type(typed->ptype, a);
            string str = doc_to_str(doc, a);
            out.error_message =
                string_cat(mv_string("Typechecking error: not all unification vars were instantiated. Term:\n"), 
                           str, a);
            delete_string(str, a);
        }
    }
    return out;
}
