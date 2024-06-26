#include "pico/analysis/typecheck.h"

#include "memory/arena.h"
#include "pico/binding/environment.h"
#include "pico/binding/type_env.h"
#include "pico/analysis/unify.h"


// Check a toplevel expression

type_result type_check(toplevel* top, environment* env, allocator a) {
    // If this is a definition, lookup the type to check against 
    pi_type* check_against = NULL;
    syntax* term = NULL; 

    switch (top->type) {
    case TLDef: {
        term = top->def.value;
        env_entry e = env_lookup(top->def.bind, env);
        if (e.success == Ok) {
            check_against = e.type;
        }
        break;
    }
    case TLExpr:
        term = &top->expr;
        break;
    }

    if (check_against != NULL) {
        return type_check_expr(term, *check_against, env, a);
    } else {
        return type_infer_expr(term, env, a);
    }
}

// Forward declarations for implementation
result type_infer_i(syntax* untyped, type_env* env, uvar_generator* gen, allocator a);
result type_check_i(syntax* untyped, pi_type* type, type_env* env, uvar_generator* gen, allocator a);
result squash_types(syntax* untyped, allocator a);

// -----------------------------------------------------------------------------
// Interface
// -----------------------------------------------------------------------------
type_result type_infer_expr(syntax* untyped, environment* env, allocator a) {
    // Arena allocator to place types
    allocator arena = mk_arena_allocator(2048, a);
    type_env *t_env = mk_type_env(env, arena);
    uvar_generator* gen = mk_gen(a);
    result impl = type_infer_i (untyped, t_env, gen, arena);
    delete_gen(gen, a);

    type_result out;
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

    out.release_type_memory = &release_arena_allocator;
    out.arena = arena;
    return out;
}

type_result type_check_expr(syntax* untyped, pi_type type, environment* env, allocator a) {
    // TODO copy type, use in type_check_i

    allocator arena = mk_arena_allocator(2048, a);
    type_env *t_env = mk_type_env(env, arena);
    uvar_generator* gen = mk_gen(a);
    result impl = type_check_i (untyped, &type, t_env, gen, arena);
    delete_gen(gen, a);

    type_result out;
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

    out.release_type_memory = &release_arena_allocator;
    out.arena = arena;
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
    case SLiteral:
        out.type = Ok;
        untyped->ptype = mem_alloc(sizeof(pi_type),a);
        untyped->ptype->sort = TPrim; 
        untyped->ptype->prim = Int_64;
        break;
    case SVariable: {
        type_entry te = type_env_lookup(untyped->variable, env);
        if (te.type == TELocal || te.type == TEGlobal) {
            untyped->ptype = te.ptype;
            out.type = Ok;
        } else {
            out.type = Err;
            out.error_message = mk_string("Couldn't find type of variable!", a);
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
        if (fn_type.sort != TProc) {
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
    case SIf:
        out.type = Err;
        out.error_message = mk_string("Type inference not implemented for this syntactic form", a);
        break;
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
    case SLiteral:
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
    case SIf:
        out.type = Err;
        out.error_message = mk_string("Type inference not implemented for this syntactic form", a);
        break;
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
            out.error_message = mk_string("Typechecking error: not all unification vars were instantiated.", a);
        }
    }
    return out;
}
