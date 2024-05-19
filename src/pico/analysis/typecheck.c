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
result type_infer_i(syntax* untyped, type_env* env, allocator a);
result type_check_i(syntax* untyped, pi_type* type, type_env* env, allocator a);

// -----------------------------------------------------------------------------
// Interface
// -----------------------------------------------------------------------------
type_result type_infer_expr(syntax* untyped, environment* env, allocator a) {
    // Arena allocator to place types
    allocator arena = mk_arena_allocator(2048, a);
    type_env *t_env = mk_type_env(env, arena);
    result impl = type_infer_i (untyped, t_env, arena);

    type_result out;
    if (impl.type == Ok) {
        out.type = Ok;
    } else {
        out.type = Err;
        // The error message is in the arena allocator's memory, so we need to
        // copy it to grand extra persistance.
        out.error_message = copy_string(impl.error_message, a);
    }

    out.release_type_memory = &release_arena_allocator;
    out.arena = arena;
    return out;
}

type_result type_check_expr(syntax* untyped, pi_type type, environment* env, allocator a) {
    // copy type
    pi_type stype = squash_type(type, a);

    allocator arena = mk_arena_allocator(2048, a);
    type_env *t_env = mk_type_env(env, arena);
    result impl = type_check_i (untyped, &stype, t_env, arena);

    type_result out;
    if (impl.type == Ok) {
        out.type = Ok;
    } else {
        out.type = Err;
        // The error message is in the arena allocator's memory, so we need to
        // copy it to grand extra persistance.
        out.error_message = copy_string(impl.error_message, a);
    }

    out.release_type_memory = &release_arena_allocator;
    out.arena = arena;
    return out;
}

// -----------------------------------------------------------------------------
// Implementation
// -----------------------------------------------------------------------------

result type_check_i(syntax* untyped, pi_type* type, type_env* env, allocator a) {
    result out;
    out = type_infer_i(untyped, env, a);
    if (out.type == Err) return out;
    out = unify(type, untyped->ptype, a);
    return out;
}

// "internal" type inference. Destructively mutates types
result type_infer_i(syntax* untyped, type_env* env, allocator a) {
    result out;
    switch (untyped->type) {
    case SLiteral:
        out.type = Ok;
        untyped->ptype = mem_alloc(sizeof(pi_type),a);
        untyped->ptype->sort = TPrim; 
        untyped->ptype->prim = Int_64;
        break;
    case SVariable: {
        type_entry te = type_env_lookup(untyped->data.variable, env);
        if (te.type == TELocal || te.type == TEGlobal) {
            untyped->ptype = te.ptype;
            out.type = Ok;
        } else {
            out.type = Err;
            out.error_message = mk_string("Couldn't find type variable!", a);
        }

        break;
    }
    case SFunction:
        out.type = Err;
        out.error_message = mk_string("Type inference not implemented for the 'fn' syntactic form", a);
        break;
    case SApplication: {
        out = type_infer_i(untyped->data.application.function, env, a);
        if (out.type == Err) return out;
        pi_type fn_type = *untyped->data.application.function->ptype;
        if (fn_type.sort != TProc) {
            out.type = Err;
            out.error_message = mk_string("Expected LHS of application to be a proc", a);
        }
        
        if (fn_type.proc.args.len != untyped->data.application.args.len) {
            out.type = Err;
            out.error_message = mk_string("Incorrect number of function arguments", a);
        }

        for (size_t i = 0; i < fn_type.proc.args.len; i++) {
            out = type_check_i(untyped->data.application.args.data[i],
                               (pi_type*)fn_type.proc.args.data[i],
                               env, a);
            if (out.type == Err) return out;
        }

        untyped->ptype = fn_type.proc.ret;
        break;
    }
    case SConstructor:
    case SRecursor:
    case SDestructor:
    case SCorecursor:
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
