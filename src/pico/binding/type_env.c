#include "pico/binding/type_env.h"

typedef struct type_env {
    environment* env;
    sym_ptr_assoc locals; // TODO: reset locals
} address_env;

type_env* mk_type_env(environment* env, allocator a) {
    type_env* t_env = (address_env*)mem_alloc(sizeof(address_env), a);
    t_env->locals = mk_sym_ptr_assoc(32, a);
    t_env->env = env;
    return t_env; 
}

type_entry type_env_lookup(pi_symbol s, type_env* env) {
    // Search locally
    type_entry out;
    pi_type** lresult = (pi_type**) sym_ptr_alookup(s, env->locals);
    if (lresult != NULL) {
        out.type = TELocal;
        out.ptype = *lresult;
        return out;
    }

    // Now search globally
    env_entry e = env_lookup(s, env->env);
    if (e.success == Err) {
        out.type = TENotFound;
    } else {
        out.type = TEGlobal;
        out.ptype = e.type;
    }

    return out;
}


void type_var (pi_symbol var, pi_type* type, type_env* env, allocator a) {
    sym_ptr_bind(var, type, &env->locals, a);
}

void pop_type(type_env* env) {
    sym_ptr_unbind(&env->locals);
}

void pop_types(type_env* env, size_t n) {
    sym_ptr_unbindn(n, &env->locals);
}
