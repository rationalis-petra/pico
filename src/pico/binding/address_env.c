#include "pico/binding/address_env.h"

typedef struct address_env {
    environment* env;
    symbol_array locals;
} address_env;

address_env* mk_address_env(environment* env, allocator a) {
    address_env* a_env = (address_env*)mem_alloc(sizeof(address_env), a);
    a_env->locals = mk_u64_array(32, a);
    a_env->env = env;
    return a_env; 
}

void delete_address_env(address_env* env, allocator a) {
    sdelete_u64_array(env->locals, a);
    mem_free(env, a);
}

address_entry address_env_lookup(pi_symbol s, address_env* env) {
    // search for symbol
    address_entry out;
    for (size_t i = 0; i < env->locals.len; i++) {
        if (env->locals.data[i] == s) {
            // Check if the offset can fit into an immediate
            out.type = ANotFound;
            return out;
        };
    }

    // Now search globally
    env_entry e = env_lookup(s, env->env);
    if (e.success == Err) {
        out.type = ANotFound;
    } else {
        out.type = AGlobal;
        out.value = e.value;
    }

    return out;
}

void push_stack_address (pi_symbol var, address_env* env, allocator a) {
}

void address_vars (symbol_array vars, address_env* env, allocator a) {
}

void pop_address(address_env* env) {
}

void pop_addresses(address_env* env, size_t n) {
}
