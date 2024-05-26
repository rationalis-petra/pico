#include "pico/binding/address_env.h"
#include "data/meta/array_impl.h"
#include "data/option.h"

// symbol -> addr map
typedef struct saddr {
    option_t type;
    pi_symbol symbol;
} saddr;

ARRAY_HEADER(saddr, saddr)
ARRAY_IMPL(saddr, saddr)

typedef struct address_env {
    environment* env;
    saddr_array locals;
} address_env;

address_env* mk_address_env(environment* env, allocator a) {
    address_env* a_env = (address_env*)mem_alloc(sizeof(address_env), a);
    a_env->locals = mk_saddr_array(32, a);
    a_env->env = env;
    return a_env; 
}

void delete_address_env(address_env* env, allocator a) {
    sdelete_saddr_array(env->locals, a);
    mem_free(env, a);
}

address_entry address_env_lookup(pi_symbol s, address_env* env) {
    // Search for symbol
    address_entry out;
    for (size_t i = env->locals.len; i > 0; i--) {
        saddr maddr = env->locals.data[i - 1];
        if (maddr.type == Some && maddr.symbol == s) {
            // Check if the offset can fit into an immediate
            out.type = ALocal;
            out.stack_offset = (env->locals.len - i) * 8; 
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


void address_fn_vars (symbol_array vars, address_env* env, allocator a) {
    for (size_t i = 0; i < vars.len; i++) {
        saddr local;
        local.type = Some;
        local.symbol = vars.data[i];
        push_saddr(local, &env->locals, a);
    }
    saddr padding;
    padding.type = None;
    padding.symbol = 0;
    push_saddr(padding, &env->locals, a);
    push_saddr(padding, &env->locals, a);
}

void pop_fn_vars(address_env* env) {
    for (size_t i = env->locals.len; i > 0; i--) {
        saddr local = pop_saddr(&env->locals);
        if (local.type == None) break;
    }
}

void push_stack_address (pi_symbol var, address_env* env, allocator a) {
}


void pop_addresses(address_env* env, size_t n) {
}
