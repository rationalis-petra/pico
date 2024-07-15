#include "pico/binding/address_env.h"
#include "data/meta/array_impl.h"
#include "data/option.h"

// symbol -> addr map
typedef struct saddr {
    option_t type;
    pi_symbol symbol;
    
    size_t stack_offset;
} saddr;

ARRAY_HEADER(saddr, saddr)
ARRAY_IMPL(saddr, saddr)

typedef struct address_env {
    environment* env;

    bool rec;
    pi_symbol recname;
    
    saddr_array locals;
} address_env;

address_env* mk_address_env(environment* env, pi_symbol* sym, allocator a) {
    address_env* a_env = (address_env*)mem_alloc(sizeof(address_env), a);
    a_env->rec = sym != NULL;
    if (a_env->rec)
        a_env->recname = *sym;
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
            out.stack_offset = maddr.stack_offset; 
            return out;
        };
    }

    // now search the recsym
    if (env->rec && env->recname == s) {
        out.type = AGlobal;
        out.value = NULL;
        return out;
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


/* void address_vars (sym_size_assoc vars, address_env* env, allocator a); */
/* void pop_fn_vars(address_env* env); */
void address_fn_vars (sym_size_assoc vars, address_env* env, allocator a) {
    size_t stack_offset = 0;
    if (env->locals.len > 0) {
        stack_offset = env->locals.data[env->locals.len - 1].stack_offset;
    }

    saddr padding;
    padding.type = None;
    padding.symbol = 0;
    stack_offset += 8; // 8 = num bytes in uint64_t
    padding.stack_offset = stack_offset;
    push_saddr(padding, &env->locals, a);

    // Variables are in reverse order!
    // due to how the stack pushes/pops args.
    for (size_t i = vars.len; i > 0; i--) {
        saddr local;
        local.type = Some;

        local.symbol = vars.data[i - 1].key;
        stack_offset += vars.data[i - 1].val;
        local.stack_offset = stack_offset;

        push_saddr(local, &env->locals, a);
    }

}

void pop_fn_vars(address_env* env) {
    for (size_t i = env->locals.len; i > 0; i--) {
        saddr local = pop_saddr(&env->locals);
        if (local.type == None) break;
    }
}
