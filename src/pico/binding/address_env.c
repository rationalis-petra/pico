#include "pico/binding/address_env.h"
#include "data/meta/array_impl.h"
#include "data/option.h"

// symbol -> addr map
typedef struct {
    option_t type;
    pi_symbol symbol;
    
    int64_t stack_offset;
} SAddr;

ARRAY_HEADER(SAddr, saddr)
ARRAY_IMPL(SAddr, saddr)

typedef struct {
    int64_t stack_head;
    saddr_array vars;
} LocalAddrs;

struct AddressEnv {
    environment* env;

    bool rec;
    pi_symbol recname;
    
    ptr_array local_envs;
};

AddressEnv* mk_address_env(environment* env, pi_symbol* sym, allocator a) {
    AddressEnv* a_env = (AddressEnv*)mem_alloc(sizeof(AddressEnv), a);
    a_env->rec = sym != NULL;
    if (a_env->rec)
        a_env->recname = *sym;
    a_env->local_envs = mk_ptr_array(32, a);
    a_env->env = env;
    
    LocalAddrs* local = mem_alloc(sizeof(LocalAddrs), a);
    local->stack_head = 0;
    local->vars = mk_saddr_array(32, a);
    push_ptr(local, &a_env->local_envs, a);

    return a_env; 
}

void delete_local_env(LocalAddrs* local, allocator a) {
    sdelete_saddr_array(local->vars, a);
    mem_free(local, a);
}

void delete_address_env(AddressEnv* env, allocator a) {
    typedef void(*deleter)(void* data, allocator a);
    delete_ptr_array(env->local_envs, (deleter)delete_local_env, a);
    mem_free(env, a);
}

AddressEntry address_env_lookup(pi_symbol s, AddressEnv* env) {
    if (env->local_envs.len == 0) {
        return (AddressEntry) {
            .type = ANotFound,
        };
    }

    // Search for symbol
    LocalAddrs locals = *(LocalAddrs*)env->local_envs.data[env->local_envs.len - 1];
    for (size_t i = locals.vars.len; i > 0; i--) {
        SAddr maddr = locals.vars.data[i - 1];
        if (maddr.type == Some && maddr.symbol == s) {
            // Check if the offset can fit into an immediate
            return (AddressEntry) {
                .type = ALocal,
                .stack_offset = maddr.stack_offset,
            };
        };
    }

    // Now search the recsym
    if (env->rec && env->recname == s) {
        return (AddressEntry) {
            .type = AGlobal,
            .value = NULL,
        };
    }

    // Now search globally
    env_entry e = env_lookup(s, env->env);
    if (e.success == Err) {
        return (AddressEntry) { .type = ANotFound, };
    } else {
        return (AddressEntry) {
            .type = AGlobal,
            .value = e.value
        };
    }
}


/* void address_vars (sym_size_assoc vars, address_env* env, allocator a); */
/* void pop_fn_vars(address_env* env); */
void address_start_proc (sym_size_assoc vars, AddressEnv* env, allocator a) {
    LocalAddrs* new_local = mem_alloc(sizeof(LocalAddrs), a);
    new_local->vars = mk_saddr_array(32, a);
    size_t stack_offset = 0;

    SAddr padding;
    padding.type = None;
    padding.symbol = 0;
    stack_offset += 8; // 8 = num bytes in uint64_t
    padding.stack_offset = stack_offset;
    push_saddr(padding, &new_local->vars, a);

    // Variables are in reverse order!
    // due to how the stack pushes/pops args.
    for (size_t i = vars.len; i > 0; i--) {
        SAddr local;
        local.type = Some;

        local.symbol = vars.data[i - 1].key;
        stack_offset += vars.data[i - 1].val;
        local.stack_offset = stack_offset;

        push_saddr(local, &new_local->vars, a);
    }

    new_local->stack_head = 0;
    push_ptr(new_local, &env->local_envs, a);
}


void address_end_proc (AddressEnv* env, allocator a) {
    saddr_array* old_locals = env->local_envs.data[env->local_envs.len - 1];
    pop_ptr(&env->local_envs);
    sdelete_saddr_array(*old_locals, a);
}

void address_bind_enum_vars (sym_size_assoc vars, size_t enum_size, AddressEnv* env, allocator a) {
    // Note: We don't adjust the stack head
    // 

    LocalAddrs* locals = (LocalAddrs*)env->local_envs.data[env->local_envs.len - 1];
    size_t stack_offset = locals->stack_head;

    SAddr padding;
    padding.type = None;
    padding.symbol = 0;
    stack_offset += 8;
    padding.stack_offset = stack_offset;
    push_saddr(padding, &locals->vars, a);

    // Variables are in reverse order!
    // due to how the stack pushes/pops args.
    for (size_t i = 0; i < vars.len; i++) {
        SAddr local;
        local.type = Some;

        local.symbol = vars.data[i].key;
        local.stack_offset = stack_offset;
        stack_offset += vars.data[i].val;

        push_saddr(local, &locals->vars, a);
    }

    padding.stack_offset = stack_offset + 8;
    push_saddr(padding, &locals->vars, a);
}

void address_unbind_enum_vars(AddressEnv* env) {
    LocalAddrs* locals = (LocalAddrs*)env->local_envs.data[env->local_envs.len - 1];

    pop_saddr(&locals->vars);
    for (size_t i = env->local_envs.len; i > 0; i--) {
        SAddr local = pop_saddr(&locals->vars);
        if (local.type == None) {
            break;
        }
    }
}

void address_stack_grow(AddressEnv* env, size_t amount) {
    LocalAddrs* locals = (LocalAddrs*)env->local_envs.data[env->local_envs.len - 1];
    locals->stack_head -= amount;
}

void address_stack_shrink(AddressEnv* env, size_t amount) {
    LocalAddrs* locals = (LocalAddrs*)env->local_envs.data[env->local_envs.len - 1];
    locals->stack_head += amount;
}
