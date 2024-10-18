#include "data/string.h"
#include "data/meta/array_impl.h"
#include "platform/signals.h"
#include "platform/machine_info.h"

#include "pico/binding/address_env.h"
#include "pico/codegen/codegen.h"

// Address Environment: Implementation
// -----------------------------------
// There are several important types used in the implementation of the address
// environment. These are: 
// • Symbol Address (SAddr). This associates a symbol with a stack offset. This
//   may be a direct offset (monomorphic functions) or indirect offset
//   (polymorphic functions). A symbol address may also be a sentinel, which
//   denotes the beginning of a group of variables  as bound by, e.g. a let
//   expression, procedure or pattern match.
//  
// • Local Address Environment (LocalAddrs). This encapsulates the local namespace of
//   one function. A local address environment will be either monomorphic or
//   polymorphic.

typedef enum {
    SADirect,
    SAIndirect,
    SASentinel,
} SAddr_t;

typedef struct {
    SAddr_t type;
    Symbol symbol;
    
    int64_t stack_offset;
} SAddr;

ARRAY_HEADER(SAddr, saddr, SAddr)
ARRAY_IMPL(SAddr, saddr, SAddr)

typedef enum {
    LPolymorphic,
    LMonomorphic,
} LocalType;

typedef struct {
    LocalType type;

    int64_t stack_head;
    SAddrArray vars;
} LocalAddrs;

struct AddressEnv {
    LocalType type;
    Environment* env;

    // To handle recursive top level definitions.
    bool rec;
    Symbol recname;
    
    PtrArray local_envs;
};

AddressEnv* mk_address_env(Environment* env, Symbol* sym, Allocator* a) {
    AddressEnv* a_env = (AddressEnv*)mem_alloc(sizeof(AddressEnv), a);
    a_env->rec = sym != NULL;
    if (a_env->rec)
        a_env->recname = *sym;
    a_env->local_envs = mk_ptr_array(32, a);
    a_env->env = env;
    
    LocalAddrs* local = mem_alloc(sizeof(LocalAddrs), a);
    local->stack_head = 0;
    local->vars = mk_saddr_array(32, a);
    push_ptr(local, &a_env->local_envs);

    return a_env; 
}

void delete_local_env(LocalAddrs* local, Allocator* a) {
    sdelete_saddr_array(local->vars);
    mem_free(local, a);
}

void delete_address_env(AddressEnv* env, Allocator* a) {
    for (size_t i = 0; i < env->local_envs.len; i++)
        delete_local_env(env->local_envs.data[i], a);
    sdelete_ptr_array(env->local_envs);
    mem_free(env, a);
}

AddressEntry address_env_lookup(Symbol s, AddressEnv* env) {
    if (env->local_envs.len == 0) {
        return (AddressEntry) {
            .type = ANotFound,
        };
    }

    // Search for symbol
    LocalAddrs locals = *(LocalAddrs*)env->local_envs.data[env->local_envs.len - 1];

    for (size_t i = locals.vars.len; i > 0; i--) {
        SAddr maddr = locals.vars.data[i - 1];
        if (maddr.type != SASentinel && maddr.symbol == s) {
            // TOOD: Check if the offset can fit into an immediate
            return (AddressEntry) {
                .type = maddr.type == SADirect ? ALocalDirect : ALocalIndirect,
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
    EnvEntry e = env_lookup(s, env->env);
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
void address_start_proc(SymSizeAssoc vars, AddressEnv* env, Allocator* a) {
    LocalAddrs* new_local = mem_alloc(sizeof(LocalAddrs), a);
    new_local->vars = mk_saddr_array(32, a);
    new_local->type = LMonomorphic;
    size_t stack_offset = 0;

    SAddr padding;
    padding.type = SASentinel;
    padding.symbol = 0;
    stack_offset += ADDRESS_SIZE; // We add the register size to account for the
                                  // return address.
    padding.stack_offset = stack_offset;
    push_saddr(padding, &new_local->vars);

    // Variables are in reverse order!
    // due to how the stack pushes/pops args.
    for (size_t i = vars.len; i > 0; i--) {
        SAddr local;
        local.type = SADirect;

        local.symbol = vars.data[i - 1].key;
        stack_offset += vars.data[i - 1].val;
        local.stack_offset = stack_offset;

        push_saddr(local, &new_local->vars);
    }

    new_local->stack_head = 0;
    push_ptr(new_local, &env->local_envs);
}


void address_end_proc(AddressEnv* env, Allocator* a) {
    LocalAddrs* old_locals = env->local_envs.data[env->local_envs.len - 1];
    pop_ptr(&env->local_envs);
    sdelete_saddr_array(old_locals->vars);
    mem_free(old_locals, a);
}

void address_start_poly(SymbolArray types, SymbolArray vars, AddressEnv* env, Allocator* a) {
    LocalAddrs* new_local = mem_alloc(sizeof(LocalAddrs), a);
    new_local->vars = mk_saddr_array(32, a);
    new_local->type = LMonomorphic;
    size_t stack_offset = 0;
    stack_offset += ADDRESS_SIZE; // We add the register size to account for the
                                  // old RBP.
    SAddr padding;
    padding.type = SASentinel;
    padding.symbol = 0;
    padding.stack_offset = stack_offset;
    push_saddr(padding, &new_local->vars);

    for (size_t i = vars.len; i > 0; i--) {
        SAddr local;
        local.type = SADirect;

        local.symbol = types.data[i - 1];
        stack_offset += REGISTER_SIZE;
        local.stack_offset = stack_offset;

        push_saddr(local, &new_local->vars);
    }

    // Variables are in reverse order!
    // due to how the stack pushes/pops args.
    for (size_t i = vars.len; i > 0; i--) {
        SAddr local;
        local.type = SAIndirect;

        local.symbol = vars.data[i - 1];
        stack_offset += REGISTER_SIZE;
        local.stack_offset = stack_offset;

        push_saddr(local, &new_local->vars);
    }

    new_local->stack_head = 0;
    push_ptr(new_local, &env->local_envs);
}

void address_end_poly(AddressEnv* env, Allocator* a) {
    LocalAddrs* old_locals = env->local_envs.data[env->local_envs.len - 1];
    pop_ptr(&env->local_envs);
    sdelete_saddr_array(old_locals->vars);
    mem_free(old_locals, a);
}

void address_bind_enum_vars(SymSizeAssoc vars, AddressEnv* env, Allocator* a) {
    // Note: We don't adjust the stack head
    LocalAddrs* locals = (LocalAddrs*)env->local_envs.data[env->local_envs.len - 1];
    size_t stack_offset = locals->stack_head;

    switch (locals->type) {
    case LMonomorphic: {
        SAddr padding;
        padding.type = SASentinel;
        padding.symbol = 0;
        stack_offset += REGISTER_SIZE;
        padding.stack_offset = stack_offset;
        push_saddr(padding, &locals->vars);

        // Variables are in reverse order!
        // due to how the stack pushes/pops args.
        for (size_t i = 0; i < vars.len; i++) {
            SAddr local;
            local.type = SASentinel;

            local.symbol = vars.data[i].key;
            local.stack_offset = stack_offset;
            stack_offset += vars.data[i].val;

            push_saddr(local, &locals->vars);
        }

        padding.stack_offset = stack_offset + REGISTER_SIZE;
        push_saddr(padding, &locals->vars);
        break;
    } 
    case LPolymorphic: {
        panic(mv_string("Polymorphic enum bind not implemented"));
        break;
    }
    }
}

void address_unbind_enum_vars(AddressEnv* env) {
    LocalAddrs* locals = (LocalAddrs*)env->local_envs.data[env->local_envs.len - 1];

    pop_saddr(&locals->vars);
    for (size_t i = env->local_envs.len; i > 0; i--) {
        SAddr local = pop_saddr(&locals->vars);
        if (local.type == SASentinel) {
            break;
        }
    }
}

void address_stack_grow(AddressEnv* env, size_t amount) {
    LocalAddrs* locals = (LocalAddrs*)env->local_envs.data[env->local_envs.len - 1];
    if (locals->type == LMonomorphic) locals->stack_head -= amount;
}

void address_stack_shrink(AddressEnv* env, size_t amount) {
    LocalAddrs* locals = (LocalAddrs*)env->local_envs.data[env->local_envs.len - 1];
    if (locals->type == LMonomorphic) locals->stack_head += amount;
}
