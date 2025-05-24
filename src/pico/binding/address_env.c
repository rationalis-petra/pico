#include "data/string.h"
#include "data/meta/array_impl.h"
#include "platform/signals.h"
#include "platform/machine_info.h"

#include "pico/binding/address_env.h"
#include "pico/binding/type_env.h"

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
    SATypeVar,
    SAInaccessibleLocal,
    SASentinel,
    SALabel,
} SAddr_t;

typedef struct {
    SAddr_t type;
    Symbol symbol;
    
    int64_t stack_offset;
} SAddr;

int compare_saddr(SAddr lhs, SAddr rhs) {
    int out = lhs.type - rhs.type;
    if (!out) return out;
    out = cmp_symbol(lhs.symbol, rhs.symbol);
    if (!out) return out;
    out = lhs.stack_offset - rhs.stack_offset;
    return out;
}

ARRAY_HEADER(SAddr, saddr, SAddr)
ARRAY_CMP_IMPL(SAddr, compare_saddr, saddr, SAddr)

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
    local->type = LMonomorphic;
    local->stack_head = 0;
    local->vars = mk_saddr_array(32, a);
    push_ptr(local, &a_env->local_envs);

    return a_env; 
}

AddressEnv* mk_type_address_env(TypeEnv* env, Symbol* sym, Allocator* a) {
    AddressEnv* a_env = (AddressEnv*)mem_alloc(sizeof(AddressEnv), a);
    a_env->rec = sym != NULL;
    if (a_env->rec)
        a_env->recname = *sym;
    a_env->local_envs = mk_ptr_array(32, a);
    a_env->env = get_base(env);
    
    LocalAddrs* local = mem_alloc(sizeof(LocalAddrs), a);
    local->type = LMonomorphic;
    local->stack_head = 0;
    local->vars = mk_saddr_array(32, a);
    push_ptr(local, &a_env->local_envs);


    // NOTE: BELOW CODE IS EXTRACT FROM BIND_TYPE 
    // ------------------------------------------
    SymLocalAssoc syms = get_local_vars(env);
    for (size_t i = 0; i < syms.len; i++) {
        SAddr value;
        if (syms.data[i].val.type->sort == TKind) {
            value.type = SATypeVar;
            value.symbol = syms.data[i].key;
        } else {
            value.type = SAInaccessibleLocal;
        }
        value.stack_offset = 0;
        // TODO INVESTIGATE (compiler warning): value.stack_offset may be uninitialized
        push_saddr(value, &local->vars);
    }

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
        if ((maddr.type == SAInaccessibleLocal) && symbol_eq(maddr.symbol, s)) {
            return (AddressEntry) {
                .type = ANotFound,
            };
        }
        if ((maddr.type == SADirect || maddr.type == SAIndirect) && symbol_eq(maddr.symbol, s)) {
            // TODO: Check if the offset can fit into an immediate
            return (AddressEntry) {
                .type = maddr.type == SADirect ? ALocalDirect : ALocalIndirect,
                .stack_offset = maddr.stack_offset,
            };
        };

        if (maddr.type == SATypeVar) {
            return (AddressEntry) {.type = ATypeVar};
        }
    }

    // Now search the recsym
    if (env->rec && symbol_eq(env->recname, s)) {
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

AddressEntry address_abs_lookup(AbsVariable s, AddressEnv* env) {
    if (s.value) {
        return (AddressEntry) {
            .type = AGlobal,
            .value = s.value,
        };
    } else {
        // Search for symbol
        LocalAddrs locals = *(LocalAddrs*)env->local_envs.data[env->local_envs.len - 1];

        for (size_t i = locals.vars.len; i > 0; i--) {
            SAddr maddr = locals.vars.data[i - 1];
            if (maddr.type != SASentinel) s.index--; 

            // TODO: check for type vars?
            if (s.index == 0) {
                // TODO: Check if the offset can fit into an immediate
                return (AddressEntry) {
                    .type = maddr.type == SADirect ? ALocalDirect : ALocalIndirect,
                    .stack_offset = maddr.stack_offset,
                };
            };
        }

        return (AddressEntry) {.type = ANotFound,};
    }
}

LabelEntry label_env_lookup(Symbol s, AddressEnv* env) {
    if (env->local_envs.len == 0) return (LabelEntry) {.type = Err,};

    // Search for symbol
    LocalAddrs locals = *(LocalAddrs*)env->local_envs.data[env->local_envs.len - 1];
    int64_t current_head = 0;
    if (locals.type == LMonomorphic) { current_head = locals.stack_head; }
    else { panic(mv_string("label env lookup only supported in monomorphic envs.")); }

    for (size_t i = locals.vars.len; i > 0; i--) {
        SAddr maddr = locals.vars.data[i - 1];
        if (maddr.type == SALabel && symbol_eq(maddr.symbol, s)) {
            // TODO: Check if the offset can fit into an immediate
            return (LabelEntry) {
                .type = Ok,
                .stack_offset = maddr.stack_offset - current_head ,
            };
        };
    }

    return (LabelEntry) {.type = Err};
}


/* void address_vars (sym_size_assoc vars, address_env* env, allocator a); */
/* void pop_fn_vars(address_env* env); */
void address_start_proc(SymSizeAssoc implicits, SymSizeAssoc vars, AddressEnv* env, Allocator* a) {
    LocalAddrs* new_local = mem_alloc(sizeof(LocalAddrs), a);
    new_local->vars = mk_saddr_array(32, a);
    new_local->type = LMonomorphic;
    size_t stack_offset = 0;

    SAddr padding;
    padding.type = SASentinel;
    // padding.symbol = 0;
    stack_offset += 3 * ADDRESS_SIZE; // We add 2x the register size to account for the
                                      // return address, rbp and the dynamic
                                      // memory ptr.
    padding.stack_offset = stack_offset;
    push_saddr(padding, &new_local->vars);

    // Variables are in reverse order!
    // due to how the stack pushes/pops args.
    for (size_t i = vars.len; i > 0; i--) {
        SAddr local;
        local.type = SADirect;

        local.symbol = vars.data[i - 1].key;
        local.stack_offset = stack_offset;
        stack_offset += vars.data[i - 1].val;

        push_saddr(local, &new_local->vars);
    }
    for (size_t i = implicits.len; i > 0; i--) {
        SAddr local;
        local.type = SADirect;

        local.symbol = implicits.data[i - 1].key;
        local.stack_offset = stack_offset;
        stack_offset += implicits.data[i - 1].val;

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
    // padding.symbol = 0;
    padding.stack_offset = stack_offset;
    push_saddr(padding, &new_local->vars);

    // Variables are in reverse order!
    // due to how the stack pushes/pops args.
    // This also means that we push args first, then types!
    for (size_t i = vars.len; i > 0; i--) {
        SAddr local;
        local.type = SAIndirect;

        local.symbol = vars.data[i - 1];
        stack_offset += REGISTER_SIZE;
        local.stack_offset = stack_offset;

        push_saddr(local, &new_local->vars);
    }

    for (size_t i = types.len; i > 0; i--) {
        SAddr local;
        local.type = SADirect;

        local.symbol = types.data[i - 1];
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

void address_bind_type(Symbol s, AddressEnv* env) {
    LocalAddrs* locals = (LocalAddrs*)env->local_envs.data[env->local_envs.len - 1];

    if (locals->type == LPolymorphic) {
        panic(mv_string("Cannot bind type var in polymorphic env!"));
    }

    SAddr value;
    value.type = SATypeVar;
    value.symbol = s;
    // TODO INVESTIGATE (compiler warning): value.stack_offset may be uninitialized
    push_saddr(value, &locals->vars);
}

void address_bind_relative(Symbol s, size_t offset, AddressEnv* env) {
    LocalAddrs* locals = (LocalAddrs*)env->local_envs.data[env->local_envs.len - 1];
    size_t stack_offset = locals->stack_head;

    if (locals->type == LPolymorphic) {
        panic(mv_string("Cannot bind relative in polymorphic env!"));
    }

    SAddr value;
    value.type = SADirect;
    value.symbol = s;
    value.stack_offset = stack_offset + offset;
    push_saddr(value, &locals->vars);
}

void address_pop_n(size_t n, AddressEnv* env) {
    LocalAddrs* locals = (LocalAddrs*)env->local_envs.data[env->local_envs.len - 1];
    for (size_t i = 0; i < n; i++) {
        pop_saddr(&locals->vars);
    }
}

void address_pop(AddressEnv* env) {
    LocalAddrs* locals = (LocalAddrs*)env->local_envs.data[env->local_envs.len - 1];
    pop_saddr(&locals->vars);
}

void address_bind_enum_vars(SymSizeAssoc vars, AddressEnv* env) {
    // Note: We don't adjust the stack head
    LocalAddrs* locals = (LocalAddrs*)env->local_envs.data[env->local_envs.len - 1];
    size_t stack_offset = locals->stack_head;

    switch (locals->type) {
    case LMonomorphic: {
        SAddr padding;
        padding.type = SASentinel;
        // padding.symbol = 0;
        stack_offset += REGISTER_SIZE;
        padding.stack_offset = stack_offset;
        push_saddr(padding, &locals->vars);

        // Variables are in reverse order!
        // due to how the stack pushes/pops args.

        // TODO: in (some x), x getting bound to a sentinel!
        for (size_t i = 0; i < vars.len; i++) {
            SAddr local;
            local.type = SADirect;

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

void address_bind_label_vars(SymSizeAssoc vars, AddressEnv* env) {
    // Note: We don't adjust the stack head
    LocalAddrs* locals = (LocalAddrs*)env->local_envs.data[env->local_envs.len - 1];
    int64_t stack_offset = locals->stack_head;

    switch (locals->type) {
    case LMonomorphic: {
        SAddr padding;
        padding.type = SASentinel;

        // padding.symbol = 0;
        padding.stack_offset = stack_offset;
        push_saddr(padding, &locals->vars);

        // Variables are in reverse order!
        // due to how the stack pushes/pops args.

        for (size_t i = 0; i < vars.len; i++) {
            SAddr local;
            local.type = SADirect;

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

void address_unbind_label_vars(AddressEnv* env) {
    LocalAddrs* locals = (LocalAddrs*)env->local_envs.data[env->local_envs.len - 1];

    pop_saddr(&locals->vars);
    for (size_t i = env->local_envs.len; i > 0; i--) {
        SAddr local = pop_saddr(&locals->vars);
        if (local.type == SASentinel) {
            break;
        }
    }
}

void address_start_labels(SymbolArray labels, AddressEnv* env) {
    LocalAddrs* locals = (LocalAddrs*)env->local_envs.data[env->local_envs.len - 1];
    size_t stack_offset = locals->stack_head;

    SAddr padding;
    padding.type = SASentinel;
    // padding.symbol = 0;
    padding.stack_offset = stack_offset;
    push_saddr(padding, &locals->vars);

    // Variables are in reverse order!
    // due to how the stack pushes/pops args.
    for (size_t i = labels.len; i > 0; i--) {
        SAddr local;
        local.type = SALabel;

        local.symbol = labels.data[i - 1];
        local.stack_offset = stack_offset;

        push_saddr(local, &locals->vars);
    }
}

void address_end_labels(AddressEnv* env) {
    LocalAddrs* locals = (LocalAddrs*)env->local_envs.data[env->local_envs.len - 1];

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
