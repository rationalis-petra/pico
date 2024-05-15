
#include "pico/binding/environment.h"
#include "pico/binding/shadow_env.h"


typedef struct shadow_env {
    environment* env;
    symbol_array shadowed;
} shadow_env;

shadow_env* mk_shadow_env(allocator a, environment* env) {
    shadow_env* s_env = mem_alloc(sizeof(shadow_env), a);
    s_env->env = env;
    s_env->shadowed = mk_u64_array(32, a);
    return s_env;
}

void delete_shadow_env(shadow_env* env, allocator a) {
    sdelete_u64_array(env->shadowed, a);
    mem_free(env, a);
}

void shadow_var (pi_symbol var, shadow_env* env, allocator a) {
    push_u64(var, &(env->shadowed), a);
}

void shadow_vars (symbol_array vars, shadow_env* env, allocator a) {
    for (size_t i = 0; i < vars.len; i++) {
        push_u64(aref_u64(i, vars), &(env->shadowed), a);
    }
}

void pop_shadow(shadow_env* env, size_t n) {
    env->shadowed.len -= n;
}

shadow_entry shadow_env_lookup(pi_symbol s, shadow_env* env) {
    shadow_entry entry;
    for (size_t i = 0; i < env->shadowed.len; i++) {
        if (s == aref_u64(i, env->shadowed)) {
            entry.type = SShadowed;
            return entry;
        }
    }
    env_entry e = env_lookup(s, env->env);
    switch (e.success) {
    case Ok:
        entry.type = SGlobal;
        entry.vtype = e.type;
        entry.value = e.value; 
        break;
    case Err:
        entry.type = SErr;
    };
    return entry;
}
