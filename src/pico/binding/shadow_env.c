
#include "pico/binding/environment.h"
#include "pico/binding/shadow_env.h"


struct ShadowEnv {
    Environment* env;
    SymbolArray shadowed;
};

ShadowEnv* mk_shadow_env(Allocator* a, Environment* env) {
    ShadowEnv* s_env = mem_alloc(sizeof(ShadowEnv), a);
    s_env->env = env;
    s_env->shadowed = mk_u64_array(32, a);
    return s_env;
}

void delete_shadow_env(ShadowEnv* env, Allocator* a) {
    sdelete_u64_array(env->shadowed);
    mem_free(env, a);
}

void shadow_var (Symbol var, ShadowEnv* env) {
    push_u64(var, &(env->shadowed));
}

void shadow_vars (SymbolArray vars, ShadowEnv* env) {
    for (size_t i = 0; i < vars.len; i++) {
        push_u64(aref_u64(i, vars), &(env->shadowed));
    }
}

void pop_shadow(ShadowEnv* env, size_t n) {
    env->shadowed.len -= n;
}

shadow_entry shadow_env_lookup(Symbol s, ShadowEnv* env) {
    shadow_entry entry;
    for (size_t i = 0; i < env->shadowed.len; i++) {
        if (s == aref_u64(i, env->shadowed)) {
            entry.type = SShadowed;
            return entry;
        }
    }
    EnvEntry e = env_lookup(s, env->env);
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
