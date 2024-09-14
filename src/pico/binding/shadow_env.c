
#include "pico/data/sym_ptr_assoc.h"
#include "pico/binding/environment.h"
#include "pico/binding/shadow_env.h"


struct ShadowEnv {
    Environment* env;
    SymPtrAssoc locals;
};

ShadowEnv* mk_shadow_env(Allocator* a, Environment* env) {
    ShadowEnv* s_env = mem_alloc(sizeof(ShadowEnv), a);
    s_env->env = env;
    s_env->locals = mk_sym_ptr_assoc(32, a);
    return s_env;
}

void delete_shadow_env(ShadowEnv* env, Allocator* a) {
    sdelete_sym_ptr_assoc(env->locals);
    mem_free(env, a);
}

void shadow_var (Symbol var, ShadowEnv* env) {
    sym_ptr_bind(var, NULL, &env->locals);
}

void shadow_vars (SymbolArray vars, ShadowEnv* env) {
    for (size_t i = 0; i < vars.len; i++) {
        sym_ptr_bind(vars.data[i], NULL, &env->locals);
    }
}

void shadow_pop(ShadowEnv* env, size_t n) {
    env->locals.len -= n;
}

void shadow_bind(Symbol var, PiType* type, ShadowEnv* env) {
    sym_ptr_bind(var, type, &env->locals);
}

ShadowEntry shadow_env_lookup(Symbol s, ShadowEnv* env) {
    ShadowEntry entry;
    for (size_t i = env->locals.len; i > 0; i--) {
        if (s == env->locals.data[i-1].key) {
            PiType* type = env->locals.data[i-1].val;
            if (type) {
                entry = (ShadowEntry) {.type = SLocal, .value = type,};
            } else {
                entry = (ShadowEntry) {.type = SShadowed,};
            }
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
