#include "pico/binding/environment.h"

struct environment {
    sym_val_amap local_bindings;
    sym_ptr_amap module_bindings;
    sym_ptr_amap modules;
};

//struct env_capture env_capture;

environment* env_empty(allocator a) {
    environment* env = mem_alloc(sizeof(environment), a);
    env->local_bindings = mk_sym_val_amap(20, a);
    return env;
}


void env_insert_inplace(ob_symbol sym, ob_value val, environment* env, allocator a) {
    sym_val_insert(sym, val, &env->local_bindings, a);
}

ob_value* env_lookup(ob_symbol sym, environment* env) {
    return sym_val_lookup(sym, env->local_bindings);
}

void delete_env(environment* env, allocator a) {
    // TODO: garbage collect/unroot bound variables
    sdelete_sym_val_amap(env->local_bindings, a);
    mem_free(env, a);
}
