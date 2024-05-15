#include "pico/binding/environment.h"

struct environment {
    sym_ptr_amap modules;
};


environment* env_from_module(pi_module* module, allocator a) {
    environment* env = mem_alloc(sizeof(environment), a);
    env->modules = mk_sym_ptr_amap(32, a);
    // loop for entry in module:
    symbol_array arr = get_symbols(module, a);
    for (size_t i = 0; i < arr.len; i++ ) {
        sym_ptr_insert(arr.data[i], (void*)module, &(env->modules), a);
    }
    sdelete_u64_array(arr, a);

    return env;
}


env_entry env_lookup(pi_symbol sym, environment* env) {
    env_entry result;
    pi_module** module = (pi_module**)sym_ptr_lookup(sym, env->modules);
    if (module) {
        module_entry* mentry = get_def(sym, *module); 
        if (mentry) {
            result.success = Ok;
            result.type = mentry->type;
            result.value = mentry->value;
        } else {
            result.success = Err;
        }
    } else {
        result.success = Err;
    }
    return result;
}

void delete_env(environment* env, allocator a) {
    // TODO: garbage collect/unroot bound variables
    mem_free(env, a);
}
