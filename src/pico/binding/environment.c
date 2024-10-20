#include "pico/binding/environment.h"
#include "pico/data/sym_ptr_amap.h"

struct Environment {
    SymPtrAMap modules;
};

Environment* env_from_module(Module* module, Allocator* a) {
    Environment* env = mem_alloc(sizeof(Environment), a);
    env->modules = mk_sym_ptr_amap(32, a);
    // loop for entry in module:
    SymbolArray arr = get_symbols(module, a);
    for (size_t i = 0; i < arr.len; i++ ) {
        sym_ptr_insert(arr.data[i], (void*)module, &(env->modules));
    }
    sdelete_u64_array(arr);

    return env;
}

EnvEntry env_lookup(Symbol sym, Environment* env) {
    EnvEntry result;
    Module** module = (Module**)sym_ptr_lookup(sym, env->modules);
    if (module) {
        ModuleEntry* mentry = get_def(sym, *module); 
        if (mentry != NULL) {
            result.success = Ok;
            result.type = &mentry->type;
            result.value = mentry->value;
        } else {
            result.success = Err;
        }
    } else {
        result.success = Err;
    }
    return result;
}

void delete_env(Environment* env, Allocator* a) {
    sdelete_sym_ptr_amap(env->modules);
    mem_free(env, a);
}
