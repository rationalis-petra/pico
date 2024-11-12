#include "pico/binding/environment.h"

#include "platform/signals.h"

#include "pico/data/sym_ptr_amap.h"

struct Environment {
    // Maps a symbol to the module it is impoted from/defined in. 
    SymPtrAMap symbol_origins;
};

Environment* env_from_module(Module* module, Allocator* a) {
    Environment* env = mem_alloc(sizeof(Environment), a);
    env->symbol_origins = mk_sym_ptr_amap(128, a);

    // TODO (PERF): this is quite expensive every REPL iteration... possibly 
    // cache results?
    Imports imports = get_imports(module);
    Package* package = get_package(module);
    //Module* parent = get_parent(module);

    for (size_t i = 0; i < imports.clauses.size; i++) {
        // TODO (BUG): currently, we only search in the package, not the parent
        // module's submodules!
        ImportClause clause = imports.clauses.data[i];
        switch (clause.type) {
        case ImportName:
            panic(mv_string("Do not support import by name yet!"));
            break;
        case ImportNameAs:
            panic(mv_string("Do not support import by name + rename yet!"));
            break;
        case ImportPath:
            panic(mv_string("Do not support import by path yet!"));
            break;
        case ImportPathMany:
            panic(mv_string("Do not support import by path many yet!"));
            break;
        case ImportPathAll: {
            // Find the package
            Module* importee = get_module(clause.name, package);
            SymbolArray arr = get_exported_symbols(importee, a);
            for (size_t i = 0; i < arr.len; i++ ) {
                sym_ptr_insert(arr.data[i], importee, &(env->symbol_origins));
            }
            sdelete_u64_array(arr);
            break;
        }
        default:
            panic(mv_string("Unrecognized import form in env_from_module"));
        }

    }

    // The local (module) definitions have the highest priority, so they  
    // get loaded first
    SymbolArray arr = get_exported_symbols(module, a);
    for (size_t i = 0; i < arr.len; i++ ) {
        sym_ptr_insert(arr.data[i], module, &(env->symbol_origins));
    }
    sdelete_u64_array(arr);

    return env;
}

EnvEntry env_lookup(Symbol sym, Environment* env) {
    EnvEntry result;
    Module** module = (Module**)sym_ptr_lookup(sym, env->symbol_origins);
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
    sdelete_sym_ptr_amap(env->symbol_origins);
    mem_free(env, a);
}
