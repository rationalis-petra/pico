#include "pico/binding/environment.h"

#include "platform/signals.h"

#include "pico/data/sym_ptr_amap.h"

struct Environment {
    // Maps a symbol to the module it is impoted from/defined in. 
    SymPtrAMap symbol_origins;

    // Map ids to arrays of implementations - each element of the  
    //  'implementation set' points to the relevant module.
    SymPtrAMap instances;
};

Environment* env_from_module(Module* module, Allocator* a) {
    Environment* env = mem_alloc(sizeof(Environment), a);
    env->symbol_origins = mk_sym_ptr_amap(128, a);
    env->instances = mk_sym_ptr_amap(128, a);

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
            SymbolArray syms = get_exported_symbols(importee, a);
            for (size_t i = 0; i < syms.len; i++ ) {
                sym_ptr_insert(syms.data[i], importee, &env->symbol_origins);
            }
            sdelete_u64_array(syms);

            // Get all implicits
            PtrArray instances = get_exported_instances(importee, a);
            for (size_t i = 0; i < instances.len; i++ ) {
                InstanceSrc* instance = instances.data[i];

                PtrArray* p = (PtrArray*)sym_ptr_lookup(instance->id, env->instances);
                if (!p) {
                    p = mem_alloc(sizeof(PtrArray), a);
                    *p = mk_ptr_array(8, a);
                    // TODO: we know this isn't in the instances; could perhaps
                    // speed up the process?
                    sym_ptr_insert(instance->id, p, &env->instances);
                }

                // Add this instance to the array
                push_ptr(instance, p);
            }

            // We do not delete instance entries are they are now in
            // the environment's instances - just delete the array.
            sdelete_ptr_array(instances);

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

    // Get all implicits
    PtrArray instances = get_exported_instances(module, a);
    for (size_t i = 0; i < instances.len; i++ ) {
        InstanceSrc* instance = instances.data[i];
        PtrArray** res = (PtrArray**)sym_ptr_lookup(instance->id, env->instances);
        PtrArray* p;
        if (res) {
            p = *res;
        } else {
            p = mem_alloc(sizeof(PtrArray), a);
            *p = mk_ptr_array(8, a);
            // TODO: we know this isn't in the instances; could perhaps
            // speed up the process?
            sym_ptr_insert(instance->id, p, &env->instances);
        }

        // Add this instance to the array
        push_ptr(instance, p);
    }

    return env;
}

void delete_env(Environment* env, Allocator* a) {
    sdelete_sym_ptr_amap(env->symbol_origins);
    mem_free(env, a);
}

EnvEntry env_lookup(Symbol sym, Environment* env) {
    EnvEntry result;
    Module** module = (Module**)sym_ptr_lookup(sym, env->symbol_origins);
    if (module) {
        ModuleEntry* mentry = get_def(sym, *module); 
        if (mentry != NULL) {
            result.success = Ok;
            result.is_module = mentry->is_module;
            result.value = mentry->value;
            result.type = mentry->is_module ? NULL : &mentry->type;
        } else {
            result.success = Err;
        }
    } else {
        result.success = Err;
    }
    return result;
}

PtrArray* env_implicit_lookup(uint64_t id, Environment* env) {
    PtrArray** implicits = (PtrArray**)sym_ptr_lookup(id, env->instances);
    return implicits ? *implicits : NULL;
}
