#include "pico/binding/environment.h"

#include "platform/signals.h"

#include "pico/data/name_ptr_amap.h"
#include "pico/data/name_ptr_amap.h"

struct Environment {
    // Maps a symbol to the module it is impoted from/defined in. 
    NamePtrAMap symbol_origins;

    // Map ids to arrays of implementations - each element of the  
    //  'implementation set' points to the relevant module.
    NamePtrAMap instances;
};

// Helper function:
Module* path_parent(SymbolArray path, Module* root) {
    Module* current = root;

    // Don't go to the last part of the path!
    for (size_t i = 0; i + 1 < path.len; i++) {
        ModuleEntry* e = get_def(path.data[i], current);
        if (e) {
          if (e->is_module) {
              current = e->value;
          } else {
              panic(mv_string("error in environment.c: module not found"));
          }
        } else {
            panic(mv_string("error in environment.c: module not found"));
        }
    }
    return current;
}

Module* path_all(SymbolArray path, Module* root) {
    Module* current = root;

    // Don't go to the last part of the path!
    for (size_t i = 0; i < path.len; i++) {
        ModuleEntry* e = get_def(path.data[i], current);
        if (e) {
          if (e->is_module) {
              current = e->value;
          } else {
              panic(mv_string("error in environment.c: module not found"));
          }
        } else {
            panic(mv_string("error in environment.c: module not found"));
        }
    }
    return current;
}

Environment* env_from_module(Module* module, Allocator* a) {
    Environment* env = mem_alloc(sizeof(Environment), a);
    env->symbol_origins = mk_name_ptr_amap(128, a);
    env->instances = mk_name_ptr_amap(128, a);

    // TODO (PERFORMANCE): this is quite expensive every REPL iteration... possibly 
    // cache results?
    Imports imports = get_imports(module);
    Package* package = get_package(module);
    Module* root_module = get_root_module(package);
    //Module* parent = get_parent(module);

    for (size_t i = 0; i < imports.clauses.len; i++) {
        // TODO (BUG): currently, we only search in the package, not the parent
        // module's submodules!
        ImportClause clause = imports.clauses.data[i];
        switch (clause.type) {
        case Import: {
            Symbol last_symbol = clause.path.data[clause.path.len - 1];
            Module* parent = path_parent(clause.path, root_module);
            name_ptr_insert(last_symbol.name, parent, &env->symbol_origins);
            break;
        }
        case ImportAs:
            panic(mv_string("Do not support renaming import yet!"));
            break;
        case ImportMany:
            panic(mv_string("Do not support import many yet!"));
            break;
        case ImportAll: {
            // Find the package
            Module* importee = path_all(clause.path, root_module);
            SymbolArray syms = get_exported_symbols(importee, a);
            for (size_t i = 0; i < syms.len; i++ ) {
                name_ptr_insert(syms.data[i].name, importee, &env->symbol_origins);
            }
            sdelete_symbol_array(syms);

            // Get all implicits
            PtrArray instances = get_exported_instances(importee, a);
            for (size_t i = 0; i < instances.len; i++ ) {
                InstanceSrc* instance = instances.data[i];

                PtrArray* p = (PtrArray*)name_ptr_lookup(instance->id, env->instances);
                if (!p) {
                    p = mem_alloc(sizeof(PtrArray), a);
                    *p = mk_ptr_array(8, a);
                    // TODO: we know this isn't in the instances; could perhaps
                    // speed up the process?
                    name_ptr_insert(instance->id, p, &env->instances);
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
        name_ptr_insert(arr.data[i].name, module, &(env->symbol_origins));
    }
    sdelete_symbol_array(arr);

    // Get all implicits
    PtrArray instances = get_exported_instances(module, a);
    for (size_t i = 0; i < instances.len; i++ ) {
        InstanceSrc* instance = instances.data[i];
        PtrArray** res = (PtrArray**)name_ptr_lookup(instance->id, env->instances);
        PtrArray* p;
        if (res) {
            p = *res;
        } else {
            p = mem_alloc(sizeof(PtrArray), a);
            *p = mk_ptr_array(8, a);
            // TODO: we know this isn't in the instances; could perhaps
            // speed up the process?
            name_ptr_insert(instance->id, p, &env->instances);
        }

        // Add this instance to the array
        push_ptr(instance, p);
    }

    return env;
}

void delete_env(Environment* env, Allocator* a) {
    sdelete_name_ptr_amap(env->symbol_origins);
    mem_free(env, a);
}

EnvEntry env_lookup(Symbol sym, Environment* env) {
    if (sym.did != 0) return (EnvEntry) {.success = Err, };

    EnvEntry result;
    Module** module = (Module**)name_ptr_lookup(sym.name, env->symbol_origins);
    if (module) {
        ModuleEntry* mentry = get_def(sym, *module); 
        if (mentry != NULL) {
            result.success = Ok;
            result.is_module = mentry->is_module;
            result.value = mentry->value;
            result.type = mentry->is_module ? NULL : &mentry->type;
            result.source = *module;
        } else {
            result.success = Err;
        }
    } else {
        result.success = Err;
    }
    return result;
}

PtrArray* env_implicit_lookup(Name id, Environment* env) {
    PtrArray** implicits = (PtrArray**)name_ptr_lookup(id, env->instances);
    return implicits ? *implicits : NULL;
}
