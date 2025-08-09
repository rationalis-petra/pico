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
    Module* base;
};

// Helper function:
Module* path_parent(SymbolArray path, Module* root, Module* mfor, ErrorPoint* point, Allocator* ea) {
    Module* current = root;

    // Don't go to the last part of the path!
    for (size_t i = 0; i + 1 < path.len; i++) {
        ModuleEntry* e = get_def(path.data[i], current);
        if (e) {
          if (e->is_module) {
              current = e->value;
          } else {
              String message = string_ncat(ea, 4, mv_string("Module not found: "),
                                           symbol_to_string(path.data[i], ea),
                                           mv_string(" while constructing environment for "),
                                           get_name(mfor, ea));
              throw_error(point, message);
          }
        } else {
              String message = string_ncat(ea, 4, mv_string("Module not found: "),
                                           symbol_to_string(path.data[i], ea),
                                           mv_string(" while constructing environment for "),
                                           get_name(mfor, ea));
              throw_error(point, message);
        }
    }
    return current;
}

Module* path_all(SymbolArray path, Module* root, Module* mfor, ErrorPoint* point, Allocator* ea) {
    Module* current = root;

    // Don't go to the last part of the path!
    for (size_t i = 0; i < path.len; i++) {
        ModuleEntry* e = get_def(path.data[i], current);
        if (e) {
          if (e->is_module) {
              current = e->value;
          } else {
              String message = string_ncat(ea, 4, mv_string("Module not found: "),
                                           symbol_to_string(path.data[i], ea),
                                           mv_string(" while constructing environment for "),
                                           get_name(mfor, ea));
              throw_error(point, message);
          }
        } else {
            String message = string_ncat(ea, 4, mv_string("Module not found: "),
                                         symbol_to_string(path.data[i], ea),
                                         mv_string(" while constructing environment for "),
                                         get_name(mfor, ea));
            throw_error(point, message);
        }
    }
    return current;
}

bool import_path_valid(Environment *env, SymbolArray path) {
    Module* current = get_root_module(get_package(env->base));

    for (size_t i = 0; i < path.len; i++) {
        ModuleEntry* e = get_def(path.data[i], current);
        if (e) {
          if (e->is_module) {
              current = e->value;
          } else {
              return false;
          }
        } else {
            return false;
        }
    }
    return true;
}

void refresh_env(Environment* env, Allocator* a) {
    Module* module = env->base;
    // The local (module) definitions have the highest priority, so they  
    // get loaded first
    SymbolArray arr = get_defined_symbols(module, a);
    for (size_t i = 0; i < arr.len; i++ ) {
        name_ptr_insert(arr.data[i].name, module, &(env->symbol_origins));
    }
    sdelete_symbol_array(arr);

    // Get all implicits
    PtrArray instances = get_defined_instances(module, a);
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
    sdelete_ptr_array(instances);
}

Environment* env_from_module(Module* module, ErrorPoint* point, Allocator* a) {
    Environment* env = mem_alloc(sizeof(Environment), a);
    *env = (Environment) {
        .symbol_origins = mk_name_ptr_amap(128, a),
        .instances = mk_name_ptr_amap(128, a),
        .base = module,
    };

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
            Module* parent = path_parent(clause.path, root_module, module, point, a);
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
            Module* importee = path_all(clause.path, root_module, module, point, a);
            SymbolArray syms = get_defined_symbols(importee, a);
            for (size_t j = 0; j < syms.len; j++ ) {
                name_ptr_insert(syms.data[j].name, importee, &env->symbol_origins);
            }
            sdelete_symbol_array(syms);

            // Get all implicits
            PtrArray instances = get_defined_instances(importee, a);
            for (size_t j = 0; j < instances.len; j++ ) {
                InstanceSrc* instance = instances.data[j];

                PtrArray* arr = NULL;
                PtrArray** p = (PtrArray**)name_ptr_lookup(instance->id, env->instances);
                if (p == NULL) {
                    arr = mem_alloc(sizeof(PtrArray), a);
                    *arr = mk_ptr_array(8, a);
                    // TODO: we know this isn't in the instances; could perhaps
                    // speed up the process?
                    name_ptr_insert(instance->id, arr, &env->instances);
                } else {
                    arr = *p;
                }

                // Add this instance to the array
                push_ptr(instance, arr);
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
    SymbolArray arr = get_defined_symbols(module, a);
    for (size_t i = 0; i < arr.len; i++ ) {
        name_ptr_insert(arr.data[i].name, module, &(env->symbol_origins));
    }
    sdelete_symbol_array(arr);

    // Get all implicits
    PtrArray instances = get_defined_instances(module, a);
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
    sdelete_ptr_array(instances);

    return env;
}

void delete_env(Environment* env, Allocator* a) {
    sdelete_name_ptr_amap(env->symbol_origins);
    for (size_t i = 0; i < env->instances.len; i++) {
        mem_free(env->instances.data[i].val, &env->instances.gpa);
    };
    sdelete_name_ptr_amap(env->instances);
    mem_free(env, a);
}

EnvEntry env_lookup(Symbol sym, Environment* env) {
    if (sym.did != 0) return (EnvEntry) {.success = Err, };

    EnvEntry result;
    Module** module = (Module**)name_ptr_lookup(sym.name, env->symbol_origins);
    if (module) {
        ModuleEntry* mentry = get_def(sym, *module); 
        if (mentry != NULL && mentry->value) {
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

PiType* env_lookup_tydecl(Symbol sym, Environment* env) {
    if (sym.did != 0) return NULL;

    Module** module = (Module**)name_ptr_lookup(sym.name, env->symbol_origins);
    if (module) {
        ModuleEntry* mentry = get_def(sym, *module); 
        if (mentry != NULL) {
            if (mentry->declarations) {
                PtrArray decls = *mentry->declarations;
                for (size_t i = 0; i < decls.len; i++) {
                    ModuleDecl* decl = decls.data[i];
                    if (decl->sort == DeclType) {
                        return decl->type;
                    }
                }
            }
        } 
    } 

    return NULL;
}

PtrArray* env_implicit_lookup(Name id, Environment* env) {
    PtrArray** implicits = (PtrArray**)name_ptr_lookup(id, env->instances);
    return implicits ? *implicits : NULL;
}
