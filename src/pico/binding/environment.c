#include "pico/binding/environment.h"

#include "platform/signals.h"

#include "pico/data/name_ptr_amap.h"
#include "pico/data/u64_name_amap.h"

struct Environment {
    // Maps a symbol to the module it is impoted from/defined in. 
    NamePtrAMap symbol_origins;
    U64NameAMap symbol_renames;

    // Map ids to arrays of implementations - each element of the  
    //  'implementation set' points to the relevant module.
    NamePtrAMap instances;
    Module* base;
    Allocator* gpa;
};

// Helper function:
void check_entry(ModuleEntry* entry, bool expect_module, Symbol name, Module* mfor, ErrorPoint* point, Allocator* a) {
    if (!entry) {
        PtrArray nodes = mk_ptr_array(4, a);
        push_ptr(mk_str_doc(mv_string("Module not found:"), a), &nodes);
        push_ptr(mk_paren_doc("'", "'", mk_str_doc(view_symbol_string(name), a), a), &nodes);
        push_ptr(mk_str_doc(mv_string("while constructing environment for module"), a), &nodes);
        push_ptr(mk_paren_doc("'", "'", mk_str_doc(view_symbol_string(module_name(mfor)), a), a), &nodes);
        throw_error(point, mv_sep_doc(nodes, a));
    }
    if (!entry->is_module && expect_module) {
        String message = string_ncat(a, 4, mv_string("Symbol does not refer to a module: "),
                                     symbol_to_string(name, a),
                                     mv_string(" while constructing environment for module "),
                                     symbol_to_string(module_name(mfor), a));
        throw_error(point, mv_str_doc(message, a));
    }
}

typedef struct {
    bool all_modules;
    PtrArray values;
    U64Array names;
} Origins; 

Origins path_trace_internal(PathSegmentArray path, bool gather_names, Module* root, Module* mfor, ErrorPoint* point, Allocator* a) {
    PtrArray working_set = mk_ptr_array(1, a);
    PtrArray next_set = mk_ptr_array(1, a);
    U64Array out_names = mk_u64_array(1, a);
    push_ptr(root, &working_set);

    bool all_modules = true;
    for (size_t i = 0; i < path.len; i++) {
        next_set.len = 0;
        PathSegment segment = path.data[i];
        bool last_iteration = i + 1 == path.len;
        for (size_t j = 0; j < working_set.len; j++) {
            Module* module = working_set.data[j];
            switch (segment.type) {
            case SegSymbol: {
                ModuleEntry* entry = get_def_external(segment.symbol, module);
                check_entry(entry, !last_iteration, segment.symbol, mfor, point, a);
                all_modules &= entry->is_module;
                if (last_iteration && gather_names) {
                    push_ptr(module, &next_set);
                    push_u64(segment.symbol.name, &out_names);
                } else {
                    push_ptr(entry->value, &next_set);
                }
                break;
            }
            case SegSymbols: {
                for (size_t k = 0; k < segment.symbols.len; k++) {
                    ModuleEntry* entry = get_def_external(segment.symbols.data[k], module);
                    check_entry(entry, !last_iteration, segment.symbols.data[k], mfor, point, a);
                    all_modules &= entry->is_module;
                    if (last_iteration && gather_names) {
                        push_ptr(module, &next_set);
                        push_u64(segment.symbols.data[k].name, &out_names);
                    } else {
                        push_ptr(entry->value, &next_set);
                    }
                }
                break;
            }
            case SegWildcard: {
                SymbolArray symbols = get_exported_symbols(module, a);
                for (size_t k = 0; k < symbols.len; k++) {
                    ModuleEntry* entry = get_def_external(symbols.data[k], module);
                    check_entry(entry, !last_iteration, symbols.data[k], mfor, point, a);
                    all_modules &= entry->is_module;
                    if (last_iteration && gather_names) {
                        push_ptr(module, &next_set);
                        push_u64(symbols.data[k].name, &out_names);
                    } else {
                        push_ptr(entry->value, &next_set);
                    }
                }
                break;
            }
            }
        }
        PtrArray tmp = working_set;
        working_set = next_set;
        next_set = tmp;
    }
    sdelete_ptr_array(next_set);

    return (Origins) {
        .all_modules = all_modules,
        .values = working_set,
        .names = out_names,
    }; 
}

typedef struct {
    PtrArray values;
} Targets; 

Targets get_targets(PathSegmentArray path, Module* root, Module* mfor, ErrorPoint* point, Allocator* a) {
    Origins origins = path_trace_internal(path, false, root, mfor, point, a);
    sdelete_u64_array(origins.names);
    return (Targets) {
        .values = origins.values,
    };
}

typedef struct {
    PtrArray values;
    U64Array names;
} ParentMap; 

ParentMap get_origins(PathSegmentArray path, Module* root, Module* mfor, ErrorPoint* point, Allocator* a) {
    Origins origins = path_trace_internal(path, true, root, mfor, point, a);
    return (ParentMap) {
        .values = origins.values,
        .names = origins.names,
    };
}

void refresh_env(Environment* env) {
    Module* module = env->base;
    // The local (module) definitions have the highest priority, so they  
    // get loaded first
    SymbolArray arr = get_defined_symbols(module, env->gpa);
    for (size_t i = 0; i < arr.len; i++ ) {
        name_ptr_insert(arr.data[i].name, module, &(env->symbol_origins));
    }
    sdelete_symbol_array(arr);

    // Get all implicits
    // TODO: We should flush old instances from the current module?
    //       otherwise we will get stale instance bugs, where a instance is
    //       bound to a non-instance, but still lives on in the env's instances
    PtrArray instances = get_defined_instances(module, env->gpa);
    for (size_t i = 0; i < instances.len; i++ ) {
        InstanceSrc* instance = instances.data[i];
        PtrArray** res = (PtrArray**)name_ptr_lookup(instance->id, env->instances);
        PtrArray* p;
        if (res) {
            p = *res;
        } else {
            p = mem_alloc(sizeof(PtrArray), env->gpa);
            *p = mk_ptr_array(8, env->gpa);
            name_ptr_insert(instance->id, p, &env->instances);
        }

        // Has the instance been replaced
        size_t replaces = p->len;
        for (size_t i = 0; i < p->len; i++) {
            InstanceSrc* other = p->data[i];
            if (other->src == instance->src && symbol_eq(other->src_sym, instance->src_sym)) {
                replaces = i;
            }
        }
         // Add this instance to the array, or overwrite the existing source.
        if (replaces == p->len) {
            push_ptr(instance, p);
        } else {
            InstanceSrc* other = p->data[replaces];
            p->data[replaces] = instance;
            mem_free(other, env->gpa);
        }
    }
    sdelete_ptr_array(instances);
}

Module* env_module(Environment *env) {
    return env->base;
}

void add_instances_from(Module* importee, Environment* env, Allocator* a) {
    // Get all implicits
    PtrArray instances = get_defined_instances(importee, a);
    for (size_t j = 0; j < instances.len; j++) {
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
}

Environment* env_from_module(Module* module, ErrorPoint* point, Allocator* a) {
    Environment* env = mem_alloc(sizeof(Environment), a);
    *env = (Environment) {
        .symbol_origins = mk_name_ptr_amap(256, a),
        .symbol_renames = mk_u64_name_amap(16, a),
        .instances = mk_name_ptr_amap(128, a),
        .base = module,
        .gpa = a,
    };

    // TODO (PERFORMANCE): this is quite expensive every REPL iteration... possibly 
    // cache results?
    Imports imports = get_imports(module);
    Package* package = get_package(module);
    Module* root_module = package_root_module(package);

    // TODO: importing a specific sympol allows you to bypass which symbols
    // are/aren't exported.
    for (size_t i = 0; i < imports.clauses.len; i++) {
        // TODO (BUG): currently, we only search in the package, not the parent
        // module's submodules!
        ImportClause clause = imports.clauses.data[i];
        switch (clause.type) {
        case ImportSimple: {
            ParentMap parents = get_origins(clause.path, root_module, module, point, a);
            for (size_t i = 0; i < parents.names.len; i++) {
                name_ptr_insert(parents.names.data[i], parents.values.data[i], &env->symbol_origins);
            }
            sdelete_ptr_array(parents.values);
            sdelete_u64_array(parents.names);
            break;
        }
        case ImportComplex: {
            Targets targets = get_targets(clause.path, root_module, module, point, a);

            for (size_t i = 0; i < targets.values.len; i++ ) {
                Module* target = targets.values.data[i];
                if (clause.import_instances | clause.import_types) {
                    SymbolArray syms = get_exported_symbols(target, a);
                    // TODO: lookup here is probably n^2. Can we just...
                    // generate a list of exports instead?
                    for (size_t j = 0; j < syms.len; j++) {
                        ModuleEntry* entry = get_def_external(syms.data[j], target);
                        if (clause.import_instances & (entry->type.sort == TTraitInstance)) {
                            name_ptr_insert(syms.data[j].name, target, &env->symbol_origins);
                        }
                        if (clause.import_types & (entry->type.sort == TKind)) {
                            name_ptr_insert(syms.data[j].name, target, &env->symbol_origins);
                        }
                    }
                    if (clause.import_instances) {
                        add_instances_from(target, env, a);
                    }
                    sdelete_symbol_array(syms);
                }
                if (clause.import_values) {
                    for (size_t j = 0; j < clause.values.len; j++) {
                        // TODO: make sure is exported
                        Symbol sym = clause.values.data[j].from;
                        ModuleEntry* entry = get_def_external(sym, target);
                        check_entry(entry, false, sym, module, point, a);
                        if (clause.values.data[j].should_rename) {
                            u64_name_insert(env->symbol_origins.len, sym.name, &env->symbol_renames);
                            name_ptr_insert(clause.values.data[j].to.name, target, &env->symbol_origins);
                        } else {
                            name_ptr_insert(sym.name, target, &env->symbol_origins);
                        }
                    }
                }
            }
            sdelete_ptr_array(targets.values);
            break;
        }
        case ImportAll: {
            // Find the package
            Targets targets = get_targets(clause.path, root_module, module, point, a);

            for (size_t i = 0; i < targets.values.len; i++ ) {
                Module* target = targets.values.data[i];
                SymbolArray syms = get_exported_symbols(target, a);
                for (size_t j = 0; j < syms.len; j++ ) {
                    name_ptr_insert(syms.data[j].name, target, &env->symbol_origins);
                }
                sdelete_symbol_array(syms);
                add_instances_from(target, env, a);
            }
            sdelete_ptr_array(targets.values);
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

ImportClauseStatus import_clause_valid(Environment* env, ImportClause clause, Allocator* a) {
    PathSegmentArray path = clause.path;
    Package* package = get_package(env->base);
    Module* root = package_root_module(package);

    PtrArray working_set = mk_ptr_array(1, a);
    PtrArray next_set = mk_ptr_array(1, a);
    push_ptr(root, &working_set);

    for (size_t i = 0; i < path.len; i++) {
        next_set.len = 0;
        PathSegment segment = path.data[i];
        bool last_iteration = i + 1 == path.len;
        for (size_t j = 0; j < working_set.len; j++) {
            Module* module = working_set.data[j];
            switch (segment.type) {
            case SegSymbol: {
                // TODO: make sure is exported
                ModuleEntry* entry = get_def_external(segment.symbol, module);
                if (entry == NULL) {
                    return (ImportClauseStatus) {
                        .type = ICNotExists,
                        .bad_symbol = segment.symbol,
                    };
                }
                if (!entry->is_module && (!last_iteration || clause.type == ImportComplex)) {
                    return (ImportClauseStatus) {
                        .type = ICNotModule,
                        .bad_symbol = segment.symbol,
                    };
                }
                push_ptr(entry->value, &next_set);
                break;
            }
            case SegSymbols: {
                for (size_t k = 0; k < segment.symbols.len; k++) {
                    // TODO: make sure is exported
                    ModuleEntry* entry = get_def_external(segment.symbols.data[k], module);
                    if (entry == NULL) {
                        return (ImportClauseStatus) {
                            .type = ICNotExists,
                            .bad_symbol = segment.symbols.data[k],
                        };
                    }
                    if (!entry->is_module && (!last_iteration || clause.type == ImportComplex)) {
                        return (ImportClauseStatus) {
                            .type = ICNotModule,
                            .bad_symbol = segment.symbols.data[k],
                        };
                    }
                    push_ptr(entry->value, &next_set);
                }
                break;
            }
            case SegWildcard: {
                SymbolArray symbols = get_exported_symbols(module, a);
                for (size_t k = 0; k < symbols.len; k++) {
                    // TODO: make sure is exported
                    ModuleEntry* entry = get_def_external(symbols.data[k], module);
                    if (!entry->is_module && (!last_iteration || clause.type == ImportComplex)) {
                        return (ImportClauseStatus) {
                            .type = ICNotModule,
                            .bad_symbol = symbols.data[k],
                        };
                    }
                    push_ptr(entry->value, &next_set);
                }
                break;
            }
            }
        }
        PtrArray tmp = working_set;
        working_set = next_set;
        next_set = tmp;
    }

    if (clause.type == ImportComplex && clause.import_values) {
        for (size_t i = 0; i < working_set.len; i++) {
            Module* target = working_set.data[i];
            for (size_t j = 0; j < clause.values.len; j++) {
                ImportValue value = clause.values.data[j];
                ModuleEntry* entry = get_def_external(value.from, target);
                if (!entry) {
                    return (ImportClauseStatus) {
                        .type = ICNotExists,
                        .bad_symbol = value.from,
                    };
                }
            }
        }
    }

    return (ImportClauseStatus) {.type = ICValid,};
}

void delete_env(Environment* env, Allocator* a) {
    sdelete_name_ptr_amap(env->symbol_origins);
    sdelete_u64_name_amap(env->symbol_renames);
    for (size_t i = 0; i < env->instances.len; i++) {
        PtrArray arr = *(PtrArray*)env->instances.data[i].val;
        for (size_t j = 0; j < arr.len; j++) {
            mem_free(arr.data[j], a);
        }
        sdelete_ptr_array(arr);
        mem_free(env->instances.data[i].val, &env->instances.gpa);
    };
    sdelete_name_ptr_amap(env->instances);
    mem_free(env, a);
}

EnvEntry env_lookup(Symbol sym, Environment* env) {
    if (sym.did != 0) return (EnvEntry) {.success = Err, };

    EnvEntry result;
    size_t idx;
    bool found = (Module**)name_ptr_find(&idx, sym.name, env->symbol_origins);
    if (found) {
        Name* rename = u64_name_lookup(idx, env->symbol_renames);
        Module* module = env->symbol_origins.data[idx].val;
        if (rename) {
            sym.name = *rename;
        };
        ModuleEntry *mentry = module == env->base
            ? get_def_internal(sym, module)
            : get_def_external(sym, module); 
        if (mentry != NULL && mentry->value) {
            result.success = Ok;
            result.is_module = mentry->is_module;
            result.value = mentry->value;
            result.type = mentry->is_module ? NULL : &mentry->type;
            result.source = module;
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
        ModuleEntry* mentry = get_def_internal(sym, *module); 
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
