#include "platform/memory/executable.h"
#include "platform/signals.h"

#include "components/pretty/stream_printer.h"

#include "pico/data/sym_ptr_amap.h"
#include "pico/values/modular.h"

#include "pico/binding/environment.h"
#include "pico/parse/parse.h"
#include "pico/analysis/abstraction.h"
#include "pico/analysis/typecheck.h"
#include "pico/codegen/codegen.h"
#include "pico/eval/call.h"
#include "pico/stdlib/platform/submodules.h"
#include "pico/stdlib/meta/meta.h"

#include "atlas/eval/instance.h"

struct AtlasInstance {
    Package* project_package;
    PtrArray packages;

    SymPtrAMap targets;
    bool project_set;
    Project project;

    Allocator* gpa;
};

typedef struct {
    Symbol name;
    String path;
    StringOption filename;
    SymbolOption entrypoint;
    SymbolArray target_dependencies;
    StringArray file_dependencies;
    Module* module;
} AtlasTarget;

AtlasInstance* make_atlas_instance(Allocator* a) {
    AtlasInstance* instance = mem_alloc(sizeof(AtlasInstance), a);
    *instance = (AtlasInstance) {
        .packages = mk_ptr_array(4, a),
        .targets = mk_sym_ptr_amap(32, a),
        .project_set = false,
        .gpa = a,
    };
    return instance;
}

void delete_atlas_instance(AtlasInstance* instance) {
    Allocator* a = instance->gpa;
    sdelete_ptr_array(instance->packages);
    for (size_t i = 0; i < instance->targets.len; i++) {
        AtlasTarget* target = instance->targets.data[i].val;
        if (target->filename.type == Some) {
            mem_free(target->filename.value.bytes, a);
        }
        for (size_t i = 0; i < target->file_dependencies.len; i++) {
            delete_string(target->file_dependencies.data[i], a);
        }
        sdelete_string_array(target->file_dependencies);
        sdelete_symbol_array(target->target_dependencies);
        mem_free(target, a);
    }

    if (instance->project_package) {
        delete_package(instance->project_package);
    }

    sdelete_sym_ptr_amap(instance->targets);
    mem_free(instance, a);
}

static Module* atlas_load_target(AtlasInstance* instance, Package* package, AtlasTarget* target, RegionAllocator* region, AtErrorPoint* point);

void atlas_run(AtlasInstance* instance, String target_name, RegionAllocator* region, AtErrorPoint* point) {
    Allocator ra = ra_to_gpa(region);
    Symbol sym = string_to_symbol(target_name);

    if (!instance->project_set) {
        AtlasError err = {
            .range = (Range) {},
            .message = mk_str_doc(mv_string("No atlas project was found, please ensure there is an 'atlas-project' file in the current directory."), &ra),
        };
        throw_at_error(point, err);
    }

    size_t tidx;
    if (sym_ptr_find(&tidx, sym, instance->targets)) {
        AtlasTarget* target = instance->targets.data[tidx].val;
        SymbolOption entry = target->entrypoint;
        if (entry.type == None) {
            PtrArray nodes = mk_ptr_array(5, &ra);
            push_ptr(mk_str_doc(mv_string("Target '"), &ra), &nodes);
            push_ptr(mk_str_doc(target_name, &ra), &nodes);
            push_ptr(mk_str_doc(mv_string("' has no entry-point and is therefore is not runnable."), &ra), &nodes);

            AtlasError err = {
                .range = (Range) {},
                .message = mv_cat_doc(nodes, &ra),
            };
            throw_at_error(point, err);
        }

        // First, create a new package for the project
        PiAllocator pico_alloc = get_std_perm_allocator();
        Package* package;
        if (instance->project_package) {
            package = instance->project_package;
        } else {
            package = mk_package(instance->project.package.name.name, pico_alloc);
            set_instance_package(instance, package);
        }

        // Then, add all dependencies
        SymbolArray deps = instance->project.package.dependencies;
        PtrArray avail = instance->packages;
        for (size_t i = 0; i < deps.len; i++) {
            bool found_dep = false;
            Name dep_name = deps.data[i].name;
            for (size_t j = 0; j < avail.len; j++) {
                Package* avail_package = avail.data[j];
                Name pkg_name = package_name(avail_package);
                if (dep_name == pkg_name) {
                    add_dependency(package, avail_package);
                    found_dep = true;
                    break;
                }
            }

            if (!found_dep) {
                PtrArray nodes = mk_ptr_array(5, &ra);
                push_ptr(mk_str_doc(mv_string("Dependency '"), &ra), &nodes);
                push_ptr(mk_str_doc(view_name_string(dep_name), &ra), &nodes);
                push_ptr(mk_str_doc(mv_string("' could not be found."), &ra), &nodes);

                AtlasError err = {
                    .range = (Range) {},
                    .message = mv_cat_doc(nodes, &ra),
                };
                throw_at_error(point, err);
            }
        }

        Module* module = atlas_load_target(instance, package, target, region, point);
        ModuleEntry* e = get_def(entry.value, module);
        if (!e) {
            PtrArray nodes = mk_ptr_array(5, &ra);
            push_ptr(mk_str_doc(mv_string("Entry Point '"), &ra), &nodes);
            push_ptr(mk_str_doc(view_symbol_string(target->entrypoint.value), &ra), &nodes);
            push_ptr(mk_str_doc(mv_string("' in target '"), &ra), &nodes);
            push_ptr(mk_str_doc(target_name, &ra), &nodes);
            push_ptr(mk_str_doc(mv_string("' could not be found."), &ra), &nodes);

            AtlasError err = {
                .range = (Range) {},
                .message = mv_cat_doc(nodes, &ra),
            };
            throw_at_error(point, err);
        }

        PiAllocator pia = convert_to_pallocator(&ra);
        PiType* check_ty = mk_proc_type(&pia, 0, mk_prim_type(&pia, Unit));
        if (pi_type_eql(check_ty, &e->type, &ra)) {
            call_unit_fn(*(void**)e->value, &ra);
        } else {
            PtrArray nodes = mk_ptr_array(5, &ra);
            {
                PtrArray ep_nodes = mk_ptr_array(5, &ra);
                push_ptr(mk_str_doc(mv_string("Entry Point: '"), &ra), &ep_nodes);
                push_ptr(mk_str_doc(view_symbol_string(target->entrypoint.value), &ra), &ep_nodes);
                push_ptr(mk_str_doc(mv_string("' has type:"), &ra), &ep_nodes);
                push_ptr(mv_cat_doc(ep_nodes, &ra), &nodes);
            }
            push_ptr(pretty_type(&e->type, &ra), &nodes);
            push_ptr(mk_str_doc(mv_string("but entry points must have type"), &ra), &nodes);
            push_ptr(pretty_type(check_ty, &ra), &nodes);
            AtlasError err = {
                .message = mv_sep_doc(nodes, &ra),
            };
            throw_at_error(point, err);
        }
    } else {
        PtrArray nodes = mk_ptr_array(5, &ra);
        push_ptr(mk_str_doc(mv_string("Unrecognized target: '"), &ra), &nodes);
        push_ptr(mk_str_doc(target_name, &ra), &nodes);
        push_ptr(mk_str_doc(mv_string("'"), &ra), &nodes);
        AtlasError err = {
            .message = mv_cat_doc(nodes, &ra),
        };
        throw_at_error(point, err);
    }
}

Module* atlas_load_file(String filename, Package* package, Module* parent, StringArray dependencies, RegionAllocator* region, AtErrorPoint* point) {
    // Create new Module in package
    // TODO: use different procedure for scripts?
    // Load module from system

    // Step 1: Setup necessary state
    Allocator ra = ra_to_gpa(region);
    RegionAllocator* iter_region = make_subregion(region);
    Allocator itera = ra_to_gpa(iter_region);
    Allocator exec = mk_executable_allocator(&ra);
    Logger* logger = NULL;

    PiAllocator pico_itera = convert_to_pallocator(&itera);

    Target gen_target = (Target) {
        .target = mk_assembler(current_cpu_feature_flags(), &exec),
        .code_aux = mk_assembler(current_cpu_feature_flags(), &exec),
        .data_aux = mem_alloc(sizeof(U8Array), &ra),
    };
    *gen_target.data_aux = mk_u8_array(256, &ra);

    ModuleHeader* volatile header = NULL;
    Module* volatile module = NULL;
    Module* volatile old_module = NULL;

    IStream* in = open_file_istream(filename, &ra);
    if (!in) {
        if (old_module) set_std_current_module(old_module);
        release_subregion(iter_region);
        release_executable_allocator(exec);

        PtrArray docs = mk_ptr_array(4, &ra);
        push_ptr(mk_str_doc(mv_string("File not found:"), &ra), &docs);
        push_ptr(mk_str_doc(filename, &ra), &docs);
        AtlasError err = {
            .message = mv_sep_doc(docs, &ra),
        };
        throw_at_error(point, err);
    }
    IStream* cin = mk_capturing_istream(in, &ra);
    reset_bytecount(cin);

    // Step 2:
    // Setup error reporting
    ErrorPoint err_point;
    if (catch_error(err_point)) goto on_error;

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) goto on_pi_error;

    // Step 3: Parse Module header, get the result (ph_res)
    ParseResult ph_res = parse_rawtree(cin, &pico_itera, &itera);
    if (ph_res.type == ParseNone) goto on_noparse;

    if (ph_res.type == ParseFail) {
        throw_pi_error(&pi_point, ph_res.error);
    }

    // Step 3: check / abstract module header
    // • module_header header = parse_module_header
    // Note: volatile is to protect from clobbering by longjmp
    header = abstract_header(ph_res.result, &itera, &pi_point);

    // Step 4:
    //  • Create new module
    //  • Update module based on imports
    // Note: volatile is to protect from clobbering by longjmp
    module = mk_module(*header, package, NULL);
    if (parent) {
        add_module_def(parent, header->name, module);
    } else {
        add_module(header->name, module, package);
    }

    old_module = get_std_current_module();
    set_std_current_module(module);

    for (size_t i = 0; i < dependencies.len; i++) {
        atlas_load_file(dependencies.data[i], package, module, mk_string_array(0, &ra), region, point);
    }


    // Step 5:
    //  • Using the environment, parse and run each expression/definition in the module
    bool next_iter = true;
    ErrorPoint env_point;
    if (catch_error(env_point)) {
        pi_point.multi = (MultiError) {
            .has_many = false,
            .error.message = env_point.error_message,
            .error.range = header->range,
        };

        goto on_pi_error;
    }
    Environment* env = env_from_module(module, &env_point, &ra);

    while (next_iter) {
        reset_subregion(iter_region);
        refresh_env(env);

        ph_res = parse_rawtree(cin, &pico_itera, &itera);
        if (ph_res.type == ParseNone) goto on_exit;

        if (ph_res.type == ParseFail) {
            goto on_parse_error;
        }
        if (ph_res.type != ParseSuccess) {
            panic(mv_string("Parse Returned Invalid Result!\n"));
        }

        // -------------------------------------------------------------------------
        // Resolution
        // -------------------------------------------------------------------------

        TopLevel abs = abstract(ph_res.result, env, &itera, &pi_point);

        // -------------------------------------------------------------------------
        // Type Checking
        // -------------------------------------------------------------------------

        // Note: typechecking annotates the syntax tree with types, but doesn't have
        // an output.
        TypeCheckContext tc_ctx = {
            .a = &itera, .pia = &pico_itera, .point = &pi_point, .target = gen_target, .logger = logger 
        };
        type_check(&abs, env, tc_ctx);

        // -------------------------------------------------------------------------
        // Code Generation
        // -------------------------------------------------------------------------

        // Ensure the target is 'fresh' for code-gen
        clear_target(gen_target);
        CodegenContext cg_ctx = {
            .a = &itera, .point = &err_point, .target = gen_target, .logger = logger
        };
        LinkData links = generate_toplevel(abs, env, cg_ctx);

        // -------------------------------------------------------------------------
        // Evaluation
        // -------------------------------------------------------------------------

        pico_run_toplevel(abs, gen_target, links, module, &itera, &err_point);
    }

 on_exit:
    // TODO: proper exit?
 on_noparse:
    delete_istream(in, &ra);
    if (old_module) set_std_current_module(old_module);
    uncapture_istream(cin);
    release_subregion(iter_region);
    release_executable_allocator(exec);
    return module;
 on_parse_error: {
        Document* out = copy_doc(ph_res.error.message, &ra);
        AtlasError new_err = {
            .range = ph_res.error.range,
            .message = out,
            .filename = filename,
            .captured_file = copy_string(*get_captured_buffer(cin), &ra),
        };

        delete_istream(in, &ra);
        if (old_module) set_std_current_module(old_module);
        release_subregion(iter_region);
        release_executable_allocator(exec);

        throw_at_error(point, new_err);
    }

 on_pi_error: {
        MultiError error;
        if (pi_point.multi.has_many) {
            PtrArray copied_errors = mk_ptr_array(pi_point.multi.errors.len, &ra); 
            for (size_t i = 0; i < pi_point.multi.errors.len; i++) {
                PicoError* old_error = pi_point.multi.errors.data[i];
                PicoError* new_error = mem_alloc(sizeof(PicoError), &ra);
                *new_error = (PicoError) {
                    .range = old_error->range,
                    .message = copy_doc(old_error->message, &ra),
                };
                push_ptr(new_error, &copied_errors);
            }
            error = (MultiError) {
                .has_many = true,
                .errors = copied_errors,
            };
        } else {
          error = (MultiError) {
              .has_many = false,
              .error.message = copy_doc(pi_point.multi.error.message, &ra),
              .error.range = pi_point.multi.error.range,
          };
        }
        AtlasMultiError new_err = {
            .error = error,
            .filename = filename,
            .captured_file = copy_string(*get_captured_buffer(cin), &ra),
        };

        delete_istream(in, &ra);
        if (old_module) set_std_current_module(old_module);
        release_subregion(iter_region);
        release_executable_allocator(exec);

        throw_at_multi_error(point, new_err);
    }
    

 on_error: {
        AtlasError new_err = {
            .range = pi_point.multi.error.range,
            .message = err_point.error_message,
            .filename = filename,
            .captured_file = copy_string(*get_captured_buffer(cin), &ra),
        };

        delete_istream(in, &ra);
        if (old_module) set_std_current_module(old_module);
        release_subregion(iter_region);
        release_executable_allocator(exec);

        throw_at_error(point, new_err);
    }
}

Module* atlas_load_target(AtlasInstance* instance, Package* package, AtlasTarget* target, RegionAllocator* region, AtErrorPoint* point) {
    /* Loading algorithm
     *  - For now, assume that dependencies form not just a DAG, but a tree
     *  - Therefore, we do NOT need to check for duplicates and recursion
     *  - This will need to change as projects and dependencies get more complex;
     */
    if (target->module) return target->module;
    Allocator ra = ra_to_gpa(region);

    // Does the target have an entry-point? If so, it is an executable,
    // otherwise it is a module.
    Module* out = NULL;
    // First, load all dependencies
    for (size_t i = 0; i < target->target_dependencies.len; i++) {
        size_t tidx;
        Symbol dep_sym = target->target_dependencies.data[i];
        if (sym_ptr_find(&tidx, dep_sym, instance->targets)) {
            atlas_load_target(instance, package, instance->targets.data[tidx].val, region, point);
        } else {
            PtrArray nodes = mk_ptr_array(5, &ra);
            push_ptr(mk_str_doc(mv_string("Unrecognized target: '"), &ra), &nodes);
            push_ptr(mk_str_doc(view_symbol_string(dep_sym), &ra), &nodes);
            push_ptr(mk_str_doc(mv_string("'"), &ra), &nodes);
            AtlasError err = {
                .message = mv_cat_doc(nodes, &ra),
            };
            throw_at_error(point, err);
        }
    }

    if (target->filename.type == Some) {
        out = atlas_load_file(target->filename.value, package, NULL, target->file_dependencies, region, point);
    } else {
        ModuleHeader header = (ModuleHeader) {
            .name = target->name,
            .imports = (Imports) {
                .clauses = mk_import_clause_array(0, &ra),
            },
            .exports = (Exports) {
                .export_all = true,
                .clauses = mk_export_clause_array(0, &ra),
            },
        };
        out = mk_module(header, package, NULL);
        add_module(target->name, out, package);
        for (size_t i = 0; i < target->file_dependencies.len; i++) {
            atlas_load_file(target->file_dependencies.data[i], package, out, mk_string_array(0, &ra), region, point);
        }
    }
    
    target->module = out;
    return out;
}

void register_package(AtlasInstance* instance, Package *package) {
    push_ptr(package, &instance->packages);
}

void set_instance_package(AtlasInstance* instance, Package* package) {
    instance->project_package = package;
}

void set_instance_project(AtlasInstance* instance, Project project) {
    instance->project_set = true;
    instance->project = project;
}

void add_library(Library library, String path, AtlasInstance* instance) {
    AtlasTarget* target = mem_alloc(sizeof(AtlasTarget), instance->gpa);

    StringOption filename = {.type = None};
    if (library.filename.type == Some) {
        filename = (StringOption) {
            .type = Some,
            .value = copy_string(library.filename.value, instance->gpa),
        };
    }

    *target = (AtlasTarget) {
        .name = library.name,
        .path = path,
        .filename = filename,
        .entrypoint = (SymbolOption) {.type = None},
        .target_dependencies = mk_symbol_array(0, instance->gpa),
        .file_dependencies = mk_string_array(library.submodules.len, instance->gpa),
        .module = NULL,
    };
    // Path = library path + "/" + filename + ".rl"
    for (size_t i = 0; i < library.submodules.len; i++) {
        String submodule_path = string_ncat(instance->gpa, 4,
                                            path,
                                            mv_string("/"),
                                            library.submodules.data[i],
                                            mv_string(".rl"));
        push_string(submodule_path, &target->file_dependencies);
    }
    
    sym_ptr_insert(library.name, target, &instance->targets);
}

void add_executable(Executable executable, String path, AtlasInstance* instance) {
    AtlasTarget* target = mem_alloc(sizeof(AtlasTarget), instance->gpa);

    *target = (AtlasTarget) {
        .name = executable.name,
        .path = path,
        .filename = (StringOption) {
          .type = Some,
          .value = string_ncat(instance->gpa, 4,
                               path, mv_string("/"), executable.filename, mv_string(".rl")),
        },
        .entrypoint = (SymbolOption) {.type = Some, .value = executable.entry_point },
        .target_dependencies = scopy_symbol_array(executable.dependencies, instance->gpa),
        .file_dependencies = mk_string_array(0, instance->gpa),
        .module = NULL,
    };
    sym_ptr_insert(executable.name, target, &instance->targets);
}

