#include <inttypes.h>
#include <stdio.h>
#include <string.h>

#include "platform/memory/executable.h"
#include "platform/memory/arena.h"
#include "platform/filesystem/filesystem.h"
#include "platform/signals.h"

#include "data/stream.h"

#include "pico/data/string_array.h"
#include "pico/values/modular.h"
#include "pico/binding/environment.h"
#include "pico/parse/parse.h"
#include "pico/abstraction/abstraction.h"
#include "pico/typecheck/typecheck.h"
#include "pico/codegen/codegen.h"
#include "pico/eval/call.h"
#include "pico/build/build.h"
#include "pico/stdlib/platform/submodules.h"
#include "pico/stdlib/meta/meta.h"

#include "rune/eval/eval.h"

#include "atlas/eval/target.h"
#include "atlas/eval/instance.h"

struct AtlasInstance {
    Package* project_package;
    PtrArray packages;

    bool project_set;
    Project project;

    RuneEnv* env;
    ArenaAllocator* target_arena;
    Allocator ta;

    ValRef target_type;

    Allocator* gpa;
};

AtlasInstance* make_atlas_instance(Allocator* a) {
    AtlasInstance* instance = mem_alloc(sizeof(AtlasInstance), a);
    *instance = (AtlasInstance) {
      .packages = mk_ptr_array(4, a),
      .project_set = false,
      .env = mk_rune_env(a),
      .target_arena = make_arena_allocator(4096, a),
      .gpa = a,
    };
    instance->ta = aa_to_gpa(instance->target_arena);

    // Create a target type & insert it into the heap.  
    //ValueHeap* heap = get_pools(instance->env).value;

    //instance->target_type = mk_type_value(heap);
    return instance;
}

void delete_atlas_instance(AtlasInstance* instance) {
    Allocator* a = instance->gpa;
    sdelete_ptr_array(instance->packages);
    delete_rune_env(instance->env);

    if (instance->project_package) {
        delete_package(instance->project_package);
    }
    delete_arena_allocator(instance->target_arena);

    mem_free(instance, a);
}

AtlasDefaultTargets atlas_default_targets(AtlasInstance* instance) {
    return (AtlasDefaultTargets) { 
        .build = instance->project.package.default_build,
        .run = instance->project.package.default_run,
        .test = instance->project.package.default_test,
    };
}

ExprPool* get_expr_pool(AtlasInstance* instance) {
    return get_pools(instance->env).expr;
}

static void atlas_load_target(AtlasInstance* instance, Package* package, AtlasTarget* target, RegionAllocator* region, AtErrorPoint* point);
static Module* atlas_load_pico_target(AtlasInstance* instance, Package* package, PicoTarget* target, RegionAllocator* region, AtErrorPoint* point);

AtlasTarget* get_target(AtlasInstance* instance, Name name, RegionAllocator* region, AtErrorPoint* point) {
    RuneEvalResult res = get_value(name, instance->env, region);
    if (res.type != AValue) {
        AtlasError err = {
            .message = res.error_message,
        };
        throw_at_error(point, err);
    }
    ValueHeap* heap = get_pools(instance->env).value;

    AtlasTarget target = translate_from_rune(res.value, heap, region, point);
    AtlasTarget* tptr = mem_alloc(sizeof(AtlasTarget), instance->gpa);
    *tptr = target;
    return tptr;
}

void atlas_run(AtlasInstance* instance, String target_name, RegionAllocator* region, AtErrorPoint* point) {
    Allocator ra = ra_to_gpa(region);
    Name name = string_to_name(target_name);

    if (!instance->project_set) {
        AtlasError err = {
            .message = mk_str_doc(mv_string("No atlas project was found, please ensure there is an 'atlas-project' file in the current directory."), &ra),
        };
        throw_at_error(point, err);
    }
    AtlasTarget* target = get_target(instance, name, region, point);

    NameOption entry = target->is_generic ? (NameOption){.type = None} : target->pico.entrypoint;
    if (entry.type == None) {
        PtrArray nodes = mk_ptr_array(5, &ra);
        push_ptr(mk_str_doc(mv_string("Target '"), &ra), &nodes);
        push_ptr(mk_str_doc(target_name, &ra), &nodes);
        push_ptr(mk_str_doc(mv_string("' has no entry-point and is therefore is not runnable."), &ra), &nodes);

        AtlasError err = {
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
        package = mk_package(instance->project.package.name, pico_alloc);
        set_instance_package(instance, package);
    }

    // Then, add all dependencies
    NameArray deps = instance->project.package.dependencies;
    PtrArray avail = instance->packages;
    for (size_t i = 0; i < deps.len; i++) {
        bool found_dep = false;
        Name dep_name = deps.data[i];
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
                .message = mv_cat_doc(nodes, &ra),
            };
            throw_at_error(point, err);
        }
    }

    // We can guarantee at the moment that this is a pico
    Module* module = atlas_load_pico_target(instance, package, &target->pico, region, point);
    ModuleEntry* e = get_def_external(entry.val, module);
    if (!e) {
        PtrArray nodes = mk_ptr_array(5, &ra);
        push_ptr(mk_str_doc(mv_string("Entry Point '"), &ra), &nodes);
        push_ptr(mk_str_doc(view_name_string(target->pico.entrypoint.val), &ra), &nodes);
        push_ptr(mk_str_doc(mv_string("' in target '"), &ra), &nodes);
        push_ptr(mk_str_doc(target_name, &ra), &nodes);
        push_ptr(mk_str_doc(mv_string("' could not be found."), &ra), &nodes);

        AtlasError err = {
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
            push_ptr(mk_str_doc(view_name_string(target->pico.entrypoint.val), &ra), &ep_nodes);
            push_ptr(mk_str_doc(mv_string("' has type:"), &ra), &ep_nodes);
            push_ptr(mv_cat_doc(ep_nodes, &ra), &nodes);
        }
        push_ptr(pretty_type(&e->type, default_ptp, &ra), &nodes);
        push_ptr(mk_str_doc(mv_string("but entry points must have type"), &ra), &nodes);
        push_ptr(pretty_type(check_ty, default_ptp, &ra), &nodes);
        AtlasError err = {
            .message = mv_sep_doc(nodes, &ra),
        };
        throw_at_error(point, err);
    }
}

void atlas_build(AtlasInstance* instance, String target_name, RegionAllocator* region, AtErrorPoint* point) {
    Allocator ra = ra_to_gpa(region);
    Name name = string_to_name(target_name);

    if (!instance->project_set) {
        AtlasError err = {
            .message = mk_str_doc(mv_string("No atlas project was found, please ensure there is an 'atlas-project' file in the current directory."), &ra),
        };
        throw_at_error(point, err);
    }

    AtlasTarget* target = get_target(instance, name, region, point);

    NameOption entry = target->is_generic ? (NameOption){.type = None} : target->pico.entrypoint;
    if (entry.type == None) {
        PtrArray nodes = mk_ptr_array(5, &ra);
        push_ptr(mk_str_doc(mv_string("Target '"), &ra), &nodes);
        push_ptr(mk_str_doc(target_name, &ra), &nodes);
        push_ptr(mk_str_doc(mv_string("' has no entry-point and is therefore cannot be built."), &ra), &nodes);

        AtlasError err = {
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
        package = mk_package(instance->project.package.name, pico_alloc);
        set_instance_package(instance, package);
    }

    // Then, add all dependencies
    NameArray deps = instance->project.package.dependencies;
    PtrArray avail = instance->packages;
    for (size_t i = 0; i < deps.len; i++) {
        bool found_dep = false;
        Name dep_name = deps.data[i];
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
                .message = mv_cat_doc(nodes, &ra),
            };
            throw_at_error(point, err);
        }
    }

    // Guaranteed to be pico target as has an entry-point
    Module* module = atlas_load_pico_target(instance, package, &target->pico, region, point);
    ModuleEntry* e = get_def_external(entry.val, module);
    if (!e) {
        PtrArray nodes = mk_ptr_array(5, &ra);
        push_ptr(mk_str_doc(mv_string("Entry Point '"), &ra), &nodes);
        push_ptr(mk_str_doc(view_name_string(target->pico.entrypoint.val), &ra), &nodes);
        push_ptr(mk_str_doc(mv_string("' in target '"), &ra), &nodes);
        push_ptr(mk_str_doc(target_name, &ra), &nodes);
        push_ptr(mk_str_doc(mv_string("' could not be found."), &ra), &nodes);

        AtlasError err = {
            .message = mv_cat_doc(nodes, &ra),
        };
        throw_at_error(point, err);
    }

    PiAllocator pia = convert_to_pallocator(&ra);
    PiType* check_ty = mk_proc_type(&pia, 0, mk_prim_type(&pia, Unit));
    if (pi_type_eql(check_ty, &e->type, &ra)) {
        BuildErrorPoint build_point;
        if (catch_error(build_point)) {
            if (build_point.multi.has_many) {
                PtrArray out = mk_ptr_array(build_point.multi.errors.len, &ra);
                for (size_t i = 0; i < build_point.multi.errors.len; i++) {
                    BuildError* berr = build_point.multi.errors.data[i];
                    AtlasError* err = mem_alloc(sizeof(AtlasError), &ra);
                    *err = (AtlasError) {
                        .message = berr->message,
                    };
                }
                AtlasMultiError err = {
                    .error.has_many = true,
                    .error.errors = out,
                };
                throw_at_multi_error(point, err);
            } else {
                AtlasError err = {
                    .message = build_point.multi.error.message,
                };
                throw_at_error(point, err);
            }
        }

        // TODO: replace with proper allocator??
        RelicProgram* program = build_program(module, entry.val, &build_point, &ra);
        String image = path_cat(mv_string("build"), target_name, &ra);
        write_program(program, image, &ra);
        //link_program(String program, String lib, String out_name);

        FormattedOStream* fout = get_formatted_stdout();
        write_fstring(mv_string("Wrote image to "), fout);
        write_fstring(image, fout);
        write_fstring(mv_string("\n"), fout);
    } else {
        PtrArray nodes = mk_ptr_array(5, &ra);
        {
            PtrArray ep_nodes = mk_ptr_array(5, &ra);
            push_ptr(mk_str_doc(mv_string("Entry Point: '"), &ra), &ep_nodes);
            push_ptr(mk_str_doc(view_name_string(target->pico.entrypoint.val), &ra), &ep_nodes);
            push_ptr(mk_str_doc(mv_string("' has type:"), &ra), &ep_nodes);
            push_ptr(mv_cat_doc(ep_nodes, &ra), &nodes);
        }
        push_ptr(pretty_type(&e->type, default_ptp, &ra), &nodes);
        push_ptr(mk_str_doc(mv_string("but entry points must have type"), &ra), &nodes);
        push_ptr(pretty_type(check_ty, default_ptp, &ra), &nodes);
        AtlasError err = {
            .message = mv_sep_doc(nodes, &ra),
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
    volatile String vol_filename = filename;

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
    module = mk_module(*header, package, parent);

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

        SynTape tape = mk_syn_tape(&ra, 128);
        AbstractionCtx ab_ctx = {
            .tape = tape, .env = env, .a = &itera, .point = &pi_point,
        };
        TopLevel abs = abstract(ph_res.result, ab_ctx);

        // -------------------------------------------------------------------------
        // Type Checking
        // -------------------------------------------------------------------------

        // Note: typechecking annotates the syntax tree with types, but doesn't have
        // an output.
        TypeCheckContext tc_ctx = {
            .tape = tape, .a = &itera, .pia = &pico_itera, .point = &pi_point, .target = gen_target, .logger = logger 
        };
        type_check(&abs, env, tc_ctx);


        // -------------------------------------------------------------------------
        // Code Generation
        // -------------------------------------------------------------------------

        // Ensure the target is 'fresh' for code-gen
        clear_target(gen_target);
        CodegenContext cg_ctx = {
            .tape = tape, .a = &itera, .pia = &pico_itera, .point = &err_point, .target = gen_target, .logger = logger
        };
        LinkData links = generate_toplevel(abs, env, cg_ctx);

        // -------------------------------------------------------------------------
        // Evaluation
        // -------------------------------------------------------------------------

        EvalCtx ev_ctx = {
            .tape = tape, .target = gen_target, .links = links, .module = module, .a = &itera, .point = &err_point
        };
        pico_run_toplevel(abs, ev_ctx);
    }

 on_exit: {
        // Check that all exported definitions are defined
        CheckExportResult result = check_exports(module, &itera);
        if (result.result == Err) {
            PtrArray docs = mk_ptr_array(4, &ra);
            push_ptr(mk_str_doc(mv_string("Module at"), &ra), &docs);
            push_ptr(mk_str_doc(vol_filename, &ra), &docs);
            push_ptr(mk_str_doc(mv_string("did not define all names it claimed to export. Undefinded values are:"), &ra), &docs);
            PtrArray names = mk_ptr_array(result.not_implemented.len, &ra);
            for (size_t i = 0; i < result.not_implemented.len; i++) {
                Document* name = mv_str_doc(view_name_string(result.not_implemented.data[i]), &ra);
                push_ptr(name, &names);
            }
            push_ptr(mk_hsep_doc(names, &ra), &docs);
            AtlasError err = {
                .message = mv_sep_doc(docs, &ra),
            };
            throw_at_error(point, err);
        }
    }
    // TODO: proper exit?
 on_noparse:
    delete_istream(in, &ra);
    if (old_module) set_std_current_module(old_module);
    uncapture_istream(cin);
    release_subregion(iter_region);
    release_executable_allocator(exec);
    return module;

 on_parse_error: {
        if (old_module) set_std_current_module(old_module);
        Document* out = copy_doc(ph_res.error.message, &ra);
        AtlasError new_err = {
            .range = ph_res.error.range,
            .message = out,
            .filename = vol_filename,
            .captured_file = copy_string(*get_captured_buffer(cin), &ra),
        };

        delete_istream(in, &ra);
        if (old_module) set_std_current_module(old_module);
        release_subregion(iter_region);
        release_executable_allocator(exec);

        throw_at_error(point, new_err);
    }

 on_pi_error: {
        if (old_module) set_std_current_module(old_module);
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
            .filename = vol_filename,
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
            .message = copy_doc(err_point.error_message, &ra),
            .filename = filename,
        };

        delete_istream(in, &ra);
        if (old_module) set_std_current_module(old_module);
        release_subregion(iter_region);
        release_executable_allocator(exec);

        throw_at_error(point, new_err);
    }
}

Module* atlas_load_pico_target(AtlasInstance* instance, Package* package, PicoTarget* target, RegionAllocator* region, AtErrorPoint* point) {
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
        Dependency dep = target->target_dependencies.data[i];
        switch (dep.type) {
        case DepSubmoduleFile:
            // Skip; will be loaded by atlas_load_file (below)
            break;
        case DepExternalFile:
            // Skip; TODO: check that this doesn't itself invoke any targets,
            // check file exists.
            break;
        case DepTarget:
            atlas_load_target(instance, package, dep.target, region, point);
            break;
        }
    }

    if (target->filename.type == Some) {
        out = atlas_load_file(target->filename.val, package, NULL, target->file_dependencies, region, point);
    } else {
        if (target->name.type == None) {
            AtlasError err = {
                .message = mv_cstr_doc("Any library/executable without a name must have a filename.", &ra),
            };
            throw_at_error(point, err);
        }
        ModuleHeader header = (ModuleHeader) {
            .name = target->name.val,
            .imports = (Imports) {
                .clauses = mk_import_clause_array(0, &ra),
            },
            .exports = (Exports) {
                .export_all = true,
                .clauses = mk_export_clause_array(0, &ra),
            },
        };
        out = mk_module(header, package, NULL);
        for (size_t i = 0; i < target->file_dependencies.len; i++) {
            atlas_load_file(target->file_dependencies.data[i], package, out, mk_string_array(0, &ra), region, point);
        }
    }
    
    target->module = out;
    return out;
}

void atlas_load_target(AtlasInstance* instance, Package* package, AtlasTarget* target, RegionAllocator* region, AtErrorPoint* point) {
    if (target->is_generic) {
        panic(mv_string("TODO: load generic targets"));
    } else {
        atlas_load_pico_target(instance, package, &target->pico, region, point);
    }
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

void atlas_add_def(AtlasInstance* instance, Def def) {
    rune_add_def(def.name, def.expr, instance->env);
}


