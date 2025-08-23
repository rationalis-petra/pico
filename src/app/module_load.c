#include "app/module_load.h"

#include "platform/memory/arena.h"
#include "platform/memory/executable.h"
#include "components/assembler/assembler.h"

#include "pico/binding/environment.h"
#include "pico/parse/parse.h"
#include "pico/analysis/abstraction.h"
#include "pico/analysis/typecheck.h"
#include "pico/codegen/codegen.h"
#include "pico/eval/call.h"

#include "pico/stdlib/meta/meta.h"


void load_module_from_istream(IStream* in, FormattedOStream* serr, const char* filename, Package* package, Module* parent, Allocator* a) {
    Allocator arena = mk_arena_allocator(16384, a);
    Allocator iter_arena = mk_arena_allocator(16384, a);
    Allocator exec = mk_executable_allocator(a);

    Target target = (Target) {
        .target = mk_assembler(current_cpu_feature_flags(), &exec),
        .code_aux = mk_assembler(current_cpu_feature_flags(), &exec),
        .data_aux = mem_alloc(sizeof(U8Array), a),
    };
    *target.data_aux = mk_u8_array(256, a);

    ModuleHeader* volatile header = NULL;
    Module* volatile module = NULL;
    Module* volatile old_module = NULL;

    // Step 1:
    // Setup error reporting
    in = mk_capturing_istream(in, &arena);
    reset_bytecount(in);

    ErrorPoint point;
    if (catch_error(point)) goto on_error;

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) goto on_pi_error;

    // Step 2: Parse Module header, get the result (ph_res)
    ParseResult ph_res = parse_rawtree(in, &arena);
    if (ph_res.type == ParseNone) goto on_noparse;

    if (ph_res.type == ParseFail) {
        MultiError multi = (MultiError) {
            .has_many = false,
            .error = ph_res.error,
        };
        display_error(multi, in, serr, filename, a);
        goto on_noparse;
    }

    // Step 3: check / abstract module header
    // • module_header header = parse_module_header
    // Note: volatile is to protect from clobbering by longjmp
    header = abstract_header(ph_res.result, &arena, &pi_point);

    // Step 4:
    //  • Create new module
    //  • Update module based on imports
    // Note: volatile is to protect from clobbering by longjmp
    module = mk_module(*header, package, parent, a);
    if (parent) {
        add_module_def(parent, header->name, module);
    } else {
        add_module(header->name, module, package);
    }

    old_module = get_std_current_module();
    set_std_current_module(module);

    // Step 5:
    //  • Using the environment, parse and run each expression/definition in the module
    bool next_iter = true;
    Environment* env = env_from_module(module, &point, &arena);
    while (next_iter) {
        reset_arena_allocator(iter_arena);
        refresh_env(env, &iter_arena);

        ParseResult res = parse_rawtree(in, &iter_arena);
        if (res.type == ParseNone) goto on_exit;

        if (res.type == ParseFail) {
            MultiError multi = (MultiError) {
                .has_many = false,
                .error = ph_res.error,
            };
            display_error(multi, in, serr, filename, a);
            goto on_error_generic;
            return;
        }
        if (res.type != ParseSuccess) {
            write_fstring(mv_string("Parse Returned Invalid Result!\n"), serr);
            goto on_error_generic;
            return;
        }

        // -------------------------------------------------------------------------
        // Resolution
        // -------------------------------------------------------------------------

        TopLevel abs = abstract(res.result, env, &iter_arena, &pi_point);

        // -------------------------------------------------------------------------
        // Type Checking
        // -------------------------------------------------------------------------

        // Note: typechecking annotates the syntax tree with types, but doesn't have
        // an output.
        TypeCheckContext ctx = (TypeCheckContext) {
            .a = &iter_arena, .point = &pi_point, .target = target
        };
        type_check(&abs, env, ctx);

        // -------------------------------------------------------------------------
        // Code Generation
        // -------------------------------------------------------------------------

        // Ensure the target is 'fresh' for code-gen
        clear_target(target);
        LinkData links = generate_toplevel(abs, env, target, &iter_arena, &point);

        // -------------------------------------------------------------------------
        // Evaluation
        // -------------------------------------------------------------------------

        pico_run_toplevel(abs, target, links, module, &iter_arena, &point);
    }
    return;

 on_exit:
    goto cleanup;

 on_noparse:
 cleanup:
    if (old_module) set_std_current_module(old_module);
    release_arena_allocator(arena);
    release_arena_allocator(iter_arena);
    release_executable_allocator(exec);
    sdelete_u8_array(*target.data_aux);
    mem_free(target.data_aux, a);
    return;

 on_pi_error:
    display_error(pi_point.multi, in, serr, filename, a);
    goto on_error_generic;

 on_error:
    write_fstring(point.error_message, serr);
    write_fstring(mv_string("\n"), serr);
    goto on_error_generic;
    
 on_error_generic:
    if (old_module) set_std_current_module(old_module);
    release_arena_allocator(arena);
    release_arena_allocator(iter_arena);
    release_executable_allocator(exec);
    sdelete_u8_array(*target.data_aux);
    mem_free(target.data_aux, a);
    return;
}


void run_script_from_istream(IStream* in, FormattedOStream* serr, const char* filename, Module* current, Allocator* a) {
    Allocator arena = mk_arena_allocator(16384, a);
    Allocator exec = mk_executable_allocator(a);

    Target target = (Target) {
        .target = mk_assembler(current_cpu_feature_flags(), &exec),
        .code_aux = mk_assembler(current_cpu_feature_flags(), &exec),
        .data_aux = mem_alloc(sizeof(U8Array), a),
    };
    *target.data_aux = mk_u8_array(256, a);

    ErrorPoint point;
    if (catch_error(point)) goto on_error;

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) goto on_pi_error;

    in = mk_capturing_istream(in, a);
    reset_bytecount(in);

    bool next_iter = true;
    while (next_iter) {
        // Prep the arena for another round
        clear_assembler(target.target);
        clear_assembler(target.code_aux);
        target.data_aux->len = 0;

        reset_arena_allocator(arena);
        Environment* env = env_from_module(current, &point, &arena);

        ParseResult res = parse_rawtree(in, &arena);
        if (res.type == ParseNone) goto on_exit;

        if (res.type == ParseFail) {
            MultiError multi = (MultiError) {
                .has_many = false,
                .error = res.error,
            };
            display_error(multi, in, serr, filename, a);
            release_arena_allocator(arena);
            return;
        }
        if (res.type != ParseSuccess) {
            write_fstring(mv_string("Parse Returned Invalid Result!\n"), serr);
            release_arena_allocator(arena);
            return;
        }

        // -------------------------------------------------------------------------
        // Resolution
        // -------------------------------------------------------------------------

        TopLevel abs = abstract(res.result, env, &arena, &pi_point);

        // -------------------------------------------------------------------------
        // Type Checking
        // -------------------------------------------------------------------------

        // Note: typechecking annotates the syntax tree with types, but doesn't have
        // an output.
        TypeCheckContext ctx = (TypeCheckContext) {
            .a = &arena, .point = &pi_point, .target = target
        };
        type_check(&abs, env, ctx);

        // -------------------------------------------------------------------------
        // Code Generation
        // -------------------------------------------------------------------------

        LinkData links = generate_toplevel(abs, env, target, &arena, &point);

        // -------------------------------------------------------------------------
        // Evaluation
        // -------------------------------------------------------------------------

        pico_run_toplevel(abs, target, links, current, &arena, &point);
    }

 on_exit:
    delete_assembler(target.target);
    delete_assembler(target.code_aux);
    sdelete_u8_array(*target.data_aux);
    mem_free(target.data_aux, a);
    uncapture_istream(in);
    release_arena_allocator(arena);
    release_executable_allocator(exec);
    return;

 on_pi_error:
    display_error(pi_point.multi, in, serr, filename, &arena);
    goto on_error_generic;

 on_error:
    write_fstring(point.error_message, serr);
    write_fstring(mv_string("\n"), serr);
 goto on_error_generic;

 on_error_generic:
    delete_assembler(target.target);
    delete_assembler(target.code_aux);
    sdelete_u8_array(*target.data_aux);
    mem_free(target.data_aux, a);
    uncapture_istream(in);
    release_arena_allocator(arena);
    release_executable_allocator(exec);
    return;
}
