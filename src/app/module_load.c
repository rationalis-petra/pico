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


void load_module_from_istream(IStream* in, FormattedOStream* serr, const char* filename, Package* package, Module* parent, PiAllocator pia) {
    // Step 1: Setup necessary state
    Allocator c_allocator = convert_to_callocator(&pia);
    Allocator* a = &c_allocator;
    
    ArenaAllocator* arena = make_arena_allocator(16384, a);
    ArenaAllocator* iter_arena = make_arena_allocator(16384, a);
    Allocator aa = aa_to_gpa(arena);
    Allocator ia = aa_to_gpa(iter_arena);
    Allocator exec = mk_executable_allocator(a);

    PiAllocator pico_arena = convert_to_pallocator(&aa);
    PiAllocator pico_iter_arena = convert_to_pallocator(&ia);

    Target target = (Target) {
        .target = mk_assembler(current_cpu_feature_flags(), &exec),
        .code_aux = mk_assembler(current_cpu_feature_flags(), &exec),
        .data_aux = mem_alloc(sizeof(U8Array), a),
    };
    *target.data_aux = mk_u8_array(256, a);

    ModuleHeader* volatile header = NULL;
    Module* volatile module = NULL;
    Module* volatile old_module = NULL;

    IStream* cin = mk_capturing_istream(in, &aa);
    reset_bytecount(cin);

    // Step 2:
    // Setup error reporting
    ErrorPoint point;
    if (catch_error(point)) goto on_error;

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) goto on_pi_error;

    // Step 3: Parse Module header, get the result (ph_res)
    ParseResult ph_res = parse_rawtree(cin, &pico_arena, &aa);
    if (ph_res.type == ParseNone) goto on_noparse;

    if (ph_res.type == ParseFail) {
        MultiError multi = (MultiError) {
            .has_many = false,
            .error = ph_res.error,
        };
        display_error(multi, cin, serr, filename, a);
        goto on_noparse;
    }

    // Step 3: check / abstract module header
    // • module_header header = parse_module_header
    // Note: volatile is to protect from clobbering by longjmp
    header = abstract_header(ph_res.result, &aa, &pi_point);

    // Step 4:
    //  • Create new module
    //  • Update module based on imports
    // Note: volatile is to protect from clobbering by longjmp
    module = mk_module(*header, package, parent, pia);
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
    Environment* env = env_from_module(module, &point, &aa);
    while (next_iter) {
        reset_arena_allocator(iter_arena);
        refresh_env(env);

        ParseResult res = parse_rawtree(cin, &pico_iter_arena, &ia);
        if (res.type == ParseNone) goto on_exit;

        if (res.type == ParseFail) {
            MultiError multi = (MultiError) {
                .has_many = false,
                .error = res.error,
            };
            display_error(multi, cin, serr, filename, a);
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

        TopLevel abs = abstract(res.result, env, &ia, &pi_point);

        // -------------------------------------------------------------------------
        // Type Checking
        // -------------------------------------------------------------------------

        // Note: typechecking annotates the syntax tree with types, but doesn't have
        // an output.
        TypeCheckContext ctx = (TypeCheckContext) {
            .a = &ia, .pia = &pico_arena, .point = &pi_point, .target = target
        };
        type_check(&abs, env, ctx);

        // -------------------------------------------------------------------------
        // Code Generation
        // -------------------------------------------------------------------------

        // Ensure the target is 'fresh' for code-gen
        clear_target(target);
        LinkData links = generate_toplevel(abs, env, target, &ia, &point);

        // -------------------------------------------------------------------------
        // Evaluation
        // -------------------------------------------------------------------------

        pico_run_toplevel(abs, target, links, module, &ia, &point);
    }
    return;

 on_exit:
    goto cleanup;

 on_noparse:
 cleanup:
    if (old_module) set_std_current_module(old_module);
    uncapture_istream(cin);
    delete_arena_allocator(arena);
    delete_arena_allocator(iter_arena);
    release_executable_allocator(exec);
    sdelete_u8_array(*target.data_aux);
    mem_free(target.data_aux, a);
    return;

 on_pi_error:
    display_error(pi_point.multi, cin, serr, filename, a);
    goto on_error_generic;

 on_error:
    write_fstring(point.error_message, serr);
    write_fstring(mv_string("\n"), serr);
    goto on_error_generic;
    
 on_error_generic:
    if (old_module) set_std_current_module(old_module);
    delete_arena_allocator(arena);
    delete_arena_allocator(iter_arena);
    release_executable_allocator(exec);
    sdelete_u8_array(*target.data_aux);
    mem_free(target.data_aux, a);
    return;
}


void run_script_from_istream(IStream* in, FormattedOStream* serr, const char* filename, Module* current, Allocator* a) {
    ArenaAllocator* arena = make_arena_allocator(16384, a);
    Allocator aa = aa_to_gpa(arena);
    PiAllocator pico_arena = convert_to_pallocator(&aa);
    Allocator exec = mk_executable_allocator(a);

    Target target = (Target) {
        .target = mk_assembler(current_cpu_feature_flags(), &exec),
        .code_aux = mk_assembler(current_cpu_feature_flags(), &exec),
        .data_aux = mem_alloc(sizeof(U8Array), a),
    };
    *target.data_aux = mk_u8_array(256, a);

    IStream* cin = mk_capturing_istream(in, a);
    reset_bytecount(cin);

    ErrorPoint point;
    if (catch_error(point)) goto on_error;

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) goto on_pi_error;

    bool next_iter = true;
    while (next_iter) {
        // Prep the arena for another round
        clear_assembler(target.target);
        clear_assembler(target.code_aux);
        target.data_aux->len = 0;

        reset_arena_allocator(arena);
        Environment* env = env_from_module(current, &point, &aa);

        ParseResult res = parse_rawtree(cin, &pico_arena, &aa);
        if (res.type == ParseNone) goto on_exit;

        if (res.type == ParseFail) {
            MultiError multi = (MultiError) {
                .has_many = false,
                .error = res.error,
            };
            display_error(multi, cin, serr, filename, a);
            delete_arena_allocator(arena);
            return;
        }
        if (res.type != ParseSuccess) {
            write_fstring(mv_string("Parse Returned Invalid Result!\n"), serr);
            delete_arena_allocator(arena);
            return;
        }

        // -------------------------------------------------------------------------
        // Resolution
        // -------------------------------------------------------------------------

        TopLevel abs = abstract(res.result, env, &aa, &pi_point);

        // -------------------------------------------------------------------------
        // Type Checking
        // -------------------------------------------------------------------------

        // Note: typechecking annotates the syntax tree with types, but doesn't have
        // an output.
        TypeCheckContext ctx = (TypeCheckContext) {
            .a = &aa, .pia = &pico_arena, .point = &pi_point, .target = target
        };
        type_check(&abs, env, ctx);

        // -------------------------------------------------------------------------
        // Code Generation
        // -------------------------------------------------------------------------

        // Ensure the target is 'fresh' for code-gen
        clear_target(target);
        LinkData links = generate_toplevel(abs, env, target, &aa, &point);

        // -------------------------------------------------------------------------
        // Evaluation
        // -------------------------------------------------------------------------

        pico_run_toplevel(abs, target, links, current, &aa, &point);
    }

 on_exit:
    delete_assembler(target.target);
    delete_assembler(target.code_aux);
    sdelete_u8_array(*target.data_aux);
    mem_free(target.data_aux, a);
    uncapture_istream(cin);
    delete_arena_allocator(arena);
    release_executable_allocator(exec);
    return;

 on_pi_error:
    display_error(pi_point.multi, cin, serr, filename, &aa);
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
    uncapture_istream(cin);
    delete_arena_allocator(arena);
    release_executable_allocator(exec);
    return;
}
