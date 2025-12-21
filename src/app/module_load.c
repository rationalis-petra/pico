#include "app/module_load.h"

#include "platform/memory/region.h"
#include "platform/memory/executable.h"
#include "components/assembler/assembler.h"
#include "components/pretty/stream_printer.h"

#include "pico/binding/environment.h"
#include "pico/parse/parse.h"
#include "pico/analysis/abstraction.h"
#include "pico/analysis/typecheck.h"
#include "pico/codegen/codegen.h"
#include "pico/eval/call.h"

#include "pico/stdlib/meta/meta.h"


void load_module_from_istream(IStream* in, FormattedOStream* serr, String filename, Package* package, Module* parent, PiAllocator module_allocator, RegionAllocator* region) {

    // Step 1: Setup necessary state
    Allocator ra = ra_to_gpa(region);
    RegionAllocator* iter_region = make_subregion(region);
    Allocator itera = ra_to_gpa(iter_region);
    Allocator exec = mk_executable_allocator(&ra);

    PiAllocator pico_itera = convert_to_pallocator(&itera);

    Target target = (Target) {
        .target = mk_assembler(current_cpu_feature_flags(), &exec),
        .code_aux = mk_assembler(current_cpu_feature_flags(), &exec),
        .data_aux = mem_alloc(sizeof(U8Array), &ra),
    };
    *target.data_aux = mk_u8_array(256, &ra);

    ModuleHeader* volatile header = NULL;
    Module* volatile module = NULL;
    Module* volatile old_module = NULL;

    IStream* cin = mk_capturing_istream(in, &ra);
    reset_bytecount(cin);

    // Step 2:
    // Setup error reporting
    ErrorPoint point;
    if (catch_error(point)) goto on_error;

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) goto on_pi_error;

    // Step 3: Parse Module header, get the result (ph_res)
    ParseResult ph_res = parse_rawtree(cin, &pico_itera, &itera);
    if (ph_res.type == ParseNone) goto on_noparse;

    if (ph_res.type == ParseFail) {
        MultiError multi = (MultiError) {
            .has_many = false,
            .error = ph_res.error,
        };
        display_error(multi, *get_captured_buffer(cin), serr, filename, &itera);
        goto on_noparse;
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
    Environment* env = env_from_module(module, &point, &ra);
    while (next_iter) {
        reset_subregion(iter_region);
        refresh_env(env);

        ParseResult res = parse_rawtree(cin, &pico_itera, &itera);
        if (res.type == ParseNone) goto on_exit;

        if (res.type == ParseFail) {
            MultiError multi = (MultiError) {
                .has_many = false,
                .error = res.error,
            };
            display_error(multi, *get_captured_buffer(cin), serr, filename, &itera);
            goto on_error_generic;
        }
        if (res.type != ParseSuccess) {
            write_fstring(mv_string("Parse Returned Invalid Result!\n"), serr);
            goto on_error_generic;
        }

        // -------------------------------------------------------------------------
        // Resolution
        // -------------------------------------------------------------------------

        TopLevel abs = abstract(res.result, env, &itera, &pi_point);

        // -------------------------------------------------------------------------
        // Type Checking
        // -------------------------------------------------------------------------

        // Note: typechecking annotates the syntax tree with types, but doesn't have
        // an output.
        TypeCheckContext ctx = (TypeCheckContext) {
            .a = &itera, .pia = &pico_itera, .point = &pi_point, .target = target
        };
        type_check(&abs, env, ctx);

        // -------------------------------------------------------------------------
        // Code Generation
        // -------------------------------------------------------------------------

        // Ensure the target is 'fresh' for code-gen
        clear_target(target);
        LinkData links = generate_toplevel(abs, env, target, &itera, &point);

        // -------------------------------------------------------------------------
        // Evaluation
        // -------------------------------------------------------------------------

        pico_run_toplevel(abs, target, links, module, &itera, &point);
    }
    return;

 on_exit:
    goto cleanup;

 on_noparse:
 cleanup:
    if (old_module) set_std_current_module(old_module);
    uncapture_istream(cin);
    release_subregion(iter_region);
    release_executable_allocator(exec);
    return;

 on_pi_error:
    display_error(pi_point.multi, *get_captured_buffer(cin), serr, filename, &itera);
    goto on_error_generic;

 on_error:
    write_doc_formatted(point.error_message, 120, serr);
    write_fstring(mv_string("\n"), serr);
    goto on_error_generic;
    
 on_error_generic:
    if (old_module) set_std_current_module(old_module);
    release_subregion(iter_region);
    release_executable_allocator(exec);
    return;
}

void run_script_from_istream(IStream* in, FormattedOStream* serr, String filename, Module* current, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);
    Allocator exec = mk_executable_allocator(&ra);

    Target target = (Target) {
        .target = mk_assembler(current_cpu_feature_flags(), &exec),
        .code_aux = mk_assembler(current_cpu_feature_flags(), &exec),
        .data_aux = mem_alloc(sizeof(U8Array), &ra),
    };
    *target.data_aux = mk_u8_array(256, &ra);

    IStream* cin = mk_capturing_istream(in, &ra);
    reset_bytecount(cin);

    RegionAllocator* subregion = make_subregion(region);
    bool next_iter = true;
    Allocator itera = ra_to_gpa(subregion);
    PiAllocator pia = convert_to_pallocator(&itera);

    ErrorPoint point;
    if (catch_error(point)) goto on_error;

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) goto on_pi_error;

    while (next_iter) {
        // Prep the arena for another round
        clear_assembler(target.target);
        clear_assembler(target.code_aux);
        target.data_aux->len = 0;
        reset_subregion(subregion);

        Environment* env = env_from_module(current, &point, &itera);

        ParseResult res = parse_rawtree(cin, &pia, &itera);
        if (res.type == ParseNone) goto on_exit;

        if (res.type == ParseFail) {
            MultiError multi = (MultiError) {
                .has_many = false,
                .error = res.error,
            };
            display_error(multi, *get_captured_buffer(cin), serr, filename, &itera);
            goto on_error_generic;
        }
        if (res.type != ParseSuccess) {
            write_fstring(mv_string("Parse Returned Invalid Result!\n"), serr);
            goto on_error_generic;
        }

        // -------------------------------------------------------------------------
        // Resolution
        // -------------------------------------------------------------------------

        TopLevel abs = abstract(res.result, env, &itera, &pi_point);

        // -------------------------------------------------------------------------
        // Type Checking
        // -------------------------------------------------------------------------

        // Note: typechecking annotates the syntax tree with types, but doesn't have
        // an output.
        TypeCheckContext ctx = (TypeCheckContext) {
            .a = &itera, .pia = &pia, .point = &pi_point, .target = target
        };
        type_check(&abs, env, ctx);

        // -------------------------------------------------------------------------
        // Code Generation
        // -------------------------------------------------------------------------

        // Ensure the target is 'fresh' for code-gen
        clear_target(target);
        LinkData links = generate_toplevel(abs, env, target, &itera, &point);

        // -------------------------------------------------------------------------
        // Evaluation
        // -------------------------------------------------------------------------

        pico_run_toplevel(abs, target, links, current, &itera, &point);
    }

 on_exit:
    uncapture_istream(cin);
    release_subregion(subregion);
    release_executable_allocator(exec);
    return;

 on_pi_error:
    display_error(pi_point.multi, *get_captured_buffer(cin), serr, filename, &itera);
    goto on_error_generic;

 on_error:
    write_doc_formatted(point.error_message, 120, serr);
    write_fstring(mv_string("\n"), serr);
    goto on_error_generic;

 on_error_generic:
    uncapture_istream(cin);
    release_subregion(subregion);
    release_executable_allocator(exec);
    return;
}
