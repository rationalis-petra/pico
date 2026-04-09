#include "data/string.h"
#include "data/stream.h"

#include "platform/terminal/terminal.h"

#include "components/assembler/assembler.h"
#include "components/pretty/stream_printer.h"

#include "pico/data/error.h"
#include "pico/parse/parse.h"
#include "pico/abstraction/abstraction.h"
#include "pico/typecheck/typecheck.h"
#include "pico/codegen/codegen.h"
#include "pico/eval/call.h"
#include "pico/stdlib/extra.h"

#include "app/repl.h"

bool noninteractive_repl_iter(Allocator* stdalloc, RegionAllocator* region, Allocator* exec, Module* module, IterOpts opts) {
    IStream *cin = get_stdin_stream();
    FormattedOStream* cout = get_formatted_stdout();
    // Note: we need to be aware of the arena and error point, as both are used
    // by code in the 'true' branches of the nonlocal exits, and may be stored
    // in registers, so they cannotbe changed (unless marked volatile).
    Allocator ra = ra_to_gpa(region);
    PiAllocator pico_region = convert_to_pallocator(&ra);

    IStream* volatile  cp_in = mk_capturing_istream(cin, &ra);
    reset_bytecount(cp_in);

    Target gen_target = {
        .target = mk_assembler(current_cpu_feature_flags(), exec),
        .code_aux = mk_assembler(current_cpu_feature_flags(), exec),
        .data_aux = mem_alloc(sizeof(U8Array), &ra)
    };
    *gen_target.data_aux = mk_u8_array(128, &ra);

    jump_buf exit_point;
    if (set_jump(exit_point)) goto on_exit;
    set_exit_callback(&exit_point);

    ErrorPoint point;
    if (catch_error(point)) goto on_error;

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) goto on_pi_error;

    Environment* env = env_from_module(module, &point, &ra);

    if (!opts.is_eval) {
        String name = symbol_to_string(module_name(module), &ra);
        write_fstring(name, cout);
        write_fstring(mv_string(" > "), cout);
    }

    ParseResult res = parse_rawtree(cp_in, &pico_region, &ra);

    if (res.type == ParseNone) {
        write_fstring(mv_string("\n"), cout);
        goto on_exit;
    }
    if (res.type == ParseFail) {
        MultiError multi = (MultiError) {
            .has_many = false,
            .error = res.error,
        };
        display_error(multi, *get_captured_buffer(cp_in), get_formatted_stdout(), mv_string("stdin"), stdalloc);
        return true;
    }
    if (res.type != ParseSuccess) {
        // If parse is invalid, means internal bug, so better exit soon!
        write_fstring(mv_string("Parse Returned Invalid Result!\n"), cout);
        return false;
    }

    Document* doc;
    if (opts.debug_print) {
        doc = pretty_rawtree(res.result, &ra);
        start_underline(cout);
        write_fstring(mv_string("Raw Syntax\n"), cout);
        end_underline(cout);
        write_doc_formatted(doc, 120, cout);
        write_fstring(mv_string("\n"), cout);
    }

    // -------------------------------------------------------------------------
    // Resolution
    // -------------------------------------------------------------------------

    SynTape tape = mk_syn_tape(&ra, 128);
    AbstractionCtx ab_ctx = {
        .tape = tape, .env = env, .a = &ra, .point = &pi_point,
    };
    TopLevel abs = abstract(res.result, ab_ctx);

    if (opts.debug_print) {
        start_underline(cout);
        write_fstring(mv_string("Abstract Syntax:\n"), cout);
        end_underline(cout);
        doc = pretty_toplevel(&abs, tape, &ra);
        write_doc_formatted(doc, 120, cout);
        write_fstring(mv_string("\n"), cout);
    }

    // -------------------------------------------------------------------------
    // Type Checking
    // -------------------------------------------------------------------------

    // Note: typechecking annotates the syntax tree with types, but doesn't have
    // an output.
    TypeCheckContext tc_ctx = {
        .tape = tape, .a = &ra, .pia = &pico_region, .point = &pi_point, .target = gen_target,
    };
    type_check(&abs, env, tc_ctx);

    if (opts.debug_print) {
        PiType* ty = toplevel_type(abs, tape);
        if (ty) {
            start_underline(cout);
            write_fstring(mv_string("Inferred Type\n"), cout);
            end_underline(cout);
            doc = mv_nest_doc(2, pretty_type(ty, default_ptp, &ra), &ra);
            write_doc_formatted(doc, 120, cout);
            write_fstring(mv_string("\n"), cout);
        }
    }

    // -------------------------------------------------------------------------
    // Code Generation
    // -------------------------------------------------------------------------

    clear_target(gen_target);
    CodegenContext cg_ctx = {
        .tape = tape, .a = &ra, .point = &point, .target = gen_target,
    };
    LinkData links = generate_toplevel(abs, env, cg_ctx);

    if (opts.debug_print) {
        start_underline(cout);
        write_fstring(mv_string("Compiled Binary:\n"), cout);
        end_underline(cout);
        write_fstring(mv_string("Execute Assembly:\n"), cout);
        doc = pretty_assembler(gen_target.target, &ra);
        write_doc_formatted(doc, 120, cout);
        write_fstring(mv_string("\nCode Segment:\n"), cout);
        doc = pretty_assembler(gen_target.code_aux, &ra);
        write_doc_formatted(doc, 120, cout);
        write_fstring(mv_string("\nData Segment:\n"), cout);
        // TODO (FEAT): as per string (below)
        write_fstring(mv_string("TODO: data segment needs to type-annotate data to display properly!"), cout);
        //write_fstring(string_from_ASCII(*gen_target.data_aux, &arena), cout);
        write_fstring(mv_string("\n"), cout);
    }

    // -------------------------------------------------------------------------
    // Evaluation
    // -------------------------------------------------------------------------

    EvalCtx ev_ctx = {
        .tape = tape, .target = gen_target, .links = links, .module = module, .a = &ra, .point = &point
    };
    EvalResult call_res = pico_run_toplevel(abs, ev_ctx);
    if (opts.debug_print) {
        write_fstring(mv_string("Evaluation Result\n"), cout);
    }

    if (opts.debug_print || !opts.is_eval) {
        doc = pretty_res(call_res, &ra);
        write_doc_formatted(doc, 140, get_formatted_stdout());
        write_fstring(mv_string("\n"), cout);
    }

    delete_assembler(gen_target.target);
    delete_assembler(gen_target.code_aux);
    return true;

 on_pi_error:
    display_error(pi_point.multi, *get_captured_buffer(cp_in), cout, mv_string("stdin"), &ra);
    delete_assembler(gen_target.target);
    delete_assembler(gen_target.code_aux);
    return true;

 on_error:
    write_doc_formatted(point.error_message, 120, cout);
    write_fstring(mv_string("\n"), cout);
    delete_assembler(gen_target.target);
    delete_assembler(gen_target.code_aux);
    return true;

 on_exit:
    delete_assembler(gen_target.target);
    delete_assembler(gen_target.code_aux);
    return false;
}
