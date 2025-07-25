﻿#include "platform/error.h"
#include "platform/memory/std_allocator.h"
#include "platform/memory/executable.h"
#include "platform/memory/arena.h"
#include "platform/jump.h"
#include "platform/io/terminal.h"
#include "platform/window/window.h"
#include "platform/hedron/hedron.h"

#include "data/string.h"
#include "data/stream.h"

#include "assembler/assembler.h"
#include "pretty/stream_printer.h"
#include "pretty/document.h"

#include "pico/syntax/concrete.h"
#include "pico/parse/parse.h"
#include "pico/binding/environment.h"
#include "pico/analysis/abstraction.h"
#include "pico/analysis/typecheck.h"
#include "pico/codegen/codegen.h"
#include "pico/eval/call.h"
#include "pico/stdlib/stdlib.h"
#include "pico/stdlib/extra.h"
#include "pico/values/types.h"

#include "app/command_line_opts.h"
#include "app/module_load.h"
#include "app/help_string.h"

static const char* version = "0.0.7";

typedef struct {
    bool debug_print;
    bool interactive;
} IterOpts;

bool repl_iter(IStream* cin, FormattedOStream* cout, Allocator* a, Allocator* exec, Module* module, IterOpts opts) {
    // Note: we need to be aware of the arena and error point, as both are used
    // by code in the 'true' branches of the nonlocal exits, and may be stored
    // in registers, so they cannotbe changed (unless marked volatile).
    Allocator arena = mk_arena_allocator(4096, a);

    cin = mk_capturing_istream(cin, &arena);
    reset_bytecount(cin);

    Target gen_target = {
        .target = mk_assembler(current_cpu_feature_flags(), exec),
        .code_aux = mk_assembler(current_cpu_feature_flags(), exec),
        .data_aux = mem_alloc(sizeof(U8Array), &arena)
    };
    *gen_target.data_aux = mk_u8_array(128, &arena);

    Environment* env = env_from_module(module, &arena);

    jump_buf exit_point;
    if (set_jump(exit_point)) goto on_exit;
    set_exit_callback(&exit_point);

    ErrorPoint point;
    if (catch_error(point)) goto on_error;

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) goto on_pi_error;

    if (opts.interactive) {
        String* name = get_name(module);
        if (name) write_fstring(*name, cout);
        write_fstring(mv_string(" > "), cout);
    }

    ParseResult res = parse_rawtree(cin, &arena);

    if (res.type == ParseNone) {
        write_fstring(mv_string("\n"), cout);
        goto on_exit;
    }
    if (res.type == ParseFail) {
        MultiError multi = (MultiError) {
            .has_many = false,
            .error = res.error,
        };
        display_error(multi, cin, get_formatted_stdout(), a);
        release_arena_allocator(arena);
        return true;
    }
    if (res.type != ParseSuccess) {
        // If parse is invalid, means internal bug, so better exit soon!
        write_fstring(mv_string("Parse Returned Invalid Result!\n"), cout);
        release_arena_allocator(arena);
        return false;
    }

    Document* doc;
    if (opts.debug_print) {
        doc = pretty_rawtree(res.result, &arena);
        start_underline(cout);
        write_fstring(mv_string("Raw Syntax\n"), cout);
        end_underline(cout);
        write_doc_formatted(doc, 120, cout);
        write_fstring(mv_string("\n"), cout);
    }

    // -------------------------------------------------------------------------
    // Resolution
    // -------------------------------------------------------------------------

    TopLevel abs = abstract(res.result, env, &arena, &pi_point);

    if (opts.debug_print) {
        start_underline(cout);
        write_fstring(mv_string("Abstract Syntax:\n"), cout);
        end_underline(cout);
        doc = pretty_toplevel(&abs, &arena);
        write_doc_formatted(doc, 120, cout);
        write_fstring(mv_string("\n"), cout);
    }

    // -------------------------------------------------------------------------
    // Type Checking
    // -------------------------------------------------------------------------

    // Note: typechecking annotates the syntax tree with types, but doesn't have
    // an output.
    type_check(&abs, env, &arena, &pi_point);

    if (opts.debug_print) {
        PiType* ty = toplevel_type(abs);
        if (ty) {
            start_underline(cout);
            write_fstring(mv_string("Inferred Type\n"), cout);
            end_underline(cout);
            doc = mv_nest_doc(2, pretty_type(ty, &arena), a);
            write_doc_formatted(doc, 120, cout);
            write_fstring(mv_string("\n"), cout);
        }
    }

    // -------------------------------------------------------------------------
    // Code Generation
    // -------------------------------------------------------------------------

    LinkData links = generate_toplevel(abs, env, gen_target, &arena, &point);

    if (opts.debug_print) {
        start_underline(cout);
        write_fstring(mv_string("Compiled Binary:\n"), cout);
        end_underline(cout);
        write_fstring(mv_string("Execute Assembly:\n"), cout);
        doc = pretty_assembler(gen_target.target, &arena);
        write_doc_formatted(doc, 120, cout);
        write_fstring(mv_string("\nCode Segment:\n"), cout);
        doc = pretty_assembler(gen_target.code_aux, &arena);
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

    EvalResult call_res = pico_run_toplevel(abs, gen_target, links, module, &arena, &point);
    if (opts.debug_print) {
        write_fstring(mv_string("Evaluation Result\n"), cout);
    }

    if (opts.debug_print || opts.interactive) {
        doc = pretty_res(call_res, &arena);
        write_doc_formatted(doc, 140, get_formatted_stdout());
        write_fstring(mv_string("\n"), cout);
    }

    delete_assembler(gen_target.target);
    delete_assembler(gen_target.code_aux);
    release_arena_allocator(arena);
    return true;

 on_pi_error:
    display_error(pi_point.multi, cin, cout, &arena);
    delete_assembler(gen_target.target);
    delete_assembler(gen_target.code_aux);
    release_arena_allocator(arena);
    return true;

 on_error:
    write_fstring(point.error_message, cout);
    write_fstring(mv_string("\n"), cout);
    delete_assembler(gen_target.target);
    delete_assembler(gen_target.code_aux);
    release_arena_allocator(arena);
    return true;

 on_exit:
    delete_assembler(gen_target.target);
    delete_assembler(gen_target.code_aux);
    release_arena_allocator(arena);
    return false;
}

int main(int argc, char** argv) {

    // Setup
    Allocator* stdalloc = get_std_allocator();
    IStream* cin = get_stdin_stream();
    OStream* cout = get_stdout_stream();
    Allocator exalloc = mk_executable_allocator(stdalloc);

    // Init terminal first, as other initializers may panic (and therefore write
    // to stdout)
    init_terminal(stdalloc);

    // Initialization order here is not important
    init_ctypes();
    init_asm();
    init_symbols(stdalloc);
    init_dynamic_vars(stdalloc);
    if (init_window_system(stdalloc)) {
        write_string(mv_string("Warning: failed to init window system!\n"), cout);
    }
    if (is_hedron_supported()) {
        if (init_hedron(stdalloc)) {
            write_string(mv_string("Warning: failed to init hedron!\n"), cout);
      }
    }
    thread_init_dynamic_vars();

    Assembler* ass = mk_assembler(current_cpu_feature_flags(), &exalloc);
    Assembler* ass_base = mk_assembler(current_cpu_feature_flags(), &exalloc);
    Package* base = base_package(ass_base, stdalloc, stdalloc);
    delete_assembler(ass_base);

    Module* module = get_module(string_to_symbol(mv_string("user")), base);

    set_std_current_module(module);
    set_current_package(base);
    set_std_istream(cin);
    set_std_ostream(cout);

    // Argument parsing
    StringArray args = mk_string_array(argc - 1, stdalloc);
    for (int i = 1; i < argc; i++) {
        push_string(mv_string(argv[i]), &args);
    }
    Command command = parse_command(args);
    sdelete_string_array(args);

    switch (command.type) {
    case CRepl: {
        IterOpts opts = (IterOpts) {
            .debug_print = command.repl.debug_print,
            .interactive = true,
        };
        write_string(mv_string("Pico Relic Compiler\n  version: "), cout);
        write_string(mv_string(version), cout);
        write_string(mv_string("\n"), cout);
        while (repl_iter(cin, get_formatted_stdout(), stdalloc, &exalloc, module, opts));
        break;
    }
    case CScript: {
        IStream* fin = open_file_istream(command.script.filename, stdalloc);
        if (fin) {
            run_script_from_istream(fin, get_formatted_stdout(), module, stdalloc);
            delete_istream(fin, stdalloc);
        } else {
            write_string(mv_string("Failed to open file: "), cout);
            write_string(command.script.filename, cout);
            write_string(mv_string("\n"), cout);
        }
        break;
    }
    case CEval: {
        IterOpts opts = (IterOpts) {
            .debug_print = false,
            .interactive = false,
        };

        IStream* sin = mv_string_istream(command.eval.expr, stdalloc);
        while (repl_iter(sin, get_formatted_stdout(), stdalloc, &exalloc, module, opts));
        delete_istream(sin, stdalloc);
        break;
    }
    case CHelp:
        write_help_string(get_formatted_stdout());
        break;
    case CVersion:
        write_string(mv_string("Pico Relic compiler - Version 0.0.3\n"), cout);
        break;
    case CInvalid:
        write_string(command.error_message, cout);
        write_string(mv_string("\n"), cout);
        break;
    default:
        write_string(mv_string("Invalid Command Produced by parse_command!"), cout);
        write_string(mv_string("\n"), cout);
        break;
    }

    // Cleanup
    delete_package(base);
    delete_assembler(ass);
    release_executable_allocator(exalloc);

    clear_symbols();
    thread_clear_dynamic_vars();
    clear_dynamic_vars();
    teardown_window_system();

    if (is_hedron_supported()) {
        teardown_hedron();
    }

    return 0;
}
