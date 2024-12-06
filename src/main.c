#include "platform/error.h"
#include "platform/memory/std_allocator.h"
#include "platform/memory/executable.h"
#include "platform/memory/arena.h"
#include "platform/jump.h"

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
#include "pico/values/stdlib.h"
#include "pico/values/types.h"

#include "app/command_line_opts.h"

typedef struct {
    bool debug_print;
    bool interactive;
} IterOpts;

bool repl_iter(IStream* cin, OStream* cout, Allocator* a, Assembler* ass, Module* module, IterOpts opts) {
    // TODO (TAGS: UB BUG INVESTIGATE): Possibly need to add volatile qualifier to arena?
    // however, we are not expecting the arena to be mutated, so...
    // Create an arena allocator to use in this iteration.
    Allocator arena = mk_arena_allocator(4096, a);

    clear_assembler(ass);
    Environment* env = env_from_module(module, &arena);

    jump_buf exit_point;
    if (set_jump(exit_point)) goto on_exit;
    set_exit_callback(&exit_point);

    ErrorPoint point;
    if (catch_error(point)) goto on_error;

    if (opts.interactive) {
        String* name = get_name(module);
        if (name) write_string(*name, cout);
        write_string(mv_string(" > "), cout);
    }

    ParseResult res = parse_rawtree(cin, &arena);
    if (res.type == ParseNone) {
        write_string(mv_string("\n"), cout);
        goto on_exit;
    }

    if (res.type == ParseFail) {
        write_string(mv_string("Parse Failed :(\n"), cout);
        release_arena_allocator(arena);
        return false;
    }
    if (res.type != ParseSuccess) {
        write_string(mv_string("Parse Returned Invalid Result!\n"), cout);
        release_arena_allocator(arena);
        return false;
    }

    Document* doc;
    if (opts.debug_print) {
        doc = pretty_rawtree(res.data.result, &arena);
        write_string(mv_string("Pretty Printing Raw Syntax\n"), cout);
        write_doc(doc, cout);
        write_string(mv_string("\n"), cout);
    }

    // -------------------------------------------------------------------------
    // Resolution
    // -------------------------------------------------------------------------

    TopLevel abs = abstract(res.data.result, env, &arena, &point);

    if (opts.debug_print) {
        write_string(mv_string("Pretty Printing Resovled Syntax:\n"), cout);
        doc = pretty_toplevel(&abs, &arena);
        write_doc(doc, cout);
        write_string(mv_string("\n"), cout);
    }

    // -------------------------------------------------------------------------
    // Type Checking
    // -------------------------------------------------------------------------

    // Note: typechecking annotates the syntax tree with types, but doesn't have
    // an output.
    type_check(&abs, env, &arena, &point);

    if (opts.debug_print) {
        write_string(mv_string("Pretty Printing Inferred Type\n"), cout);
        doc = pretty_type(toplevel_type(abs), &arena);
        write_doc(doc, cout);
        write_string(mv_string("\n"), cout);
    }

    // -------------------------------------------------------------------------
    // Code Generation
    // -------------------------------------------------------------------------

    GenResult gen_res = generate_toplevel(abs, env, ass, &arena, &point);

    if (opts.debug_print) {
        write_string(mv_string("Pretty Printing Binary\n"), cout);
        doc = pretty_assembler(ass, &arena);
        write_doc(doc, cout);
        write_string(mv_string("\n"), cout);
    }

    // -------------------------------------------------------------------------
    // Evaluation
    // -------------------------------------------------------------------------

    EvalResult call_res = pico_run_toplevel(abs, ass, &(gen_res.backlinks), module, &arena, &point);
    if (opts.debug_print) {
        write_string(mv_string("Pretty Printing Evaluation Result\n"), cout);
    }

    if (opts.debug_print || opts.interactive) {
        doc = pretty_res(call_res, &arena);

        write_doc(doc, cout);
        write_string(mv_string("\n"), cout);
    }

    release_arena_allocator(arena);
    return true;

 on_error:
    write_string(point.error_message, cout);
    write_string(mv_string("\n"), cout);
    release_arena_allocator(arena);
    return true;

 on_exit:
    release_arena_allocator(arena);
    return false;
}

int main(int argc, char** argv) {
    // Setup

    Allocator* stdalloc = get_std_allocator();
    IStream* cin = get_stdin_stream();
    OStream* cout = get_stdout_stream();
    Allocator exalloc = mk_executable_allocator(stdalloc);

    asm_init();
    init_symbols(stdalloc);
    init_dynamic_vars(stdalloc);
    thread_init_dynamic_vars();

    Assembler* ass = mk_assembler(&exalloc);
    Assembler* ass_base = mk_assembler(&exalloc);
    Package* base = base_package(ass_base, stdalloc, stdalloc);
    delete_assembler(ass_base);

    Module* module = get_module(string_to_symbol(mv_string("user")), base);

    set_current_module(module);
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
        while (repl_iter(cin, cout, stdalloc, ass, module, opts));
        break;
    }
    case CScript: {
        IterOpts opts = (IterOpts) {
            .debug_print = false,
            .interactive = false,
        };

        IStream* fin = open_file_istream(command.script.filename, stdalloc);
        while (repl_iter(fin, cout, stdalloc, ass, module, opts));
        delete_istream(fin, stdalloc);

        break;
    }
    case CEval: {
        IterOpts opts = (IterOpts) {
            .debug_print = false,
            .interactive = false,
        };

        IStream* sin = mv_string_istream(command.eval.expr, stdalloc);
        while (repl_iter(sin, cout, stdalloc, ass, module, opts));
        delete_istream(sin, stdalloc);
        break;
    }
    case CInvalid:
        write_string(command.error_message, cout);
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

    return 0;
}
