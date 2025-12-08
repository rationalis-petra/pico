#include "platform/memory/std_allocator.h"
#include "platform/memory/arena.h"
#include "pico/codegen/codegen.h"

#include "test/command_line_opts.h"

static bool try_parse_backend(String arg, CodegenBackend *dest, Allocator* a) {
    if (begins_with(arg, mv_string("--backend=")) == 0) {
        String backend_name = substring(10, arg.memsize, arg, a);

        if (string_cmp(backend_name, mv_string("direct")) == 0) {
            *dest = CodegenDirect;
        } else if (string_cmp(backend_name, mv_string("pvm")) == 0) {
            *dest = CodegenPVM;
        } else {
            return true;
        }
    }
    return false;
}

TestOpts parse_test_opts(StringArray args, size_t start, Allocator* a) {
    TestOpts opts = (TestOpts) {
        .report_file  = mv_string(""),
        .report_type  = Text,
        .report_level = 0,
        .print_level  = 2, // default = print failures + errors
        .backend = CodegenDirect,
    };

    for (size_t i = start; i < args.len; i++) {
        // verbose
        if (string_cmp(mv_string("-v"), args.data[i]) == 0) {
            opts.print_level = 3;
        }

        if (try_parse_backend(args.data[i], &opts.backend, a)) {
            // TODO: fail
            /* return (TestOpts) { */
            /*     .type = CInvalid, */
            /*     .error_message = mv_string("Invalid backend"), */
            /* }; */
        }
    }
    return opts;
}

TestCommand parse_test_command(StringArray args) {
    
    // Step 1: the default command (assuming no arguments) is repl
    if (args.len == 0) {
        return (TestCommand) {
            .type = CAll,
            .opts.report_file  = mv_string(""),
            .opts.report_type  = Text,
            .opts.report_level = 0,
            .opts.print_level  = 2, // default = print failures + errors
        };
    }

    String subcommand = args.data[0];
    TestCommand out;
    ArenaAllocator* arena = make_arena_allocator(4096, get_std_allocator());
    Allocator gpa = aa_to_gpa(arena);
    if (string_cmp(subcommand, mv_string("all")) == 0) {
        out = (TestCommand) {
            .type = CAll,
            .opts = parse_test_opts(args, 1, &gpa),
        };
    } else if (string_cmp(subcommand, mv_string("except")) == 0) {
        out = (TestCommand) {
            .type = CExcept,
            .opts = parse_test_opts(args, 1, &gpa),
        };
    } else if (string_cmp(subcommand, mv_string("only")) == 0) {
        out = (TestCommand) {
            .type = COnly,
            .opts = parse_test_opts(args, 1, &gpa),
        };
    } else {
        out = (TestCommand) {
            .type = CInvalid,
            .error_message = mv_string("Unrecognized subcommand name!"),
        };
    }
    delete_arena_allocator(arena);
    return out;
}
