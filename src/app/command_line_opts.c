#include "platform/memory/std_allocator.h"
#include "platform/memory/arena.h"

#include "app/command_line_opts.h"

static Command internal_parse_command(StringArray args, Allocator* a);

Command parse_command(StringArray args) {
    Allocator arena = mk_arena_allocator(4096, get_std_allocator());
    Command cmd = internal_parse_command(args, &arena);
    release_arena_allocator(arena);
    return cmd;
}

static bool try_parse_backend(String arg, CodegenBackend *dest, Allocator* a) {
    if (begins_with(arg, mv_string("--backend="))) {
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

static Command internal_parse_command(StringArray args, Allocator* a) {
    // Step 1: the default command (assuming no arguments) is repl
    if (args.len == 0) {
        return (Command) {
            .type = CRepl,
            .repl.debug_print = false,
            .repl.backend = CodegenDirect,
        };
    }

    String subcommand = args.data[0];
    if (string_cmp(subcommand, mv_string("repl")) == 0) {
        bool debug_print = false;
        CodegenBackend backend = CodegenDirect;
        for (size_t i = 1; i < args.len; i++) {
            if (string_cmp(args.data[i], mv_string("-d")) == 0) {
                debug_print = true;
            }

            if (try_parse_backend(args.data[i], &backend, a)) {
                return (Command) {
                    .type = CInvalid,
                    .error_message = mv_string("Invalid backend"),
                };
            }
        } 


        return (Command) {
            .type = CRepl,
            .repl.debug_print = debug_print,
            .repl.backend = backend,
        };
    } else if (string_cmp(subcommand, mv_string("script")) == 0) {
        if (args.len < 2) {
            return (Command) {
                .type = CInvalid,
                .error_message = mv_string("script subcommand expects at least one argument!"),
            };
        }

        CodegenBackend backend = CodegenDirect;
        for (size_t i = 2; i < args.len; i++) {
            if (try_parse_backend(args.data[i], &backend, a)) {
                return (Command) {
                    .type = CInvalid,
                    .error_message = mv_string("Invalid backend"),
                };
            }
        } 

        return (Command) {
            .type = CScript,
            .script.filename = args.data[1],
            .script.backend = backend,
        };
    } else if (string_cmp(subcommand, mv_string("eval")) == 0) {
        if (args.len < 2) {
            return (Command) {
                .type = CInvalid,
                .error_message = mv_string("eval subcommand expects at least one argument!"),
            };
        }

        CodegenBackend backend = CodegenDirect;
        for (size_t i = 2; i < args.len; i++) {
            if (try_parse_backend(args.data[i], &backend, a)) {
                return (Command) {
                    .type = CInvalid,
                    .error_message = mv_string("Invalid backend"),
                };
            }
        } 

        return (Command) {
            .type = CEval,
            .eval.expr = args.data[1],
            .eval.backend = backend,
        };
    } else if (string_cmp(subcommand, mv_string("help")) == 0) {
        if (args.len != 1) {
            return (Command) {
                .type = CInvalid,
                .error_message = mv_string("Help subcommand expects no arguments. Correct usage: 'pico help'"),
            };
        }

        return (Command) {
            .type = CHelp,
            .help.help_all = true,
        };
    } else if (string_cmp(subcommand, mv_string("version")) == 0) {
        if (args.len != 1) {
            return (Command) {
                .type = CInvalid,
                .error_message = mv_string("Version subcommand expects no arguments. Correct usage: 'pico version'"),
            };
        }

        return (Command) {
            .type = CVersion,
        };
    } else {
        return (Command) {
            .type = CInvalid,
            .error_message = mv_string("Unrecognized subcommand name!"),
        };
    }
}
