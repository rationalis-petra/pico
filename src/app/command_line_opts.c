#include "app/command_line_opts.h"

Command parse_command(StringArray args) {
    // Step 1: the default command (assuming no arguments) is repl
    // 
    if (args.len == 0) {
        return (Command) {
            .type = CRepl,
            .repl.debug_print = false,
        };
    }

    String subcommand = args.data[0];
    if (string_cmp(subcommand, mv_string("repl")) == 0) {
        bool debug_print = false;
        for (size_t i = 1; i < args.len; i++) {
            if (string_cmp(args.data[i], mv_string("-d")) == 0) {
                debug_print = true;
            }
        } 
                

        return (Command) {
            .type = CRepl,
            .repl.debug_print = debug_print,
        };
    } else if (string_cmp(subcommand, mv_string("script")) == 0) {
        if (args.len != 2) {
            return (Command) {
                .type = CInvalid,
                .error_message = mv_string("script subcommand expects exactly one argument!"),
            };
        }

        return (Command) {
            .type = CScript,
            .script.filename = args.data[1],
        };
    } else if (string_cmp(subcommand, mv_string("eval")) == 0) {
        if (args.len != 2) {
            return (Command) {
                .type = CInvalid,
                .error_message = mv_string("eval subcommand expects exactly one argument!"),
            };
        }

        return (Command) {
            .type = CEval,
            .eval.expr = args.data[1],
        };
    } else {
        return (Command) {
            .type = CInvalid,
            .error_message = mv_string("Unrecognized subcommand name!"),
        };
    }
}
