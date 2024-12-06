#include "test/command_line_opts.h"

TestOpts parse_test_opts(StringArray args, size_t len) {
}

TestCommand parse_test_command(StringArray args) {
    // Step 1: the default command (assuming no arguments) is repl
    // 
    if (args.len == 0) {
        return (TestCommand) {
            .type = CAll,
            .opts.report_file  = mv_string(""),
            .opts.report_type  = Text,
            .opts.report_level = 3,
            .opts.print_level  = 0,
        };
    }

    String subcommand = args.data[0];
    if (string_cmp(subcommand, mv_string("all")) == 0) {
        return (TestCommand) {
            .type = CAll,
            .opts = parse_test_opts(args, 1),
        };
    } else if (string_cmp(subcommand, mv_string("except")) == 0) {
        return (TestCommand) {
            .type = CExcept,
            .opts = parse_test_opts(args, 1),
        };
    } else if (string_cmp(subcommand, mv_string("only")) == 0) {
        return (TestCommand) {
            .type = COnly,
            .opts = parse_test_opts(args, 1),
        };
    } else {
        return (TestCommand) {
            .type = CInvalid,
            .error_message = mv_string("Unrecognized subcommand name!"),
        };
    }
}
