#include "platform/memory/std_allocator.h"
#include "platform/memory/arena.h"

#include "atlas/app/command_line_opts.h"

static AtlasCommand internal_parse_command(StringArray args, ArenaAllocator* a);

AtlasCommand atlas_parse_command(StringArray args) {
    ArenaAllocator* arena = make_arena_allocator(1024, get_std_allocator());
    AtlasCommand cmd = internal_parse_command(args, arena);
    delete_arena_allocator(arena);
    return cmd;
}

static AtlasCommand internal_parse_command(StringArray args, ArenaAllocator* a) {
    if (args.len == 0) {
        return (AtlasCommand) {
            .type = CInvalid,
            .error_message = mv_string("Atlas must take at least 1 argument - try 'atlas help' for more info."),
        };
    }

    String subcommand = args.data[0];
    if (string_cmp(subcommand, mv_string("init")) == 0) {
        if (args.len != 2) {
            return (AtlasCommand) {
                .type = CInvalid,
                .error_message = mv_string("'atlas init' must take exactly 1 argument."),
            };
        } else {
            return (AtlasCommand) {
                .type = CInit,
                .init.name = args.data[1],
            };
        }
    } else if (string_cmp(subcommand, mv_string("run")) == 0) {
        if (args.len != 2) {
            return (AtlasCommand) {
                .type = CInvalid,
                .error_message = mv_string("'atlas run' must take exactly 1 argument."),
            };
        } else {
            return (AtlasCommand) {
                .type = CRun,
                .run.target = args.data[1],
            };
        }
    } else if (string_cmp(subcommand, mv_string("help")) == 0) {
        if (args.len != 1) {
            return (AtlasCommand) {
                .type = CInvalid,
                .error_message = mv_string("Help subcommand expects no arguments. Correct usage: 'atlas help'"),
            };
        }

        return (AtlasCommand) {
            .type = CHelp,
            .help.help_all = true,
        };
    } else {
        return (AtlasCommand) {
            .type = CInvalid,
            .error_message = mv_string("Invalid Atlas command."),
        };
    }
}
