#include "atlas/atlas.h"

#include "atlas/app/command_line_opts.h"
#include "atlas/app/help_string.h"

static const char* version = "0.0.1";

void run_atlas(StringArray args, OStream* out) {
    AtlasCommand command = atlas_parse_command(args);

    switch (command.type) {
    case CInit:
        break;
    case CRun:
        break;
    case CHelp:
        write_atlas_help_string(get_formatted_stdout());
        break;
    case CVersion:
        write_string(mv_string("Atlas Relic Build System - Version "), out);
        write_string(mv_string(version), out);
        write_string(mv_string("\n"), out);
        break;
    case CInvalid:
        write_string(command.error_message, out);
        write_string(mv_string("\n"), out);
        break;
    default:
        write_string(mv_string("Error in atlas command line parser: invalid result returned"), out);
        break;
    }
}
