#ifndef __ATLAS_APP_COMMAND_LINE_OPTS
#define __ATLAS_APP_COMMAND_LINE_OPTS

#include "data/string.h"
#include "pico/data/string_array.h"
#include "pico/codegen/codegen.h"
#include <stdbool.h>

typedef enum {
    CInit,
    CRun,
    CHelp,
    CVersion,
    CInvalid,
} SubCommand_t;

typedef struct {
    String name;
} InitOpts;

typedef struct {
    String target;
} RunOpts;

typedef struct {
    bool help_all;
    SubCommand_t help_for;
} HelpOpts;

typedef struct {
    SubCommand_t type;
    union {
        InitOpts init;
        RunOpts run;
        HelpOpts help;
        String error_message;
    };
} AtlasCommand;

AtlasCommand atlas_parse_command(StringArray args);

#endif
