#ifndef __APP_COMMAND_LINE_OPTS
#define __APP_COMMAND_LINE_OPTS

#include "data/string.h"
#include "pico/data/string_array.h"
#include "pico/codegen/codegen.h"
#include <stdbool.h>

typedef enum {
    CRepl,
    CScript,
    CEval,
    CHelp,
    CVersion,
    CInvalid,
} SubCommand_t;

typedef struct {
    bool debug_print;
    CodegenBackend backend;
} ReplOpts;

typedef struct {
    String filename;
    CodegenBackend backend;
} ScriptOpts;

typedef struct {
    String expr;
    CodegenBackend backend;
} EvalOpts;

typedef struct {
    bool help_all;
    SubCommand_t help_for;
} HelpOpts;

typedef struct {
    SubCommand_t type;
    union {
        ReplOpts repl;
        ScriptOpts script;
        EvalOpts eval;
        HelpOpts help;
        String error_message;
    };
} Command;

Command parse_command(StringArray args);

#endif
