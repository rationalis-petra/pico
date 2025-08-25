#ifndef __TEST_COMMAND_LINE_OPTS
#define __TEST_COMMAND_LINE_OPTS

#include <stdbool.h>

#include "data/string.h"
#include "pico/codegen/codegen.h"
#include "pico/data/string_array.h"

/* Command line interface
 * test <subcommand> -- <options> where subcommand is
 *  • all
 *  • only path1 path2 ...
 *  • except path1 path2 ...
 * and options are
 * --report-file=<filename>
 * --report-type=<extname>
 * --report-level=[0-3]
 * --print-level=[0-3]
 */

typedef enum {
    CAll,
    COnly,
    CExcept,
    CInvalid,
} SubCommand_t;

typedef enum {
    Html,
    Text,
} ReportType;

typedef struct {
    String report_file;
    ReportType report_type;
    uint8_t report_level;
    uint8_t print_level;
    CodegenBackend backend;
} TestOpts;

typedef struct {
    SubCommand_t type;
    PtrArray test_strings;
    TestOpts opts;
    String error_message;
} TestCommand;

TestCommand parse_test_command(StringArray args);

#endif
