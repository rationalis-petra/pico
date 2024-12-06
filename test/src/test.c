/* Qualities of a good unit test framework:
 * -----------------------------------------------------------------------------
 * • Can run specific sub-tests.
 * • Allows hooks for providing useful error messages, e.g.
 *   • Expected values/comparison operation
 *   • Description of the intent of a test
 * • Run as many suites as possible? - report failure
 *  • Indicate which suite (if any) is responsible for any failures (segfault etc.)
 * • Reporting:
 *   • Fine-grained as possible 
 *   • Control over what gets reported, how (console, file, html, etc.)
 *   • As much diagnostic info as possible.
 * • Run tests in parallel (if possible).
 * 
 */

/* Pico-specific
 * -----------------------------------------------------------------------------
 * • Utilities to wrap parsing/syntax creation, etc. 
 * • Allow 'wiping' of the user module, module creation etc.
 */


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

#include "test/command_line_opts.h"
#include "test_pico/pico.h"

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
    Package* base = base_package(ass_base, stdalloc);
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
    TestCommand command = parse_test_command(args);
    sdelete_string_array(args);

    // TODO: setup_tests
    TestLog* log = mk_test_log(cout, stdalloc);

    switch (command.type) {
    case CAll: {
        RunDescriptor run_all = (RunDescriptor) {.type = RunAll};
        run_pico_tests(run_all, log, stdalloc);
        write_string(mv_string("\n"), cout);
        break;
    }
    case COnly: {
        write_string(mv_string("Test Only!"), cout);
        write_string(mv_string("\n"), cout);
        break;
    }
    case CExcept: {
        write_string(mv_string("Test Except!"), cout);
        write_string(mv_string("\n"), cout);
        break;
    }
    case CInvalid:
        write_string(command.error_message, cout);
        write_string(mv_string("\n"), cout);
        break;
    default:
        write_string(mv_string("Invalid Command Produced by parse_command!"), cout);
        write_string(mv_string("\n"), cout);
        break;
    }

    delete_test_log(log, stdalloc);

    // Cleanup
    delete_package(base);
    delete_assembler(ass);
    release_executable_allocator(exalloc);

    clear_symbols();
    thread_clear_dynamic_vars();
    clear_dynamic_vars();

    return 0;
}
