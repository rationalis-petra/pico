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


#include "platform/memory/std_allocator.h"

#include "data/string.h"
#include "data/stream.h"

#include "pico/stdlib/extra.h"

#include "test/command_line_opts.h"
#include "test_pico/pico.h"
#include "test_assembler/test_assembler.h"

void all_suites(TestLog *log, Allocator *a);

int main(int argc, char** argv) {
    // Setup

    Allocator* stdalloc = get_std_allocator();
    IStream* cin = get_stdin_stream();
    OStream* cout = get_stdout_stream();

    // Init terminal first, as other initializers may panic (and therefore write
    // to stdout)
    init_terminal(stdalloc);

    // Initialization order here is not important
    init_ctypes();
    init_asm();
    init_symbols(stdalloc);
    init_dynamic_vars(stdalloc);
    thread_init_dynamic_vars();

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
    FormattedOStream* cos = mk_formatted_ostream(cout, stdalloc);
    Verbosity v = (Verbosity) {
        .show_passes = false,
        .show_fails = false,
        .show_info = false,
        .show_errors = false,
    };
    switch (command.opts.print_level) {
    default:
    case 4:
        v.show_info = true;
        // fall through
    case 3:
        v.show_passes = true;
        // fall through
    case 2:
        v.show_errors = true;
        // fall through
    case 1:
        v.show_fails = true;
        // fall through
    case 0:
        break;
    }

    TestLog* log = mk_test_log(cos, v, stdalloc);

    int out = 1;
    switch (command.type) {
    case CAll: {
        all_suites(log, stdalloc);
        out = summarize_tests(log, stdalloc);
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
    delete_formatted_ostream(cos, stdalloc);

    // Cleanup
    clear_symbols();
    thread_clear_dynamic_vars();
    clear_dynamic_vars();

    return out;
}

void all_suites(TestLog *log, Allocator *a) {
    if (suite_start(log, mv_string("pico"))) {
        run_pico_tests(log, a);
        suite_end(log);
    }

    if (suite_start(log, mv_string("assembler"))) {
        run_assembler_tests(log, a);
        suite_end(log);
    }
}
