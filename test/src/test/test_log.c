#include <stdlib.h>
#include <signal.h>

#include "platform/signals.h"
#include "platform/memory/std_allocator.h"
#include "data/stringify.h"

#include "test/test_log.h"

const Colour err_colour = (Colour){.r = 220, .g = 180, .b = 50};
const Colour fail_colour = (Colour){.r = 200, .g = 50, .b = 50};
const Colour info_colour = (Colour){.r = 100, .g = 210, .b = 230};
const Colour pass_colour = (Colour){.r = 30, .g = 200, .b = 30};

static TestLog* current_log = NULL;

void on_segfault() {
    // Panic will signal 'abort' by default. Usually, this would then cause the 
    //   test to call the function associated with the 'abort' handler (below),
    //   but in this case we don't want to do that!
    signal(SIGABRT, SIG_DFL);  
    panic(string_cat(mv_string("Segfault in test: "),
          current_log->current_test,
          get_std_allocator()));
}

void on_abort() {
    FormattedOStream* fout = get_formatted_stdout();
    start_coloured_text(colour(200, 0, 0), fout);
    write_fstring(mv_string("Program Aborted While Test Running! \n\n"), fout);
    end_coloured_text(fout);
    write_fstring(mv_string("Failing the test and attempting to print as much diagnostic logging info as possible\n\n"), fout);

    test_fail(current_log);
    signal(SIGABRT, SIG_DFL);  
    abort();
}

TestLog* mk_test_log(FormattedOStream* stream, Verbosity v, Allocator* a) {
  TestLog* out = mem_alloc(sizeof(TestLog), a);
  *out = (TestLog) {
      .stream = stream,
      .verbosity = v,
      .start_time = query_performance_timer(),

      .current_suites = mk_ptr_array(8, a),

      .test_count = 0,
      .passed_tests = 0,
      .failed_tests = 0,
      .gpa = a,
  };
  current_log = out;
  signal(SIGSEGV, on_segfault);
  signal(SIGABRT, on_abort);
  return out;
}

void finish_setup(TestLog *log) {
    log->setup_time = query_performance_timer();
}

bool suite_start(TestLog *log, String name) {
    String* str = mem_alloc(sizeof(String), log->gpa);
    *str = name;
    push_ptr(str, &log->current_suites);
    return true;
}

void suite_end(TestLog *log) {
    String* str = pop_ptr(&log->current_suites);
    mem_free(str, log->gpa);
}

bool test_start(TestLog* log, String name) {
    log->in_test = true;
    log->current_test = name;
    log->test_count++;
    return true;
}

void test_pass(TestLog* log) {
    log->passed_tests++;

    if (log->slogger) {
        delete_logger(log->slogger);
        log->slogger = NULL;
    }

    if (!log->in_test) {
        panic(mv_string("Ending test suite - passing a test but one has not been started!"));
    }

    log->in_test = false;
    if (log->verbosity.show_passes) {
        start_coloured_text(pass_colour, log->stream);
        write_fstring(mv_string("[Pass] "), log->stream);
        for (size_t i = 0; i < log->current_suites.len; i++) {
            write_fstring(*(String*)log->current_suites.data[i], log->stream);
            write_fstring(mv_string("."), log->stream);
        }
        write_fstring(mv_string("\n"), log->stream);
        end_coloured_text(log->stream);
    }
}

void test_skip(TestLog* log) {
    log->skipped_tests++;

    if (!log->in_test) {
        panic(mv_string("Ending test suite - passing a test but one has not been started!"));
    }

    if (log->slogger) {
        delete_logger(log->slogger);
        log->slogger = NULL;
    }

    log->in_test = false;
    if (log->verbosity.show_passes) {
        start_coloured_text(pass_colour, log->stream);
        write_fstring(mv_string("[Skip] "), log->stream);
        for (size_t i = 0; i < log->current_suites.len; i++) {
            write_fstring(*(String*)log->current_suites.data[i], log->stream);
            write_fstring(mv_string("."), log->stream);
        }
        write_fstring(mv_string("\n"), log->stream);
        end_coloured_text(log->stream);
    }
}

void test_fail(TestLog* log) {
    log->failed_tests++;

    if (!log->in_test) {
        panic(mv_string("Ending test suite - failing a test but one has not been started!"));
    }

    if (log->slogger && log->verbosity.show_fails) {
        log_to_formatted_ostream(log->slogger, 120, log->stream);
    }

    if (log->slogger) {
        delete_logger(log->slogger);
        log->slogger = NULL;
    }

    if (log->verbosity.show_fails) {
        start_coloured_text(fail_colour, log->stream);
        write_fstring(mv_string("[Fail] "), log->stream);
        for (size_t i = 0; i < log->current_suites.len; i++) {
            write_fstring(*(String*)log->current_suites.data[i], log->stream);
            write_fstring(mv_string("."), log->stream);
        }
        write_fstring(log->current_test, log->stream);
        write_fstring(mv_string("\n"), log->stream);
        end_coloured_text(log->stream);
    }
}

void delete_test_log(TestLog* log, Allocator* a) {
    sdelete_ptr_array(log->current_suites);
    mem_free(log, a);
}

Logger* get_structured_logger(TestLog *log) {
    if (!log->slogger && log->in_test) {
        log->slogger = make_logger(log->gpa);
    }
    return log->slogger;
}

void test_log_error(TestLog* log, String message) {
    if (log->verbosity.show_errors) {
        start_coloured_text(err_colour, log->stream);
        write_fstring(mv_string("[Error] "), log->stream);
        write_fstring(message, log->stream);
        write_fstring(mv_string("\n"), log->stream);
        end_coloured_text(log->stream);
    }
}

void test_log_info(TestLog* log, String message) {
    if (log->verbosity.show_info) {
        start_coloured_text(info_colour, log->stream);
        write_fstring(mv_string("[Info] "), log->stream);
        write_fstring(message, log->stream);
        write_fstring(mv_string("\n"), log->stream);
        end_coloured_text(log->stream);
    }
}

FormattedOStream *get_fstream(TestLog *log) {
    return log->stream;
}

int summarize_tests(TestLog *log, Allocator* a) {
    int err_code = 0;
    PerfTime end_time = query_performance_timer();
    double cpu_time_used = time_to_double(time_diff(log->start_time, end_time), Seconds);
    double setup_time_used = time_to_double(time_diff(log->start_time, log->setup_time), Seconds);
    double test_time_used = time_to_double(time_diff(log->setup_time, end_time), Seconds);

    write_fstring(mv_string("──────────────────────────────────────────────────────────\n\n"), log->stream);
    if (log->passed_tests + log->failed_tests + log->skipped_tests != log->test_count) {
        err_code = 1;
        start_coloured_text(fail_colour, log->stream);
        write_fstring(mv_string("Failure of test suite: total tests != passed + failed"), log->stream);
        end_coloured_text(log->stream);
    }

    if (log->failed_tests == 0 && log->skipped_tests == 0 && err_code == 0) {
        start_coloured_text(pass_colour, log->stream);
        write_fstring(mv_string("                     All Tests Passed\n"), log->stream);
        end_coloured_text(log->stream);
    } else if (log->skipped_tests != 0 && err_code == 0) {
        err_code = 1;
        start_coloured_text(err_colour, log->stream);
        write_fstring(mv_string("                    Some Tests Skipped\n"), log->stream);
        end_coloured_text(log->stream);
    } else if (log->failed_tests != 0) {
        err_code = 1;
        start_coloured_text(fail_colour, log->stream);
        write_fstring(mv_string("                     Some Tests Failed\n"), log->stream);
        end_coloured_text(log->stream);
    } else {
        write_fstring(mv_string("                     Unexpected Error\n"), log->stream);
    }
    String str;

    write_fstring(mv_string("\n                      Passed : "), log->stream);
    str = string_u64(log->passed_tests, a);
    write_fstring(str, log->stream);
    delete_string(str, a);
    write_fstring(mv_string("\n                      Failed : "), log->stream);
    str = string_u64(log->failed_tests, a);
    write_fstring(str, log->stream);
    delete_string(str, a);
    if (log->skipped_tests != 0) {
        write_fstring(mv_string("\n                     Skipped : "), log->stream);
        str = string_u64(log->skipped_tests, a);
        write_fstring(str, log->stream);
        delete_string(str, a);
    }
    write_fstring(mv_string("\n                       Total : "), log->stream);
    str = string_u64(log->test_count, a);
    write_fstring(str, log->stream);
    delete_string(str, a);
    write_fstring(mv_string("\n"), log->stream);
    write_fstring(mv_string("\n                  Time Taken : "), log->stream);
    String time = string_double(cpu_time_used, a) ;
    write_fstring(time, log->stream);
    write_fstring(mv_string("s"), log->stream);
    delete_string(time, a);

    write_fstring(mv_string("\n                  Setup Time : "), log->stream);
    time = string_double(setup_time_used, a) ;
    write_fstring(time, log->stream);
    write_fstring(mv_string("s"), log->stream);
    delete_string(time, a);

    write_fstring(mv_string("\n                   Test Time : "), log->stream);
    time = string_double(test_time_used, a) ;
    write_fstring(time, log->stream);
    write_fstring(mv_string("s"), log->stream);
    delete_string(time, a);

    write_fstring(mv_string("\n\n──────────────────────────────────────────────────────────\n"), log->stream);

    if (log->failed_tests != 0) err_code = 2;
    return err_code; 
}
