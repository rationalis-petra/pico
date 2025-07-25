#include "test_pico/eval/components.h"
#include "test_pico/helper.h"

void run_pico_eval_literals_tests(TestLog *log, Module* module, Allocator *a) {
    if (test_start(log, mv_string("positive-int-literal"))) {
        int64_t expected = 10;
        test_toplevel_eq("10", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("negative-int-literal"))) {
        int64_t expected = -10;
        test_toplevel_eq("-10", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("positive-float-literal"))) {
        double expected = 10.5f;
        test_toplevel_eq("10.5", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("positive-small-float-literal"))) {
        double expected = 0.5f;
        test_toplevel_eq("0.5", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("negative-float-literal"))) {
        double expected = -10.5f;
        test_toplevel_eq("-10.5", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("negative-small-float-literal"))) {
        double expected = -0.5f;
        test_toplevel_eq("-0.5", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("complicated-float-literal"))) {
        double expected = 1342.734375;
        test_toplevel_eq("1342.734375", &expected, module, log, a) ;
    }
}
