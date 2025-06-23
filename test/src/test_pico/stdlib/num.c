#include "test_pico/stdlib/components.h"
#include "test_pico/helper.h"

void run_pico_stdlib_num_tests(TestLog *log, Module* module, Allocator *a) {
    // Unsigned Int 64
    if (test_start(log, mv_string("unsigned-add"))) {
        uint64_t expected = 4294967294;
        test_toplevel_eq("(u64.+ 2147483647 2147483647)", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("signed-sub"))) {
        int64_t expected = -2000000000;
        test_toplevel_eq("(i64.- 147483647 2147483647)", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("signed-mul"))) {
        int64_t expected = 2577581361;
        test_toplevel_eq("(i64.* 9231 279231)", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("signed-div"))) {
        int64_t expected = 2231;
        test_toplevel_eq("(i64./ 20594361 9231)", &expected, module, log, a) ;
    }

    // Unsigned Int 32
    if (test_start(log, mv_string("unsigned-add"))) {
        uint64_t expected = 188628;
        test_toplevel_eq("(u32.+ 65535 123093)", &expected, module, log, a) ;
    }

    // Unsigned Int 16
    if (test_start(log, mv_string("unsigned-add"))) {
        uint64_t expected = 6493;
        test_toplevel_eq("(u16.+ 1026 5467)", &expected, module, log, a) ;
    }

    // Unsigned Int 8
    if (test_start(log, mv_string("unsigned-add"))) {
        uint64_t expected = 129;
        test_toplevel_eq("(u8.+ 100 29)", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("signed-add"))) {
        int64_t expected = 0;
        test_toplevel_eq("(i64.+ 2147483647 -2147483647)", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("unsigned-mul"))) {
        int64_t expected = 24;
        test_toplevel_eq("(i64.* 4 6)", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("and-ff"))) {
        uint8_t expected = 0;
        test_toplevel_eq("(bool.and :false :true)", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("and-ft"))) {
        uint8_t expected = 0;
        test_toplevel_eq("(bool.and :false :true)", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("and-tt"))) {
        uint8_t expected = 1;
        test_toplevel_eq("(bool.and :true :true)", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("or-ff"))) {
        uint8_t expected = 0;
        test_toplevel_eq("(bool.or :false :false)", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("or-ft"))) {
        uint8_t expected = 1;
        test_toplevel_eq("(bool.or :false :true)", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("or-tt"))) {
        uint8_t expected = 1;
        test_toplevel_eq("(bool.or :true :true)", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("not-t"))) {
        bool expected = false;
        test_toplevel_eq("(bool.not :true)", &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("not-f"))) {
        bool expected = true;
        test_toplevel_eq("(bool.not :false)", &expected, module, log, a) ;
    }
}
