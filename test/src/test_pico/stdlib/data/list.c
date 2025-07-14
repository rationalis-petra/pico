#include "platform/memory/arena.h"

#include "pico/stdlib/extra.h"

#include "test_pico/stdlib/components.h"
#include "test_pico/helper.h"

void run_pico_stdlib_data_list_tests(TestLog *log, Module* module, Allocator *a) {
    Allocator arena = mk_arena_allocator(4096, a);

    //run_toplevel("(open list)", module, log, a);
    run_toplevel("(def list-1 (list.mk-list {I64} 5 10))", module, log, a);
    if (test_start(log, mv_string("list-len"))) {
        int64_t expected = 5;
        test_toplevel_eq("list-1.len ", &expected, module, log, a);
    }

    if (test_start(log, mv_string("list-capacity"))) {
        int64_t expected = 10;
        test_toplevel_eq("list-1.capacity ", &expected, module, log, a);
    }

    if (test_start(log, mv_string("elt-matches-eset"))) {
        int64_t expected = -123986;
        run_toplevel("(list.eset 0 -123986 list-1)", module, log, a) ;
        test_toplevel_eq("(list.elt 0 list-1) ", &expected, module, log, a);
    }

    /* if (test_start(log, mv_string("list-literal-macro"))) { */
    /*     int64_t expected = -2; */
    /*     test_toplevel_eq("(list.elt 1 (list.list 1 -2 3 -4))", &expected, module, log, a); */
    /* } */

    /* if (test_start(log, mv_string("complex-elt-matches-eset"))) { */
    /*     typedef struct { */
    /*         int32_t a; */
    /*         int32_t b; */
    /*         int32_t c; */
    /*         int32_t d; */
    /*     } Point4; */
    /*     Point4 expected = (Point4) {.a = 0, .b = -1, .c = 2, .d = -3}; */
    /*     run_toplevel("(def Point4 Struct [.a I32] [.b I32] [.b I32] [.c I32])", module, log, a) ; */
    /*     run_toplevel("(def Point4 Struct [.a I32] [.b I32] [.b I32] [.c I32])", module, log, a) ; */
    /*     run_toplevel("(list.eset 0 () list-1)", module, log, a) ; */
    /*     test_toplevel_eq("(list.elt 0 list-1) ", &expected, module, log, a); */
    /* } */

    if (test_start(log, mv_string("each-print"))) {
        Allocator current_old = get_std_current_allocator();
        set_std_current_allocator(arena);
        char* expected = "01234";
        run_toplevel("(loop [for i from 0 below 5] (list.eset i (narrow i I64) list-1))", module, log, a);
        test_toplevel_stdout("(list.each (proc [x] print (i64.to-string x)) list-1)", expected, module, log, a);
        set_std_current_allocator(current_old);
    }

    if (test_start(log, mv_string("map-add-1"))) {
        run_toplevel("(def list-2 list.map (proc [x] i64.+ 1 x) list-1)", module, log, a);

        Allocator current_old = get_std_current_allocator();
        char* expected = "12345";
        set_std_current_allocator(arena);
        test_toplevel_stdout("(list.each (proc [x] print (i64.to-string x)) list-2)", expected, module, log, a);
        set_std_current_allocator(current_old);
    }

    // Free the data associated with the lists generated durin the test
    run_toplevel("(free list-1.data)", module, log, a);
    run_toplevel("(free list-2.data)", module, log, a);

    release_arena_allocator(arena);
}
