#include "platform/memory/arena.h"

#include "pico/stdlib/extra.h"

#include "test_pico/stdlib/components.h"
#include "test_pico/helper.h"

#define RUN(str) run_toplevel(str, module, env, log, a); refresh_env(env, a)
#define TEST_EQ(str) test_toplevel_eq(str, &expected, module, env, log, a);
#define TEST_STDOUT(str) test_toplevel_stdout(str, expected, module, env, log, a);

void run_pico_stdlib_data_list_tests(TestLog *log, Module* module, Environment* env, Allocator* a) {
    Allocator arena = mk_arena_allocator(4096, a);

    RUN("(def list-1 (list.mk-list {I64} 5 10))");
    if (test_start(log, mv_string("list-len"))) {
        int64_t expected = 5;
        TEST_EQ("list-1.len ");
    }

    if (test_start(log, mv_string("list-capacity"))) {
        int64_t expected = 10;
        TEST_EQ("list-1.capacity ");
    }

    if (test_start(log, mv_string("elt-matches-eset"))) {
        int64_t expected = -123986;
        RUN("(list.eset 0 -123986 list-1)");
        TEST_EQ("(list.elt 0 list-1) ");
    }

    if (test_start(log, mv_string("list-literal-macro"))) {
        int64_t expected = -2;
        TEST_EQ("(seq [let! mlist (list.list 1 -2 3 -4)\n"
                         "          elt (list.elt 1 mlist)]\n"
                         "    (free mlist.data)\n"
                         "    elt)");
    }

    if (test_start(log, mv_string("each-print"))) {
        Allocator current_old = get_std_current_allocator();
        set_std_current_allocator(arena);
        char* expected = "01234";
        RUN("(loop [for i from 0 below 5] (list.eset i (narrow i I64) list-1))");
        TEST_STDOUT("(list.each (proc [x] print (i64.to-string x)) list-1)");
        set_std_current_allocator(current_old);
    }

    if (test_start(log, mv_string("map-add-1"))) {
        RUN("(def list-2 list.map (proc [x] i64.+ 1 x) list-1)");

        Allocator current_old = get_std_current_allocator();
        char* expected = "12345";
        set_std_current_allocator(arena);
        TEST_STDOUT("(list.each (proc [x] print (i64.to-string x)) list-2)");
        set_std_current_allocator(current_old);
    }

    // Free the data associated with the lists generated durin the test
    RUN("(free list-1.data)");
    RUN("(free list-2.data)");

    release_arena_allocator(arena);
}
