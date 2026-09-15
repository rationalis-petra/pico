#include "pico/stdlib/platform/submodules.h"

#include "test_pico/helper.h"

void run_pico_stdlib_data_list_tests(TestLog *log, Module* module, Environment* env, Target target, RegionAllocator* region) {
    TestContext context = (TestContext) {
        .env = env,
        .region = region,
        .log = log,
        .target = target,
    };
    Allocator ra = ra_to_gpa(region);
    PiAllocator pregion = convert_to_pallocator(&ra);

    if (suite_setup(log)) {
        RUN("(def list-1 (list.init {I64} 5 10))");
    }
    if (test_start(log, mv_string("list-len"))) {
        int64_t expected = 5;
        TEST_EQ("list-1.len");
    }

    if (test_start(log, mv_string("list-capacity"))) {
        int64_t expected = 10;
        TEST_EQ("list-1.data.len");
    }

    if (test_start(log, mv_string("elt-matches-eset"))) {
        int64_t expected = -123986;
        RUN("(list.eset 0 -123986 list-1)");
        TEST_EQ("(list.elt 0 list-1)");
    }

    if (test_start(log, mv_string("seq-elt-matches-eset"))) {
        I64Option expected = {.type = Some, .val = -123986};
        RUN("(list.eset 0 -123986 list-1)");
        TEST_EQ("(abs.sequence.elt 0 list-1)");
    }

    if (test_start(log, mv_string("seq-elt->len-returns-none"))) {
        I64Option expected = {.type = None, .val = 0};
        TEST_EQ("(abs.sequence.elt 128 list-1)");
    }

    if (test_start(log, mv_string("list-literal-macro"))) {
        int64_t expected = -2;
        TEST_EQ("(seq [let! mlist list.list 1 -2 3 -4]\n"
                "  [let! elt  list.elt 1 mlist]\n"
                "    (list.de-init mlist)\n"
                "    elt)");
    }

    if (test_start(log, mv_string("each-print"))) {
        PiAllocator current_old = get_std_current_allocator();
        set_std_current_allocator(pregion);
        char* expected = "01234";
        RUN("(loop [for i from 0 below 5] (list.eset i (narrow I64 i) list-1))");
        TEST_STDOUT("(list.each (proc [x] terminal.write-string (prim.i64.to-string x)) list-1)");
        set_std_current_allocator(current_old);
    }

    if (test_start(log, mv_string("map-add-1"))) {
        RUN("(def list-2 list.map (proc [x] + 1 x) list-1)");

        PiAllocator current_old = get_std_current_allocator();
        char* expected = "12345";
        set_std_current_allocator(pregion);
        TEST_STDOUT("(list.each (proc [x] terminal.write-string (prim.i64.to-string x)) list-2)");
        set_std_current_allocator(current_old);
    }

    if (test_start(log, mv_string("push"))) {
        RUN("(def list-3 pointer.new (list.init {I64} 0 3))");
        RUN("(list.push 12 list-3)");
        RUN("(list.push 13 list-3)");
        RUN("(list.push 14 list-3)");

        PiAllocator current_old = get_std_current_allocator();
        char* expected = "121314";
        set_std_current_allocator(pregion);
        TEST_STDOUT("(list.each (proc [x] terminal.write-string (prim.i64.to-string x)) (pointer.get list-3))");
        set_std_current_allocator(current_old);
    }

    // Free the data associated with the lists generated durin the test
    if (suite_teardown(log)) {
        RUN("(list.de-init list-1)");
        RUN("(list.de-init list-2)");
        RUN("(delete list-3)");
    }
}
