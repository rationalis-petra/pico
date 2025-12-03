#include <string.h>
#include "platform/memory/static.h"

#include "pico/stdlib/extra.h"

#include "test_pico/eval/components.h"
#include "test_pico/helper.h"

#define RUN(str) run_toplevel(str, module, context); refresh_env(env, &ra)
#define TEST_EQ(str) test_toplevel_eq(str, &expected, module, context)
#define TEST_STDOUT(str) test_toplevel_stdout(str, expected, module, context)
#define TEST_MEM(str) test_toplevel_mem(str, &expected, start, sizeof(expected), module, context)

void run_pico_eval_proc_tests(TestLog *log, Module* module, Environment* env, Target target, RegionAllocator* region) {
    TestContext context = (TestContext) {
        .env = env,
        .region = region,
        .log = log,
        .target = target,
    };
    Allocator ra = ra_to_gpa(region);

    // Literals
    if (test_start(log, mv_string("const-return"))) {
        int64_t expected = -28;
        TEST_EQ("((proc [] -28))");
    }

    if (test_start(log, mv_string("large-static-argument"))) {
        RUN("(def Quad Struct [.x I64] [.y U64] [.z I64] [.p I64])");
        typedef struct {int64_t x; uint64_t y; int64_t z; int64_t p;} Quad;

        Quad expected = (Quad) {.x = -5, .y = 10, .z = -676, .p = -897};
        TEST_EQ("((proc [(s Quad)] s) (struct Quad [.x -5] [.y 10] [.z -676] [.p -897]))");
    }

    /* (def my-func proc [] seq */
    /*   [let! my-list (list.mk-list {I64} 4 4)] */
    /*   ;;(list.each print-fn my-list) */
    /*   my-list) */
    
    /* if (test_start(log, mv_string("large-static-argument"))) { */
    /*     uint64_t expected = 4; */
    /*     RUN("(def my-func proc [] seq [let! my-list (list.mk-list {I64} 4 4)] my-list)"); */
    /*     TEST_EQ("(my-func).len"); */
    /* } */
}
