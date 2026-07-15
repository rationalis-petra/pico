#include "pico/stdlib/platform/submodules.h"

#include "test_pico/eval/components.h"
#include "test_pico/helper.h"

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
    
    if (test_start(log, mv_string("large-static-argument"))) {
        uint64_t expected = 4;
        RUN("(def my-func proc [] seq [let! my-list (list.mk-list {I64} 4 4)] my-list)");

        PiAllocator pia = convert_to_pallocator(&ra);
        PiAllocator old = set_std_current_allocator(pia);
        TEST_EQ("(my-func).len");
        set_std_current_allocator(old);
    }
}
