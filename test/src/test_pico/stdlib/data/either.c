#include "platform/memory/arena.h"

#include "pico/stdlib/extra.h"

#include "test_pico/stdlib/components.h"
#include "test_pico/helper.h"

void run_pico_stdlib_data_either_tests(TestLog *log, Module* module, Environment* env, Target target, RegionAllocator* region) {
    TestContext context = (TestContext) {
        .env = env,
        .region = region,
        .log = log,
        .target = target,
    };

    typedef struct {
        uint64_t tag;
        int64_t data;
    } Either;

    if (test_start(log, mv_string("either-lhs-simple"))) {
        Either expected = (Either) {.tag = 0, .data = -35};
        TEST_EQ("((either.Either I64 I64):left -35)");
    }

    if (test_start(log, mv_string("either-lhs-simple"))) {
        Either expected = (Either) {.tag = 1, .data = 8796};
        TEST_EQ("((either.Either I64 I64):right 8796)");
    }

    if (test_start(log, mv_string("left-fn"))) {
        Either expected = (Either) {.tag = 0, .data = -987};
        TEST_EQ("(either.left {I64 U64} -987)");
    }
}
