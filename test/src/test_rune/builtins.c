#include <string.h>
#include "platform/memory/region.h"
#include "test/test_log.h"

#include "test_rune/helper.h"
#include "test_rune/rune.h"
#include "rune/eval/eval.h"

void run_rune_builtin_tests(TestLog* log, RuneEnv* env, RegionAllocator* region) {
    TestContext context = {
        .log = log,
        .env = env,
        .region = region,
    };
    Pools pools = get_pools(env);

    if (test_start(log, mv_string("string-join"))) {
        String c_string = mv_string("lhs-rhs");
        ValRef expected = mk_rune_string(c_string.memsize, pools.value);
        String expected_string = get_rune_string(expected);
        memcpy(expected_string.bytes, c_string.bytes, c_string.memsize);
        TEST_EQ("(join \"lhs\" \"-\" \"rhs\")");
    }
}
