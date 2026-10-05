#include "platform/memory/region.h"
#include "test/test_log.h"

#include "test_rune/helper.h"
#include "test_rune/rune.h"
#include "rune/eval/eval.h"

void run_rune_tests(TestLog* log, Allocator* a) {
    RegionAllocator* region = make_region_allocator(4096, true, a);
    TestContext context = {
        .log = log,
        .region = region,
    };

    RuneEnv* env = mk_rune_env(a);
    Pools pools = get_pools(env);

    if (suite_start(log, mv_string("rune"))) {
        if (test_start(log, mv_string("untyped-data-none"))) {
            Name nm = string_to_name(mv_string("none"));
            ValRef expected = mk_rune_data(nm, (ValRefOption){}, 0, pools.value);
            TEST_EQ(":none");
        }
        suite_end(log);
    }

    delete_rune_env(env);
    delete_region_allocator(region);
}
