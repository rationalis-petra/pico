#include <string.h>
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
        if (test_start(log, mv_string("int-value"))) {
            ValRef expected = mk_rune_int(9871, pools.value);
            TEST_EQ("9871");
        }

        if (test_start(log, mv_string("string-value"))) {
            String c_string = mv_string("test");
            ValRef expected = mk_rune_string(c_string.memsize, pools.value);
            String expected_string = get_rune_string(expected);
            memcpy(expected_string.bytes, c_string.bytes, c_string.memsize);
            TEST_EQ("\"test\"");
        }

        if (test_start(log, mv_string("list-value"))) {
            ValRef expected = mk_rune_list(3, pools.value);
            RuneList list = get_rune_list(expected);
            list.data[0] = mk_rune_int(1, pools.value);
            list.data[1] = mk_rune_int(2, pools.value);
            list.data[2] = mk_rune_int(3, pools.value);
            TEST_EQ("(list 1 2 3)");
        }

        if (test_start(log, mv_string("untyped-data-none"))) {
            Name nm = string_to_name(mv_string("none"));
            ValRef expected = mk_rune_data(nm, (ValRefOption){}, 0, pools.value);
            TEST_EQ(":none");
        }

        if (test_start(log, mv_string("untyped-data-some"))) {
            Name nm = string_to_name(mv_string("some"));
            ValRef expected = mk_rune_data(nm, (ValRefOption){}, 1, pools.value);
            RuneData* data = get_rune_data(expected);
            data->values[0] = mk_rune_int(82731, pools.value);
            TEST_EQ("(:some 82731)");
        }

        if (test_start(log, mv_string("call-simble-lambda"))) {
            ValRef expected = mk_rune_int(72, pools.value);
            TEST_EQ("((fn [x] x) 72)");
        }

        if (test_start(log, mv_string("call-multi-arg-lambda"))) {
            ValRef expected = mk_rune_int(43, pools.value);
            TEST_EQ("((fn [x y] y) 72 43)");
        }

        if (test_start(log, mv_string("call-curry"))) {
            ValRef expected = mk_rune_int(12, pools.value);
            TEST_EQ("((fn [f x] f x) ((fn [x y] x) 12) 72)");
        }

        if (test_start(log, mv_string("call-closure"))) {
            ValRef expected = mk_rune_int(1, pools.value);
            TEST_EQ("((fn [x] (fn [y] x)) 1 2)");
        }
        suite_end(log);
    }

    delete_rune_env(env);
    delete_region_allocator(region);
}
