#include <string.h>
#include "platform/memory/region.h"
#include "test/test_log.h"

#include "test_rune/helper.h"
#include "test_rune/rune.h"
#include "rune/eval/eval.h"

void run_rune_eval_tests(TestLog* log, RuneEnv* env, RegionAllocator* region) {
    TestContext context = {
        .log = log,
        .env = NULL,
        .region = region,
    };
    Pools pools = get_pools(env);

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

    if (test_start(log, mv_string("untyped-record-point"))) {
        ValRef expected = mk_rune_record((ValRefOption){}, 2, pools.value);
        RuneRecord* record = get_rune_record(expected);
        record->values[0] = (NameValPr) {
            .name = string_to_name(mv_string("x")),
            .val = mk_rune_int(64, pools.value),
        };
        record->values[1] = (NameValPr) {
            .name = string_to_name(mv_string("y")),
            .val = mk_rune_int(-32, pools.value),
        };
        TEST_EQ("(record [.x 64] [.y -32])");
    }

    if (test_start(log, mv_string("call-simple-lambda"))) {
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
}
