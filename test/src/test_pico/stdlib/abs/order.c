#include "test_pico/stdlib/components.h"
#include "test_pico/helper.h"

void run_pico_stdlib_abs_order_tests(TestLog *log, Module* module, Environment* env, Target target, RegionAllocator* region) {
    TestContext context = (TestContext) {
        .env = env,
        .region = region,
        .log = log,
        .target = target,
    };

    if (test_start(log, mv_string("order-less"))) {
        bool expected = true;
        TEST_EQ("(< 2 3)");
    }

    if (test_start(log, mv_string("order-less-false"))) {
        bool expected = false;
        TEST_EQ("(< 2 2)");
    }

    if (test_start(log, mv_string("order-less-eq"))) {
        bool expected = true;
        TEST_EQ("(<= 2 2)");
    }

    if (test_start(log, mv_string("order-less-eq-false"))) {
        bool expected = false;
        TEST_EQ("(<= 5 2)");
    }

    if (test_start(log, mv_string("order-greater"))) {
        bool expected = true;
        TEST_EQ("(> 3 2)");
    }

    if (test_start(log, mv_string("order-greater-false"))) {
        bool expected = false;
        TEST_EQ("(> 3 3)");
    }

    if (test_start(log, mv_string("order-greater-eq"))) {
        bool expected = true;
        TEST_EQ("(>= 3 3)");
    }

    if (test_start(log, mv_string("order-greater-eq-false"))) {
        bool expected = false;
        TEST_EQ("(>= 2 3)");
    }

    if (test_start(log, mv_string("order-eq"))) {
        RUN("(def ord-eq all [A] proc {(ord (Ord A))} [(x A) (y A)] ord.eq.= x y)");
        bool expected = true;
        TEST_EQ("(ord-eq 10 10)");
    }
}
