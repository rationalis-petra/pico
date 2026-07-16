#include "test_pico/eval/components.h"
#include "test_pico/helper.h"

void run_pico_eval_modular_tests(TestLog *log, Module* module, Environment* env, Target target, RegionAllocator* region) {
    TestContext context = (TestContext) {
        .env = env,
        .region = region,
        .log = log,
        .target = target,
    };

    if (test_start(log, mv_string("simple-def"))) {
        int64_t expected = 197823;
        RUN("(def t1 197823)");
        TEST_EQ("t1");
    }

    if (test_start(log, mv_string("small-def"))) {
        int8_t expected = 67;
        RUN("(def t2 (is I8 67))");
        TEST_EQ("t2");
    }

    if (test_start(log, mv_string("simple-redef"))) {
        int64_t expected = -10297310;
        RUN("(def t3 197823)");
        RUN("(def t3 -10297310)");
        TEST_EQ("t3");
    }

    if (test_start(log, mv_string("simple-module-function"))) {
        int64_t expected = -78;
        RUN("(def f1 proc [] -78)");
        TEST_EQ("(f1)");
    }
     
    if (test_start(log, mv_string("function-recursive"))) {
        int64_t expected = 55;
        RUN("(def recur proc [(n I64)] \n"
            "  (if (i64.< n 1) n (i64.+ n (recur (i64.- n 1)))))");
        TEST_EQ("(recur 10)");
    }
}
