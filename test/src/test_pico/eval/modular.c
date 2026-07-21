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

    if (test_start(log, mv_string("module-header"))) {
        int64_t expected = 1;
        MODULE("(module test-module (import (core :all)) (export :all)) \n"
            "  (def val1 1) (def val2 2) (def val3 3)");
        TEST_EQ("test-module.val1");
    }

    if (test_start(log, mv_string("modlue-non-export"))) {
        MODULE("(module test-module (import (core :all)) (export val1 val2)) \n"
            "  (def val1 1) (def val2 2) (def val3 3)");
        TEST_ABSTRACT_FAIL("test-module.val3");
    }
    
    if (test_start(log, mv_string("modlue-import-path-all"))) {
        MODULE("(module test-module (import (core :all) (num.i64 :all)) (export val)) \n"
            "  (def val (+ 1 3))");
        int64_t expected = 4;
        TEST_EQ("test-module.val");
    }

    if (test_start(log, mv_string("modlue-import-path-visible-private-siblings"))) {
        MODULE("(module test-module (import (core :all) (num.i64 :all)) (export val)) \n"
            "  (def val (+ 1 3))");
        int64_t expected = 4;
        TEST_EQ("test-module.val");
    }

    if (test_start(log, mv_string("module-import-path-split"))) {
        MODULE("(module sub1 (import (core :all) (num.i64 :all)) (export val1)) \n"
               "(def val1 1)");
        MODULE("(module sub2 (import (core :all) (num.i64 :all)) (export val2)) \n"
               "(def val2 2)");
        MODULE("(module test-module (import (core :all) (eval-test-module.(sub1 sub2) :all) (num.i64 :all)) (export val)) \n"
            "  (def val (+ val1 val2))");
        int64_t expected = 3;
        TEST_EQ("test-module.val");
    }
}
