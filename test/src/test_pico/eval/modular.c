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

    if (test_start(log, mv_string("module-header-import-all-export-all"))) {
        int64_t expected = 1;
        MODULE("(module test-module (import (core.kernel :all)) (export :all)) \n"
            "  (def val1 1) (def val2 2) (def val3 3)");
        TEST_EQ("test-module.val1");
    }

    if (test_start(log, mv_string("modlue-non-export"))) {
        MODULE("(module test-module (import (core.kernel :all)) (export val1 val2)) \n"
            "  (def val1 1) (def val2 2) (def val3 3)");
        TEST_ABSTRACT_FAIL("test-module.val3");
    }

    if (test_start(log, mv_string("modlue-import-path-all"))) {
        MODULE("(module test-module (import (core.kernel :all) (core.prim.i64 :all)) (export val)) \n"
            "  (def val (+ 1 3))");
        int64_t expected = 4;
        TEST_EQ("test-module.val");
    }

    if (test_start(log, mv_string("module-export-annotated"))) {
        MODULE("(module test-sub (import (core.kernel :all) (core.prim.i64 :all)) (export sub-val)) \n"
            "(declare sub-val [.type I64]) "
            "(def sub-val (+ 1 3))");
        MODULE("(module test-module (import (core.kernel :all) (test-sub :all) (core.prim.i64 :all)) (export val)) \n"
            "(def val (+ 1 sub-val))");
        int64_t expected = 5;
        TEST_EQ("test-module.val");
    }

    if (test_start(log, mv_string("modlue-import-path-visible-private-siblings"))) {
        MODULE("(module test-sub (import (core.kernel :all) (core.prim.i64 :all)) (export sub-val)) \n"
            "(def sub-val (+ 1 3))");
        MODULE("(module test-module (import (core.kernel :all) (test-sub :all) (core.prim.i64 :all)) (export val)) \n"
            "(def val (+ 1 sub-val))");
        int64_t expected = 5;
        TEST_EQ("test-module.val");
    }

    if (test_start(log, mv_string("module-import-path-split"))) {
        MODULE("(module sub1 (import (core.kernel :all)) (export val1)) \n"
               "(def val1 1)");
        MODULE("(module sub2 (import (core.kernel :all)) (export val2)) \n"
               "(def val2 2)");
        MODULE("(module test-module (import (core.kernel :all) (eval-test-module.(sub1 sub2) :all) (core.prim.i64 :all)) (export val)) \n"
            "  (def val (+ val1 val2))");
        int64_t expected = 3;
        TEST_EQ("test-module.val");
    }

    if (test_start(log, mv_string("module-import-path-split-head"))) {
        MODULE("(module sub1 (import (core.kernel :all)) (export val1)) \n"
               "(def val1 1)");
        MODULE("(module sub2 (import (core.kernel :all)) (export val2)) \n"
               "(def val2 2)");
        MODULE("(module test-module (import (core.kernel :all) ((sub1 sub2) :all) (core.prim.i64 :all)) (export val)) \n"
            "  (def val (+ val1 val2))");
        int64_t expected = 3;
        TEST_EQ("test-module.val");
    }

    if (test_start(log, mv_string("modlue-import-as"))) {
        MODULE("(module sub1 (import (core.kernel :all)) (export val)) \n"
               "(def val 898712)");
        MODULE("(module test-module (import (core.kernel :all) (sub1 :as s)) (export val)) \n"
            "  (def val s.val)");
        int64_t expected = 898712;
        TEST_EQ("test-module.val");
    }

    if (test_start(log, mv_string("module-import-types"))) {
        MODULE(
          "(module sub1 (import (core.kernel :all)) (export TestStruct)) \n"
          "(def TestStruct Struct [.x I64] [.y I64])\n");
        MODULE("(module test-module (import (core.kernel :all) (sub1 :types)) (export val)) \n"
            "  (def val struct TestStruct [.x 3] [.y 4])");
        int64_t expected[2] = {3, 4};
        TEST_EQ("test-module.val");
    }

    if (test_start(log, mv_string("module-re-export-values-import-all"))) {
        MODULE("(module sub1 (import (core.kernel :all)) (export val1)) \n"
               "(def val1 7)");
        MODULE("(module sub2 (import (core.kernel :all)) (re-export (sub1 :values (val1)))) \n");
        MODULE("(module test-module (import (core.kernel :all) (sub2 :all)) (export val)) \n"
            "  (def val val1)");
        int64_t expected = 7;
        TEST_EQ("test-module.val");
    }

    if (test_start(log, mv_string("module-re-export-import-values"))) {
        MODULE("(module sub1 (import (core.kernel :all)) (export val1)) \n"
               "(def val1 8)");
        MODULE("(module sub2 (import (core.kernel :all)) (re-export (sub1 :values (val1)))) \n");
        MODULE("(module test-module (import (core.kernel :all) (sub2 :values (val1))) (export val)) \n"
            "  (def val val1)");
        int64_t expected = 8;
        TEST_EQ("test-module.val");
    }

    if (test_start(log, mv_string("module-re-export-types"))) {
        MODULE("(module sub1 (import (core.kernel :all)) (export TestStruct)) \n"
               "(def TestStruct Struct [.x I64] [.y I64])\n");
        MODULE("(module sub2 (import (core.kernel :all)) (re-export (sub1 :types))) \n");
        MODULE("(module test-module (import (core.kernel :all) (sub2 :types)) (export val)) \n"
            "  (def val struct TestStruct [.x 3] [.y 4])");
        int64_t expected[2] = {3, 4};
        TEST_EQ("test-module.val");
    }

    if (test_start(log, mv_string("module-re-export-instances"))) {
        MODULE("(module sub-eq (import (core.kernel :all)) (export :all)) \n"
               "(def Eql Trait Eql [A] [.eq Proc [A A] Bool])\n"
               "(def eql all [A] proc {(eq (Eql A))} [(x A) (y A)] eq.eq x y)\n");
        MODULE("(module sub1 (import (core.kernel :all) core.prim.i64 (sub-eq :types)) (export :all)) \n"
               "(def eq-i64 instance (Eql I64) [.eq proc [x y] i64.= x y])\n");
        // TODO (BUG): investigate use-after-free when we remove the trailing
        // ')' from the below expression/module header
        MODULE("(module sub2 (import (core.kernel :all) core.prim.i64) (re-export (sub1 :instances)))\n");
        MODULE("(module test-module (import (core.kernel :all) (sub-eq :all) (sub2 :types)) (export val))\n"
            "  (def val eql 3 4)");
        bool expected = false;
        TEST_EQ("test-module.val");
    }

    /**
     * TODO: the below test demonstrates a bug with instance importing...
     * 
    if (test_start(log, mv_string("module-re-export-instances-unambiguous"))) {
        MODULE("(module sub-eq (import (core :all) (num.i64 :all)) (export :all)) \n"
               "(def Eq Trait Eq [A] [.eq Proc [A A] Bool])\n"
               "(def eql all [A] proc {(eq (Eq A))} [(x A) (y A)] eq.eq x y)\n");
        MODULE("(module sub1 (import (core :all) (num.i64 :all) (sub-eq :types)) (export :all)) \n"
               "(def eq-i64 instance (Eq I64) [.eq proc [x y] = x y])\n");
        MODULE("(module sub2 (import (core :all) (num.i64 :all)) (re-export (sub1 :instances)))\n");
        MODULE("(module test-module (import (core :all) (sub-eq :all) (sub2 :instances) (sub1 :instances)) (export val))\n"
            "  (def val eql 3 4)");
        bool expected = false;
        TEST_EQ("test-module.val");
    }
    */

    if (test_start(log, mv_string("module-re-export-multiple-clauses"))) {
        MODULE("(module sub-x (import (core.kernel :all)) (export :all)) \n"
               "(def x 10)\n");
        MODULE("(module sub-y (import (core.kernel :all)) (export :all)) \n"
               "(def y 15)\n");
        MODULE("(module sub-comb (import (core.kernel :all)) (re-export (sub-x :all) (sub-y :all))) \n");
        MODULE("(module test-module (import (core.kernel :all) (core.prim.i64 :all) (sub-comb :all)) (export val))\n"
            "  (def val + x y)");
        int64_t expected = 25;
        TEST_EQ("test-module.val");
    }
}
