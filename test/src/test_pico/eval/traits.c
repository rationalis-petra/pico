#include "pico/stdlib/platform/submodules.h"

#include "test_pico/eval/components.h"
#include "test_pico/helper.h"

#define TEST_MEM(str) test_toplevel_mem(str, &expected, start, sizeof(expected), module, context)

void run_pico_eval_trait_tests(TestLog *log, Module* module, Environment* env, Target target, RegionAllocator* region) {
    TestContext context = (TestContext) {
        .env = env,
        .region = region,
        .log = log,
        .target = target,
    };
    //Allocator ra = ra_to_gpa(region);

    // -----------------------------------------------------
    ///
    //  Instances
    // 
    // -----------------------------------------------------

    if (test_start(log, mv_string("instance-const"))) {
        int64_t expected = 77;
        RUN("(def Inhabited Trait Inhabited [A] [.value A])");
        // TODO (BUG)
        // swapping the order of below statements gives an 'ambiguous instance' error?
        RUN("(def get-value all [A] proc {(in (Inhabited A))} [(x A)] in.value)");
        RUN("(def i64-inhabited instance (Inhabited I64) [.value 77])");

        TEST_EQ("(get-value {I64} 5)");
    }

    if (test_start(log, mv_string("instance-multi-val"))) {
        int64_t expected = -77;
        RUN("(def MultiInhabited Trait MultiInhabited [A] [.val-1 A] [.val-2 A])");
        // TODO (BUG)
        // swapping the order of below statements gives an 'ambiguous instance' error?
        RUN("(def get-second-value all [A] proc {(in (MultiInhabited A))} [(x A)] in.val-2)");
        RUN("(def i64-multi-inhabited instance (MultiInhabited I64) [.val-1 237] [.val-2 -77])");

        TEST_EQ("(get-second-value {I64} 5)");
    }

    if (test_start(log, mv_string("instance-out-of-order"))) {
        int64_t expected = -77;
        RUN("(def MultiInhabited Trait MultiInhabited [A] [.val-1 A] [.val-2 A])");
        // TODO (BUG)
        // swapping the order of below statements gives an 'ambiguous instance' error?
        RUN("(def get-second-value all [A] proc {(in (MultiInhabited A))} [(x A)] in.val-2)");
        RUN("(def i64-multi-inhabited instance (MultiInhabited I64) [.val-2 -77] [.val-1 237])");

        TEST_EQ("(get-second-value {I64} 5)");
    }

    if (test_start(log, mv_string("instance-const-single"))) {
        uint64_t expected = 43;
        RUN("(def MultiConstInhabited Trait MultiConstInhabited [A] [.val-1 A] [.val-2 U64])");
        RUN("(def get-snd-const-value all [A] proc {(in (MultiConstInhabited A))} [(x A)] in.val-2)");
        RUN("(def i64-multi-const-inhabited instance (MultiConstInhabited I64) [.val-1 77] [.val-2 43])");

        TEST_EQ("(get-snd-const-value {I64} 5)");
    }

    // TODO: enable me!
    if (test_start(log, mv_string("instance-const-unaligned"))) {
        int64_t expected = -98;
        RUN("(def Inhabited Trait Inhabited [A] [.value A])");

        RUN("(def get-value all [A] proc {(in (Inhabited A))} [(x A)] in.value)");
        RUN("(def i8-inhabited instance (Inhabited I8) [.value -98])");
        TEST_EQ("(get-value {I8} 5)");
    }

    if (test_start(log, mv_string("instance-dependent"))) {
        RUN("(def Addable Trait Addable [A] [.add Proc [A A] A])");
        RUN("(def ID Distinct ID Family [A] A)");
        RUN("(def add-i64 instance (Addable I64)"
            "  [.add prim.i64.+])");
        RUN("(def add-id instance [A] {(inner (Addable A))} (Addable (ID A))"
            "  [.add proc [x y] "
            "    (into (ID A) (inner.add"
            "      (out-of (ID A) x) "
            "      (out-of (ID A) y)))])");
        RUN("(def poly-add all [A] proc {(add (Addable A))} [(x A) (y A)] (add.add x y))");

        int64_t expected = 72;
        TEST_EQ("(poly-add (into (ID I64) 42) (into (ID I64) 30))");
    }

    if (test_start(log, mv_string("multi-inline-proc-in-instance"))) {
        RUN("(def Eql Trait Eql [A] [.eql Proc [A A] Bool] [.not-eql Proc [A A] Bool])");
        RUN("(def eql-bool instance (Eql Bool)"
            "  [.eql proc [a b] (bool.or (bool.and a b) (bool.not (bool.or a b)))]"
            "  [.not-eql proc [a b] (bool.not (bool.or (bool.and a b) (bool.not (bool.or a b))))])");
        RUN("(def poly-eq all [A] proc {(eql (Eql A))} [(x A) (y A)] (eql.eql x y))");

        bool expected = false;
        TEST_EQ("(poly-eq :false :true)");
    }

    if (test_start(log, mv_string("instance-inline-proc-out-of-order"))) {
        RUN("(def Eql Trait Eql [A] [.eql Proc [A A] Bool] [.not-eql Proc [A A] Bool])");
        RUN("(def eql-bool instance (Eql Bool)"
            "  [.not-eql proc [a b] (bool.not (bool.or (bool.and a b) (bool.not (bool.or a b))))]"
            "  [.eql proc [a b] (bool.or (bool.and a b) (bool.not (bool.or a b)))])");
        RUN("(def poly-eq all [A] proc {(eql (Eql A))} [(x A) (y A)] (eql.eql x y))");

        bool expected = false;
        TEST_EQ("(poly-eq :false :true)");
    }

    if (test_start(log, mv_string("instance-inferred-for-local-variables"))) {
        RUN("(def Eql Trait Eql [A] [.eql Proc [A A] Bool] [.not-eql Proc [A A] Bool])");
        RUN("(def eql-bool instance (Eql Bool)"
            "  [.not-eql proc [a b] (bool.not (bool.or (bool.and a b) (bool.not (bool.or a b))))]"
            "  [.eql proc [a b] (bool.or (bool.and a b) (bool.not (bool.or a b)))])");
        RUN("(def poly-eq all [A] proc {(eql (Eql A))} [(x A) (y A)] (eql.eql x y))");

        RUN("(def Id Family [A] Struct [.v A])");
        RUN("(def id-eq all [A] proc {(eql (Eql A))} [(x (Id A)) (y (Id A))] (poly-eq x.v y.v))");

        bool expected = false;
        TEST_EQ("(id-eq (struct [.v :false]) (struct [.v :true]))");
    }
}
