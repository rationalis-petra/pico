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

    if (test_start(log, mv_string("instance-parametric-field-out-of-order"))) {
        RUN("(def Pair Trait Pair [A] [.left I64] [.right I64])");
        RUN("(def pair-i64 instance [A] (Pair A) [.right 7] [.left 42])");
        RUN("(def get-left all [A] proc {(p (Pair A))} [(x A)] p.left)");

        int64_t expected = 42;
        TEST_EQ("(get-left {I64} 0)");
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

    if (test_start(log, mv_string("instance-parent-inferred-for-local-variables"))) {
        RUN("(def Eql Trait Eql [A] [.eql Proc [A A] Bool])");
        RUN("(def Rod Trait Rod [A] {.eql Eql A} [.less Proc [A A] Bool])");
        RUN("(def eql-bool instance (Eql Bool)"
            "  [.eql proc [a b] (bool.or (bool.and a b) (bool.not (bool.or a b)))])");
        // TODO: If we replace rod-bool here with eql-bool (meaning we redefine
        //       the above line), then there is an error because we overwrite
        //       the above line but also it is our own parent slot...
        RUN("(def rod-bool instance (Rod Bool)"
            "  [.less proc [a b] (bool.and (bool.not a) b)])");
        RUN("(def poly-eq all [A] proc {(eql (Eql A))} [(x A) (y A)] (eql.eql x y))");
        RUN("(def uses-eq all [A] proc {(rod (Rod A))} [(x A) (y A)] (poly-eq x y))");

        bool expected = false;
        TEST_EQ("(uses-eq :false :true)");
    }

    if (test_start(log, mv_string("trait-higher-kinded"))) {
        RUN("(def Cont Trait Cont [(C Kind [Type] Type)] [.get All [A] Proc [(C A)] A])");
        RUN("(def Ptr Named Ptr Family [A] Address)");
        RUN("(def ptr-cont instance (Cont Ptr) \n"
            "[.get all [A] proc [(ptr (Ptr A))] (address.load {A} (unname ptr))])");

        RUN("(def get all [A (C Kind [Type] Type)] proc {(cont (Cont C))} [(c (C A))] \n"
            "  cont.get {A} c)");

        int64_t expected = 9798;
        TEST_EQ("(seq [let! ptr (name (Ptr I64) (dyn-alloc (size-of I64)))] (address.store (unname ptr) 9798) (get ptr))");
    }

    if (test_start(log, mv_string("trait-local-instantiation"))) {
        RUN("(def Monoid Trait Monoid [A] [.+ Proc [A A] A] [.zero A])");
        RUN("(def Vec2 Named Vec2 Family [A] Tile [2] A)");
        RUN("(def vec2-zero all [A] proc {(mon (Monoid A))} [] name (Vec2 A) tile [mon.zero mon.zero])");
        RUN("(def vec2-monoid instance [A] {(mon Monoid A)} (Monoid (Vec2 A))\n"
            "  [.+ proc [ln rn]\n"
            "        let [l unname ln]\n"
            "            [r unname rn]\n"
            "          name (Vec2 A) with [i] [2] (mon.+ (telt [i] l) (telt [i] r))]\n"
            "  [.zero (vec2-zero)])\n");
        RUN("(def i64-monoid instance (Monoid I64)\n"
            "  [.+ prim.i64.+]"
            "  [.zero 0])\n");

        RUN("(def + all [A] proc {(n (Monoid A))} [(x A) (y A)] n.+ x y)");

        int64_t expected[] = {7, 3};
        TEST_EQ("(let [v1 name (Vec2 I64) tile [3 1]] [v2 name (Vec2 I64) tile [4 2]] (+ v1 v2))");
    }

    if (test_start(log, mv_string("trait-local-instantiation-vec3-f32"))) {
         MODULE("(module vec3-traits (import (core.kernel :all)) (export :all))\n"
             "(def Monoid Trait Monoid [A] [.+ Proc [A A] A] [.zero A])\n"
             "(def + all [A] proc {(n (Monoid A))} [(x A) (y A)] n.+ x y)");
         MODULE("(module vec3-math (import (core.kernel :all) (core.prim :as prim) (vec3-traits :all)) (export :all))\n"
               "(def Vec3 Named Vec3 Family [A] Tile [3] A)\n"
               "(def vec3 all [A] proc [x y z] name (Vec3 A) tile [x y z])\n"
             "(def f32-monoid instance (Monoid F32) [.+ prim.f32.+] [.zero 0.0])\n"
               "(def vec3-zero all [A] proc {(mon Monoid A)} [] name (Vec3 A) with [i] [3] mon.zero)\n"
               "(def vec3-monoid instance [A] {(mon Monoid A)} (Monoid (Vec3 A))"
            "  [.+ proc [ln rn]"
            "        let [l unname ln]"
            "            [r unname rn]"
            "          name (Vec3 A) with [i] [3] (mon.+ (telt [i] l) (telt [i] r))]"
            "  [.zero (vec3-zero)])");
        MODULE("(module vec3-player (import (core.kernel :all) (vec3-traits :all) (vec3-math :all)) (export :all))\n"
               "(def add-position proc [(position (Vec3 F32)) (move (Vec3 F32))] (+ position move))");

        {
            float expected[] = {5.0f, 7.0f, 9.0f};
            TEST_EQ("(vec3-player.add-position (vec3-math.vec3 {F32} 1.0 2.0 3.0) (vec3-math.vec3 {F32} 4.0 5.0 6.0))");
        }
    }

    if (test_start(log, mv_string("trait-implicit-field-offset"))) {
        RUN("(def Left Trait Left [A] [.identity Proc [A] A])");
        RUN("(def i64-left instance (Left I64) [.identity proc [x] x])");

        RUN("(def Padded Trait Padded [A] {.left Left A} [.values (Tile 2 A)])");
        RUN("(def i64-padded instance (Padded I64) [.values tile [42 7]])");

        int64_t expected = 7;
        TEST_EQ("(let [p i64-padded] (telt [1] p.values))");
    }

    if (test_start(log, mv_string("trait-parametric-implicit-field-layout"))) {
        RUN("(def Left Trait Left [A] [.identity Proc [A] A])");
        RUN("(def i64-left instance (Left I64) [.identity proc [x] x])");

        RUN("(def Padded Trait Padded [A] {.left Left A} [.values (Tile 2 I64)])");
        RUN("(def poly-padded instance [A] {(left (Left A))} (Padded A) [.values tile [42 7]])");
        RUN("(def padded-left all [A] proc {(p (Padded A))} [(x A)] p.left.identity x)");

        int64_t expected = 42;
        TEST_EQ("(padded-left {I64} 42)");
    }

    if (test_start(log, mv_string("trait-member-deductions"))) {
        RUN("(def Group Trait Group [A] {.mon Monoid A} [.- Proc [A A] A])");
        RUN("(def vec2-group instance [A] {(grp Group A)} (Group (Vec2 A))"
            "  [.- proc [ln rn]"
            "        let [l unname ln]"
            "            [r unname rn]"
            "          name (Vec2 A) with [i] [2] (grp.- (telt [i] l) (telt [i] r))])")
        RUN("(def i64-group instance (Group I64)\n"
            "  [.- prim.i64.-])\n");

        RUN("(def - all [A] proc {(n (Group A))} [(x A) (y A)] n.- x y)");

        int64_t expected[] = {2, -3};
        TEST_EQ("(let [v1 name (Vec2 I64) tile [12 6]] [v2 name (Vec2 I64) tile [10 9]] (- v1 v2))");
    }
}
