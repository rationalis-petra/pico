#include "test_pico/stdlib/components.h"
#include "test_pico/helper.h"

void run_pico_stdlib_abs_numeric_tests(TestLog *log, Module* module, Environment* env, Target target, RegionAllocator* region) {
    TestContext context = (TestContext) {
        .env = env,
        .region = region,
        .log = log,
        .target = target,
    };

    if (test_start(log, mv_string("numeric-add"))) {
        int64_t expected = 55;
        TEST_EQ("(+ 10 45)");
    }

    if (test_start(log, mv_string("numeric-add-instance-f32"))) {
        float expected = 3.4 + 678.0;
        TEST_EQ("(is F32 (+ 3.4 678.0))");
    }

    if (test_start(log, mv_string("numeric-add-vec3-f32-instance"))) {
        MODULE("(module vec3-math (import (prelude :all) (num :all) (abs.numeric :all)) (export :all))\n"
               "(def Vec3 Named Vec3 Family [A] Tile [3] A)\n"
               "(def vec3 all [A] proc [x y z] name (Vec3 A) tile [x y z])\n"
               "(def vec3-zero all [A] proc {(mon Monoid A)} [] name (Vec3 A) with [i] [3] mon.zero)\n"
               "(def vec3-monoid instance [A] {(mon Monoid A)} (Monoid (Vec3 A))"
            "  [.+ proc [ln rn]"
            "        let [l unname ln]"
            "            [r unname rn]"
            "          name (Vec3 A) with [i] [3] (mon.+ (telt [i] l) (telt [i] r))]"
            "  [.zero (vec3-zero)])");
        MODULE("(module vec3-player (import (prelude :all) (num :all) (abs.numeric :all) (vec3-math :all)) (export :all))\n"
               "(def add-position proc [(position (Vec3 F32)) (move (Vec3 F32))] (+ position move))");

        float expected[] = {5.0f, 7.0f, 9.0f};
        TEST_EQ("(vec3-player.add-position (vec3-math.vec3 {F32} 1.0 2.0 3.0) (vec3-math.vec3 {F32} 4.0 5.0 6.0))");
    }

    if (test_start(log, mv_string("numeric-div-instance-f64"))) {
        double expected = 3.0 / 4.5;
        TEST_EQ("(/ 3.0 4.5)");
    }

    if (test_start(log, mv_string("numeric-div-instance-f32"))) {
        float expected = 3.0 / 4.5;
        TEST_EQ("(is F32 (/ 3.0 4.5))");
    }

    if (test_start(log, mv_string("numeric-mul"))) {
        double expected = 3.0 * 4.5;
        TEST_EQ("(* 3.0 4.5)");
    }

    if (test_start(log, mv_string("numeric-mul"))) {
        double expected = 3.0 * 4.5;
        TEST_EQ("(* 3.0 4.5)");
    }
}
