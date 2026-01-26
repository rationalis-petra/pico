#include "data/float.h"

#include "test_pico/stdlib/components.h"
#include "test_pico/helper.h"

#define TEST_EQ(str) test_toplevel_eq(str, &expected, module, context); reset_subregion(region)

void run_pico_stdlib_num_tests(TestLog *log, Module* module, Environment* env, Target target, RegionAllocator* region) {
    TestContext context = (TestContext) {
        .env = env,
        .region = region,
        .log = log,
        .target = target,
    };

    // Unsigned Int 64
    if (test_start(log, mv_string("unsigned-add"))) {
        uint64_t expected = 4294967294;
        TEST_EQ("(u64.+ 2147483647 2147483647)");
    }

    if (test_start(log, mv_string("signed-sub"))) {
        int64_t expected = -2000000000;
        TEST_EQ("(i64.- 147483647 2147483647)");
    }

    if (test_start(log, mv_string("unsigned-modulo"))) {
        uint64_t expected = 7001;
        TEST_EQ("(u64.mod 20594361 9232)");
    }

    if (test_start(log, mv_string("unsigned-divide"))) {
        uint64_t expected = 2231;
        TEST_EQ("(u64./ 20594361 9231)");
    }

    // TODO: something like this crashed qoi.rl: see index code.
    /* if (test_start(log, mv_string("test-widening"))) { */
    /*     uint8_t expected = 2; */
    /*     TEST_EQ("(u8.mod (u8.* 255 11) 64)"); */
    /* } */

    if (test_start(log, mv_string("small-unsigned-divide"))) {
        uint8_t expected = 16;
        TEST_EQ("(u8./ 128 8)");
    }

    if (test_start(log, mv_string("well-defined-overflow"))) {
        uint8_t expected = 1;
        TEST_EQ("(u8.+ 255 2)");
    }

    if (test_start(log, mv_string("small-unsigned-divide-with-remainder"))) {
        uint8_t expected = 18;
        TEST_EQ("(u8./ 128 7)");
    }

    if (test_start(log, mv_string("small-unsigned-modulo"))) {
        uint8_t expected = 2;
        TEST_EQ("(u8.mod 128 7)");
    }

    if (test_start(log, mv_string("signed-mul"))) {
        int64_t expected = 2577581361;
        TEST_EQ("(i64.* 9231 279231)");
    }

    if (test_start(log, mv_string("signed-divide"))) {
        int64_t expected = 2231;
        TEST_EQ("(i64./ 20594361 9231)");
    }

    if (test_start(log, mv_string("signed-modulo"))) {
        int64_t expected = 7001;
        TEST_EQ("(i64.mod 20594361 9232)");
    }

    if (test_start(log, mv_string("unsigned-64-shr"))) {
        uint64_t expected = 16384;
        TEST_EQ("(u64.shl 10 16)");
    }

    // Unsigned Int 32
    if (test_start(log, mv_string("unsigned-add"))) {
        uint32_t expected = 188628;
        TEST_EQ("(u32.+ 65535 123093)");
    }

    if (test_start(log, mv_string("unsigned-shr"))) {
        uint32_t expected = 128;
        TEST_EQ("(u32.shl 3 16)");
    }

    // Unsigned Int 16
    if (test_start(log, mv_string("unsigned-add"))) {
        uint64_t expected = 6493;
        TEST_EQ("(u16.+ 1026 5467)");
    }

    // Unsigned Int 32
    if (test_start(log, mv_string("unsigned-bswap"))) {
        uint64_t expected = 262144;
        TEST_EQ("(u32.byte-swap 1024)");
    }

    // Unsigned Int 8
    if (test_start(log, mv_string("unsigned-add"))) {
        uint64_t expected = 129;
        TEST_EQ("(u8.+ 100 29)");
    }

    if (test_start(log, mv_string("unsigned-8-shl"))) {
        uint8_t expected = 2;
        TEST_EQ("(u8.shr 3 16)");
    }

    if (test_start(log, mv_string("signed-add"))) {
        int64_t expected = 0;
        TEST_EQ("(i64.+ 2147483647 -2147483647)");
    }

    if (test_start(log, mv_string("unsigned-mul"))) {
        int64_t expected = 24;
        TEST_EQ("(i64.* 4 6)");
    }

    // -------------------------------------------------------------------------
    //
    //      Floating point numbers
    //
    // -------------------------------------------------------------------------

    if (test_start(log, mv_string("f32-add"))) {
        float expected = 51.5 + 2.45;
        TEST_EQ("(f32.+ 51.5 2.45)");
    }

    if (test_start(log, mv_string("f32-sub"))) {
        float expected = 51.5 - 2.45;
        TEST_EQ("(f32.- 51.5 2.45)");
    }

    if (test_start(log, mv_string("f32-div"))) {
        float expected = 51.5 / 2.45;
        TEST_EQ("(f32./ 51.5 2.45)");
    }

    if (test_start(log, mv_string("f32-mul"))) {
        float expected = 51.5 * 2.45;
        TEST_EQ("(f32.* 51.5 2.45)");
    }

    if (test_start(log, mv_string("f32-sin"))) {
        float expected = sin_f32(3.14159);
        TEST_EQ("(f32.sin 3.14159)");
    }

    if (test_start(log, mv_string("f32-cos"))) {
        float expected = cos_f32(3.14159);
        TEST_EQ("(f32.cos 3.14159)");
    }

    if (test_start(log, mv_string("f64-add"))) {
        double expected = 51.5 + 2.45;
        TEST_EQ("(f64.+ 51.5 2.45)");
    }

    if (test_start(log, mv_string("f64-sub"))) {
        double expected = 51.5 - 2.45;
        TEST_EQ("(f64.- 51.5 2.45)");
    }

    if (test_start(log, mv_string("f64-div"))) {
        double expected = 51.5 / 2.45;
        TEST_EQ("(f64./ 51.5 2.45)");
    }

    if (test_start(log, mv_string("f64-mul"))) {
        double expected = 51.5 * 2.45;
        TEST_EQ("(f64.* 51.5 2.45)");
    }

    if (test_start(log, mv_string("f64-sin"))) {
        double expected = sin_f64(3.14159);
        TEST_EQ("(f64.sin 3.14159)");
    }

    if (test_start(log, mv_string("f64-cos"))) {
        double expected = cos_f64(3.14159);
        TEST_EQ("(f64.cos 3.14159)");
    }

    // -------------------------------------------------------------------------
    //
    //      Booleans - not, and, or, etc.
    //
    // -------------------------------------------------------------------------

    if (test_start(log, mv_string("and-ff"))) {
        uint8_t expected = 0;
        TEST_EQ("(bool.and :false :true)");
    }

    if (test_start(log, mv_string("and-ft"))) {
        uint8_t expected = 0;
        TEST_EQ("(bool.and :false :true)");
    }

    if (test_start(log, mv_string("and-tt"))) {
        uint8_t expected = 1;
        TEST_EQ("(bool.and :true :true)");
    }

    if (test_start(log, mv_string("or-ff"))) {
        uint8_t expected = 0;
        TEST_EQ("(bool.or :false :false)");
    }

    if (test_start(log, mv_string("or-ft"))) {
        uint8_t expected = 1;
        TEST_EQ("(bool.or :false :true)");
    }

    if (test_start(log, mv_string("or-tt"))) {
        uint8_t expected = 1;
        TEST_EQ("(bool.or :true :true)");
    }

    if (test_start(log, mv_string("not-t"))) {
        bool expected = false;
        TEST_EQ("(bool.not :true)");
    }

    if (test_start(log, mv_string("not-f"))) {
        bool expected = true;
        TEST_EQ("(bool.not :false)");
    }
}
