#include "test_pico/helper.h"

void run_pico_stdlib_meta_gen_tests(TestLog *log, Module* module, Environment* env, Target target, RegionAllocator* region) {
    TestContext context = (TestContext) {
        .env = env,
        .region = region,
        .log = log,
        .target = target,
    };

    // -----------------------------------------------------
    // 
    //      Macros
    // 
    // -----------------------------------------------------

    if (test_start(log, mv_string("macro-id"))) {
        // We write it like this to avoid relying on the list module.
        // The list module is tested independently.
        RUN("(def ID macro proc [x] (:right (load {gen.Syntax} (num-to-address (u64.+ (address-to-num x.data) (size-of gen.Syntax))))))");

        int64_t expected = 177;
        TEST_EQ("(ID 177)");
    }
}
