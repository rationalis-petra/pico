#include "platform/memory/arena.h"

#include "pico/stdlib/extra.h"

#include "test_pico/stdlib/components.h"
#include "test_pico/helper.h"

#define RUN(str) run_toplevel(str, module, context); refresh_env(env, a)
#define TEST_EQ(str) test_toplevel_eq(str, &expected, module, context)
#define TEST_STDOUT(str) test_toplevel_stdout(str, expected, module, context)

void run_pico_stdlib_meta_gen_tests(TestLog *log, Module* module, Environment* env, Target target, Allocator *a) {
    Allocator arena = mk_arena_allocator(16384, a);
    TestContext context = (TestContext) {
        .env = env,
        .a = &arena,
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

    release_arena_allocator(arena);
}
