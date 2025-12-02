#include "platform/memory/arena.h"

#include "pico/stdlib/platform/submodules.h"

#include "test_pico/stdlib/components.h"
#include "test_pico/helper.h"

#define RUN(str) run_toplevel(str, module, context); refresh_env(env, a)
#define TEST_EQ(str) test_toplevel_eq(str, &expected, module, context)
#define TEST_STDOUT(str) test_toplevel_stdout(str, expected, module, context)

void run_pico_stdlib_extra_tests(TestLog *log, Module* module, Environment* env, Target target, Allocator *a) {
    Allocator arena = mk_arena_allocator(16384, a);
    PiAllocator parena = convert_to_pallocator(&arena);
    TestContext context = (TestContext) {
        .env = env,
        .a = &arena,
        .log = log,
        .target = target,
    };

    if (test_start(log, mv_string("single-for-upto"))) {
        PiAllocator current_old = get_std_current_allocator();
        set_std_current_allocator(parena);
        const char* expected = "12345678910";
        TEST_STDOUT("(loop [for i from 1 upto 10] (terminal.write-string (u64.to-string i)))");
        set_std_current_allocator(current_old);
        reset_arena_allocator(arena);
    }

    if (test_start(log, mv_string("single-for-below"))) {
        PiAllocator current_old = get_std_current_allocator();
        set_std_current_allocator(parena);
        const char* expected = "123456789";
        TEST_STDOUT("(loop [for i from 1 below 10] (terminal.write-string (u64.to-string i)))");
        set_std_current_allocator(current_old);
        reset_arena_allocator(arena);
    }

    if (test_start(log, mv_string("single-for-downto"))) {
        PiAllocator current_old = get_std_current_allocator();
        set_std_current_allocator(parena);
        const char* expected = "10987654321";
        TEST_STDOUT("(loop [for i from 10 downto 1] (terminal.write-string (u64.to-string i)))");
        set_std_current_allocator(current_old);
        reset_arena_allocator(arena);
    }

    if (test_start(log, mv_string("single-for-above"))) {
        PiAllocator current_old = get_std_current_allocator();
        set_std_current_allocator(parena);
        const char* expected = "1098765432";
        TEST_STDOUT("(loop [for i from 10 above 1] (terminal.write-string (u64.to-string i)))");
        set_std_current_allocator(current_old);
        reset_arena_allocator(arena);
    }

    if (test_start(log, mv_string("double-for-loop"))) {
        PiAllocator current_old = get_std_current_allocator();
        set_std_current_allocator(parena);
        const char* expected = "90817263544536271809";
        TEST_STDOUT("(loop [for i from 9 downto 0] [for j from 0 below 10]\n"
                             "(terminal.write-string (u64.to-string i)) (terminal.write-string (u64.to-string j)))");
        set_std_current_allocator(current_old);
        reset_arena_allocator(arena);
    }

    if (test_start(log, mv_string("for-then-expr-loop"))) {
        PiAllocator current_old = get_std_current_allocator();
        set_std_current_allocator(parena);
        const char* expected = "0101010101";
        TEST_STDOUT("(loop [for i from 1 upto 10] [for j = 0 then (u64.mod (u64.+ 1 j) 2)] (terminal.write-string (u64.to-string j)))");
        set_std_current_allocator(current_old);
        reset_arena_allocator(arena);
    }
    release_arena_allocator(arena);
}
