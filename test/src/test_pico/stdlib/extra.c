#include "platform/memory/arena.h"
#include "platform/filesystem/filesystem.h"

#include "pico/stdlib/extra.h"

#include "test_pico/stdlib/components.h"
#include "test_pico/helper.h"

#define RUN(str) run_toplevel(str, module, env, log, a); refresh_env(env, a)
#define TEST_EQ(str) test_toplevel_eq(str, &expected, module, env, log, a)
#define TEST_STDOUT(str) test_toplevel_stdout(str, expected, module, env, log, a)

void run_pico_stdlib_extra_tests(TestLog *log, Module* module, Environment* env, Allocator *a) {
    Allocator arena = mk_arena_allocator(4096, a);
    if (test_start(log, mv_string("print"))) {
        const char* expected = "test";
        TEST_STDOUT("(print \"test\")");
    }

    if (test_start(log, mv_string("single-for-upto"))) {
        Allocator current_old = get_std_current_allocator();
        set_std_current_allocator(arena);
        const char* expected = "12345678910";
        TEST_STDOUT("(loop [for i from 1 upto 10] (print (u64.to-string i)))");
        set_std_current_allocator(current_old);
        reset_arena_allocator(arena);
    }

    if (test_start(log, mv_string("single-for-below"))) {
        Allocator current_old = get_std_current_allocator();
        set_std_current_allocator(arena);
        const char* expected = "123456789";
        TEST_STDOUT("(loop [for i from 1 below 10] (print (u64.to-string i)))");
        set_std_current_allocator(current_old);
        reset_arena_allocator(arena);
    }

    if (test_start(log, mv_string("single-for-downto"))) {
        Allocator current_old = get_std_current_allocator();
        set_std_current_allocator(arena);
        const char* expected = "10987654321";
        TEST_STDOUT("(loop [for i from 10 downto 1] (print (u64.to-string i)))");
        set_std_current_allocator(current_old);
        reset_arena_allocator(arena);
    }

    if (test_start(log, mv_string("single-for-above"))) {
        Allocator current_old = get_std_current_allocator();
        set_std_current_allocator(arena);
        const char* expected = "1098765432";
        TEST_STDOUT("(loop [for i from 10 above 1] (print (u64.to-string i)))");
        set_std_current_allocator(current_old);
        reset_arena_allocator(arena);
    }

    if (test_start(log, mv_string("double-for-loop"))) {
        Allocator current_old = get_std_current_allocator();
        set_std_current_allocator(arena);
        const char* expected = "90817263544536271809";
        TEST_STDOUT("(loop [for i from 9 downto 0] [for j from 0 below 10]\n"
                             "(print (u64.to-string i)) (print (u64.to-string j)))");
        set_std_current_allocator(current_old);
        reset_arena_allocator(arena);
    }

    if (test_start(log, mv_string("for-then-expr-loop"))) {
        Allocator current_old = get_std_current_allocator();
        set_std_current_allocator(arena);
        const char* expected = "0101010101";
        TEST_STDOUT("(loop [for i from 1 upto 10] [for j = 0 then (u64.mod (u64.+ 1 j) 2)] (print (u64.to-string j)))");
        set_std_current_allocator(current_old);
        reset_arena_allocator(arena);
    }

    if (test_start(log, mv_string("run-script"))) {
        String filename = string_cat(get_tmpdir(&arena), mv_string("/script.rl"), &arena);
        File *file = open_file(filename, Read | Write, &arena);
        const char contents[] = " (print (u64.to-string 123456789)) ";
        U8Array data = (U8Array) {
            .len = sizeof(contents),
            .data = (uint8_t*)contents,
        };
        write_chunk(file, data);
        close_file(file);
        Allocator current_old = get_std_current_allocator();
        set_std_current_allocator(arena);
        String to_run = string_ncat(&arena, 3,
                                    mv_string("(seq (run-script \""),
                                    filename,
                                    mv_string("\") :unit)"));
        const char* expected = "123456789";
        TEST_STDOUT((char*)to_run.bytes);
        set_std_current_allocator(current_old);
        reset_arena_allocator(arena);
    }

    if (test_start(log, mv_string("load-module"))) {
        String filename = string_cat(get_tmpdir(&arena), mv_string("/module.rl"), &arena);
        File *file = open_file(filename, Read | Write, &arena);
        const char contents[] = "(module test (import (core :all)) (export x)) (def x 3)";
        U8Array data = (U8Array) {
            .len = sizeof(contents),
            .data = (uint8_t*)contents,
        };
        write_chunk(file, data);
        close_file(file);
        Allocator current_old = get_std_current_allocator();
        set_std_current_allocator(arena);
        String to_run = string_ncat(&arena, 3,
                                    mv_string("(seq (load-module \""),
                                    filename,
                                    mv_string("\") :unit)"));
        RUN((char*)to_run.bytes);
        int64_t expected = 3;
        TEST_EQ("test.x");
        set_std_current_allocator(current_old);
        reset_arena_allocator(arena);
    }
    release_arena_allocator(arena);
}
