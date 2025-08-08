#include "pico/stdlib/meta/submodules.h"

#include "platform/memory/arena.h"
#include "platform/filesystem/filesystem.h"

#include "pico/stdlib/extra.h"

#include "test_pico/stdlib/components.h"
#include "test_pico/helper.h"

#define RUN(str) run_toplevel(str, module, env, log, a); refresh_env(env, a)
#define TEST_EQ(str) test_toplevel_eq(str, &expected, module, env, log, a)
#define TEST_STDOUT(str) test_toplevel_stdout(str, expected, module, env, log, a)

void run_pico_stdlib_meta_refl_tests(TestLog *log, Module* module, Environment* env, Allocator *a) {
    Allocator arena = mk_arena_allocator(16384, a);
    // -----------------------------------------------------
    // 
    //      Struct
    // 
    // -----------------------------------------------------


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
                                    mv_string("(seq (refl.run-script \""),
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
                                    mv_string("(seq (refl.load-module \""),
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
