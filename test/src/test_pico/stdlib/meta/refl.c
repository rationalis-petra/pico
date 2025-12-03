#include "platform/memory/arena.h"
#include "platform/filesystem/filesystem.h"

#include "pico/stdlib/platform/submodules.h"

#include "test_pico/stdlib/components.h"
#include "test_pico/helper.h"

#define RUN(str) run_toplevel(str, module, context); refresh_env(env, &gpa)
#define TEST_EQ(str) test_toplevel_eq(str, &expected, module, context)
#define TEST_STDOUT(str) test_toplevel_stdout(str, expected, module, context)

void run_pico_stdlib_meta_refl_tests(TestLog *log, Module* module, Environment* env, Target target, RegionAllocator* region) {
    TestContext context = (TestContext) {
        .env = env,
        .region = region,
        .log = log,
        .target = target,
    };
    Allocator gpa = ra_to_gpa(region);

    // -----------------------------------------------------
    // 
    //      Struct
    // 
    // -----------------------------------------------------


    if (test_start(log, mv_string("run-script"))) {
        String filename = string_cat(get_tmpdir(&gpa), mv_string("/script.rl"), &gpa);
        File *file = open_file(filename, Read | Write, &gpa);
        const char contents[] = "(terminal.write-string (u64.to-string 123456789)) ";
        U8Array data = (U8Array) {
            .len = sizeof(contents),
            .data = (uint8_t*)contents,
        };
        write_chunk(file, data);
        close_file(file);
        PiAllocator current_old = get_std_current_allocator();
        PiAllocator pi_gpa = convert_to_pallocator(&gpa);
        set_std_current_allocator(pi_gpa);
        String to_run = string_ncat(&gpa, 3,
                                    mv_string("(seq (refl.run-script ~\""),
                                    filename,
                                    mv_string("\" :none) :unit)"));
        const char* expected = "123456789";
        TEST_STDOUT((char*)to_run.bytes);
        set_std_current_allocator(current_old);
        reset_subregion(region);
    }

    if (test_start(log, mv_string("load-module"))) {
        String filename = string_cat(get_tmpdir(&gpa), mv_string("/module.rl"), &gpa);
        File *file = open_file(filename, Read | Write, &gpa);
        const char contents[] = "(module test (import (core :all)) (export x)) (def x 3)";
        U8Array data = (U8Array) {
            .len = sizeof(contents),
            .data = (uint8_t*)contents,
        };
        write_chunk(file, data);
        close_file(file);
        // Note: memory should be cleaned up by module being made part of
        //   current module, and therefore deleted with current module.
        String to_run = string_ncat(&gpa, 3,
                                    mv_string("(seq (refl.load-module ~\""),
                                    filename,
                                    mv_string("\" (:some (use refl.current-module))) :unit)"));
        RUN((char*)to_run.bytes);
        int64_t expected = 3;
        TEST_EQ("test.x");
        reset_subregion(region);
    }
}
