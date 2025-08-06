#include "platform/memory/executable.h"
#include "components/assembler/assembler.h"

#include "pico/stdlib/stdlib.h"
#include "pico/stdlib/extra.h"

#include "test_pico/parse/parse.h"
#include "test_pico/stdlib/stdlib.h"
#include "test_pico/eval/eval.h"
#include "test_pico/typecheck.h"

#include "test_pico/pico.h"


void run_pico_tests(TestLog* log, Allocator* a) {
    Allocator* stdalloc = get_std_allocator();
    Allocator exalloc = mk_executable_allocator(stdalloc);

    Assembler* ass_base = mk_assembler(current_cpu_feature_flags(), &exalloc);
    Package* base = base_package(ass_base, stdalloc, stdalloc);
    delete_assembler(ass_base);

    Module* module = get_module(string_to_symbol(mv_string("user")), base);

    set_std_current_module(module);
    set_current_package(base);

    if (suite_start(log, mv_string("parse"))) {
        run_pico_parse_tests(log, a);
        suite_end(log);
    }

    if (suite_start(log, mv_string("typecheck"))) {
        run_pico_typecheck_tests(log, a);
        suite_end(log);
    }

    if (suite_start(log, mv_string("eval"))) {
        run_pico_eval_tests(log, a);
        suite_end(log);
    }

    if (suite_start(log, mv_string("stdlib"))) {
        run_pico_stdlib_tests(log, a);
        suite_end(log);
    }

    delete_package(base);
    release_executable_allocator(exalloc);
}
