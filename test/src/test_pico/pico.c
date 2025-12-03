#include "platform/memory/executable.h"
#include "platform/memory/std_allocator.h"
#include "components/assembler/assembler.h"

#include "pico/codegen/codegen.h"
#include "pico/stdlib/stdlib.h"
#include "pico/stdlib/meta/meta.h"

#include "test_pico/parse/parse.h"
#include "test_pico/stdlib/stdlib.h"
#include "test_pico/eval/eval.h"
#include "test_pico/typecheck.h"

#include "test_pico/pico.h"


static Package* base = NULL;
void run_pico_tests(TestLog* log, Allocator* a) {
    Allocator* stdalloc = get_std_allocator();
    Allocator exec = mk_executable_allocator(stdalloc);
    RegionAllocator* region = make_region_allocator(16384, true, stdalloc);

    if (!base) {
        RegionAllocator* subregion = make_subregion(region); 
        Assembler* ass_base = mk_assembler(current_cpu_feature_flags(), &exec);
        PiAllocator module_allocator = convert_to_pallocator(stdalloc);

        base = base_package(ass_base, stdalloc, &module_allocator, subregion);

        delete_assembler(ass_base);
        release_subregion(subregion);
    }

    Target target = (Target) {
        .target = mk_assembler(current_cpu_feature_flags(), &exec),
        .code_aux = mk_assembler(current_cpu_feature_flags(), &exec),
        .data_aux = mem_alloc(sizeof(U8Array), a),
    };
    *target.data_aux = mk_u8_array(256, a);

    Module* module = get_module(string_to_symbol(mv_string("user")), base);

    set_std_current_module(module);
    set_current_package(base);

    if (suite_start(log, mv_string("parse"))) {
        run_pico_parse_tests(log, a);
        suite_end(log);
    }

    if (suite_start(log, mv_string("typecheck"))) {
        RegionAllocator* subregion = make_subregion(region);
        run_pico_typecheck_tests(log, target, subregion);
        release_subregion(subregion);
        suite_end(log);
    }

    if (suite_start(log, mv_string("eval"))) {
        RegionAllocator* subregion = make_subregion(region);
        run_pico_eval_tests(log, target, subregion);
        release_subregion(subregion);
        suite_end(log);
    }

    if (suite_start(log, mv_string("stdlib"))) {
        run_pico_stdlib_tests(log, target, stdalloc);
        suite_end(log);
    }

    sdelete_u8_array(*target.data_aux);
    mem_free(target.data_aux, a);
    release_executable_allocator(exec);
}
