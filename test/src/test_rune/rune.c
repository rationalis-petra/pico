#include <string.h>
#include "platform/memory/region.h"
#include "test/test_log.h"

#include "test_rune/helper.h"
#include "test_rune/rune.h"
#include "rune/eval/eval.h"

void run_rune_tests(TestLog* log, Allocator* a) {
    RegionAllocator* region = make_region_allocator(4096, true, a);
    RuneEnv* env = mk_rune_env(a);
    populate_std_builtins(env);

    if (suite_start(log, mv_string("rune"))) {
        if (suite_start(log, mv_string("eval"))) {
            run_rune_eval_tests(log, env, region);
            suite_end(log);
        }
        if (suite_start(log, mv_string("builtins"))) {
            run_rune_builtin_tests(log, env, region);
            suite_end(log);
        }
        suite_end(log);
    }

    delete_rune_env(env);
    delete_region_allocator(region);
}
