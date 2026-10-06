#ifndef __TEST_RUNE_H
#define __TEST_RUNE_H

#include "platform/memory/region.h"
#include "test/test_log.h"
#include "rune/eval/eval.h"

void run_rune_tests(TestLog* log, Allocator* a);

void run_rune_eval_tests(TestLog* log, RuneEnv* env, RegionAllocator* a);
void run_rune_builtin_tests(TestLog* log, RuneEnv* env, RegionAllocator* a);


#endif
