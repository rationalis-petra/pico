#ifndef __TEST_EVAL_COMPONENTS_H
#define __TEST_EVAL_COMPONENTS_H

#include "pico/binding/environment.h"
#include "pico/values/modular.h"
#include "test/test_log.h"

void run_pico_eval_literals_tests(TestLog* log, Module* module, Environment* env, Allocator* a);
void run_pico_eval_proc_tests(TestLog* log, Module* module, Environment* env, Allocator* a);
void run_pico_eval_foreign_adapter_tests(TestLog* log, Module* module, Environment* env,Allocator* a);

#endif
