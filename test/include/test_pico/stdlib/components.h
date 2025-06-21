#ifndef __TEST_STDLIB_COMPONENTS_H
#define __TEST_STDLIB_COMPONENTS_H

#include "pico/values/modular.h"

#include "test/test_log.h"

void run_pico_stdlib_core_tests(TestLog* log, Module* module, Allocator* a);
void run_pico_stdlib_num_tests(TestLog* log, Module* module, Allocator* a);
void run_pico_stdlib_extra_tests(TestLog* log, Module* module, Allocator* a);

void run_pico_stdlib_data_tests(TestLog* log, Module* module, Allocator* a);
void run_pico_stdlib_data_array_tests(TestLog* log, Module* module, Allocator* a);
void run_pico_stdlib_data_pair_tests(TestLog* log, Module* module, Allocator* a);
void run_pico_stdlib_data_ptr_tests(TestLog* log, Module* module, Allocator* a);

#endif
