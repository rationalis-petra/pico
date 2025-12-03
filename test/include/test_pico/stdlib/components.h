#ifndef __TEST_STDLIB_COMPONENTS_H
#define __TEST_STDLIB_COMPONENTS_H

#include "platform/memory/region.h"

#include "pico/codegen/codegen.h"
#include "pico/values/modular.h"
#include "pico/binding/environment.h"

#include "test/test_log.h"

void run_pico_stdlib_core_type_tests(TestLog* log, Module* module, Environment* env, Target target, RegionAllocator* region);
void run_pico_stdlib_core_tests(TestLog* log, Module* module, Environment* env, Target target, RegionAllocator* region);

void run_pico_stdlib_num_tests(TestLog* log, Module* module, Environment* env, Target target, RegionAllocator* region);
void run_pico_stdlib_extra_tests(TestLog* log, Module* module, Environment* env, Target target, RegionAllocator* region);

void run_pico_stdlib_meta_tests(TestLog* log, Module* module, Environment* env, Target target, RegionAllocator* region);
void run_pico_stdlib_meta_refl_tests(TestLog* log, Module* module, Environment* env, Target target, RegionAllocator* region);
void run_pico_stdlib_meta_gen_tests(TestLog* log, Module* module, Environment* env, Target target, RegionAllocator* region);

void run_pico_stdlib_abs_tests(TestLog* log, Module* module, Environment* env, Target target, RegionAllocator* region);
void run_pico_stdlib_abs_numeric_tests(TestLog* log, Module* module, Environment* env, Target target, RegionAllocator* region);

void run_pico_stdlib_data_tests(TestLog* log, Module* module, Environment* env, Target target, RegionAllocator* region);
void run_pico_stdlib_data_list_tests(TestLog* log, Module* module, Environment* env, Target target, RegionAllocator* region);
void run_pico_stdlib_data_pair_tests(TestLog* log, Module* module, Environment* env, Target target, RegionAllocator* region);
void run_pico_stdlib_data_either_tests(TestLog* log, Module* module, Environment* env, Target target, RegionAllocator* region);
void run_pico_stdlib_data_ptr_tests(TestLog* log, Module* module, Environment* env, Target target, RegionAllocator* region);

#endif
