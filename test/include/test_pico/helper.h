#ifndef __TEST_PICO_HELPER_H
#define __TEST_PICO_HELPER_H

#include "pico/values/modular.h"

#include "test/test_log.h"

void test_toplevel_eq(const char *string, void *expected_val, Module *module, TestLog* log, Allocator *a);
void test_toplevel_stdout(const char *string, const char *expected_stdout, Module *module, TestLog* log, Allocator *a);

void test_typecheck_eq(const char *string, void *expected_val, Module *module, TestLog* log, Allocator *a);
void run_toplevel(const char *string, Module *module, TestLog* log, Allocator *a);

void add_import(ImportClauseArray* arr, Allocator* a, size_t len, ...);
void add_import_all(ImportClauseArray* arr, Allocator* a, size_t len, ...);

#endif
