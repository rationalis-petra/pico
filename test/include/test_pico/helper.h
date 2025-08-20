#ifndef __TEST_PICO_HELPER_H
#define __TEST_PICO_HELPER_H

#include "pico/codegen/codegen.h"
#include "pico/values/modular.h"
#include "pico/binding/environment.h"

#include "test/test_log.h"

typedef struct {
    Environment *env;
    TestLog *log;
    Allocator *a;
    Target target;
} TestContext;

void test_toplevel_eq(const char *string, void *expected, Module *module, TestContext context);
void test_toplevel_stdout(const char *string, const char *expected, Module *module, TestContext context);

void test_typecheck_eq(const char *string, PiType* expected, Environment* env, TestLog* log, Allocator* a);
void run_toplevel(const char *string, Module *module, TestContext context);

void add_import(ImportClauseArray* arr, Allocator* a, size_t len, ...);
void add_import_all(ImportClauseArray* arr, Allocator* a, size_t len, ...);

#endif
