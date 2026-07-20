#ifndef __TEST_PICO_HELPER_H
#define __TEST_PICO_HELPER_H

#include "platform/memory/region.h"

#include "pico/codegen/codegen.h"
#include "pico/values/modular.h"
#include "pico/binding/environment.h"

#include "test/test_log.h"

typedef struct {
    Environment *env;
    TestLog *log;
    RegionAllocator *region;
    PiAllocator *pia;
    Target target;
} TestContext;

void test_toplevel_eq(const char *string, void *expected, Module *module, TestContext context);
void assert_toplevel_eq(const char *string, void *expected, Module *module, TestContext context);

void test_toplevel_stdout(const char *string, const char *expected, Module *module, TestContext context);
void assert_toplevel_stdout(const char *string, const char *expected, Module *module, TestContext context);

void test_toplevel_mem(const char *string, const void *exepcted, const void*actual, size_t memsize, Module *module, TestContext context);
void assert_toplevel_mem(const char *string, const void *exepcted, const void*actual, size_t memsize, Module *module, TestContext context);

void test_typecheck_eq(const char *string, PiType* expected, Environment* env, TestContext context);
void test_typecheck_fail(const char *string, Environment* env, TestContext context);

void test_abstract_fail(const char *string, Environment* env, TestContext context);

void run_toplevel(const char *string, Module *module, TestContext context);
void module_from_string(const char *string, Module *module, TestContext context);

void add_import(ImportClauseArray* arr, Allocator* a, size_t len, ...);
void add_import_all(ImportClauseArray* arr, Allocator* a, size_t len, ...);

#define RUN(str) run_toplevel(str, module, context); refresh_env(env); clear_logger(log);
#define MODULE(str) module_from_string(str, module, context); refresh_env(env); clear_logger(log);

#define TEST_EQ(str) test_toplevel_eq(str, &expected, module, context)
#define TEST_STDOUT(str) test_toplevel_stdout(str, expected, module, context)
#define TEST_MEM(str) test_toplevel_mem(str, &expected, start, sizeof(expected), module, context)

#define TEST_TYPE(str) test_typecheck_eq(str, expected, env, context)
#define TEST_TYPE_FAIL(str) test_typecheck_fail(str, env, context)
#define TEST_ABSTRACT_FAIL(str) test_abstract_fail(str, env, context)

#endif
