#ifndef __TEST_PIPELINE_HELPER_H
#define __TEST_PIPELINE_HELPER_H

#include "pico/values/modular.h"

#include "test/test_log.h"

// Compile a definition in the context of a module.
// For example, we may be in the module "i64" and call
// compile_toplevel("(def i64-addable instance Addable [I64] [.zero 0] [.add +])", 
//                  ass, i64, point, a)
// All memory allocated is free'd,
void test_toplevel(const char *string, void *expected_val, Module *module, TestLog* log, Allocator *a);

void add_import(ImportClauseArray* arr, Allocator* a, size_t len, ...);
void add_import_all(ImportClauseArray* arr, Allocator* a, size_t len, ...);

#endif
