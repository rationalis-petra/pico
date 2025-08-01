#ifndef __PICO_STDLIB_FOREIGN_H
#define __PICO_STDLIB_FOREIGN_H

#include "pico/values/modular.h"
#include "pico/data/error.h"
#include "pico/eval/call.h"

// Compile a definition in the context of a module.
// For example, we may be in the module "i64" and call
// compile_toplevel("(def i64-addable instance Addable [I64] [.zero 0] [.add +])", 
//                  ass, i64, point, a)
// All memory allocated is free'd, 
void compile_toplevel(const char *string, Module *module, Target target, ErrorPoint *final_point, PiErrorPoint *final_pi_point, Allocator *a);

void add_import(ImportClauseArray* arr, Allocator* a, size_t len, ...);
void add_import_all(ImportClauseArray* arr, Allocator* a, size_t len, ...);

#endif
