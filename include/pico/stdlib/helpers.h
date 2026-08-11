#ifndef __PICO_STDLIB_FOREIGN_H
#define __PICO_STDLIB_FOREIGN_H

#include "platform/memory/region.h"

#include "pico/codegen/codegen.h"
#include "pico/values/modular.h"
#include "pico/data/error.h"

// Compile a definition in the context of a module.
// For example, we may be in the module "i64" and call
// compile_toplevel("(def i64-addable instance Addable [I64] [.zero 0] [.add +])", 
//                  ass, i64, point, a)
// All memory allocated is free'd, 
void compile_toplevel(const char *string, Module *module, Target target, ErrorPoint *final_point, PiErrorPoint *final_pi_point, RegionAllocator* region);
void compile_str_toplevel(String string, Module *module, Target target, ErrorPoint *final_point, PiErrorPoint *final_pi_point, RegionAllocator* region);

void add_import(ImportClauseArray* arr, Allocator* a, size_t len, ...);
void add_import_all(ImportClauseArray* arr, Allocator* a, size_t len, ...);

typedef enum {
  ImportTypes = 0x1,
  ImportInstances = 0x2,
} IFlags;
void add_import_flags(ImportClauseArray* arr, Allocator* a, IFlags flags, size_t len, ...);

PathSegment seg_name(const char* string);
PathSegment seg_wild();

#endif
