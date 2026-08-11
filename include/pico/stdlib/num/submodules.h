#ifndef __PICO_STDLIB_NUM_SUBMODULES_H
#define __PICO_STDLIB_NUM_SUBMODULES_H

#include "platform/memory/region.h"

#include "pico/codegen/codegen.h"
#include "pico/values/modular.h"

void add_integral_module(LocationSize sz, bool is_signed, Assembler* ass, Target target, Module* num, RegionAllocator* a);
void add_float_module(PrimType prim, Assembler* ass, Target target, Module* num, RegionAllocator* a);
void add_bool_module(Assembler *ass, Target target, Module *num, RegionAllocator* a);

#endif
