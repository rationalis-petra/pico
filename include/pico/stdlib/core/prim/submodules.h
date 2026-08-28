#ifndef __PICO_STDLIB_CORE_PRIM_SUBMODULES_H
#define __PICO_STDLIB_CORE_PRIM_SUBMODULES_H

#include "platform/memory/region.h"

#include "pico/codegen/codegen.h"
#include "pico/values/modular.h"

void add_prim_integral_module(LocationSize sz, bool is_signed, Assembler* ass, Target target, Module* prim, RegionAllocator* a);
void add_prim_float_module(PrimType type, Assembler* ass, Target target, Module* prim, RegionAllocator* a);
void add_prim_bool_module(Assembler *ass, Target target, Module* prim, RegionAllocator* a);
void add_prim_address_module(Assembler *ass, Module* prim, RegionAllocator* a);
void add_prim_array_module(Assembler *ass, Module* prim, RegionAllocator* a);

#endif
