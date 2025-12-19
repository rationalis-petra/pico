#ifndef __PICO_STDLIB_NUM_SUBMODULES_H
#define __PICO_STDLIB_NUM_SUBMODULES_H

#include "platform/memory/region.h"

#include "pico/values/modular.h"

void add_refl_module(Assembler* ass, Module* base, RegionAllocator* region);
void add_gen_module(Assembler* ass, Module* base, RegionAllocator* region);

#endif
