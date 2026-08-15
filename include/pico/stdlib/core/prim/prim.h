#ifndef __PICO_STDLIB_CORE_PRIM_H
#define __PICO_STDLIB_CORE_PRIM_H

#include "platform/memory/region.h"

#include "pico/codegen/codegen.h"
#include "pico/values/modular.h"

void add_prim_module(Assembler* ass, Target target, Module* core, RegionAllocator* region);

#endif
