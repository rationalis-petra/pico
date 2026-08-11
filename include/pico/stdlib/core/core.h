#ifndef __PICO_STDLIB_CORE_H
#define __PICO_STDLIB_CORE_H

#include "platform/memory/region.h"

#include "pico/codegen/codegen.h"
#include "pico/values/modular.h"

void add_core_module(Assembler* ass, Target target, Package* base, RegionAllocator* region);

#endif
