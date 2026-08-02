#ifndef __PICO_STDLIB_DATA_DATA_H
#define __PICO_STDLIB_DATA_DATA_H

#include "platform/memory/region.h"

#include "pico/values/modular.h"
#include "pico/codegen/codegen.h"

void add_data_module(Assembler* ass, Target target, Package* base, RegionAllocator* region);
#endif
