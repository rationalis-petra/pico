#ifndef __PICO_STDLIB_NUM_H
#define __PICO_STDLIB_NUM_H

#include "platform/memory/region.h"

#include "pico/values/modular.h"

void add_num_module(Assembler* ass, Package* base, PiAllocator* module_allocator, RegionAllocator* region);

#endif
