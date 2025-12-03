#ifndef __PICO_STDLIB_DATA_DATA_H
#define __PICO_STDLIB_DATA_DATA_H

#include "platform/memory/region.h"

#include "pico/values/modular.h"
#include "pico/codegen/codegen.h"

void add_data_module(Target target, Package* base, PiAllocator* module_allocator, RegionAllocator* region);

#endif
