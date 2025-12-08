#ifndef __PICO_STDLIB_DEBUG_H
#define __PICO_STDLIB_DEBUG_H

#include "pico/codegen/codegen.h"
#include "pico/values/modular.h"

void add_debug_module(Target target, Package* base, PiAllocator* module_allocator, RegionAllocator* region);

#endif
