#ifndef __PICO_STDLIB_DEBUG_H
#define __PICO_STDLIB_DEBUG_H

#include "platform/memory/region.h"

#include "pico/codegen/codegen.h"
#include "pico/values/modular.h"

void add_debug_module(Target target, Package* base, RegionAllocator* region);

#endif
