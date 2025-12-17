#ifndef __PICO_STDLIB_ABS_H
#define __PICO_STDLIB_ABS_H

#include "platform/memory/region.h"

#include "pico/codegen/codegen.h"
#include "pico/values/modular.h"

void add_abs_module(Target target, Package* base, RegionAllocator* region);

#endif
