#ifndef __PICO_STDLIB_PRELUDE_H
#define __PICO_STDLIB_PRELUDE_H

#include "platform/memory/region.h"

#include "pico/values/modular.h"

void add_prelude_module(Package* base, RegionAllocator* region);

#endif
