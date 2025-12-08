#ifndef __PICO_STDLIB_USER_H
#define __PICO_STDLIB_USER_H

#include "platform/memory/region.h"

#include "pico/values/modular.h"

void add_user_module(Package* base, PiAllocator* module_allocator, RegionAllocator* region);

#endif
