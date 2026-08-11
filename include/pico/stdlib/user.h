#ifndef __PICO_STDLIB_USER_H
#define __PICO_STDLIB_USER_H

#include "platform/memory/region.h"

#include "pico/values/modular.h"

Package* mk_user_package(Package* base, PiAllocator* module_allocator, RegionAllocator* region);
void add_user_module(Package* base, RegionAllocator* region);

#endif
