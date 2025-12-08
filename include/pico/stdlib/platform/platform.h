#ifndef __PICO_STDLIB_PLATFORM_PLATFORM_H
#define __PICO_STDLIB_PLATFORM_PLATFORM_H

#include "platform/memory/region.h"

#include "pico/values/modular.h"

void add_platform_module(Assembler* ass, Package* base, Allocator* default_allocator, PiAllocator* module_allocator, RegionAllocator* region);

#endif
