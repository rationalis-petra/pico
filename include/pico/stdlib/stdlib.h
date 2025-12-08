#ifndef __PICO_STDLIB_STDLIB_H
#define __PICO_STDLIB_STDLIB_H

#include "platform/memory/region.h"

#include "pico/values/modular.h"

Package* base_package(Assembler* ass, Allocator* default_allocator, PiAllocator* module_allocator, RegionAllocator* region);
Package* get_base_package();

#endif
