#ifndef __PICO_STDLIB_STDLIB_H
#define __PICO_STDLIB_STDLIB_H

#include "platform/memory/region.h"

#include "pico/values/modular.h"

Package* base_package(Assembler* ass, Allocator* default_allocator, PiAllocator* module_allocator, RegionAllocator* region);
Package* get_base_package();

/**
 * The following are for testing ONLY. These functions exist so that the core
 * functionality can be tested BEFORE the data-modules are initialized.
 */
Package* base_package_core_only(Assembler* ass, Allocator* default_allocator, PiAllocator* module_allocator, RegionAllocator* region);
void base_package_fillout_stdlib(Package* base, Assembler* ass, Allocator* default_allocator, PiAllocator* module_allocator, RegionAllocator* region);

#endif
