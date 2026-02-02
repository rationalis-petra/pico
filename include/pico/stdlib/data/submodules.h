#ifndef __PICO_STDLIB_DATA_SUBMODULES_H
#define __PICO_STDLIB_DATA_SUBMODULES_H

#include "platform/memory/region.h"

#include "pico/values/modular.h"
#include "pico/codegen/codegen.h"

void add_pointer_module(Target target, Module* data, RegionAllocator* region);
void add_allocators_module(Assembler* ass, Module* data, RegionAllocator* region);

void add_pair_module(Target target, Module* data, RegionAllocator* region);
void add_maybe_module(Target target, Module* data, RegionAllocator* region);
void add_either_module(Target target, Module* data, RegionAllocator* region);
void add_result_module(Target target, Module* data, RegionAllocator* region);

void add_list_module(Target target, Module* data, RegionAllocator* region);
void add_string_module(Target target, Module* data, RegionAllocator* region);

#endif
