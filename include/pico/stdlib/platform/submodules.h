#ifndef __PICO_STDLIB_PLATFORM_SUBMODULES_H
#define __PICO_STDLIB_PLATFORM_SUBMODULES_H

#include "platform/memory/region.h"

#include "data/stream.h"
#include "pico/values/modular.h"

PiType* get_window_ty();

IStream* set_std_istream(IStream* current);
IStream* get_std_istream();

OStream* set_std_ostream(OStream* current);
OStream* get_std_ostream();

// Allocators from the memory module.
PiAllocator get_std_current_allocator();
PiAllocator set_std_current_allocator(PiAllocator al);

PiAllocator get_std_perm_allocator();
PiAllocator set_std_perm_allocator(PiAllocator al);

PiAllocator get_std_temp_allocator();
PiAllocator set_std_temp_allocator(PiAllocator al);

PiAllocator get_std_comptime_allocator();
PiAllocator set_std_comptime_allocator(PiAllocator al);

PiAllocator get_std_region_allocator();
PiAllocator set_std_region_allocator(PiAllocator al);

// Computation
// commented out modules are yet to be implemented
//void add_machine_info_module(Assembler* ass, Module* platform, Allocator* a);
void add_hedron_module(Assembler* ass, Module* platform, RegionAllocator* region);
void add_platform_memory_module(Assembler* ass, Module* platform, Allocator* default_allocator, RegionAllocator* region);

// IO (terminal, filesystem)
// commented out modules are yet to be implemented
void add_terminal_module(Assembler* ass, Module* platform, RegionAllocator* region);
void add_filesystem_module(Assembler* ass, Module* platform, RegionAllocator* region);
void add_window_module(Assembler* ass, Module* platform, RegionAllocator* region);
void add_time_module(Assembler* ass, Module* platform, RegionAllocator* region);

#endif
