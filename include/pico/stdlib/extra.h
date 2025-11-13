#ifndef __PICO_STDLIB_EXTRA_H
#define __PICO_STDLIB_EXTRA_H

#include "data/stream.h"
#include "pico/data/client/allocator.h"
#include "pico/values/modular.h"

// Hooks
void set_exit_callback(jump_buf* buf);

// Set the default value of dynamic variables

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

void add_extra_module(Assembler* ass, Package* base, Allocator* default_allocator, Allocator* a);

#endif
