#ifndef __PICO_STDLIB_CORE_KERNEL_H
#define __PICO_STDLIB_CORE_KERNEL_H

#include "platform/memory/region.h"

#include "pico/values/modular.h"

PiType* get_ptr_type();
PiType* get_slice_type();
PiType* get_list_type();
PiType* get_maybe_type();
PiType* get_pair_type();
PiType* get_either_type();
PiType* get_result_type();

PiType* get_allocator_vtable_type();
PiType* get_allocator_type();

void add_kernel_module(Module* core, RegionAllocator* region);

#endif
