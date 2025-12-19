#ifndef __PICO_STDLIB_EXTRA_H
#define __PICO_STDLIB_EXTRA_H

#include "platform/memory/region.h"

#include "pico/values/modular.h"

// Hooks
void set_exit_callback(jump_buf* buf);

void add_extra_module(Assembler* ass, Package* base, RegionAllocator* region);

#endif
