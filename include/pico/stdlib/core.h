#ifndef __PICO_STDLIB_CORE_H
#define __PICO_STDLIB_CORE_H

#include "pico/values/modular.h"

PiType* get_ptr_type();
PiType* get_list_type();
PiType* get_maybe_type();
PiType* get_pair_type();
PiType* get_either_type();

void add_core_module(Assembler* ass, Package* base, Allocator* a);

#endif
