#ifndef __PICO_STDLIB_CORE_H
#define __PICO_STDLIB_CORE_H

#include "pico/values/modular.h"

PiType* get_array_type();
PiType* get_syntax_type();

void add_core_module(Assembler* ass, Package* base, Allocator* a);

#endif
