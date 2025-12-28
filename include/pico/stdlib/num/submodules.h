#ifndef __PICO_STDLIB_NUM_SUBMODULES_H
#define __PICO_STDLIB_NUM_SUBMODULES_H

#include "pico/values/modular.h"

void add_integral_module(String name, LocationSize sz, bool is_signed, Assembler* ass, Module* num, Allocator* a);
void add_float_module(String name, PrimType prim, Assembler* ass, Module* num, Allocator* a);
void add_bool_module(Assembler *ass, Module *num, Allocator *a);

#endif
