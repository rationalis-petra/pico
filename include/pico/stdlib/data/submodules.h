#ifndef __PICO_STDLIB_DATA_SUBMODULES_H
#define __PICO_STDLIB_DATA_SUBMODULES_H

#include "pico/values/modular.h"

void add_list_module(Module* base, Allocator* a);
void add_pair_module(Assembler* ass, Module* base, Allocator* a);
void add_maybe_module(Assembler* ass, Module* base, Allocator* a);
void add_either_module(Assembler* ass, Module* base, Allocator* a);
void add_pointer_module(Assembler* ass, Module* base, Allocator* a);

void add_string_module(Module* base, Allocator* a);

#endif
