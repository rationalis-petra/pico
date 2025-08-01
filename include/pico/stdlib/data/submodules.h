#ifndef __PICO_STDLIB_DATA_SUBMODULES_H
#define __PICO_STDLIB_DATA_SUBMODULES_H

#include "pico/values/modular.h"
#include "pico/codegen/codegen.h"

void add_list_module(Target target, Module* base, Allocator* a);
void add_pair_module(Target target, Module* base, Allocator* a);
void add_maybe_module(Target target, Module* base, Allocator* a);
void add_either_module(Target target, Module* base, Allocator* a);
void add_pointer_module(Target target, Module* base, Allocator* a);

void add_string_module(Target target, Module* base, Allocator* a);

#endif
