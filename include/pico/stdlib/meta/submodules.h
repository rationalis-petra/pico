#ifndef __PICO_STDLIB_META_SUBMODULES_H
#define __PICO_STDLIB_META_SUBMODULES_H

#include "pico/values/modular.h"
#include "pico/codegen/codegen.h"

void add_refl_module(Assembler* ass, Module* base, Allocator* a);
void add_gen_module(Assembler* ass, Module* base, Allocator* a);

#endif
