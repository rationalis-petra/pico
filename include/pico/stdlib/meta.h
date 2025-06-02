#ifndef __PICO_STDLIB_META_H
#define __PICO_STDLIB_META_H

#include "pico/values/modular.h"

PiType* get_symbol_type();
PiType* get_syntax_type();

void add_meta_module(Assembler* ass, Package* base, Allocator* a);

#endif
