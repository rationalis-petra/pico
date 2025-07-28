#ifndef __PICO_STDLIB_ABS_SUBMODULES_H
#define __PICO_STDLIB_ABS_SUBMODULES_H

#include "pico/values/modular.h"

void add_numeric_module(Module *abs, Package* base, Allocator* a);
void add_show_module(Module *abs, Package* base, Allocator* a);

#endif
