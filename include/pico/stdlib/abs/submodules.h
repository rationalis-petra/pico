#ifndef __PICO_STDLIB_ABS_SUBMODULES_H
#define __PICO_STDLIB_ABS_SUBMODULES_H

#include "pico/values/modular.h"
#include "pico/codegen/codegen.h"

void add_numeric_module(Target target, Module *abs, Allocator* a);
void add_show_module(Target target, Module *abs, Allocator* a);

#endif
