#ifndef __PICO_STDLIB_PLATFORM_WINDOW_H
#define __PICO_STDLIB_PLATFORM_WINDOW_H

#include "pico/values/modular.h"

PiType* get_window_ty();

void add_window_module(Assembler* ass, Module* platform, Allocator* a);

#endif
