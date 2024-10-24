#ifndef __PICO_VALUES_STDLIB_H
#define __PICO_VALUES_STDLIB_H

#include "pico/values/modular.h"

void set_exit_callback(jmp_buf* buf);

Module* base_module(Assembler* ass, Allocator* a);

#endif
