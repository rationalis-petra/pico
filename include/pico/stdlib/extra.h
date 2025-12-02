#ifndef __PICO_STDLIB_EXTRA_H
#define __PICO_STDLIB_EXTRA_H

#include "data/stream.h"
#include "pico/data/client/allocator.h"
#include "pico/values/modular.h"

// Hooks
void set_exit_callback(jump_buf* buf);

// Set the default value of dynamic variables

void add_extra_module(Assembler* ass, Package* base, Allocator* a);

#endif
