#ifndef __PICO_STDLIB_EXTRA_H
#define __PICO_STDLIB_EXTRA_H

#include "data/stream.h"
#include "pico/values/modular.h"

// Hooks
void set_exit_callback(jump_buf* buf);

// Set the default value of dynamic variables

Allocator* get_std_allocator();
Module* get_std_current_module();
Module* set_std_current_module(Module* al);

void set_current_package(Package* current);
void set_std_istream(IStream* current);
void set_std_ostream(OStream* current);

Allocator* get_std_tmp_allocator();
Allocator* set_std_tmp_allocator(Allocator* al);

void add_extra_module(Assembler* ass, Package* base, Allocator* default_allocator, Allocator* a);

#endif
