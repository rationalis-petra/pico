#ifndef __PICO_VALUES_STDLIB_H
#define __PICO_VALUES_STDLIB_H

#include "platform/jump.h"
#include "data/stream.h"

#include "pico/values/modular.h"

// Hooks
void set_exit_callback(jump_buf* buf);

// Set the default value of dynamic variables

Module* get_std_current_module();
Module* set_std_current_module(Module* al);

void set_current_package(Package* current);
void set_std_istream(IStream* current);
void set_std_ostream(OStream* current);

PiType* get_syntax_type();
PiType* get_array_type();

Allocator* get_std_tmp_allocator();
Allocator* set_std_tmp_allocator(Allocator* al);

Allocator* get_std_allocator();

Package* base_package(Assembler* ass, Allocator* default_allocator, Allocator* a);

#endif
