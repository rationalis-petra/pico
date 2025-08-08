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

Package* set_current_package(Package* current);
Package* get_current_package();

void set_std_istream(IStream* current);

OStream* set_std_ostream(OStream* current);
OStream *get_std_ostream();

Allocator get_std_current_allocator();
Allocator set_std_current_allocator(Allocator al);

Allocator get_std_perm_allocator();
Allocator set_std_perm_allocator(Allocator al);

Allocator get_std_temp_allocator();
Allocator set_std_temp_allocator(Allocator al);

Allocator* get_std_comptime_allocator();
Allocator* set_std_comptime_allocator(Allocator* al);

Allocator* get_std_region_allocator();
Allocator* set_std_region_allocator(Allocator* al);

void add_extra_module(Assembler* ass, Package* base, Allocator* default_allocator, Allocator* a);

#endif
