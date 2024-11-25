#ifndef __PICO_VALUES_STDLIB_H
#define __PICO_VALUES_STDLIB_H

#include "platform/jump.h"
#include "data/stream.h"

#include "pico/values/modular.h"

// Set the default value of dynamic variables
void set_exit_callback(jump_buf* buf);
void set_current_module(Module* current);
void set_current_package(Package* current);
void set_std_istream(IStream* current);
void set_std_ostream(OStream* current);

Package* base_package(Assembler* ass, Allocator* a);

#endif
