#ifndef __PICO_STDLIB_PLATFORM_SUBMODULES_H
#define __PICO_STDLIB_PLATFORM_SUBMODULES_H

#include "data/stream.h"
#include "pico/values/modular.h"

PiType* get_window_ty();

IStream* set_std_istream(IStream* current);
IStream* get_std_istream();

OStream* set_std_ostream(OStream* current);
OStream* get_std_ostream();

// IO (terminal, filesystem)
void add_terminal_module(Assembler* ass, Module* platform, Allocator* a);
void add_filesystem_module(Assembler* ass, Module* platform, Allocator* a);
void add_window_module(Assembler* ass, Module* platform, Allocator* a);

// Computation
void add_hedron_module(Assembler* ass, Module* platform, Allocator* a);

#endif
