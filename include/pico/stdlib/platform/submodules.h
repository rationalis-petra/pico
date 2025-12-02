#ifndef __PICO_STDLIB_PLATFORM_SUBMODULES_H
#define __PICO_STDLIB_PLATFORM_SUBMODULES_H

#include "data/stream.h"
#include "pico/values/modular.h"

PiType* get_window_ty();

IStream* set_std_istream(IStream* current);
IStream* get_std_istream();

OStream* set_std_ostream(OStream* current);
OStream* get_std_ostream();

// Computation
void add_memory_module(Assembler* ass, Module* platform, Allocator* a);
// commented out modules are yet to be implemented
//void add_profiling_module(Assembler* ass, Module* platform, Allocator* a);
//void add_machine_info_module(Assembler* ass, Module* platform, Allocator* a);
void add_hedron_module(Assembler* ass, Module* platform, Allocator* a);

// IO (terminal, filesystem)
// commented out modules are yet to be implemented
//void add_time_module(Assembler* ass, Module* platform, Allocator* a);
void add_terminal_module(Assembler* ass, Module* platform, Allocator* a);
void add_filesystem_module(Assembler* ass, Module* platform, Allocator* a);
void add_window_module(Assembler* ass, Module* platform, Allocator* a);
void add_time_module(Assembler* ass, Module* platform, Allocator* a);
void add_time_module(Assembler* ass, Module* platform, Allocator* a);

#endif
