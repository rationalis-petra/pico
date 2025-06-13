#ifndef __PICO_STDLIB_STDLIB_H
#define __PICO_STDLIB_STDLIB_H

#include "pico/values/modular.h"

Package* base_package(Assembler* ass, Allocator* default_allocator, Allocator* a);
Package* get_base_package();

#endif
