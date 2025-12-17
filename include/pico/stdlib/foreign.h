#ifndef __PICO_STDLIB_FOREIGN_H
#define __PICO_STDLIB_FOREIGN_H

#include "pico/values/modular.h"

PiType* get_c_type();
void add_foreign_module(Assembler* ass, Package *base, RegionAllocator* region);

#endif
