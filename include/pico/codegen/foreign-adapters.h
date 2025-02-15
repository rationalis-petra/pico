#ifndef __PICO_CODEGEN_FOREIGN_ADAPTERS_H
#define __PICO_CODEGEN_FOREIGN_ADAPTERS_H

#include "assembler/assembler.h"
#include "pico/values/types.h"
#include "pico/values/ctypes.h"

void* convert_c_fn(CType* ctype, PiType* ptype, Assembler* ass, Allocator* a); 

bool can_reinterpret(CType* ctype, PiType* ptype); 

#endif
