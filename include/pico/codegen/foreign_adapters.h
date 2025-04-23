#ifndef __PICO_CODEGEN_FOREIGN_ADAPTERS_H
#define __PICO_CODEGEN_FOREIGN_ADAPTERS_H

#include "assembler/assembler.h"
#include "pico/values/types.h"
#include "pico/values/ctypes.h"

bool can_convert(CType* ctype, PiType* ptype); 
void convert_c_fn(void* cfn, CType* ctype, PiType* ptype, Assembler* ass, Allocator* a, ErrorPoint* point); 

bool can_reinterpret(CType* ctype, PiType* ptype); 

#endif
