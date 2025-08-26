#ifndef __PICO_CODEGEN_BACKEND_DIRECT_FOREIGN_ADAPTERS_H
#define __PICO_CODEGEN_BACKEND_DIRECT_FOREIGN_ADAPTERS_H

#include "components/assembler/assembler.h"
#include "pico/values/types.h"
#include "pico/values/ctypes.h"

bool bd_can_convert(CType* ctype, PiType* ptype); 

void bd_convert_c_fn(void* cfn, CType* ctype, PiType* ptype, Assembler* ass, Allocator* a, ErrorPoint* point); 

bool bd_can_reinterpret(CType* ctype, PiType* ptype); 

#endif
