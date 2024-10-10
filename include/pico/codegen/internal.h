#ifndef __PICO_CODEGEN_INTERNAL_H
#define __PICO_CODEGEN_INTERNAL_H

#include "assembler/assembler.h"

#include "pico/values/values.h"
#include "pico/data/sym_sarr_amap.h"


/* Utility functions shared across code generation  
 */

void backlink_global(Symbol sym, size_t offset, SymSArrAMap* links, Allocator* a);
AsmResult generate_monomorphic_copy(Regname dest, Regname src, size_t size, Assembler* ass, Allocator* a);

#endif
