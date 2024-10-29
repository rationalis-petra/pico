#ifndef __PICO_CODEGEN_INTERNAL_H
#define __PICO_CODEGEN_INTERNAL_H

#include "assembler/assembler.h"

#include "pico/values/values.h"
#include "pico/data/sym_sarr_amap.h"


/* Utility functions shared across code generation  
 */

typedef struct {
    SymSArrAMap backlinks;
    SymSArrAMap gotolinks;
} LinkData;

void backlink_global(Symbol sym, size_t offset, LinkData* links, Allocator* a);
void backlink_goto(Symbol sym, size_t offset, LinkData* links, Allocator* a);

void generate_monomorphic_copy(Regname dest, Regname src, size_t size, Assembler* ass, Allocator* a, ErrorPoint* point);

#endif
