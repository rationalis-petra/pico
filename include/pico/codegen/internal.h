#ifndef __PICO_CODEGEN_INTERNAL_H
#define __PICO_CODEGEN_INTERNAL_H

#include "data/meta/array_header.h"
#include "data/array.h"
#include "assembler/assembler.h"

#include "pico/data/sym_sarr_amap.h"
#include "pico/syntax/syntax.h"
#include "pico/values/values.h"

#include "pico/codegen/codegen.h"


/* Utility functions shared across code generation  
 */

typedef struct {
    size_t offset;
    Syntax* expr;
} ToGenerate;

ARRAY_HEADER(ToGenerate, to_gen, ToGen);

typedef struct {
    SymSArrAMap backlinks;
    SymSArrAMap gotolinks;
    ToGenArray to_generate;
    
    U8Array bytes;
    LinkMetaArray data_meta;
    LinkMetaArray code_meta;
} LinkData;

void backlink_global(Symbol sym, size_t offset, LinkData* links, Allocator* a);
void backlink_goto(Symbol sym, size_t offset, LinkData* links, Allocator* a);

void generate_monomorphic_copy(Regname dest, Regname src, size_t size, Assembler* ass, Allocator* a, ErrorPoint* point);
void generate_monomorphic_swap(Regname loc1, Regname loc2, size_t size, Assembler* ass, Allocator* a, ErrorPoint* point);

#endif
