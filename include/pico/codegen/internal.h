#ifndef __PICO_CODEGEN_INTERNAL_H
#define __PICO_CODEGEN_INTERNAL_H

#include "data/meta/array_header.h"
#include "data/array.h"
#include "assembler/assembler.h"

#include "pico/data/sym_sarr_amap.h"
#include "pico/syntax/syntax.h"
#include "pico/values/values.h"

#include "pico/codegen/codegen.h"

/* Utility functions shared across code generation */

typedef struct {
    size_t offset;
    Syntax* expr;
} ToGenerate;

ARRAY_HEADER(ToGenerate, to_gen, ToGen);

typedef struct {
    //SymSArrAMap backlinks;
    SymSArrAMap gotolinks;
    LinkData links;
    //ToGenArray to_generate;
    
    /* U8Array bytes; */
    /* LinkMetaArray data_meta; */
    /* LinkMetaArray code_meta; */
} InternalLinkData;

void backlink_global(Symbol sym, size_t offset, InternalLinkData* links, Allocator* a);
void backlink_goto(Symbol sym, size_t offset, InternalLinkData* links, Allocator* a);

void generate_monomorphic_copy(Regname dest, Regname src, size_t size, Assembler* ass, Allocator* a, ErrorPoint* point);
void generate_monomorphic_swap(Regname loc1, Regname loc2, size_t size, Assembler* ass, Allocator* a, ErrorPoint* point);

// Wrappers around c functions
void generate_tmp_malloc(Location dest, Location mem_size, Assembler* ass, Allocator* a, ErrorPoint* point);

void gen_mk_family_app(size_t nfields, Assembler* ass, Allocator* a, ErrorPoint* point);

void gen_mk_proc_ty(Location dest, Location nfields, Location data, Location ret, Assembler* ass, Allocator* a, ErrorPoint* point);
void gen_mk_struct_ty(Location dest, Location nfields, Location data, Assembler* ass, Allocator* a, ErrorPoint* point);
void gen_mk_enum_ty(Location dest, SynEnumType shape, Location data, Assembler* ass, Allocator* a, ErrorPoint* point);
void gen_mk_reset_ty(Assembler* ass, Allocator* a, ErrorPoint* point);
void gen_mk_dynamic_ty(Assembler* ass, Allocator* a, ErrorPoint* point);

void gen_mk_type_var(Symbol var, Assembler* ass, Allocator* a, ErrorPoint* point);
void gen_mk_forall_ty(SymbolArray syms, Assembler* ass, Allocator* a, ErrorPoint* point);
void gen_mk_fam_ty(SymbolArray syms, Assembler* ass, Allocator* a, ErrorPoint* point);
void gen_mk_distinct_ty(Assembler* ass, Allocator* a, ErrorPoint* point);
void gen_mk_opaque_ty(Assembler* ass, Allocator* a, ErrorPoint* point);
void gen_mk_trait_ty(SymbolArray syms, Location dest, Location nfields, Location data, Assembler* ass, Allocator* a, ErrorPoint* point);


#endif
