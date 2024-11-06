#ifndef __PICO_CODEGEN_CODEGEN_H
#define __PICO_CODEGEN_CODEGEN_H

#include "data/meta/array_header.h"

#include "assembler/assembler.h"

#include "pico/binding/environment.h"
#include "pico/syntax/syntax.h"
#include "pico/data/sym_sarr_amap.h"

/* Brainstorm for side-allocations
 * Two extra bits: 
 *  + Data storage for string/array literals
 *  + Data storage for procedures/functions
 * We also need to relate these "Side allocations" for definitions/storage
 * Thus, for each, of data/code store an array of:
 *  (origin: bool, origin_offset: size_t, array_index: size_t)
 */

typedef struct {
    size_t origin_offset;
    size_t data_index;
} LinkMetaData;

ARRAY_HEADER(LinkMetaData, link_meta, LinkMeta)

typedef struct {
    SymSArrAMap backlinks;

    LinkMetaArray code_metadata;
    LinkMetaArray data_metadata;
} GenResult;

GenResult generate_toplevel(TopLevel top, Environment* env, Assembler* ass, Allocator* a, ErrorPoint* point);

GenResult generate_expr(Syntax* syn, Environment* env, Assembler* ass, Allocator* a, ErrorPoint* point);

#endif
