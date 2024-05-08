#ifndef __PICO_SYNTAX_CONCRETE_H
#define __PICO_SYNTAX_CONCRETE_H

#include "data/array.h"
#include "memory/allocator.h"
#include "pretty/document.h"
#include "pico/values/values.h"

/* This file describes the concrete syntax tree of pico. It is manipulated directly by L1 macros.
 * 
 */

typedef enum ob_rawtype {
    RawList,
    RawAtom
} ob_rawtype;

typedef struct ob_rawtree {
    ob_rawtype type;
    union {
        ob_value value;
        ptr_array nodes;
    } data;
} ob_rawtree;

document* pretty_rawtree(ob_rawtree tree, allocator a);
void delete_rawtree(ob_rawtree tree, allocator a);
void delete_rawtree_ptr(ob_rawtree* tree_ptr, allocator a);

#endif
