#ifndef __PICO_SYNTAX_CONCRETE_H
#define __PICO_SYNTAX_CONCRETE_H

#include "data/array.h"
#include "memory/allocator.h"
#include "pretty/document.h"
#include "pico/values/values.h"

/* This file describes the concrete syntax tree of pico. It is manipulated directly by L1 macros.
 * 
 */

typedef enum pi_atom_t {
    AI64,
    ASymbol,
} pi_atom_t;

// Total value
typedef struct pi_atom {
    pi_atom_t type;
    union {
        int64_t int_64;
        pi_symbol symbol;
    };
} pi_atom;

AMAP_HEADER(pi_symbol, pi_atom, sym_atom)


typedef enum pi_rawtype {
    RawList,
    RawAtom
} pi_rawtype;

typedef struct pi_rawtree {
    pi_rawtype type;
    union {
        pi_atom atom;
        ptr_array nodes;
    } data;
} pi_rawtree;

document* pretty_rawtree(pi_rawtree tree, allocator a);
void delete_rawtree(pi_rawtree tree, allocator a);
void delete_rawtree_ptr(pi_rawtree* tree_ptr, allocator a);

document* pretty_atom(pi_atom val, allocator a);
#endif
