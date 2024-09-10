#ifndef __PICO_SYNTAX_CONCRETE_H
#define __PICO_SYNTAX_CONCRETE_H

#include "data/meta/amap_header.h"
#include "data/array.h"
#include "memory/allocator.h"
#include "pretty/document.h"
#include "pico/values/values.h"

/* This file describes the concrete syntax tree of pico. It is manipulated directly by L1 macros.
 * 
 */

typedef enum Atom_t {
    ABool,
    AI64,
    ASymbol,
} Atom_t;

// Total value
typedef struct Atom {
    Atom_t type;
    union {
        int64_t int_64;
        Symbol symbol;
    };
} Atom;

AMAP_HEADER(Symbol, Atom, sym_atom, SymAtom)

typedef enum RawTree_t {
    RawList,
    RawAtom
} RawTree_t;

typedef struct RawTree {
    RawTree_t type;
    union {
        Atom atom;
        PtrArray nodes;
    } data;
} RawTree;

Document* pretty_rawtree(RawTree tree, Allocator* a);
void delete_rawtree(RawTree tree, Allocator* a);
void delete_rawtree_ptr(RawTree* tree_ptr, Allocator* a);

Document* pretty_atom(Atom val, Allocator* a);
#endif
