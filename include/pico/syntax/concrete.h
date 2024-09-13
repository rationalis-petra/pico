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

typedef enum {
    ABool,
    AI64,
    ASymbol,
} Atom_t;

// Total value
typedef struct {
    Atom_t type;
    union {
        int64_t int_64;
        Symbol symbol;
    };
} Atom;

AMAP_HEADER(Symbol, Atom, sym_atom, SymAtom)

typedef enum {
    RawList,
    RawAtom,
} RawTree_t;

typedef enum {
    HNone,
    HExpression,
    HArgList,
} SyntaxHint;

typedef struct {
    RawTree_t type;
    SyntaxHint hint;
    union {
        Atom atom;
        PtrArray nodes;
    };
} RawTree;

Document* pretty_rawtree(RawTree tree, Allocator* a);
void delete_rawtree(RawTree tree, Allocator* a);
void delete_rawtree_ptr(RawTree* tree_ptr, Allocator* a);

Document* pretty_atom(Atom val, Allocator* a);
#endif
