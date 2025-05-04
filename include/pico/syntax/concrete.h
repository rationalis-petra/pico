#ifndef __PICO_SYNTAX_CONCRETE_H
#define __PICO_SYNTAX_CONCRETE_H

#include "data/meta/amap_header.h"
#include "data/array.h"
#include "platform/memory/allocator.h"
#include "pretty/document.h"
#include "pico/values/values.h"

/* This file describes the concrete syntax tree of pico. It is manipulated directly by L1 macros.
 */

typedef enum : uint64_t {
    ABool,
    AIntegral,
    AFloating,
    ASymbol,
    AString,
} Atom_t;

// Total value
typedef struct {
    Atom_t type;
    union {
        int64_t int_64;
        double float_64;
        Symbol symbol;
        String string;
    };
} Atom;

typedef enum : uint64_t {
    HNone,
    HExpression,
    HSpecial,
    HImplicit,
} SyntaxHint;

typedef struct {
    SyntaxHint hint;
    PtrArray nodes;
} Branch;

AMAP_HEADER(Symbol, Atom, sym_atom, SymAtom)

typedef enum : uint64_t {
    RawAtom,
    RawBranch,
} RawTree_t;


typedef struct {
    RawTree_t type;
    union {
        Atom atom;
        Branch branch;
    };
} RawTree;

Document* pretty_rawtree(RawTree tree, Allocator* a);
void delete_rawtree(RawTree tree, Allocator* a);
void delete_rawtree_ptr(RawTree* tree_ptr, Allocator* a);

Document* pretty_atom(Atom val, Allocator* a);
#endif
