#ifndef __PICO_SYNTAX_CONCRETE_H
#define __PICO_SYNTAX_CONCRETE_H

#include "data/meta/amap_header.h"
#include "pico/data/client/meta/list_header.h"
#include "platform/memory/allocator.h"
#include "components/pretty/document.h"

#include "pico/data/range.h"
#include "pico/values/values.h"
#include "pico/values/types.h"

/* This file describes the concrete syntax tree of pico. It is manipulated directly by L1 macros.
 */

typedef enum : uint64_t {
    ABool,
    AIntegral,
    AFloating,
    ASymbol,
    AString,
    ACapture,
} Atom_t;

typedef struct  {
    PiType* type;
    void* value;
} Capture;

// Total value
typedef struct {
    Atom_t type;
    union {
        bool boolean;
        int64_t int_64;
        double float_64;
        Symbol symbol;
        String string;
        Capture capture;
    };
} Atom;

typedef enum : uint64_t {
    HExpression,
    HSpecial,
    HImplicit,
    HData,
} SyntaxHint;

typedef struct RawTree RawTree;
PICO_LIST_HEADER(RawTree, rawtree, RawTree);

typedef struct {
    SyntaxHint hint;
    RawTreePiList nodes;
} Branch;

AMAP_HEADER(Symbol, Atom, sym_atom, SymAtom)

typedef enum : uint64_t {
    RawAtom,
    RawBranch,
} RawTree_t;

struct RawTree {
    RawTree_t type;
    Range range;
    union {
        Atom atom;
        Branch branch;
    };
};

bool is_expr(RawTree tree);
Document* pretty_rawtree(RawTree tree, Allocator* a);
void delete_rawtree(RawTree tree, Allocator* a);
void delete_rawtree_ptr(RawTree* tree_ptr, Allocator* a);

Document* pretty_atom(Atom val, Allocator* a);
#endif
