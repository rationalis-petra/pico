#ifndef __ATLAS_SYNTAX_CONCRETE_H
#define __ATLAS_SYNTAX_CONCRETE_H

#include <stdint.h>

#include "pico/data/range.h"
#include "pico/values/values.h"
#include "data/meta/array_header.h"

typedef enum : uint64_t {
    AtVersion,
    AtSymbol,
    AtKeyword,
    AtString,
} AtAtom_t;

typedef struct {
    uint16_t major;
    uint16_t minor;
    uint16_t patch;
} Version;

typedef struct {
    AtAtom_t type;
    union {
        Version version;
        Symbol symbol;
        Symbol keyword;
        String string;
    };
} AtAtom;

typedef enum : uint64_t {
    AtlAtom,
    AtlBranch,
} RawAtlas_t;

typedef struct RawAtlas RawAtlas;
ARRAY_HEADER(RawAtlas, rawatlas, RawAtlas);

struct RawAtlas {
    RawAtlas_t type;
    Range range;
    union {
        AtAtom atom;
        RawAtlasArray branch;
    };
};

Document* pretty_rawatlas(RawAtlas atlas, Allocator* a);
Document* pretty_atlas_atom(AtAtom val, Allocator* a);

#endif
