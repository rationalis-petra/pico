#ifndef __ATLAS_SYNTAX_CONCRETE_H
#define __ATLAS_SYNTAX_CONCRETE_H

#include <stdint.h>

#include "pico/data/error.h"
#include "pico/values/values.h"
#include "pico/data/client/meta/list_header.h"

typedef enum : uint64_t {
    AVersion,
    ASymbol
} AtAtom_t;

typedef struct {
    uint16_t major;
    uint16_t minor;
    uint16_t patch;
} Version;

typedef struct {
    AtAtom_t type;
    union {
        int64_t int_64;
        Symbol symbol;
    };
} AtAtom;

typedef enum : uint64_t {
    RawAtom,
    RawBranch,
} RawAtlas_t;

typedef struct RawAtlas RawAtlas;
PICO_LIST_HEADER(RawAtlas, rawatlas, RawAtlas);

struct RawAtlas {
    RawAtlas_t type;
    Range range;
    union {
        AtAtom atom;
        RawAtlasPiList branch;
    };
};

#endif
