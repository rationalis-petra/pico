#ifndef __ATLAS_SYNTAX_STANZA_H
#define __ATLAS_SYNTAX_STANZA_H

#include "data/option.h"
#include "data/string.h"

#include "components/pretty/document.h"

#include "pico/values/values.h"
#include "pico/data/string_array.h"
#include "pico/data/symbol_array.h"
#include "pico/data/range.h"

typedef struct {
    Option_t type;
    String value;
} StringOption;

typedef struct {
    Option_t type;
    Symbol value;
} SymbolOption;

typedef enum {
    StExecutable,
    StLibrary,
} Stanza_t;

typedef struct {
    Symbol name;
    String filename;
    String entry_point;
    SymbolArray dependencies;
} Executable;

typedef struct {
    Symbol name;
    StringOption filename;
    StringArray submodules;
} Library;

typedef struct {
    Stanza_t type;
    Range range;
    union {
        Executable executable;
        Library library;
    };
} Stanza;

Document* pretty_stanza(Stanza stanza, Allocator* a);

#endif
