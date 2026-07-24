#ifndef __ATLAS_SYNTAX_STANZA_H
#define __ATLAS_SYNTAX_STANZA_H

#include "data/option.h"
#include "data/string.h"

#include "components/pretty/document.h"

#include "pico/values/values.h"
#include "pico/data/string_array.h"
#include "pico/data/range.h"

typedef struct {
    Option_t type;
    Name value;
} NameOption;

typedef enum {
    StExecutable,
    StLibrary,
} Stanza_t;

typedef struct {
    Name name;
    String filename;
    Name entry_point;
    NameArray dependencies;
} Executable;

typedef struct {
    Name name;
    StringOption filename;
    StringArray submodules;
    NameArray dependencies;
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
