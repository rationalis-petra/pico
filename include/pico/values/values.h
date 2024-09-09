#ifndef __PICO_VALUES_VALUES_H
#define __PICO_VALUES_VALUES_H

#include <stdint.h>

#include "data/array.h"
#include "data/string.h"
#include "pretty/document.h"

typedef uint64_t Symbol;
typedef U64Array SymbolArray;

// Forward declarations of environment.h (to avoid circular includes!)
typedef struct env_capture env_capture;

// Symbol table
String* symbol_to_string(Symbol symbol);
Symbol string_to_symbol(String string);
void clear_symbols();

// useful for container methods
void delete_symbol(Symbol s, Allocator* a);
Symbol copy_symbol(Symbol s, Allocator* a);

typedef enum TermFormer {
    FDefine,
    FProcedure,
    FApplication,
    FVariant,
    FMatch,
    FStructure,
    FProjector,
    FIf,
    FLet,

    // Type formers
    FProcType,
    FStructType,
    FEnumType,
} TermFormer;

Document* pretty_former(TermFormer op, Allocator* a);

#endif
