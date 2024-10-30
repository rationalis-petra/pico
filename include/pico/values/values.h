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
void delete_symbol(Symbol s);
Symbol copy_symbol(Symbol s, Allocator* a);

typedef enum TermFormer {
    // Top Level Former
    FDefine,
    FDeclare,

    // Term Formers: value construction/destruction
    FProcedure,
    FAll,
    FApplication,
    FVariant,
    FMatch,
    FStructure,
    FProjector,

    // Term Formers: Control flow + binding
    FLet,
    FIf,
    FLabels,
    FGoTo,
    FWithReset,
    FResetTo,
    FSequence,

    // Special Term formers
    FIs,
    FSize,

    // Type formers
    FProcType,
    FStructType,
    FEnumType,
    FResetType,
    FAllType,
} TermFormer;

Document* pretty_former(TermFormer op, Allocator* a);

#endif
