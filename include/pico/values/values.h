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
void init_symbols(Allocator* a);
void clear_symbols();

// Useful for container methods
void delete_symbol(Symbol s);
Symbol copy_symbol(Symbol s, Allocator* a);

// Dynamic Variables
void init_dynamic_vars(Allocator* a);
void clear_dynamic_vars();

void thread_init_dynamic_vars();
void thread_clear_dynamic_vars();

uint64_t mk_dynamic_var(size_t size, void* default_val);
void delete_dynamic_var(uint64_t var);
void* get_dynamic_val(uint64_t dvar);
void* get_dynamic_memory();

typedef enum TermFormer {
    // Top Level Former
    FDefine,
    FDeclare,

    // Term Formers: value construction/destruction
    FProcedure,
    FAll,
    FMacro,
    FApplication,
    FVariant,
    FMatch,
    FStructure,
    FProjector,
    FInstance,
    FDynamic,
    FDynamicUse,

    // Term Formers: Control flow + binding
    FDynamicLet,
    FLet,
    FIf,
    FLabels,
    FGoTo,
    FWithReset,
    FResetTo,
    FSequence,
    FModule,

    // Special Term formers
    FIs,
    FInTo,
    FOutOf,
    FDynAlloc,
    FSizeOf,
    FAlignOf,

    // Type formers
    FProcType,
    FStructType,
    FEnumType,
    FResetType,
    FDynamicType,
    FDistinctType,
    FOpaqueType,
    FTraitType,
    FAllType,
    FFamily,

    // Temporary formers: TODO delegate to macros.
    FReinterpret,
    FConvert,
} TermFormer;

Document* pretty_former(TermFormer op, Allocator* a);

#endif
