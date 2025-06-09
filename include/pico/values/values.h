#ifndef __PICO_VALUES_VALUES_H
#define __PICO_VALUES_VALUES_H

#include <stdint.h>

#include "data/meta/array_header.h"
#include "data/array.h"
#include "data/string.h"
#include "pretty/document.h"

typedef uint64_t Name;
typedef U64Array NameArray;

typedef struct {
    Name name;
    uint64_t did;
} Symbol;

// Forward declarations of environment.h (to avoid circular includes!)
typedef struct env_capture env_capture;

// Symbol table
String* symbol_to_string(Symbol symbol);
Symbol string_to_symbol(String string);
Symbol string_to_unique_symbol(String string);
Name string_to_name(String string);
String* name_to_string(Name name);
void init_symbols(Allocator* a);
void clear_symbols();

bool symbol_eq(Symbol lhs, Symbol rhs);
int64_t cmp_symbol(Symbol lhs, Symbol rhs);

// Useful for container methods
void delete_symbol(Symbol s);
Symbol copy_symbol(Symbol s, Allocator* a);
void delete_name(Name n);
Name copy_name(Name n, Allocator* a);

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
    FOpen,

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

    // Special Term formers, usually manipulating types 
    // or being pseudo-functions
    FIs,
    FInTo,
    FOutOf,
    FName,
    FUnName,
    FWiden,
    FNarrow,
    FDynAlloc,
    FSizeOf,
    FAlignOf,

    // Type formers
    FProcType,
    FStructType,
    FEnumType,
    FResetType,
    FDynamicType,
    FNamedType,
    FDistinctType,
    FOpaqueType,
    FTraitType,
    FAllType,
    FFamily,

    // The C type former is somewhat special, as it
    // is more explicit about taking in a *value* in the form of a c-type.
    // It may be part of the furture, moving towards a more 'dependent-types'
    // style typechecker.
    FLiftCType,

    // Temporary formers: TODO delegate to macros.
    FReinterpretNative,
    FReinterpretRelic,
    FConvertNative,
    FConvertRelic,

    // Formers for the meta module
    FTypeOf,
    FDescribe,
    FQuote,
} TermFormer;

Document* pretty_former(TermFormer op, Allocator* a);

#endif
