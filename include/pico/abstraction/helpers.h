#ifndef __PICO_ABSTRACTION_HELPERS_H
#define __PICO_ABSTRACTION_HELPERS_H

#include "pico/data/error.h"
#include "pico/syntax/concrete.h"
#include "pico/values/values.h"

Symbol get_symbol_err(RawTree raw, PiErrorPoint* point, Allocator* a);


// Helper functions for implementing macros (also used in the standard library)
bool eq_symbol(RawTree* raw, Symbol s);
bool is_symbol(RawTree raw);
bool get_symbol(RawTree raw, Symbol* out);
// Return 'true' if the rawtree is :<symbol>, i.e. (: <symbol>)
bool is_key_symbol(RawTree raw, Symbol symbol);

typedef enum {
    FDot      = 0x1,
    FColon    = 0x2,
} FieldSpec;

// Return 'true' if the rawtree is :<symbol> or (. symbol), and write symbol to out
bool get_fieldname(RawTree* raw, FieldSpec spec, Symbol* fieldname);
bool get_symbol_list(SymbolArray* arr, RawTree nodes, Allocator* a);
bool get_name_list(NameArray* arr, RawTree nodes, Allocator* a);

// Assuming that the provided rawtree is a branch, return a (pointer to) a new
// rawtree whose 
RawTree raw_slice(RawTree* raw, size_t drop);

#endif
