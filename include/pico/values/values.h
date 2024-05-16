#ifndef __PICO_VALUES_VALUES_H
#define __PICO_VALUES_VALUES_H

#include <stdint.h>

#include "data/meta/amap_header.h"
#include "data/array.h"
#include "data/string.h"
#include "pretty/document.h"

typedef uint64_t pi_symbol;
typedef u64_array symbol_array;

// Forward declarations of environment.h (to avoid circular includes!)
typedef struct env_capture env_capture;

// Symbol table
string* symbol_to_string(pi_symbol symbol);
pi_symbol string_to_symbol(string string);
void clear_symbols();

AMAP_HEADER(pi_symbol, void*, sym_ptr)

typedef enum pi_term_former_t {
    FDefine,
    FApplication,
    FProcedure,
    FDestructor,
    FCorecursor,
    FConstructor,
    FRecursor,
    FStructure,
    FProjector,
    FIf,
    FLet,
} pi_term_former_t;

typedef enum pi_primop_t {
    AddI64,
    SubI64,
    MulI64,
    QuotI64,
} pi_primop_t;

document* pretty_primop(pi_primop_t op, allocator a);

#endif
