#ifndef __PICO_VALUES_VALUES_H
#define __PICO_VALUES_VALUES_H

#include <stdint.h>

#include "data/meta/amap_header.h"
#include "data/array.h"
#include "data/string.h"
#include "pretty/document.h"

typedef struct pi_value pi_value;
typedef uint64_t pi_symbol;
typedef u64_array symbol_array;

// Forward declarations of environment.h (to avoid circular includes!)
typedef struct env_capture env_capture;

// Symbol table
string* symbol_to_string(pi_symbol symbol);
pi_symbol string_to_symbol(string string);
void clear_symbols();

AMAP_HEADER(pi_symbol, void*, sym_ptr)


// Total value
typedef enum pi_value_t {
    VI64,
    VPrimOp,
    VSymbol,
    VFormer,
    VProcedure,
    VRef,
} pi_value_t;

typedef enum pi_term_former_t {
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

// Total value
typedef struct pi_value {
    pi_value_t type;
    union {
        int64_t int_64;
        pi_primop_t primop;
        pi_symbol symbol;
        pi_term_former_t former;
        pi_value* ref;
    } term;
} pi_value;

AMAP_HEADER(pi_symbol, pi_value, sym_val)

document* pretty_primop(pi_primop_t op, allocator a);
document* pretty_value(pi_value val, allocator a);

#endif
