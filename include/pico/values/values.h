#ifndef __PICO_VALUES_VALUES_H
#define __PICO_VALUES_VALUES_H

#include <stdint.h>

#include "data/meta/amap_header.h"
#include "data/array.h"
#include "data/string.h"
#include "pretty/document.h"

typedef struct ob_value ob_value;
typedef uint64_t ob_symbol;
typedef u64_array symbol_array;

// Forward declarations of environment.h (to avoid circular includes!)
typedef struct env_capture env_capture;

// Symbol table
string* symbol_to_string(ob_symbol symbol);
ob_symbol string_to_symbol(string string);
void clear_symbols();

AMAP_HEADER(ob_symbol, void*, sym_ptr)


// Total value
typedef enum ob_value_t {
    VI64,
    VPrimOp,
    VSymbol,
    VFormer,
    VProcedure,
    VRef,
} ob_value_t;

typedef enum ob_term_former_t {
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
} ob_term_former_t;

typedef enum ob_primop_t {
    AddI64,
    SubI64,
    MulI64,
    QuotI64,
} ob_primop_t;

// Total value
typedef struct ob_value {
    ob_value_t type;
    union {
        int64_t int_64;
        ob_primop_t primop;
        ob_symbol symbol;
        ob_term_former_t former;
        ob_value* ref;
    } term;
} ob_value;

AMAP_HEADER(ob_symbol, ob_value, sym_val)

document* pretty_primop(ob_primop_t op, allocator a);
document* pretty_value(ob_value val, allocator a);

#endif
