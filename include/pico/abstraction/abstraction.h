#ifndef __PICO_ABSTRACTION_ABSTRACTION_H
#define __PICO_ABSTRACTION_ABSTRACTION_H

#include "pico/data/error.h"
#include "pico/binding/environment.h"
#include "pico/syntax/concrete.h"
#include "pico/syntax/syntax.h"
#include "pico/syntax/header.h"

typedef enum : uint64_t {
    Left, Right
} MResult_t;

typedef struct {
    String message;
    Range range;
} MacroError;

typedef struct {
    MResult_t result_type;
    union {
        RawTree term;
        MacroError err;
    };
} MacroResult;

typedef struct {
    SynTape tape;
    Allocator *a;
    Environment *env;
    PiErrorPoint* point;
} AbstractionCtx;

TopLevel abstract(RawTree raw, AbstractionCtx ctx);
SynRef abstract_expr(RawTree raw, AbstractionCtx ctx);
ModuleHeader* abstract_header(RawTree raw, Allocator* a, PiErrorPoint* point);
RawTree* raw_slice(RawTree* raw, size_t drop, PiAllocator* pia);

// Helper functions for implementing macros (also used in the standard library)
bool eq_symbol(RawTree* raw, Symbol s);
bool is_symbol(RawTree raw);

#endif
