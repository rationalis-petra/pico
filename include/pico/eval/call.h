#ifndef __PICO_EVAL_CALL_H
#define __PICO_EVAL_CALL_H
#include <stdint.h>

#include "platform/memory/allocator.h"
#include "pretty/document.h"
#include "pico/syntax/syntax.h"
#include "pico/values/modular.h"
#include "pico/codegen/codegen.h" // TOOD (ORGANISATION): Move link data out of codegen.h  


// Utilities for calling Pico functions from C (adapt to the different calling convention)
// Note: currently cannot proviide args/require no arguments!
typedef enum EvalResult_t  {
    ERValue,
    ERDef,
    ERDecl,
    EROpen,
} EvalResult_t;

typedef struct EvalVal {
    PiType* type;
    void* val;
} EvalVal;

typedef struct EvalDef {
    Symbol name;
    PiType* type;
} EvalDef;

typedef struct EvalResult {
    EvalResult_t type;
    union {
        EvalVal val;
        EvalDef def;
        PtrArray opened;
    };
} EvalResult;

EvalResult pico_run_toplevel(TopLevel top, Target target, LinkData links, Module* module, Allocator* a, ErrorPoint* point);

void* pico_run_expr(Target target, size_t size, Allocator* a, ErrorPoint* point);

Document* pretty_res(EvalResult res, Allocator* a);


#endif
