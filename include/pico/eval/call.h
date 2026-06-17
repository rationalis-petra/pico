#ifndef __PICO_EVAL_CALL_H
#define __PICO_EVAL_CALL_H
#include <stdint.h>

#include "platform/memory/allocator.h"
#include "components/pretty/document.h"
#include "pico/syntax/syntax.h"
#include "pico/values/modular.h"
#include "pico/codegen/codegen.h" // TOOD (ORGANISATION): Move link data out of codegen.h  


// Utilities for calling Pico functions from C (adapt to the different calling convention)
// Note: currently cannot proviide args/require no arguments!
typedef enum EvalResult_t  {
    ERValue,
    ERDef,
    ERDecl,
    ERImport,
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
        ImportClauseArray imported;
    };
} EvalResult;

typedef struct {
  SynTape tape;
  Target target;
  LinkData links;
  Module *module;
  Allocator *a;
  ErrorPoint* point;
} EvalCtx;

EvalResult pico_run_toplevel(TopLevel top, EvalCtx ctx);

void* pico_run_expr(Target target, size_t size, Allocator* a, ErrorPoint* point);

/**
 * Given the address of a procedure with type Proc [] Unit, run the procedure.
 *
 * Procudure is run with a fresh temporary allocator and dynamic memory,
 * allocated from the allocator. All allocated memory is freed.
 */
void call_unit_fn(void* function, Allocator* a);

/**
 * Given the address of a procedure which corresponds to an instane which 
 * is type-polymorphic, e.g. instance [A] {(show (Show A))} (Show (List A)) ...
 * call the function with a specific set of types and implicits. Requires
 * additional modifications to the functions to work properly.
 * 
 * The pico allocator is used to allocate memory for the instance and
 * the instance pointer
 * 
 * Procudure is run with a fresh temporary allocator and dynamic memory,
 * allocated from the allocator. All this memory is freed.
 */
void* call_instance_fn(void *function, U64Array types, PtrArray implicits, size_t instance_size, PiAllocator* pia, Allocator *a);

Document* pretty_res(EvalResult res, Allocator* a);


#endif
