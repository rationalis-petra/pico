#ifndef __PICO_EVAL_CALL_H
#define __PICO_EVAL_CALL_H
#include <stdint.h>

#include "memory/allocator.h"
#include "assembler/assembler.h"
#include "pretty/document.h"
#include "pico/syntax/syntax.h"
#include "pico/values/modular.h"


// Utilities for calling Pico functions from C (adapt to the different calling convention)
// Note: currently cannot proviide args/require no arguments!
typedef enum eval_result_t  {
    ERSucc,
    ERInt,
    ERBool,
} eval_result_t;

typedef struct eval_result {
    eval_result_t type;
    int64_t val;
} eval_result;


eval_result pico_run_toplevel(toplevel top, assembler* ass, pi_module* module, allocator a);

int64_t pico_run_expr(assembler* ass, allocator a);

document* pretty_res(eval_result res, allocator a);


#endif
