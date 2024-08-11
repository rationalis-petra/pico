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
    ERValue,
    ERSucc,
    ERFail,
} eval_result_t;

typedef struct eval_succ {
    pi_type* type;
    void* val;
} eval_succ;

typedef struct eval_result {
    eval_result_t type;
    union {
        eval_succ val;
        string error_message;
    };
} eval_result;


eval_result pico_run_toplevel(toplevel top, assembler* ass, sym_sarr_amap* backlinks, pi_module* module, allocator a);

void* pico_run_expr(assembler* ass, size_t size, allocator a);

document* pretty_res(eval_result res, allocator a);


#endif
