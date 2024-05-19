#ifndef __PICO_ANALYSIS_TYPECHECK_H
#define __PICO_ANALYSIS_TYPECHECK_H

#include "data/result.h"
#include "pico/binding/environment.h"
#include "pico/syntax/syntax.h"

typedef struct type_result {
    Result_t type;
    string error_message;
    void (*release_type_memory)(allocator arena);
    allocator arena;
} type_result;

type_result type_check(toplevel* untyped, environment* env, allocator a);

type_result type_check_expr(syntax* untyped, pi_type type, environment* env, allocator a);
type_result type_infer_expr(syntax* untyped, environment* env, allocator a);

#endif
