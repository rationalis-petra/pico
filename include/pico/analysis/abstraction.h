#ifndef __PICO_ANALYSIS_ABSTRACTION_H
#define __PICO_ANALYSIS_ABSTRACTION_H

#include "data/string.h"
#include "data/result.h"
#include "pico/binding/environment.h"
#include "pico/syntax/concrete.h"
#include "pico/syntax/syntax.h"

typedef struct abs_result {
    Result_t type;
    union {
        string error_message;
        toplevel out;
    };
} abs_result;

typedef struct abs_expr_result {
    Result_t type;
    union {
        string error_message;
        syntax out;
    };
} abs_expr_result;

abs_result abstract(pi_rawtree raw, environment* env, allocator a);
abs_expr_result abstract_expr(pi_rawtree raw, environment* env, allocator a);

#endif
