#ifndef __PICO_EVAL_EXPR_H
#define __PICO_EVAL_EXPR_H

// Note: pico can also execute in a:
// logic-style (HOU)
// stack-style
// Logic-style: makes use of a normalizer, not available? in Micrpico
// stack-style: not available in Micrpico
#include "data/result.h"
#include "pico/binding/environment.h"
#include "pico/syntax/syntax.h"
#include "pico/values/values.h"

typedef struct eval_result {
    Result_t type;
    union {
        string error_message;
        pi_value out;
    } data;
} eval_result;

eval_result eval_expr(syntax syntax, environment* env, allocator a);


#endif
