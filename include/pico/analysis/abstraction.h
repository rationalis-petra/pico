#ifndef __PICO_ANALYSIS_ABSTRACTION_H
#define __PICO_ANALYSIS_ABSTRACTION_H

#include "data/string.h"
#include "data/result.h"
#include "pico/binding/environment.h"
#include "pico/syntax/concrete.h"
#include "pico/syntax/syntax.h"

typedef struct AbsResult {
    Result_t type;
    union {
        String error_message;
        TopLevel out;
    };
} AbsResult;

typedef struct AbsExprResult {
    Result_t type;
    union {
        String error_message;
        Syntax out;
    };
} AbsExprResult;

AbsResult abstract(RawTree raw, Environment* env, Allocator* a);
AbsExprResult abstract_expr(RawTree raw, Environment* env, Allocator* a);

#endif
