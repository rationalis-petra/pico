#ifndef __PICO_ANALYSIS_NAME_RESOLUTINO_H
#define __PICO_ANALYSIS_NAME_RESOLUTINO_H

#include "data/string.h"
#include "data/result.h"
#include "pico/binding/environment.h"
#include "pico/syntax/concrete.h"
#include "pico/syntax/syntax.h"

typedef struct resolve_result {
    Result_t type;
    union {
        string error_message;
        syntax out;
    } data;
} resolve_result;

// Given a raw syntax tree, resolve into a syntax tree dynamically, i.e. using the runtime environment
resolve_result resolve_dynamic(pi_rawtree raw, environment* env, allocator a);

#endif
