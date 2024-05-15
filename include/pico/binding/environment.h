#ifndef __PICO_BINDING_ENVIRONMENT_H
#define __PICO_BINDING_ENVIRONMENT_H

// -----------------------------------------------------------------------------
// Environment
// -----------
// Maps symbols to global values, i.e. ones defined in modules. It is used by
// various 'client' environments which may use different local values, such as
// stack offsets (when generating code) or types (when typechecking). 
// -----------------------------------------------------------------------------

#include "data/result.h"

#include "pico/values/values.h"
#include "pico/values/types.h"
#include "pico/values/modular.h"

typedef struct environment environment;
typedef struct env_capture env_capture;

typedef struct env_entry {
    Result_t success;
    pi_type type;
    void* value;
} env_entry;

environment* env_from_module(pi_module* module, allocator a);
void delete_env(environment* env, allocator a);

env_entry env_lookup(pi_symbol sym, environment* env);


#endif
