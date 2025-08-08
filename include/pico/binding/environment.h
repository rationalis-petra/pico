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

typedef struct Environment Environment;
typedef struct EnvCapture EnvCapture;

typedef struct EnvEntry {
    Result_t success;
    bool is_module;
    PiType* type;
    void* value;
    Module* source;
} EnvEntry;

Environment* env_from_module(Module* module, ErrorPoint* point, Allocator* a);
void refresh_env(Environment* env, Allocator* a);
void delete_env(Environment* env, Allocator* a);

EnvEntry env_lookup(Symbol sym, Environment* env);
PiType* env_lookup_tydecl(Symbol sym, Environment* env);
PtrArray* env_implicit_lookup(uint64_t id, Environment* env);

#endif
