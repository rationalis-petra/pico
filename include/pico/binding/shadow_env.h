#ifndef __PICO_BINDING_SHADOW_ENV_H
#define __PICO_BINDING_SHADOW_ENV_H

#include "memory/allocator.h"
#include "pico/binding/environment.h"

typedef struct ShadowEnv ShadowEnv;

typedef enum ShadowEntry_t {
    SShadowed,
    SGlobal,
    SErr,
} ShadowEntry_t;

typedef struct ShadowEntry {
    ShadowEntry_t type;
    PiType* vtype;
    void* value;
} shadow_entry;

ShadowEnv* mk_shadow_env(Allocator a, Environment* env);
void delete_shadow_env(ShadowEnv* env);

shadow_entry shadow_env_lookup(Symbol s, ShadowEnv* env);
void shadow_var (Symbol var, ShadowEnv* env, Allocator* a);
void shadow_vars (SymbolArray vars, ShadowEnv* env, Allocator* a);
void pop_shadow(ShadowEnv* env, size_t n);

#endif
