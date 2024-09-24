#ifndef __PICO_BINDING_SHADOW_ENV_H
#define __PICO_BINDING_SHADOW_ENV_H

#include "memory/allocator.h"
#include "pico/binding/environment.h"

typedef struct ShadowEnv ShadowEnv;

typedef enum ShadowEntry_t {
    SShadowed,
    SGlobal,
    SLocal,
    SErr,
} ShadowEntry_t;

typedef struct {
    ShadowEntry_t type;
    PiType* vtype;
    void* value;
} ShadowEntry;

ShadowEnv* mk_shadow_env(Allocator* a, Environment* env);
void delete_shadow_env(ShadowEnv* env, Allocator* a);

ShadowEntry shadow_env_lookup(Symbol s, ShadowEnv* env);
void shadow_var (Symbol var, ShadowEnv* env);
void shadow_vars (SymbolArray vars, ShadowEnv* env);
void shadow_pop(size_t n, ShadowEnv* env);
void shadow_bind(Symbol var, PiType* type, ShadowEnv* env);

#endif
