#ifndef __PICO_BINDING_SHADOW_ENV_H
#define __PICO_BINDING_SHADOW_ENV_H

#include "memory/allocator.h"
#include "pico/binding/environment.h"

typedef struct shadow_env shadow_env;

typedef enum shadow_entry_t {
    SShadowed,
    SGlobal,
    SErr,
} shadow_entry_t;
typedef struct shadow_entry {
    shadow_entry_t type;
    pi_type* vtype;
    void* value;
} shadow_entry;

shadow_env* mk_shadow_env(allocator a, environment* env);
void delete_shadow_env(shadow_env* env, allocator a);

shadow_entry shadow_env_lookup(pi_symbol s, shadow_env* env);
void shadow_var (pi_symbol var, shadow_env* env, allocator a);
void shadow_vars (symbol_array vars, shadow_env* env, allocator a);
void pop_shadow(shadow_env* env, size_t n);

#endif
