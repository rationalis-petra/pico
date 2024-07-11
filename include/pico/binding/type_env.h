#ifndef __PICO_BINDING_TYPE_ENV_H
#define __PICO_BINDING_TYPE_ENV_H

#include "memory/allocator.h"
#include "pico/binding/environment.h"

typedef struct type_env type_env;

typedef enum type_entry_t {
    TELocal,
    TEGlobal,
    TENotFound,
} type_entry_t;

typedef struct type_entry {
    type_entry_t type;
    pi_type* ptype;
} type_entry;

type_env* mk_type_env(environment* env, allocator a);
// No delete, as we expect allocation via an arena allocator

type_entry type_env_lookup(pi_symbol s, type_env* env);
void type_var (pi_symbol var, pi_type* type, type_env* env, allocator a);
//void type_vars (sym_ptr_amap vars, type_env* env, allocator a);
void pop_type(type_env* env);
void pop_types(type_env* env, size_t n);

#endif
