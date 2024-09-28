#ifndef __PICO_BINDING_TYPE_ENV_H
#define __PICO_BINDING_TYPE_ENV_H

#include "memory/allocator.h"
#include "pico/binding/environment.h"

typedef struct TypeEnv TypeEnv;

typedef enum TypeEntry_t {
    TELocal,
    TEGlobal,
    TENotFound,
} TypeEntry_t;

typedef struct TypeEntry {
    TypeEntry_t type;
    PiType* ptype;
    PiType* value;
} TypeEntry;

TypeEnv* mk_type_env(Environment* env, Allocator* a);
// No delete, as we expect allocation via an arena allocator

TypeEntry type_env_lookup(Symbol s, TypeEnv* env);
void type_var (Symbol var, PiType* type, TypeEnv* env);
void pop_type(TypeEnv* env);
void pop_types(TypeEnv* env, size_t n);

#endif
