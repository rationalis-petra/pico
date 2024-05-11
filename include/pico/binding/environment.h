#ifndef __PICO_BINDING_ENVIRONMENT_H
#define __PICO_BINDING_ENVIRONMENT_H

#include "pico/values/values.h"

typedef struct environment environment;
typedef struct env_capture env_capture;

environment* env_empty(allocator a);
environment* env_insert(pi_symbol sym, pi_value val, environment* env, allocator a);
void env_insert_inplace(pi_symbol sym, pi_value val, environment* env, allocator a);
pi_value* env_lookup(pi_symbol sym, environment* env);
environment* update_from_capture(env_capture* capture, environment* env, allocator a);

env_capture* capture(environment* env, symbol_array symbols);

void delete_env(environment* env, allocator a);

#endif
