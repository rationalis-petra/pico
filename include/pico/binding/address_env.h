#ifndef __PICO_BINDING_ADDRESS_ENV_H
#define __PICO_BINDING_ADDRESS_ENV_H

#include "memory/allocator.h"
#include "pico/binding/environment.h"

typedef struct address_env address_env;

typedef enum address_entry_t {
    ALocal,
    AGlobal,
    ANotFound,
    ATooManyLocals,
} address_entry_t;

typedef struct address_entry {
    address_entry_t type;
    union {
        void* value;
        uint8_t stack_offset;
    };
} address_entry;

address_env* mk_address_env(environment* env, allocator a);
void delete_address_env(address_env* env, allocator a);

address_entry address_env_lookup(pi_symbol s, address_env* env);

// Add a set of variables to the address environment.
// offsets are adjusted to compensate for the return address
void address_fn_vars (symbol_array vars, address_env* env, allocator a);
void pop_fn_vars(address_env* env);

void push_stack_address (pi_symbol var, address_env* env, allocator a);
void pop_address(address_env* env);

#endif
