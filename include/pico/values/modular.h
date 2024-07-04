#ifndef __PICO_VALUES_MODULAR_H
#define __PICO_VALUES_MODULAR_H

#include "assembler/assembler.h"
#include "memory/allocator.h"
#include "data/string.h"
#include "data/result.h"
#include "pico/values/values.h"
#include "pico/values/types.h"

/* Packages and modules. These exist at runtime and are used by environments. 
 * Care must be taken to ensure that code behaves 'correctly' when values are
 * redefined. 
 */

typedef struct pi_package pi_package;
typedef struct pi_module pi_module;
typedef struct module_entry {
    void* value;
    pi_type type;
    ptr_array backrefs;
} module_entry;

// Package Interface
pi_package* mk_package(string name, allocator a);
void delete_package(pi_package* package);
result add_module(string name, pi_module* module, pi_package* package, allocator a);


// Module Interface
pi_module* mk_module(allocator a);
void delete_module(pi_module* module);
result add_def(pi_module* module, pi_symbol name, pi_type type, void* data); 
result add_fn_def(pi_module* module, pi_symbol name, pi_type type, assembler* fn); 
module_entry* get_def(pi_symbol sym, pi_module* module);

symbol_array get_symbols(pi_module* module, allocator a);

#endif
