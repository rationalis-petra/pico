#ifndef __PICO_VALUES_MODULAR_H
#define __PICO_VALUES_MODULAR_H

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

// Package Interface
pi_package* mk_package(string name, allocator a);
void delete_package(pi_package* package);
result add_module(string name, pi_module* module, pi_package* package, allocator a);


// Module Interface
pi_module* mk_module(allocator a);
void delete_module(pi_module* module, allocator a);
result add_def (pi_module* module, string name, pi_type type, void* data); 

#endif
