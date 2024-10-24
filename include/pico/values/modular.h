#ifndef __PICO_VALUES_MODULAR_H
#define __PICO_VALUES_MODULAR_H

#include "memory/allocator.h"

#include "data/string.h"
#include "data/result.h"

#include "assembler/assembler.h"
#include "pico/values/values.h"
#include "pico/values/types.h"
#include "pico/data/sym_sarr_amap.h"

/* Packages and modules. These exist at runtime and are used by environments. 
 * Care must be taken to ensure that code behaves 'correctly' when values are
 * redefined. 
 */

typedef struct Package Package;
typedef struct Module  Module;
typedef struct ModuleEntry {
    void* value;
    PiType type;
    SymSArrAMap* backlinks;
} ModuleEntry;

// Package Interface
Package* mk_package(String name, Allocator* a);
void delete_package(Package* package);
Result add_module(String name, Module* module, Package* package);

// Module Interface
Module* mk_module(Allocator* a);
void delete_module(Module* module);
Result add_def(Module* module, Symbol name, PiType type, void* data); 
Result add_fn_def(Module* module, Symbol name, PiType type, Assembler* fn, SymSArrAMap* backlinks); 
ModuleEntry* get_def(Symbol sym, Module* module);

SymbolArray get_symbols(Module* module, Allocator* a);

#endif
