#ifndef __PICO_VALUES_MODULAR_H
#define __PICO_VALUES_MODULAR_H

#include "platform/memory/allocator.h"
#include "data/result.h"

#include "assembler/assembler.h"

#include "pico/data/sym_sarr_amap.h"
#include "pico/syntax/header.h"
#include "pico/values/values.h"
#include "pico/values/types.h"

/* Packages and modules. These exist at runtime and are used by environments. 
 * Care must be taken to ensure that code behaves 'correctly' when values are
 * redefined. 
 */

typedef struct Package Package;
typedef struct Module  Module;
typedef struct {
    void* value;
    bool is_module;
    PiType type;
    SymSArrAMap* backlinks;
} ModuleEntry;

typedef struct {
    uint64_t id;
    PtrArray args;
    Symbol src_sym;
    Module* src;
} InstanceSrc;

// Package Interface
Package* mk_package(Symbol name, Allocator* a);
void delete_package(Package* package);
Result add_module(Symbol name, Module* module, Package* package);
Module* get_module(Symbol name, Package* package);

// Module Interface
Module* mk_module(ModuleHeader header, Package* pkg_parent, Module* parent, Allocator* a);
void delete_module(Module* module);

Result add_def(Module* module, Symbol name, PiType type, void* data); 
Result add_fn_def(Module* module, Symbol name, PiType type, Assembler* fn, SymSArrAMap* backlinks); 
Result add_module_def(Module* module, Symbol name, Module* child); 

ModuleEntry* get_def(Symbol sym, Module* module);
SymbolArray get_exported_symbols(Module* module, Allocator* a);
PtrArray get_exported_instances(Module* module, Allocator* a);

String* get_name(Module* module);
Package* get_package(Module* module);
Module* get_parent(Module* module);
Imports get_imports(Module* module);
Exports get_exports(Module* module);

#endif
