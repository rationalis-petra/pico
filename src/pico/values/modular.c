#include <string.h>
#include "data/array.h"
#include "data/meta/amap_header.h"
#include "data/meta/amap_impl.h"
#include "memory/executable.h"

#include "pico/values/values.h"
#include "pico/values/modular.h"
#include "pico/data/sym_ptr_amap.h"
#include "pico/data/sym_sarr_amap.h"

/* Module and package implementation details
 * Modules
 * • The module table maps symbols to entries, which contain: 
 *   • The address of the value
 *   • The type of the value
 *   • An array of pointers to all addresses where this term is referenced
 */

typedef struct {
    void* value;
    PiType type;
    SymSArrAMap* backlinks;
} ModuleEntryInternal;

AMAP_HEADER(Symbol, ModuleEntryInternal, entry, Entry)
AMAP_IMPL(Symbol, ModuleEntryInternal, entry, Entry)

struct Package {
    Symbol name;
    SymPtrAMap modules;
};

struct Module {
    bool has_name;
    Symbol name;
    EntryAMap entries;

    // The module owns all values defined within it, using memory allocated by
    // its' allocator. When the module is deleted, this will be used to free all
    // values (except functions) 
    Allocator* allocator;

    // RWX memory is kept separate from regular values, for storing assembled function code
    Allocator executable_allocator; 
};


// -----------------------------------------------------------------------------
// Package Implementation
// -----------------------------------------------------------------------------
Package* mk_package(String name, Allocator* a) {
    Package* package = (Package*) mem_alloc(sizeof(Package), a);
    package->name = string_to_symbol(name);
    package->modules = mk_sym_ptr_amap(32, a);
    return package;
}

Result add_module(String name, Module* module, Package* package) {
    // TOOD: check if module already exists
    sym_ptr_insert(string_to_symbol(name), (void*)module, &(package->modules));

    return (Result) {.type = Ok};
}


// -----------------------------------------------------------------------------
// Module Implementation
// -----------------------------------------------------------------------------

// Forward declaration of utility functions
void update_function(uint8_t* val, SymPtrAMap new_vals, SymSArrAMap links);

Module* mk_module(Allocator* a) {
    Module* module = (Module*) mem_alloc(sizeof(Module), a);
    module->has_name = false;
    module->entries = mk_entry_amap(32, a);
    module->allocator = a;
    module->executable_allocator = mk_executable_allocator(a);
    return module;
}

void delete_module(Module* module) {
    for (size_t i = 0; i < module->entries.len; i++) {
        ModuleEntryInternal entry = module->entries.data[i].val;
        if (entry.type.sort == TProc || entry.type.sort == TAll) {
            mem_free(entry.value, &module->executable_allocator);
        } else if (entry.type.sort == TKind) {
            delete_pi_type_p(entry.value, module->allocator);
        } else {
            mem_free(entry.value, module->allocator);
        }
        delete_pi_type(entry.type, module->allocator);
        if (entry.backlinks) {
            delete_sym_sarr_amap(*entry.backlinks,
                                 delete_symbol,
                                 sdelete_size_array);
            mem_free(entry.backlinks, module->allocator);
        }
    };
    sdelete_entry_amap(module->entries);

    release_executable_allocator(module->executable_allocator);
    mem_free(module, module->allocator);
}

Result add_def (Module* module, Symbol name, PiType type, void* data) {
    ModuleEntryInternal entry;
    size_t size = pi_size_of(type);

    if (type.sort == TKind) {
        PiType* t_val = *(PiType**)data; 
        entry.value = copy_pi_type_p(t_val, module->allocator);
    } else {
        entry.value = mem_alloc(size, module->allocator);
        memcpy(entry.value, data, size);
    }
    entry.type = copy_pi_type(type, module->allocator);
    entry.backlinks = NULL;
    entry_insert(name, entry, &(module->entries));

    Result out;
    out.type = Ok;
    return out;
}

Result add_fn_def (Module* module, Symbol name, PiType type, Assembler* fn, SymSArrAMap* backlinks) {
    ModuleEntryInternal entry;
    U8Array instrs = get_instructions(fn);
    size_t size = instrs.len;

    // copy the function definition into the module's executable memory
    entry.value = mem_alloc(size, &module->executable_allocator);
    memcpy(entry.value, instrs.data, size);
    entry.type = copy_pi_type(type, module->allocator);
    if (backlinks) {
        entry.backlinks = mem_alloc(sizeof(SymSArrAMap), module->allocator);
        *(entry.backlinks) = copy_sym_sarr_amap(*backlinks,
                                                copy_symbol,
                                                scopy_size_array,
                                                module->allocator);

        // swap out self-references
        SymPtrAMap self_ref = mk_sym_ptr_amap(1, module->allocator);
        sym_ptr_insert(name, entry.value, &self_ref);
        update_function(entry.value, self_ref, *backlinks);
        sdelete_sym_ptr_amap(self_ref);
    } else {
        entry.backlinks = NULL;
    }
    entry_insert(name, entry, &(module->entries));

    return (Result) {.type = Ok};
}

ModuleEntry* get_def(Symbol sym, Module* module) {
    return (ModuleEntry*)entry_lookup(sym, module->entries);
}

SymbolArray get_symbols(Module* module, Allocator* a) {
    SymbolArray syms = mk_u64_array(module->entries.len, a);
    for (size_t i = 0; i < module->entries.len; i++) {
        push_u64(module->entries.data[i].key, &syms);
    };
    return syms;
}

void update_function(uint8_t* val, SymPtrAMap new_vals, SymSArrAMap links) {
    for (size_t i = 0; i < new_vals.len; i++) {
        Symbol sym = new_vals.data[i].key;
        uint64_t new_loc = (uint64_t)new_vals.data[i].val;
        uint8_t* src = (uint8_t*)&new_loc;

        SizeArray* szarr = sym_sarr_lookup(sym, links);
        if (szarr) {
            for (size_t j = 0; j < szarr->len; j++) {
                size_t offset = szarr->data[j];
                for (size_t k = 0; k < sizeof(uint64_t); k++) {
                    val[offset + k] = src[k];
                }
            }
        }
    }
}
