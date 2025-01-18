#include <string.h>
#include "platform/machine_info.h"
#include "platform/memory/executable.h"

#include "data/array.h"
#include "data/meta/amap_header.h"
#include "data/meta/amap_impl.h"

#include "pico/values/values.h"
#include "pico/values/modular.h"
#include "pico/syntax/header.h"
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
    bool is_module;
    PiType type;
    SymSArrAMap* backlinks;
} ModuleEntryInternal;

AMAP_HEADER(Symbol, ModuleEntryInternal, entry, Entry)
AMAP_IMPL(Symbol, ModuleEntryInternal, entry, Entry)

struct Package {
    Symbol name;
    SymPtrAMap modules;
    Allocator* gpa;
};

struct Module {
    EntryAMap entries;

    ModuleHeader header;
    Package* lexical_parent_package;
    Module* lexical_parent_module;

    // The module owns all values defined within it, using memory allocated by
    // its' allocator. When the module is deleted, this will be used to free all
    // values (except functions, which are freed via the executable allocator) 
    Allocator* allocator;

    // RWX memory is kept separate from regular values, for storing assembled function code
    Allocator executable_allocator; 
};


// -----------------------------------------------------------------------------
// Package Implementation
// -----------------------------------------------------------------------------
Package* mk_package(Symbol name, Allocator* a) {
    Package* package = mem_alloc(sizeof(Package), a);
    package->name = name;
    package->modules = mk_sym_ptr_amap(32, a);
    package->gpa = a;
    return package;
}

void delete_package(Package* package) {
    Allocator* a = package->gpa;

    for (size_t i = 0; i < package->modules.len; i++) {
        Module* module = package->modules.data[i].val;
        delete_module(module);
    }
    sdelete_sym_ptr_amap(package->modules);
    mem_free(package, a);
}

Result add_module(Symbol name, Module* module, Package* package) {
    // TOOD (Memory leak!): check if module already exists
    sym_ptr_insert(name, (void*)module, &(package->modules));

    return (Result) {.type = Ok};
}

Module* get_module(Symbol name, Package* package) {
    Module** module = (Module**) sym_ptr_lookup(name, package->modules);
    if (module) return *module; else return NULL;
}

// -----------------------------------------------------------------------------
// Module Implementation
// -----------------------------------------------------------------------------

// Forward declaration of utility functions
void update_function(uint8_t* val, SymPtrAMap new_vals, SymSArrAMap links);

Module* mk_module(ModuleHeader header, Package* pkg_parent, Module* parent, Allocator* a) {
    Module* module = (Module*) mem_alloc(sizeof(Module), a);
    module->entries = mk_entry_amap(32, a);
    module->header = copy_module_header(header, a);
    module->lexical_parent_package = pkg_parent;
    module->lexical_parent_module = parent;
    module->allocator = a;
    module->executable_allocator = mk_executable_allocator(a);
    return module;
}

// Helper
void delete_module_entry(ModuleEntryInternal entry, Module* module) {
    if (entry.is_module) {
        delete_module(entry.value);
    } else {
        if (entry.type.sort == TProc || entry.type.sort == TAll) {
            mem_free(entry.value, &module->executable_allocator);
        } else if (entry.type.sort == TKind || entry.type.sort == TConstraint) {
            delete_pi_type_p(entry.value, module->allocator);
        } else if (entry.type.sort == TTraitInstance) {
            mem_free(*(void**)entry.value, module->allocator);
            mem_free(entry.value, module->allocator);
        } else {
            mem_free(entry.value, module->allocator);
        }
        delete_pi_type(entry.type, module->allocator);
    }

    if (entry.backlinks) {
        delete_sym_sarr_amap(*entry.backlinks,
                             delete_symbol,
                             sdelete_size_array);
        mem_free(entry.backlinks, module->allocator);
    }
}

void delete_module(Module* module) {
    for (size_t i = 0; i < module->entries.len; i++) {
        ModuleEntryInternal entry = module->entries.data[i].val;
        delete_module_entry(entry, module);
    };
    sdelete_entry_amap(module->entries);
    sdelete_import_clause_array(module->header.imports.clauses);
    sdelete_export_clause_array(module->header.exports.clauses);

    release_executable_allocator(module->executable_allocator);
    mem_free(module, module->allocator);
}

Result add_def (Module* module, Symbol name, PiType type, void* data) {
    ModuleEntryInternal entry;
    entry.is_module = false;
    size_t size = pi_size_of(type);

    if (type.sort == TKind || type.sort == TConstraint) {
        PiType* t_val = *(PiType**)data; 
        entry.value = copy_pi_type_p(t_val, module->allocator);
    } else if (type.sort == TTraitInstance){
        size_t total = 0;
        for (size_t i = 0; i < type.instance.fields.len; i++) {
            total += pi_size_of(*(PiType*)type.instance.fields.data[i].val);
        }
        void* new_memory = mem_alloc(total, module->allocator);
        memcpy(new_memory, *(void**)data, total);

        entry.value = mem_alloc(size, module->allocator);
        memcpy(entry.value, &new_memory, ADDRESS_SIZE);
    } else {
        entry.value = mem_alloc(size, module->allocator);
        memcpy(entry.value, data, size);
    }
    entry.type = copy_pi_type(type, module->allocator);
    entry.backlinks = NULL;

    // Free a previous definition (if it exists!)
    ModuleEntryInternal* old_entry = entry_lookup(name, module->entries);
    if (old_entry) delete_module_entry(*old_entry, module);

    entry_insert(name, entry, &module->entries);

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

    // Free a previous definition (if it exists!)
    ModuleEntryInternal* old_entry = entry_lookup(name, module->entries);
    if (old_entry) delete_module_entry(*old_entry, module);

    entry_insert(name, entry, &(module->entries));

    return (Result) {.type = Ok};
}

Result add_module_def(Module* module, Symbol name, Module* child) {
    ModuleEntryInternal entry = (ModuleEntryInternal) {
        .value = child,
        .is_module = true,
        .backlinks = NULL,
    };

    // Free a previous definition (if it exists!)
    // TODO BUG UB: possibly throw here?
    ModuleEntryInternal* old_entry = entry_lookup(name, module->entries);
    if (old_entry) delete_module_entry(*old_entry, module);

    entry_insert(name, entry, &module->entries);

    return (Result) {.type = Ok};
}

ModuleEntry* get_def(Symbol sym, Module* module) {
    return (ModuleEntry*)entry_lookup(sym, module->entries);
}

String* get_name(Module* module) {
    return symbol_to_string(module->header.name);
}

SymbolArray get_exported_symbols(Module* module, Allocator* a) {
    SymbolArray syms = mk_u64_array(module->entries.len, a);
    for (size_t i = 0; i < module->entries.len; i++) {
        push_u64(module->entries.data[i].key, &syms);
    };
    return syms;
}

PtrArray get_exported_instances(Module* module, Allocator* a) {
    PtrArray instances = mk_ptr_array(module->entries.len, a);
    for (size_t i = 0; i < module->entries.len; i++) {
        ModuleEntryInternal m_entry = module->entries.data[i].val;
        if (m_entry.type.sort == TTraitInstance) {
            InstanceSrc* entry = mem_alloc(sizeof(InstanceSrc), a);
            *entry = (InstanceSrc) {
                .id = m_entry.type.instance.instance_of,
                .args = m_entry.type.instance.args,
                .src_sym = module->entries.data[i].key,
                .src = module,
            };

            push_ptr(entry, &instances);
        }
    };
    return instances;
}

Package* get_package(Module* module) {
    return module->lexical_parent_package;
}

Module* get_parent(Module* module) {
    return module->lexical_parent_module;
}

Imports get_imports(Module* module) {
    return module->header.imports;
}

Exports get_exports(Module* module) {
    return module->header.exports;
}


//------------------------------------------------------------------------------
// Implementation internals.
//------------------------------------------------------------------------------

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
