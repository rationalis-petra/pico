#include "data/array.h"
#include "data/meta/amap_header.h"
#include "data/meta/amap_impl.h"

#include "pico/values/values.h"
#include "pico/values/modular.h"

/* Module and package implementation details
 * Modules
 * • The module table maps symbols to entries, which contain: 
 *   • The address of the value
 *   • The type of the value
 *   • An array of pointers to all addresses where this term is referenced
 */

typedef struct module_entry module_entry_internal;
/* typedef struct module_entry_internal { */
/*     void* value; */
/*     pi_type type; */
/*     ptr_array backrefs; */
/* } module_entry_internal; */

AMAP_HEADER(pi_symbol, module_entry_internal, entry)

AMAP_IMPL(pi_symbol, module_entry_internal, entry)

typedef struct pi_package {
    pi_symbol name;
    sym_ptr_amap modules;
} pi_package;

typedef struct pi_module {
    bool has_name;
    pi_symbol name;
    entry_amap entries;
} pi_module;



// -----------------------------------------------------------------------------
// Package Implementation
// -----------------------------------------------------------------------------
pi_package* mk_package(string name, allocator a) {
    pi_package* package = (pi_package*) mem_alloc(sizeof(pi_package), a);
    package->name = string_to_symbol(name);
    package->modules = mk_sym_ptr_amap(32, a);
    return package;
}

result add_module(string name, pi_module* module, pi_package* package, allocator a) {
    // TOOD: check if module already exists
    sym_ptr_insert(string_to_symbol(name), (void*)module, &(package->modules), a);

    result res;
    res.type = Ok;
    return res;
}


// -----------------------------------------------------------------------------
// Module Implementation
// -----------------------------------------------------------------------------

pi_module* mk_module(allocator a) {
    pi_module* mdle = (pi_module*) mem_alloc(sizeof(pi_module), a);
    mdle->has_name = false;
    mdle->entries = mk_entry_amap(32, a);
    return mdle;
}


void delete_symbol(pi_symbol s, allocator a) {}
void delete_entry(module_entry entry, allocator a) {
    if (entry.type.sort != TProc) {
        mem_free(entry.value, a);
    }
    delete_pi_type(entry.type, a);
    sdelete_ptr_array(entry.backrefs, a);
}

void delete_module(pi_module* module, allocator a) {
    delete_entry_amap(module->entries, &delete_symbol, &delete_entry, a);
    mem_free(module, a);
}

// TODO: if type is of sort PROC, module should move the relevant memory
result add_def (pi_module* module, pi_symbol name, pi_type type, void* data, allocator a) {
    module_entry_internal entry;
    entry.value = data;
    entry.type = type;
    entry.backrefs = mk_ptr_array(0, a);
    entry_insert(name, entry, &(module->entries), a);

    result out;
    out.type = Ok;
    return out;
}

module_entry* get_def(pi_symbol sym, pi_module* module) {
    return (module_entry*)entry_lookup(sym, module->entries);
}

symbol_array get_symbols(pi_module* module, allocator a) {
    symbol_array syms = mk_u64_array(module->entries.len, a);
    for (size_t i = 0; i < module->entries.len; i++) {
        push_u64(module->entries.data[i].key, &syms, a);
    };
    return syms;
}
