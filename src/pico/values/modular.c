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


typedef struct module_entry {
    void* value;
    pi_type type;
    ptr_array backrefs;
} module_entry;

AMAP_HEADER(pi_symbol, module_entry, entry)

AMAP_IMPL(pi_symbol, module_entry, entry)

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

void delete_module(pi_module* module, allocator a) {
    //delete_entry_amap();
}

result add_def (pi_module* module, string name, pi_type type, void* data) {
    result out;
    out.type = Ok;
    return out;
}
