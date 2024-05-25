#include <string.h>
#include "data/array.h"
#include "data/meta/amap_header.h"
#include "data/meta/amap_impl.h"
#include "memory/executable.h"

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

    // The module owns all values defined within it, using memory allocated by
    // its' allocator. When the module is deleted, this will be used to free all
    // values (except functions) 
    allocator allocator;

    // RWX memory is kept separate from regular values, for storing assembled function code
    allocator executable_allocator; 
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
    pi_module* module = (pi_module*) mem_alloc(sizeof(pi_module), a);
    module->has_name = false;
    module->entries = mk_entry_amap(32, a);
    module->allocator = a;
    module->executable_allocator = mk_executable_allocator(a);
    return module;
}

void delete_module(pi_module* module) {
    for (size_t i = 0; i < module->entries.len; i++) {
        module_entry_internal entry = module->entries.data[i].val;
        if (entry.type.sort == TProc) {
            mem_free(entry.value, module->executable_allocator);
        } else {
            mem_free(entry.value, module->allocator);
        }
        delete_pi_type(entry.type, module->allocator);
        sdelete_ptr_array(entry.backrefs, module->allocator);
    };
    sdelete_entry_amap(module->entries, module->allocator);
    mem_free(module, module->allocator);
}

// TODO: if type is of sort PROC, module should move the relevant memory
result add_def (pi_module* module, pi_symbol name, pi_type type, void* data) {
    module_entry_internal entry;
    size_t size = pi_size_of(type);

    entry.value = mem_alloc(size, module->allocator);
    memcpy(entry.value, data, size);
    entry.type = type;
    entry.backrefs = mk_ptr_array(0, module->allocator);
    entry_insert(name, entry, &(module->entries), module->allocator);

    result out;
    out.type = Ok;
    return out;
}

result add_fn_def (pi_module* module, pi_symbol name, pi_type type, assembler* fn) {
    module_entry_internal entry;
    u8_array instrs = get_instructions(fn);
    size_t size = instrs.len;

    // copy the function definition into the module's executable memory
    entry.value = mem_alloc(size, module->executable_allocator);
    memcpy(entry.value, instrs.data, size);
    entry.type = type;
    entry.backrefs = mk_ptr_array(0, module->allocator);
    entry_insert(name, entry, &(module->entries), module->allocator);
    
    result out;
    out.type = Ok;
    return out;
}

module_entry* get_def(pi_symbol sym, pi_module* module) {
    return (module_entry*)entry_lookup(sym, module->entries);
}

symbol_array get_symbols(pi_module* module) {
    symbol_array syms = mk_u64_array(module->entries.len, module->allocator);
    for (size_t i = 0; i < module->entries.len; i++) {
        push_u64(module->entries.data[i].key, &syms, module->allocator);
    };
    return syms;
}
