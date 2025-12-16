#include <string.h>
#include "platform/machine_info.h"
#include "platform/memory/executable.h"

#include "data/num.h"
#include "data/array.h"
#include "data/meta/amap_header.h"
#include "data/meta/amap_impl.h"

#include "pico/values/values.h"
#include "pico/values/modular.h"
#include "pico/syntax/header.h"
#include "pico/data/sym_sarr_amap.h"
#include "pico/data/sym_ptr_amap.h"

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
    PtrArray* declarations;

    U8Array* code_segment;
    U8Array* data_segment;

    // TODO (UB) if a value (e.g. proc) from a module is evaluated, it will give
    // an address. If the original definition is then deleted, the address will
    // now be dangling. This is an issue if the address has been stored somewhere! 
    // This may end up needing to be exposed to the user?
    SymSArrAMap* backlinks;
} ModuleEntryInternal;

AMAP_HEADER(Symbol, ModuleEntryInternal, entry, Entry)
AMAP_CMP_IMPL(Symbol, ModuleEntryInternal, symbol_cmp, entry, Entry)

struct Package {
    Name name;
    Module* root_module;
    AddrPiList dependencies;
    PiAllocator gpa;
};

struct Module {
    EntryAMap entries;

    ModuleHeader header;
    Package* lexical_parent_package;
    Module* lexical_parent_module;

    // The module owns all values defined within it, using memory allocated by
    // its' allocator. When the module is deleted, this will be used to free all
    // values (except functions, which are freed via the executable allocator) 
    PiAllocator pico_allocator;
    Allocator allocator;

    // RWX memory is kept separate from regular values, for storing assembled function code
    Allocator executable_allocator; 
};

// -----------------------------------------------------------------------------
// Package Implementation
// -----------------------------------------------------------------------------
Package* mk_package(Name name, PiAllocator pico_allocator) {
    Package* package = call_alloc(sizeof(Package), &pico_allocator);
    Allocator a = convert_to_callocator(&pico_allocator);

    // Setup for root module;
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("root-for-?")),
        .imports = (Imports) {.clauses = mk_import_clause_array(8, &a),},
        .exports = (Exports) {
            .export_all = true,
            .clauses = mk_export_clause_array(0, &a),
        },
    };

    *package = (Package) {
        .name = name,
        .gpa = pico_allocator,
    };
    package->root_module = mk_module(header, package, NULL),
    package->dependencies = mk_addr_list(4, &package->gpa);
    delete_module_header(header);
    return package;
}

void delete_package(Package* package) {
    PiAllocator pia = package->root_module->pico_allocator;
    sdelete_addr_list(package->dependencies);
    delete_module(package->root_module);
    call_free(package, &pia);
}

void add_dependency(Package *package, Package *dep) {
    push_addr(dep, &package->dependencies);
}

Result add_module(Symbol symbol, Module* module, Package* package) {
    return add_module_def(package->root_module, symbol, module); 
}

Name package_name(Package *package) {
    return package->name;
}

void add_import_clause(ImportClause clause, Module *module) {
    // Check if the clause already exists.
    // This saves us from redundantly traversing a module when loading the environment. 
    ImportClauseArray imclauses = module->header.imports.clauses;
    for (size_t i = 0; i < module->header.imports.clauses.len; i++) {
        if (imclause_eq(clause, imclauses.data[i])) return;
    } 
    clause.path = scopy_symbol_array(clause.path, &module->allocator);
    push_import_clause(clause, &module->header.imports.clauses);
}

Module* get_module(Symbol symbol, Package* package) {
    ModuleEntry* entry = get_def(symbol, package->root_module);
    if (entry && entry->is_module) {
        return entry->value;
    } else {
        return NULL;
    }
}

Module* package_root_module(Package* package) {
    return package->root_module;
}

// -----------------------------------------------------------------------------
// Module Implementation
// -----------------------------------------------------------------------------

// Forward declaration of utility functions
void update_function(uint8_t* val, SymPtrAMap new_vals, SymSArrAMap links);

Module* mk_module(ModuleHeader header, Package* pkg_parent, Module* parent) {
    PiAllocator pico_allocator = pkg_parent->gpa;

    Module* module = call_alloc(sizeof(Module), &pico_allocator);
    *module = (Module) {.pico_allocator = pico_allocator};
    module->allocator = convert_to_callocator(&module->pico_allocator);
    module->entries = mk_entry_amap(32, &module->allocator);
    module->header = copy_module_header(header, &module->allocator);
    module->lexical_parent_package = pkg_parent;
    module->lexical_parent_module = parent;
    module->executable_allocator = mk_executable_allocator(&module->allocator);
    return module;
}

void delete_decl(ModuleDecl decl, PiAllocator* pia) {
    switch (decl.sort) {
    case DeclType:
        delete_pi_type_p(decl.type, pia);
    }
}

void delete_decl_p(ModuleDecl* decl, PiAllocator* pia) {
    delete_decl(*decl, pia);
    call_free(decl, pia);
}

// Helper
void delete_module_entry(ModuleEntryInternal entry, Module* module) {
    if (entry.is_module) {
        delete_module(entry.value);
    } else {
        if (entry.type.sort == TKind || entry.type.sort == TConstraint) {
            delete_pi_type_p(entry.value, &module->pico_allocator);
        } else if (entry.type.sort == TTraitInstance) {
            call_free(*(void**)entry.value, &module->pico_allocator);
            call_free(entry.value, &module->pico_allocator);
        } else {
            call_free(entry.value, &module->pico_allocator);
        }
        delete_pi_type(entry.type, &module->pico_allocator);
    }

    if (entry.declarations) {
        for (size_t i = 0; i < entry.declarations->len; i++) {
            delete_decl_p(entry.declarations->data[i], &module->pico_allocator);
        }
        sdelete_ptr_array(*entry.declarations);
        call_free(entry.declarations, &module->pico_allocator);
        entry.declarations = NULL;
    }

    if (entry.backlinks) {
        delete_sym_sarr_amap(*entry.backlinks,
                             delete_symbol,
                             sdelete_size_array);
        call_free(entry.backlinks, &module->pico_allocator);
    }
    if (entry.code_segment) {
        sdelete_u8_array(*entry.code_segment);
        call_free(entry.code_segment, &module->pico_allocator);
    }
    if (entry.data_segment) {
        sdelete_u8_array(*entry.data_segment);
        call_free(entry.data_segment, &module->pico_allocator);
    }
}

void delete_module(Module* module) {
    for (size_t i = 0; i < module->entries.len; i++) {
        ModuleEntryInternal entry = module->entries.data[i].val;
        delete_module_entry(entry, module);
    };
    sdelete_entry_amap(module->entries);
    delete_module_header(module->header);

    release_executable_allocator(module->executable_allocator);
    call_free(module, &module->pico_allocator);
}


Segments prep_target(Module* module, Segments in_segments, Assembler* target, LinkData* links) {
    Segments out;
    if (in_segments.data.len != 0) {
        out.data = scopy_u8_array(in_segments.data, &module->allocator);
    } else {
        out.data = in_segments.data;
    }

    if (in_segments.code.len != 0) {
        out.code = scopy_u8_array(in_segments.code, &module->executable_allocator);
    } else {
        out.code = in_segments.code;
    }

    // Overwrite all links to data with new addresses
    if (in_segments.data.len != 0 && links) {
        U8Array executable = get_instructions(target);
        for (size_t i = 0; i < links->ed_links.len; i++) {
            LinkMetaData link = links->ed_links.data[i];
            void** address_ptr = (void**) ((void*)executable.data + link.source_offset);
            set_unaligned_ptr(address_ptr, out.data.data + link.dest_offset);
        }
        for (size_t i = 0; i < links->cd_links.len; i++) {
            LinkMetaData link = links->cd_links.data[i];
            void** address_ptr = (void**) ((void*)out.code.data + link.source_offset);
            set_unaligned_ptr(address_ptr, out.data.data + link.dest_offset);
        }

        for (size_t i = 0; i < links->dd_links.len; i++) {
            LinkMetaData link = links->dd_links.data[i];
            void** address_ptr = (void**) ((void*)out.data.data + link.source_offset);
            set_unaligned_ptr(address_ptr, out.data.data + link.dest_offset);
        }
    }

    // Overrite all links to code with new addresses
    if (in_segments.code.len != 0 && links) {
        U8Array executable = get_instructions(target);
        for (size_t i = 0; i < links->ec_links.len; i++) {
            LinkMetaData link = links->ec_links.data[i];
            void** address_ptr = (void**) ((void*)executable.data + link.source_offset);
            set_unaligned_ptr(address_ptr, out.code.data + link.dest_offset);
        }
        for (size_t i = 0; i < links->cc_links.len; i++) {
            LinkMetaData link = links->cc_links.data[i];
            void** address_ptr = (void**) ((void*)out.code.data + link.source_offset);
            set_unaligned_ptr(address_ptr, out.code.data + link.dest_offset);
        }
    }
    return out;
}

Result add_def(Module* module, Symbol symbol, PiType type, void* data, Segments segments, LinkData* links) {
    ModuleEntryInternal* old_entry = entry_lookup(symbol, module->entries);
    
    ModuleEntryInternal entry = (ModuleEntryInternal) {};
    PtrArray* declarations = NULL;
    if (old_entry) {
        declarations = old_entry->declarations;
        old_entry->declarations = NULL;
        delete_module_entry(*old_entry, module);
    }
    entry.declarations = declarations;
    entry.is_module = false;
    size_t size = pi_size_of(type);

    if (type.sort == TKind || type.sort == TConstraint) {
        PiType* t_val = *(PiType**)data; 
        entry.value = copy_pi_type_p(t_val, &module->pico_allocator);
    } else {
        if (type.sort == TTraitInstance) {
            size_t total = 0;
            for (size_t i = 0; i < type.instance.fields.len; i++) {
                total = pi_size_align(total, pi_align_of(*(PiType*)type.instance.fields.data[i].val));
                total += pi_size_of(*(PiType*)type.instance.fields.data[i].val);
            }
            void* new_memory = call_alloc(total, &module->pico_allocator);
            memcpy(new_memory, *(void**)data, total);

            entry.value = call_alloc(size, &module->pico_allocator);
            memcpy(entry.value, &new_memory, ADDRESS_SIZE);
        } else {
            entry.value = call_alloc(size, &module->pico_allocator);
            memcpy(entry.value, data, size);
        }

        if (segments.code.len != 0) {
            entry.code_segment = call_alloc(sizeof(U8Array), &module->pico_allocator);
            *entry.code_segment = segments.code;
        }
        if (segments.data.len != 0) {
            entry.data_segment = call_alloc(sizeof(U8Array), &module->pico_allocator);
            *entry.data_segment = segments.data;
        }
    }

    if (links) {
        // TODO (possible bug)
        // perhaps 'backlinks' of an entry need to be renamed/reworked so we can
        // distinguish between code segment, executable segment and value?
        entry.backlinks = call_alloc(sizeof(SymSArrAMap), &module->pico_allocator);
        *(entry.backlinks) = copy_sym_sarr_amap(links->external_code_links,
                                                copy_symbol,
                                                scopy_size_array,
                                                &module->allocator);

        if (segments.code.len != 0) {
            // Move this to thecode segment bit?
            // swap out self-references
            SymPtrAMap self_ref = mk_sym_ptr_amap(1, &module->allocator);
            sym_ptr_insert(symbol, entry.value, &self_ref);
            update_function(entry.code_segment->data, self_ref, links->external_code_links);
            sdelete_sym_ptr_amap(self_ref);
        }
    } else {
        entry.backlinks = NULL;
    }

    entry.type = copy_pi_type(type, &module->pico_allocator);

    // Free a previous definition (if it exists!)
    entry_insert(symbol, entry, &module->entries);

    Result out;
    out.type = Ok;
    return out;
}

ModuleDecl copy_decl(ModuleDecl decl, Allocator *a) {
    ModuleDecl out = (ModuleDecl) {.sort = decl.sort};
    PiAllocator pia = convert_to_pallocator(a);
    switch (decl.sort) {
    case DeclType:
        out.type = copy_pi_type_p(decl.type, &pia);
        break;
    }
    return out;
}

Result add_decl(Module *module, Symbol symbol, ModuleDecl decl) {
    ModuleEntryInternal entry = (ModuleEntryInternal) {};
    ModuleEntryInternal* old_entry = entry_lookup(symbol, module->entries);
    if (old_entry) entry = *old_entry;
    PtrArray* declarations = entry.declarations;
    if (!declarations) {
        declarations = mem_alloc(sizeof(PtrArray), &module->allocator);
        *declarations = mk_ptr_array( 1, &module->allocator);
    }

    bool found_decl = false;
    for (size_t i = 0; i < declarations->len; i++) {
        if (((ModuleDecl*)declarations->data[i])->sort == decl.sort) {
            *(ModuleDecl*)declarations->data[i] = copy_decl(decl, &module->allocator);
            found_decl = true;
        }
    }

    if (!found_decl) {
        ModuleDecl* declaration = mem_alloc(sizeof(ModuleDecl), &module->allocator);
        *declaration = copy_decl(decl, &module->allocator);
        push_ptr(declaration, declarations);
    }

    entry.declarations = declarations;
    entry_insert(symbol, entry, &module->entries);

    Result out;
    out.type = Ok;
    return out;
}

Result add_module_def(Module* module, Symbol symbol, Module* child) {
    ModuleEntryInternal entry = (ModuleEntryInternal) {
        .value = child,
        .is_module = true,
        .backlinks = NULL,
        .code_segment = NULL,
        .data_segment = NULL,
    };

    // Free a previous definition (if it exists!)
    // TODO BUG UB: possibly throw here?
    ModuleEntryInternal* old_entry = entry_lookup(symbol, module->entries);
    if (old_entry) {
        delete_module_entry(*old_entry, module);
    }

    entry_insert(symbol, entry, &module->entries);

    return (Result) {.type = Ok};
}

ModuleEntry* get_def(Symbol symbol, Module* module) {
    Module* root = module->lexical_parent_package->root_module;
    if (module != root) {
        return (ModuleEntry*)entry_lookup(symbol, module->entries);
    } else {
        ModuleEntry* e = (ModuleEntry*)entry_lookup(symbol, module->entries);
        if (e) return e;

        // Root module should also return definitions available in 
        //   imported packages...
        Package* package = module->lexical_parent_package;
        for (size_t i = 0; i < package->dependencies.len; i++) {
            Package* dep = package->dependencies.data[i];
            ModuleEntry* e = (ModuleEntry*)entry_lookup(symbol, dep->root_module->entries);
            if (e) return e;
        }
        return NULL;
    }
}

Symbol module_name(Module* module) {
    return module->header.name;
}

SymbolArray get_defined_symbols(Module* module, Allocator* a) {
    SymbolArray symbols = mk_symbol_array(module->entries.len, a);
    for (size_t i = 0; i < module->entries.len; i++) {
        push_symbol(module->entries.data[i].key, &symbols);
    };
    return symbols;
}

PtrArray get_defined_instances(Module* module, Allocator* a) {
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
