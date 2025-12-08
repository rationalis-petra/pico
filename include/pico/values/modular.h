#ifndef __PICO_VALUES_MODULAR_H
#define __PICO_VALUES_MODULAR_H

#include "data/result.h"

#include "components/assembler/assembler.h"
#include "components/assembler/link_data.h"

#include "pico/data/client/allocator.h"
#include "pico/data/client/list.h"
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
    PtrArray* declarations;
} ModuleEntry;

typedef struct {
    uint64_t id;
    AddrPiList args;
    Symbol src_sym;
    Module* src;
} InstanceSrc;

/* Declarations and Modules
 * ------------------------
 * A declaration is simply some kind of data we want to attach to a symbol
 * in a module. At the moment, the only supported declaration is a 'type'
 * declaration, which serves to report an error if a value (when defined) has a
 * type that does not match what was declared.
 *
 * Future intended declarations include:
 * - Documentation strings
 * - Custom data that is accessible by macros/metaprogramming tools
 * - Optimisation hints, such as 'inline yes/no/auto'
 * - Optimisation aggressiveness, i.e. 'speed n'
 * - Memory safetychecks, either 'unsafe' (no checks), 
 *   'enforced' (borrow checker) or 'auto' (gc/refcount integration)
 */

typedef enum {
    DeclType,
} DeclSort;

typedef struct {
    DeclSort sort;
    union {
        PiType* type;
    };
} ModuleDecl;

/* Definitions and codegen 
 * ------------------------
 * Codegen is relatively simple: given an expression e.g. (+ 2 3) and an
 * assembler, generate use the assembler to generate the code that corresponds
 * to the expression, e.g. "mov 2, rax; add rax, 3; push rax". Complexity is introduced because:
 * • Expressions may contain data such as strings. If the result is defined,
 *   these may need to be copied into the defining module. 
 * • Expressions may contain (or be) procedures, again, meaning that if defined
 *   then the procedure code needs to be copied by the defining module.
 * 
 * To accommodate this, code generation has two copyable targets as follows: 
 * • There is a 'eval segment' assembler, which is the entry-point to the experssion.
 * • There is a 'code-segment' assembler, where output of procedure code within the
 *   expression is put.
 * 
 * In order to facilitate copying of both code and data to new addresses,
 * internal (absolute) pointers must be updated, and so link information has to
 * be maintained.
 * 
 * As both the code and data are effectively byte-arrays, there are 4 pieces of
 * link data: 
 * | Matches chunks (addresses) in the | To indices in the | Name     |
 * |-----------------------------------|-------------------|----------|
 * | Eval Segment                      | Code Segment      | ec_links |
 * | Eval Segment                      | Data Segment      | ed_links |
 * | Code Segment                      | Code Segment      | cc_links |
 * | Code Segment                      | Data Segment      | cd_links |
 * | Data Segment                      | Code Segment      | N/A      |
 * | Data Segment                      | Data Segment      | N/A      |
 * 
 * Note that the data segment is only for strings, so cannot have any links to
 * other segments.
 * 
 * Finally, there are the 'external links', which are used to indicate locations
 * in the code segment which are addresses of specific symbols (values). These
 * locations need updating if the symbol is redefined.
 * 
 */

typedef struct {
    U8Array data;
    U8Array code;
} Segments;

// Package Interface
Package* mk_package(Name name, PiAllocator pico_allocator);
void delete_package(Package* package);
Result add_module(Symbol symbol, Module* module, Package* package);
Module* get_module(Symbol symbol, Package* package);
Module* get_root_module(Package* package);

// Module Interface
Module* mk_module(ModuleHeader header, Package* pkg_parent, Module* parent, PiAllocator pico_allocator);
void delete_module(Module* module);

// If we are going to define the result of evaluating (target), then it must be prepped
// so that the code and data segments are owned by the module.
// This needs to be done BEFORE evaluation
Segments prep_target(Module* module, Segments in_segments, Assembler* target, LinkData* links);

// Add a value definition in to the module's namespace. Must be prepped (see above)
Result add_def(Module* module, Symbol symbol, PiType type, void* data, Segments segments, LinkData* links); 

// Add a module definition in to the module's namespace. 
Result add_module_def(Module* module, Symbol symbol, Module* child);

// Add a declaration into the module's namespace. New declarations will override
// old ones.
Result add_decl(Module* module, Symbol symbol, ModuleDecl decl); 

// Add an import clause into a module's namespace
// Note: The import clause will be copied, so the caller is still responsible
//       for cleaning up its copy of the clause.
void add_import_clause(ImportClause clause, Module* module);

ModuleEntry* get_def(Symbol symbol, Module* module);
SymbolArray get_defined_symbols(Module* module, Allocator* a);
PtrArray get_defined_instances(Module* module, Allocator* a);

String get_name(Module* module, Allocator* a);
Package* get_package(Module* module);
Module* get_parent(Module* module);
Imports get_imports(Module* module);
Exports get_exports(Module* module);

#endif
