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
} ModuleEntry;

typedef struct {
    uint64_t id;
    PtrArray args;
    Symbol src_sym;
    Module* src;
} InstanceSrc;

/* Definitions and codegen. 
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
    size_t source_offset;
    size_t dest_offset;
} LinkMetaData;

ARRAY_HEADER(LinkMetaData, link_meta, LinkMeta)

typedef struct {
    SymSArrAMap external_links;

    LinkMetaArray ec_links;
    LinkMetaArray ed_links;

    LinkMetaArray cc_links;
    LinkMetaArray cd_links;
} LinkData;

typedef struct {
    U8Array data;
    U8Array code;
} Segments;

// Package Interface
Package* mk_package(Name name, Allocator* a);
void delete_package(Package* package);
Result add_module(Symbol symbol, Module* module, Package* package);
Module* get_module(Symbol symbol, Package* package);
Module* get_root_module(Package* package);

// Module Interface
Module* mk_module(ModuleHeader header, Package* pkg_parent, Module* parent, Allocator* a);
void delete_module(Module* module);

// If we are going to define the result of evaluating (target), then it must be prepped
// so that the code and data segments are owned by the module
// This needs to be done BEFORE evaluation
Segments prep_target(Module* module, Segments in_segments, Assembler* target, LinkData* links);

// Add a value definition in to the module's namespace. Must be prepped (see above)
Result add_def(Module* module, Symbol symbol, PiType type, void* data, Segments segments, LinkData* links); 

// Add a module definition in to the module's namespace. 
Result add_module_def(Module* module, Symbol symbol, Module* child);

// Add an import clause into a module's namespace
// Note: The import clause will be copied, so the caller is still responsible
//       for cleaning up its copy of the clause.
void add_import_clause(ImportClause clause, Module* module);

ModuleEntry* get_def(Symbol symbol, Module* module);
SymbolArray get_exported_symbols(Module* module, Allocator* a);
PtrArray get_exported_instances(Module* module, Allocator* a);

String* get_name(Module* module);
Package* get_package(Module* module);
Module* get_parent(Module* module);
Imports get_imports(Module* module);
Exports get_exports(Module* module);

#endif
