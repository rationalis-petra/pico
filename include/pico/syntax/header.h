#ifndef __PICO_SYNTAX_HEADER_H
#define __PICO_SYNTAX_HEADER_H

#include "data/meta/array_header.h"
#include "components/pretty/document.h"
#include "pico/values/values.h"
#include "pico/data/symbol_array.h"

//------------------------------------------------------------------------------
// Import Clauses
//------------------------------------------------------------------------------

typedef enum {
    Import,       // basic import, e.g. (import num.i32) imports the module
                  // i32. Only the name is available (in this case, i32), the
                  // module is not opened. Can also import values (import num.i32.+)
    ImportAs,     // import and rename, e.g. (import num.i32 as int-32) 
    ImportMany,   // Import many values from a module, e.g. (import num.i32.(+ - * /))
    ImportAll,    // Import all values from a module, e.g. (import num.i32 :all) 
} ImportClause_t;

typedef struct {
    ImportClause_t type;
    SymbolArray path;
    union {
        Symbol rename;
        Symbol member;
        SymbolArray members;
    };
} ImportClause;

ARRAY_HEADER(ImportClause, import_clause, ImportClause)

typedef struct {
    ImportClauseArray clauses;
} Imports;

bool imclause_eq(ImportClause c1, ImportClause c2);
Document* pretty_import_clause(ImportClause clause, Allocator* a);

//------------------------------------------------------------------------------
// Export Clauses
//------------------------------------------------------------------------------

typedef enum {
    ExportAll,
    ExportName,
    ExportNameAs,
} ExportClause_t;

typedef struct {
    ExportClause_t type;
    Symbol name;
    Symbol rename;
} ExportClause;

ARRAY_HEADER(ExportClause, export_clause, ExportClause)

typedef struct {
    bool export_all;
    ExportClauseArray clauses;
} Exports;

//------------------------------------------------------------------------------
// Module Header
//------------------------------------------------------------------------------

typedef struct {
    Symbol name;
    Imports imports;
    Exports exports;
} ModuleHeader;

ModuleHeader copy_module_header(ModuleHeader h, Allocator* a);
void delete_module_header(ModuleHeader h);

#endif
