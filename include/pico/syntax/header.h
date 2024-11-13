#ifndef __PICO_SYNTAX_HEADER_H
#define __PICO_SYNTAX_HEADER_H

#include "data/meta/array_header.h"
#include "pico/values/values.h"

//------------------------------------------------------------------------------
// Import Clauses
//------------------------------------------------------------------------------

typedef enum {
    ImportName,
    ImportNameAs,
    ImportPath,
    ImportPathMany,
    ImportPathAll,
} ImportClause_t;

typedef struct {
    ImportClause_t type;
    Symbol name;
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
