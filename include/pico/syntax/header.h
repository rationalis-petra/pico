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
    bool importAll;
    ImportClauseArray import_clauses;
} Imports;

//------------------------------------------------------------------------------
// Export Clauses
//------------------------------------------------------------------------------

typedef enum {
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
    ExportClauseArray export_clauses;
} Exports;

//------------------------------------------------------------------------------
// Module Header
//------------------------------------------------------------------------------

typedef struct {
    Symbol name;
    Imports imports;
    Exports exports;
} ModuleHeader;

#endif
