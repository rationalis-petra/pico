#ifndef __PICO_SYNTAX_HEADER_H
#define __PICO_SYNTAX_HEADER_H

#include "data/meta/array_header.h"
#include "data/string.h"

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
} ImportClause;

ARRAY_HEADER(ImportClause, import_caluse, ImportClause)

typedef struct {
    bool exportAll;
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
} ExportClause;

ARRAY_HEADER(ExportClause, export_caluse, ExportClause)

typedef struct {
    bool exportAll;
    ExportClauseArray export_clauses;
} Exports;

//------------------------------------------------------------------------------
// Module Header
//------------------------------------------------------------------------------

typedef struct {
    String name;
    Imports imports;
    Exports exports;
} ModuleHeader;

#endif
