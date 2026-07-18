#ifndef __PICO_SYNTAX_HEADER_H
#define __PICO_SYNTAX_HEADER_H

#include "data/meta/array_header.h"
#include "components/pretty/document.h"

#include "pico/values/values.h"
#include "pico/data/range.h"
#include "pico/data/symbol_array.h"

/**
 * ------------------------------------------------------------------------------
 *
 *     Import Clauses
 *
 * ------------------------------------------------------------------------------
*/

typedef enum : uint8_t {
  ImportSimple,  /** A regular import, covers most types of import clause */
  ImportComplex, /** A regular import, covers most types of import clause */
  ImportAll,     /** Import all values from a module, e.g. (import (num.i32 :all)) */
} ImportClause_t;

typedef struct {
    bool should_rename;
    Symbol from;
    Symbol to;
} ImportValue;

ARRAY_HEADER(ImportValue, import_value, ImportValue)

typedef enum : uint8_t {
  SegSymbol,
  SegSymbols,
  SegWildcard,
} PathSegment_t;

typedef struct {
    PathSegment_t type;
    union {
        Symbol symbol;
        SymbolArray symbols;
    };
} PathSegment;

ARRAY_HEADER(PathSegment, path_segment, PathSegment)

typedef struct {
    ImportClause_t type;
    PathSegmentArray path;

    // Target Details
    bool import_instances;
    bool import_types;
    bool import_as;
    bool import_values;
    ImportValueArray values;
    Symbol to;
} ImportClause;

ARRAY_HEADER(ImportClause, import_clause, ImportClause)

typedef struct {
    ImportClauseArray clauses;
} Imports;

bool imclause_eq(ImportClause c1, ImportClause c2);
bool is_simple_path(PathSegmentArray path);
ImportClause copy_import_clause(ImportClause clause, Allocator* a);
void delete_import_clause(ImportClause clause);
Document* pretty_import_clause(ImportClause clause, Allocator* a);

/**
 * ------------------------------------------------------------------------------
 *
 *     Export Clauses
 *
 * ------------------------------------------------------------------------------
*/

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

/**
 * ------------------------------------------------------------------------------
 *
 *     Re-Export Clauses
 *
 * TODO: implement me!
 * ------------------------------------------------------------------------------
 */

/**
 * ------------------------------------------------------------------------------
 *
 *     Moudule Header
 *
 * ------------------------------------------------------------------------------
 */

typedef struct {
    Symbol name;
    Imports imports;
    Exports exports;
    Range range;
} ModuleHeader;

ModuleHeader copy_module_header(ModuleHeader h, Allocator* a);
void delete_module_header(ModuleHeader h);

#endif
