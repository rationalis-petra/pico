
#include "pico/syntax/header.h"
#include "data/meta/array_impl.h"

int cmp_import_clauses(ImportClause lhs, ImportClause rhs) {
    // TODO: this doesn't account for renames!!
    int res = lhs.type - rhs.type;
    if (res) return res;
    return lhs.name - rhs.name;
}

int cmp_export_clauses(ExportClause lhs, ExportClause rhs) {
    // TODO: this doesn't account for renames!!
    int res = lhs.type - rhs.type;
    if (res) return res;
    return lhs.name - rhs.name;
}

ARRAY_CMP_IMPL(ImportClause, cmp_import_clauses, import_clause, ImportClause)

ARRAY_CMP_IMPL(ExportClause, cmp_export_clauses, export_clause, ExportClause)
