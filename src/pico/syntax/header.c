#include "pico/syntax/header.h"
#include "data/meta/array_impl.h"

int cmp_import_clauses(ImportClause lhs, ImportClause rhs) {
    // TODO: this doesn't account for renames!!
    int res = lhs.type - rhs.type;
    if (res) return res;
    return cmp_symbol(lhs.name, rhs.name);
}

int cmp_export_clauses(ExportClause lhs, ExportClause rhs) {
    // TODO: this doesn't account for renames!!
    int res = lhs.type - rhs.type;
    if (res) return res;
    return cmp_symbol(lhs.name, rhs.name);
}

ARRAY_CMP_IMPL(ImportClause, cmp_import_clauses, import_clause, ImportClause);

ARRAY_CMP_IMPL(ExportClause, cmp_export_clauses, export_clause, ExportClause);

ModuleHeader copy_module_header(ModuleHeader h, Allocator* a) {
    return (ModuleHeader) {
        .name = h.name,
        .imports.clauses = scopy_import_clause_array(h.imports.clauses, a),

        .exports.export_all = h.exports.export_all,
        .exports.clauses = scopy_export_clause_array(h.exports.clauses, a),
    };
}

void delete_module_header(ModuleHeader h) {
    sdelete_import_clause_array(h.imports.clauses);
    sdelete_export_clause_array(h.exports.clauses);
}
