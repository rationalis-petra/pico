#include "pico/syntax/header.h"
#include "data/meta/array_impl.h"

int cmp_import_clauses(ImportClause lhs, ImportClause rhs) {
    // TODO: this doesn't account for renames!!
    int res = lhs.type - rhs.type;
    if (res) return res;
    res = lhs.path.len - rhs.path.len;
    if (res) return res;
    for (size_t i = 0; i < lhs.path.len; i++) {
        res = cmp_symbol(lhs.path.data[i], rhs.path.data[i]);
        if (res) return res;
    }
    return 0;
}

int cmp_export_clauses(ExportClause lhs, ExportClause rhs) {
    // TODO: this doesn't account for renames!!
    int res = lhs.type - rhs.type;
    if (res) return res;
    return cmp_symbol(lhs.name, rhs.name);
}

ARRAY_CMP_IMPL(ImportClause, cmp_import_clauses, import_clause, ImportClause);

ARRAY_CMP_IMPL(ExportClause, cmp_export_clauses, export_clause, ExportClause);

ImportClause copy_import_clause(ImportClause clause, Allocator* a) {
    ImportClause out = clause;
    out.path = scopy_symbol_array(clause.path, a);
    return out;
}

ModuleHeader copy_module_header(ModuleHeader h, Allocator* a) {
    return (ModuleHeader) {
        .name = h.name,
        .imports.clauses = copy_import_clause_array(h.imports.clauses, copy_import_clause, a),

        .exports.export_all = h.exports.export_all,
        .exports.clauses = scopy_export_clause_array(h.exports.clauses, a),
    };
}

void delete_module_header(ModuleHeader h) {
    for (size_t i = 0; i < h.imports.clauses.len; i++) {
        sdelete_symbol_array(h.imports.clauses.data[i].path);
    }
    sdelete_import_clause_array(h.imports.clauses);
    sdelete_export_clause_array(h.exports.clauses);
}
