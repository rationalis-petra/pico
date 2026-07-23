#include "platform/signals.h"

#include "pico/syntax/header.h"
#include "data/meta/array_impl.h"

int cmp_import_clauses(ImportClause lhs, ImportClause rhs) {
    // TODO: this doesn't account for renames!!
    panic(mv_string("Not yet implemented: cmp_import_clauses"));
    /*
    int res = lhs.type - rhs.type;
    if (res) return res;
    res = lhs.path.len - rhs.path.len;
    if (res) return res;
    for (size_t i = 0; i < lhs.path.len; i++) {
        res = symbol_cmp(lhs.path.data[i], rhs.path.data[i]);
        if (res) return res;
    }
    return 0;
    */
}

int cmp_export_clauses(ExportClause lhs, ExportClause rhs) {
    // TODO: this doesn't account for renames!!
    int res = lhs.type - rhs.type;
    if (res) return res;
    return symbol_cmp(lhs.name, rhs.name);
}

ARRAY_COMMON_IMPL(ImportValue, import_value, ImportValue);
ARRAY_COMMON_IMPL(PathSegment, path_segment, PathSegment);
ARRAY_COMMON_IMPL(ImportClause, import_clause, ImportClause);

ARRAY_COMMON_IMPL(ExportClause, export_clause, ExportClause);

bool import_path_eq(PathSegmentArray p1, PathSegmentArray p2) {
    if (p1.len != p2.len) return false;
    for (size_t i = 0; i < p1.len; i++) {
        PathSegment s1 = p1.data[i];
        PathSegment s2 = p2.data[i];
        if (s1.type != s2.type) return false;
        switch (s1.type) {
        case SegSymbol:
            if (symbol_cmp(s1.symbol, s2.symbol) != 0) return false;
            break;
        case SegSymbols:
            if (s1.symbols.len != s2.symbols.len) return false;
            for (size_t j = 0; j < s1.symbols.len; j++) {
                if (symbol_cmp(s1.symbols.data[j], s2.symbols.data[j]) != 0) return false;
            }
            if (symbol_cmp(s1.symbol, s2.symbol) != 0) return false;
            break;
        case SegWildcard:
            // Do Nothong
            break;
        }
    }
    return true;
}

bool imclause_eq(ImportClause c1, ImportClause c2) {
    if (c1.type != c2.type) return false;
    if (c1.path.len != c2.path.len) return false;
    if (!import_path_eq(c1.path, c2.path)) return false;
    switch (c1.type) {
    case ImportSimple:
        return true;
    case ImportComplex:
      if (!((c1.import_types == c2.import_types) &
            (c1.import_instances == c2.import_instances) &
            (c1.import_values == c2.import_values) &
            (c1.import_as == c2.import_as)))
          return false;
      if (c1.import_values) {
          if (c1.values.len != c2.values.len) return false;
          for (size_t i = 0; i < c1.values.len; i++) {
              ImportValue v1 = c1.values.data[i];
              ImportValue v2 = c2.values.data[i];
              if (v1.should_rename != v2.should_rename) return false;
              if (v1.should_rename) {
                  if ((symbol_cmp(v1.from, v2.from) != 0) || (symbol_cmp(v1.to, v2.to) != 0)) return false;
              } else {
                  if (symbol_cmp(v1.from, v2.from) != 0) return false;
              }
          }
      }
      if (c1.import_as) {
        return symbol_cmp(c1.to, c2.to) == 0;
      }
      return true;
    case ImportAll:
        return true;
    }
    panic(mv_string("bad caluse"));
}

bool is_simple_path(PathSegmentArray path) {
    bool is_simple = true;
    for (size_t i = 0; i < path.len; i++) {
        is_simple &= path.data[i].type == SegSymbol;
    }
    return is_simple;
} 

ImportClause copy_import_clause(ImportClause clause, Allocator* a) {
    switch (clause.type) {
    case ImportSimple:
        return (ImportClause) {
            .type = ImportSimple,
            .path = scopy_path_segment_array(clause.path, a),
        };
    case ImportComplex:
        return (ImportClause) {
            .type = ImportComplex,
            .path = scopy_path_segment_array(clause.path, a),
            .import_instances = clause.import_instances,
            .import_types = clause.import_types,
            .import_values = clause.import_values,
            .import_as = clause.import_as,
            .values = clause.import_values ? scopy_import_value_array(clause.values, a) : (ImportValueArray){},
            .to = clause.to,
        };
    case ImportAll:
        return (ImportClause) {
            .type = ImportAll,
            .path = scopy_path_segment_array(clause.path, a),
        };
    }
    panic(mv_string("bad caluse"));
}

void delete_import_clause(ImportClause clause) {
    sdelete_path_segment_array(clause.path);
    if (clause.type == ImportComplex && clause.import_values)
        sdelete_import_value_array(clause.values);
}

Document* pretty_import_clause(ImportClause clause, Allocator* a) {
    PtrArray path_nodes = mk_ptr_array(clause.path.len * 2, a);
    for (size_t i = 0; i < clause.path.len; i++) {
        PathSegment segment = clause.path.data[i];
        switch (segment.type) {
        case SegSymbol:
            push_ptr(mk_str_doc(view_symbol_string(segment.symbol), a), &path_nodes);
            break;
        case SegSymbols: {
            PtrArray nodes = mk_ptr_array(segment.symbols.len, a);
            for (size_t i = 0; i < segment.symbols.len; i++) {
                push_ptr(mv_str_doc(view_symbol_string(segment.symbols.data[i]), a), &nodes);
            }
            Document* symbols = mv_group_doc(mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a), a);
            push_ptr(symbols, &path_nodes);
            break;
        }
        case SegWildcard: {
            PtrArray nodes = mk_ptr_array(2, a);
            push_ptr(mv_cat_doc(path_nodes, a), &nodes);
            push_ptr(mv_cstr_doc(":all", a), &nodes);
            Document* wildcard = mv_group_doc(mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a), a);
            push_ptr(wildcard, &path_nodes);
            break;
        }
        default:
            panic(mv_string("Invalid path segment provided to pretty_import_clause"));
        }
        if (i + 1 != clause.path.len)
            push_ptr(mv_cstr_doc(".", a), &path_nodes);
    }

    switch (clause.type) {
    case ImportSimple:
        return mv_cat_doc(path_nodes, a);
    case ImportComplex: {
        PtrArray nodes = mk_ptr_array(6, a);
        if (clause.import_instances) {
            push_ptr(mv_cstr_doc(":instances", a), &nodes);
        }
        if (clause.import_types) {
            push_ptr(mv_cstr_doc(":types", a), &nodes);
        }
        if (clause.import_as) {
            push_ptr(mv_cstr_doc(":as", a), &nodes);
            push_ptr(mk_str_doc(view_symbol_string(clause.to), a), &nodes);
        }
        if (clause.import_values) {
            push_ptr(mv_cstr_doc(":values", a), &nodes);
            PtrArray value_nodes = mk_ptr_array(clause.values.len, a);
            for (size_t i = 0; i < clause.values.len; i++) {
                ImportValue value = clause.values.data[i];
                if (value.should_rename) {
                    push_ptr(mk_str_doc(view_symbol_string(value.from), a), &value_nodes);
                    push_ptr(mk_cstr_doc(":as", a), &value_nodes);
                    push_ptr(mk_str_doc(view_symbol_string(value.to), a), &value_nodes);
                } else {
                    push_ptr(mk_str_doc(view_symbol_string(value.from), a), &value_nodes);
                }
            }
            push_ptr(mk_paren_doc("(", ")", mv_group_doc(mv_sep_doc(value_nodes, a), a), a), &nodes);
        }
        return mv_group_doc(mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a), a);
    }
    case ImportAll: {
        PtrArray nodes = mk_ptr_array(2, a);
        push_ptr(mv_cat_doc(path_nodes, a), &nodes);
        push_ptr(mv_cstr_doc(":all", a), &nodes);
        return mv_group_doc(mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a), a);
    }
    }
    panic(mv_string("bad import clause"));
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
        delete_import_clause(h.imports.clauses.data[i]);
    }
    sdelete_import_clause_array(h.imports.clauses);
    sdelete_export_clause_array(h.exports.clauses);
}
