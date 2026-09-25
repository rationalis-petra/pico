#include "data/string.h"

#include "platform/signals.h"

#include "pico/data/error.h"
#include "pico/values/values.h"
#include "pico/syntax/concrete.h"
#include "pico/abstraction/helpers.h"



bool eq_symbol(RawTree* raw, Symbol s) {
  return (raw->type == RawAtom &&
          raw->atom.type == ASymbol &&
          raw->atom.symbol.name == s.name && 
          raw->atom.symbol.did == s.did);
}

bool is_symbol(RawTree raw) {
    return (raw.type == RawAtom && raw.atom.type == ASymbol);
}

bool get_symbol(RawTree raw, Symbol* symbol) {
    if (raw.type != RawAtom || raw.atom.type != ASymbol) {
        return false;
    }

    *symbol = raw.atom.symbol;
    return true;
}


/**
 * Return true if the provided rawtree matches the provided symbol, e.g.
 * i.e. is_key_symbol(raw, symbol("all"));
 */
bool is_key_symbol(RawTree raw, Symbol symbol) {
    if (raw.type == RawBranch && raw.branch.nodes.len == 2) {
        RawTree head = raw.branch.nodes.data[0];
        if (!is_symbol(head) || !symbol_eq(string_to_symbol(mv_string(":")), head.atom.symbol)) {
            return false;
        }

        raw = raw.branch.nodes.data[1];
        if (is_symbol(raw)) {
            return symbol_eq(symbol, raw.atom.symbol);
        } else {
            return false;
        }
    } else {
        return false;
    }
}

// Return 'true' if the rawtree is :<symbol> or (. symbol), and write symbol to out
bool get_fieldname(RawTree* raw, FieldSpec spec, Symbol* fieldname) {
    if (raw->type != RawBranch || raw->branch.nodes.len != 2) return false;
    if (raw->branch.hint != HExpression) return false;


    RawTree head = raw->branch.nodes.data[0];
    if (!is_symbol(head)) return false;
    if ((spec & FDot) && !symbol_eq(string_to_symbol(mv_string(".")), head.atom.symbol)) return false;
    if ((spec & FColon) && !symbol_eq(string_to_symbol(mv_string(":")), head.atom.symbol)) return false;

    RawTree field = raw->branch.nodes.data[1];
    if (!is_symbol(field)) return false;

    *fieldname = field.atom.symbol;
    return true;
}
                                                            
Symbol get_symbol_err(RawTree raw, PiErrorPoint* point, Allocator* a) {
    if (raw.type != RawAtom) {
        PicoError err = {
            .range = raw.range,
            .message = mk_str_doc(mv_string("Expected symbol here, got compound term instead."), a),
        };
        throw_pi_error(point, err);
    }

    if (raw.atom.type != ASymbol) {
        PicoError err = {
            .range = raw.range,
            .message = mk_str_doc(mv_string("Expected symbol here."), a),
        };
        throw_pi_error(point, err);
    }

    return raw.atom.symbol;
}

/**
 * Helper function for retrieving a symbol list
 * returns true on success, false on failure
 */
bool get_symbol_list(SymbolArray* arr, RawTree nodes, Allocator* a) {
    if (nodes.type != RawBranch) { return false; }
    *arr = mk_symbol_array(nodes.branch.nodes.len, a);

    for (size_t i = 0; i < nodes.branch.nodes.len; i++) {
        RawTree node = nodes.branch.nodes.data[i];
        if (node.type != RawAtom || node.atom.type != ASymbol) { return false; }
        push_symbol(node.atom.symbol, arr);
    }
    return true;
}

bool get_name_list(NameArray* arr, RawTree nodes, Allocator* a) {
    if (nodes.type != RawBranch) { return false; }
    *arr = mk_name_array(nodes.branch.nodes.len, a);

    for (size_t i = 0; i < nodes.branch.nodes.len; i++) {
        RawTree node = nodes.branch.nodes.data[i];
        if (node.type != RawAtom || node.atom.type != ASymbol) { return false; }
        if (node.atom.symbol.did != 0) { return false; }
        push_name(node.atom.symbol.name, arr);
    }
    return true;
}


RawTree raw_slice(RawTree* raw, size_t drop) {
#ifdef DEBUG
  if (drop > raw->branch.nodes.len) {
      panic(mv_string("Dropping more nodes than there are!"));
  }
#endif
    return (RawTree) {
        .type = RawBranch,
        .range.start = raw->branch.nodes.data[drop].range.start,
        .range.end = raw->branch.nodes.data[raw->branch.nodes.len - 1].range.end,
        .branch.hint = raw->branch.hint,
        .branch.nodes.len = raw->branch.nodes.len - drop,
        .branch.nodes.size = raw->branch.nodes.size - drop,
        .branch.nodes.data = raw->branch.nodes.data + drop,
        .branch.nodes.gpa = raw->branch.nodes.gpa,
    };
}
