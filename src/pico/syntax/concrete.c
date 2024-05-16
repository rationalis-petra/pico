#include "pico/syntax/concrete.h"
#include "pretty/standard_types.h"


document* pretty_rawtree(pi_rawtree tree, allocator a) {
    document* out = NULL;
    switch (tree.type) {
    case RawAtom: {
        out = pretty_atom(tree.data.atom, a);
        break;
    }
    case RawList: {
        ptr_array doc_arr = mk_ptr_array(tree.data.nodes.len + 2, a);
        push_ptr(mv_str_doc(mk_string("(", a), a), &doc_arr, a);
        for (size_t i = 0; i < tree.data.nodes.len; i++) {
            pi_rawtree node = *((pi_rawtree*)aref_ptr(i, tree.data.nodes));
            document* doc = pretty_rawtree(node, a);
            push_ptr(doc, &doc_arr, a);
        }
        push_ptr(mv_str_doc(mk_string(")", a), a), &doc_arr, a);
        out = mv_sep_doc(doc_arr, a);
        break;
    }
    }
    return out;
}

void delete_rawtree_ptr(pi_rawtree* tree_ptr, allocator a) {
    delete_rawtree(*tree_ptr, a);
    mem_free(tree_ptr, a);
}

void delete_rawtree(pi_rawtree tree, allocator a) {
    switch (tree.type) {
    case RawAtom:
        // TODO: correct this when pi values get garbage collection!
        break;
    case RawList:
        delete_ptr_array(tree.data.nodes, (void(*)(void*, allocator))delete_rawtree_ptr, a);
        break;
    }
}

document* pretty_atom(pi_atom atom, allocator a) {
    document* out = NULL;
    switch (atom.type) {
    case AI64: {
        out = pretty_i64(atom.int_64, a);
        break;
    }
    case ASymbol: {
        string* str = symbol_to_string(atom.symbol);
        if (str) {
            out = mk_str_doc(*str, a);
        }
        else {
            out = mv_str_doc(mk_string("Can't find symbol!", a), a);
        }
        break;
    }
    }
    return out;
}
