#include "platform/signals.h"
#include "data/meta/array_impl.h"

#include "atlas/syntax/concrete.h"

ARRAY_COMMON_IMPL(RawAtlas, rawatlas, RawAtlas)

Document* pretty_rawatlas(RawAtlas tree, Allocator* a) {
    Document* out = NULL;
    switch (tree.type) {
    case AtlAtom: {
        out = pretty_atlas_atom(tree.atom, a);
        break;
    }
    case AtlBranch: {
        PtrArray doc_arr = mk_ptr_array(tree.branch.len, a);
        for (size_t i = 0; i < tree.branch.len; i++) {
            RawAtlas node = tree.branch.data[i];
            Document* doc = pretty_rawatlas(node, a);
            push_ptr(doc, &doc_arr);
        }
        out = mv_group_doc(mk_paren_doc("(", ")", mv_sep_doc(doc_arr, a), a), a);
        break;
    }
    }
    return out;
}


Document* pretty_atlas_atom(AtAtom atom, Allocator* a) {
    Document* out = NULL;
    switch (atom.type) {
    case AtSymbol: {
        String str = symbol_to_string(atom.symbol, a);
        out = mv_str_doc(str, a);
        break;
    }
    case AtKeyword: {
        String str = symbol_to_string(atom.keyword, a);
        out = mv_str_doc(string_cat(mv_string(":"), str, a), a);
        mem_free(str.bytes, a);
        break;
    }
    case AtString: {
        Document* delimiter = mk_str_doc(mv_string("\""), a);
        PtrArray nodes = mk_ptr_array(3, a);
        push_ptr(delimiter, &nodes);
        push_ptr(mv_str_doc(atom.string, a), &nodes);
        push_ptr(delimiter, &nodes);
        out = mk_cat_doc(nodes, a);
        break;
    }
    default:
        panic(mv_string("Invalid Atom provided to pretty_atom!"));
    }
    return out;
}
