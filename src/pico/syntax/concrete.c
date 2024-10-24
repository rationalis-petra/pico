#include "pico/syntax/concrete.h"
#include "pretty/standard_types.h"


Document* pretty_rawtree(RawTree tree, Allocator* a) {
    Document* out = NULL;
    switch (tree.type) {
    case RawAtom: {
        out = pretty_atom(tree.atom, a);
        break;
    }
    case RawList: {
        char* open; switch (tree.hint) {
          case HSpecial: open = "["; break;
          case HImplicit: open = "{"; break;
          default: open = "("; break;
        };
        char* close; switch (tree.hint) {
          case HSpecial: close = "]"; break;
          case HImplicit: close = "}"; break;
          default: close = ")"; break;
        };

        PtrArray doc_arr = mk_ptr_array(tree.nodes.len + 2, a);
        push_ptr(mv_str_doc(mk_string(open, a), a), &doc_arr);
        for (size_t i = 0; i < tree.nodes.len; i++) {
            RawTree node = *((RawTree*)tree.nodes.data[i]);
            Document* doc = pretty_rawtree(node, a);
            push_ptr(doc, &doc_arr);
        }
        push_ptr(mv_str_doc(mk_string(close, a), a), &doc_arr);
        out = mv_sep_doc(doc_arr, a);
        break;
    }
    }
    return out;
}

void delete_rawtree_ptr(RawTree* tree_ptr, Allocator* a) {
    delete_rawtree(*tree_ptr, a);
    mem_free(tree_ptr, a);
}

void delete_rawtree(RawTree tree, Allocator* a) {
    switch (tree.type) {
    case RawAtom:
        break;
    case RawList:
        for (size_t i = 0; i < tree.nodes.len; i++)
            delete_rawtree_ptr(tree.nodes.data[i], a);
        sdelete_ptr_array(tree.nodes);
        break;
    }
}

Document* pretty_atom(Atom atom, Allocator* a) {
    Document* out = NULL;
    switch (atom.type) {
    case AIntegral: {
        out = pretty_i64(atom.int_64, a);
        break;
    }
    case ABool: {
        if (atom.int_64 == 0) {
            out = mk_str_doc(mv_string("false"), a);
        } else {
            out = mk_str_doc(mv_string("true"), a);
        }
        break;
    }
    case ASymbol: {
        String* str = symbol_to_string(atom.symbol);
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
