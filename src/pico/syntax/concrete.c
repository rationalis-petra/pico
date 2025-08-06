#include "data/meta/array_impl.h"
#include "platform/signals.h"
#include "components/pretty/standard_types.h"

#include "pico/syntax/concrete.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
int cmp_rawtree(RawTree lhs, RawTree rhs) {
    panic(mv_string("cmp_rawtree not implemented!"));
}
#pragma GCC diagnostic pop

ARRAY_CMP_IMPL(RawTree, cmp_rawtree, rawtree, RawTree)

bool is_expr(RawTree tree) {
    return tree.type == RawAtom || tree.branch.hint == HExpression;
}

Document* pretty_rawtree(RawTree tree, Allocator* a) {
    Document* out = NULL;
    switch (tree.type) {
    case RawAtom: {
        out = pretty_atom(tree.atom, a);
        break;
    }
    case RawBranch: {
        char* open; switch (tree.branch.hint) {
          case HSpecial: open = "["; break;
          case HImplicit: open = "{"; break;
          default: open = "("; break;
        };
        char* close; switch (tree.branch.hint) {
          case HSpecial: close = "]"; break;
          case HImplicit: close = "}"; break;
          default: close = ")"; break;
        };

        PtrArray doc_arr = mk_ptr_array(tree.branch.nodes.len, a);
        for (size_t i = 0; i < tree.branch.nodes.len; i++) {
            RawTree node = tree.branch.nodes.data[i];
            Document* doc = pretty_rawtree(node, a);
            push_ptr(doc, &doc_arr);
        }
        out = mk_paren_doc(open, close, mv_hsep_doc(doc_arr, a), a);
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
    case RawBranch:
        for (size_t i = 0; i < tree.branch.nodes.len; i++)
            delete_rawtree(tree.branch.nodes.data[i], a);
        sdelete_rawtree_array(tree.branch.nodes);
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
    case AFloating: {
        out = pretty_f64(atom.float_64, a);
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
        if (!str) {
            panic(mv_string("Error in pretty_atom: can't find symbol in symbol table!"));
        }
        out = mk_str_doc(*str, a);
        break;
    }
    case AString: {
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
