#include "platform/signals.h"

#include "atlas/syntax/stanza.h"

Document* pretty_stanza(Stanza stanza, Allocator* a) {
    Document* out = NULL;
    DocStyle former_style = scolour(colour(60, 190, 24), dstyle);
    DocStyle field_style = scolour(colour(60, 150, 210), dstyle);
    DocStyle str_style = scolour(colour(120, 200, 60), dstyle);

    switch (stanza.type) {
    case StLibrary: {
        PtrArray nodes = mk_ptr_array(8, a);
        {
            PtrArray name_nodes = mk_ptr_array(2, a);
            push_ptr(mv_style_doc(field_style, mk_str_doc(mv_string("name"), a), a), &name_nodes);
            push_ptr(mk_str_doc(view_symbol_string(stanza.library.name), a), &name_nodes);

            Document* name_doc = mv_group_doc(mk_paren_doc("(", ")", mv_sep_doc(name_nodes, a), a), a);
            push_ptr(name_doc, &nodes);
        }

        {
            PtrArray filename_nodes = mk_ptr_array(2, a);
            push_ptr(mv_style_doc(field_style, mk_str_doc(mv_string("file"), a), a), &filename_nodes);
            if (stanza.library.filename.type == Some) {
                push_ptr(mk_str_doc(stanza.executable.filename, a), &filename_nodes);
            } else {
                push_ptr(mk_str_doc(mv_string(":none"), a), &filename_nodes);
            }

            Document* filename_doc = mv_group_doc(mk_paren_doc("(", ")", mv_sep_doc(filename_nodes, a), a), a);
            push_ptr(filename_doc, &nodes);
        }

        {
            PtrArray sub_nodes = mk_ptr_array(1 + stanza.executable.dependencies.len, a);
            push_ptr(mv_style_doc(field_style, mk_str_doc(mv_string("submodules"), a), a), &sub_nodes);

            StringArray subs = stanza.library.submodules;
            for (size_t i = 0; i < subs.len; i ++) {
                PtrArray inner_nodes = mk_ptr_array(3, a);
                push_ptr(mk_str_doc(mv_string("\""), a), &inner_nodes);
                push_ptr(mk_str_doc(subs.data[i], a), &inner_nodes);
                push_ptr(mk_str_doc(mv_string("\""), a), &inner_nodes);
                push_ptr(mv_style_doc(str_style, mv_cat_doc(inner_nodes, a), a), &sub_nodes);
            }

            Document* sub_doc = mv_group_doc(mk_paren_doc("(", ")", mv_sep_doc(sub_nodes, a), a), a);
            push_ptr(sub_doc, &nodes);
        }

        PtrArray outer_nodes = mk_ptr_array(2, a);

        Document* lib_sym_doc = mv_style_doc(former_style, mk_str_doc(mv_string("library"), a), a);
        push_ptr(lib_sym_doc, &outer_nodes);

        push_ptr(mv_nest_doc(2, mv_vsep_doc(nodes, a), a), &outer_nodes);
        out = mk_paren_doc("(", ")", mv_vsep_doc(outer_nodes, a), a);
        break;
    }

    case StExecutable: {
        PtrArray nodes = mk_ptr_array(8, a);
        {
            PtrArray name_nodes = mk_ptr_array(2, a);
            push_ptr(mv_style_doc(field_style, mk_str_doc(mv_string("name"), a), a), &name_nodes);
            push_ptr(mk_str_doc(view_symbol_string(stanza.executable.name), a), &name_nodes);

            Document* name_doc = mv_group_doc(mk_paren_doc("(", ")", mv_sep_doc(name_nodes, a), a), a);
            push_ptr(name_doc, &nodes);
        }

        {
            PtrArray filename_nodes = mk_ptr_array(2, a);
            push_ptr(mv_style_doc(field_style, mk_str_doc(mv_string("file"), a), a), &filename_nodes);
            push_ptr(mk_str_doc(stanza.executable.filename, a), &filename_nodes);

            Document* filename_doc = mv_group_doc(mk_paren_doc("(", ")", mv_sep_doc(filename_nodes, a), a), a);
            push_ptr(filename_doc, &nodes);
        }

        {
            PtrArray entry_point_nodes = mk_ptr_array(2, a);
            push_ptr(mv_style_doc(field_style, mk_str_doc(mv_string("entry-point"), a), a), &entry_point_nodes);
            push_ptr(mk_str_doc(view_symbol_string(stanza.executable.entry_point), a), &entry_point_nodes);

            Document* entry_point_doc = mv_group_doc(mk_paren_doc("(", ")", mv_sep_doc(entry_point_nodes, a), a), a);
            push_ptr(entry_point_doc, &nodes);
        }

        {
            PtrArray dep_nodes = mk_ptr_array(1 + stanza.executable.dependencies.len, a);
            push_ptr(mv_style_doc(field_style, mk_str_doc(mv_string("dependencies"), a), a), &dep_nodes);

            SymbolArray deps = stanza.executable.dependencies;
            for (size_t i = 0; i < deps.len; i ++) {
                push_ptr(mk_str_doc(view_symbol_string(deps.data[i]), a), &dep_nodes);
            }

            Document* dep_doc = mv_group_doc(mk_paren_doc("(", ")", mv_sep_doc(dep_nodes, a), a), a);
            push_ptr(dep_doc, &nodes);
        }

        PtrArray outer_nodes = mk_ptr_array(2, a);

        Document* exec_sym_doc = mv_style_doc(former_style, mk_str_doc(mv_string("executable"), a), a);
        push_ptr(exec_sym_doc, &outer_nodes);

        push_ptr(mv_nest_doc(2, mv_vsep_doc(nodes, a), a), &outer_nodes);
        out = mk_paren_doc("(", ")", mv_vsep_doc(outer_nodes, a), a);
        break;
    }

    }

    if (out == NULL)  {
        panic(mv_string("Internal Error in pretty_stanza: Unknown syntax Type"));
    }
    return out;
}
