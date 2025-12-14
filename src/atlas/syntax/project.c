#include "atlas/syntax/project.h"

Document* pretty_project(Project proj, Allocator* a) {
    DocStyle former_style = scolour(colour(60, 190, 24), dstyle);
    DocStyle field_style = scolour(colour(60, 150, 210), dstyle);

    PtrArray nodes = mk_ptr_array(8, a);
    {
        PtrArray name_nodes = mk_ptr_array(2, a);
        push_ptr(mv_style_doc(field_style, mk_str_doc(mv_string("name"), a), a), &name_nodes);
        push_ptr(mk_str_doc(view_symbol_string(proj.package.package_name), a), &name_nodes);

        Document* name_doc = mv_group_doc(mk_paren_doc("(", ")", mv_sep_doc(name_nodes, a), a), a);
        push_ptr(name_doc, &nodes);
    }

    {
        PtrArray sub_nodes = mk_ptr_array(1 + proj.package.dependent_packages.len, a);
        push_ptr(mv_style_doc(field_style, mk_str_doc(mv_string("dependencies"), a), a), &sub_nodes);

        SymbolArray deps = proj.package.dependent_packages;
        for (size_t i = 0; i < deps.len; i ++) {
            push_ptr(mk_str_doc(view_symbol_string(deps.data[i]), a), &sub_nodes);
        }

        Document* sub_doc = mv_group_doc(mk_paren_doc("(", ")", mv_sep_doc(sub_nodes, a), a), a);
        push_ptr(sub_doc, &nodes);
    }

    PtrArray outer_nodes = mk_ptr_array(2, a);

    Document* pkg_sym_doc = mv_style_doc(former_style, mk_str_doc(mv_string("package"), a), a);
    push_ptr(pkg_sym_doc, &outer_nodes);

    push_ptr(mv_nest_doc(2, mv_vsep_doc(nodes, a), a), &outer_nodes);
    return mk_paren_doc("(", ")", mv_vsep_doc(outer_nodes, a), a);
}
