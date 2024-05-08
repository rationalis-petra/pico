#include "pretty/stream_printer.h"
#include "pretty/document.h"
#include "data/stream.h"

#include <stdio.h>


void print_str_doc(string str, void* vos) {
    ostream* os = (ostream*)vos;
    write_string(str, os);
}

void print_cat_doc(ptr_array arr, void* vos) {
    ostream* os = (ostream*)vos;
    for (size_t i = 0; i < arr.len; i++) {
        document* doc = (document*)arr.data[i];
        write_doc(doc, os);
    }
}

void print_sep_doc(ptr_array arr, void* vos) {
    ostream* os = (ostream*)vos;
    for (size_t i = 0; i < arr.len; i++) {
        document* doc = (document*)arr.data[i];
        write_doc(doc, os);
        write_impl(' ', os);
    }
}

void print_vsep_doc(ptr_array arr, void* vos) {
    ostream* os = (ostream*)vos;
    for (size_t i = 0; i < arr.len; i++) {
        document* doc = (document*)arr.data[i];
        write_doc(doc, os);
        write_impl('\n', os);
    }
}

void write_doc(document* doc, ostream* os) {
    document_visitor visitor;
    visitor.ctx = (void*)os;

    visitor.on_str_doc = print_str_doc;
    visitor.on_cat_doc = print_cat_doc;
    visitor.on_sep_doc = print_sep_doc;
    visitor.on_vsep_doc = print_vsep_doc;

    visit_document(doc, &visitor);
}

