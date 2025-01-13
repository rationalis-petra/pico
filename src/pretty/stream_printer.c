#include "pretty/stream_printer.h"
#include "pretty/document.h"
#include "data/stream.h"

void print_str_doc(String str, void* vos) {
    OStream* os = (OStream*)vos;
    write_string(str, os);
}

void print_cat_doc(PtrArray arr, void* vos) {
    OStream* os = (OStream*)vos;
    for (size_t i = 0; i < arr.len; i++) {
        Document* doc = (Document*)arr.data[i];
        write_doc(doc, os);
    }
}

void print_sep_doc(PtrArray arr, void* vos) {
    OStream* os = (OStream*)vos;
    for (size_t i = 0; i < arr.len; i++) {
        Document* doc = (Document*)arr.data[i];
        write_doc(doc, os);
        if (i + 1 < arr.len) write_impl(' ', os);
    }
}

void print_vsep_doc(PtrArray arr, void* vos) {
    OStream* os = (OStream*)vos;
    for (size_t i = 0; i < arr.len; i++) {
        Document* doc = (Document*)arr.data[i];
        write_doc(doc, os);
        if (i + 1 < arr.len) write_impl('\n', os);
    }
}

void write_doc(Document* doc, OStream* os) {
    DocumentVisitor visitor;
    visitor.ctx = (void*)os;

    visitor.on_str_doc = print_str_doc;
    visitor.on_cat_doc = print_cat_doc;
    visitor.on_sep_doc = print_sep_doc;
    visitor.on_vsep_doc = print_vsep_doc;

    visit_document(doc, &visitor);
}
