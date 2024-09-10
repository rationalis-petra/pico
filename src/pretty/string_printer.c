#include "pretty/string_printer.h"
#include "pretty/document.h"

#include <string.h>

static size_t max(size_t a, size_t b) {
    return a < b ? b : a;
}

void doc_len(Document* doc, size_t* len);

void str_doc_len(String str, void* ctx) {
    size_t* sz = (size_t*)ctx;
    *sz += str.memsize - 1; // subtrat null byte
}

void cat_doc_len(PtrArray docs, void* ctx) {
    size_t* sz = (size_t*)ctx;
    *sz += max(docs.len, 1) - 1;
    for (size_t i = 0; i < docs.len; i++)
        doc_len(docs.data[i], ctx);
}

void sep_doc_len(PtrArray docs, void* ctx) {
    size_t* sz = (size_t*)ctx;
    *sz += max(docs.len, 1) - 1;
    for (size_t i = 0; i < docs.len; i++)
        doc_len(docs.data[i], ctx);
}

void vsep_doc_len(PtrArray docs, void* ctx) {
    size_t* sz = (size_t*)ctx;
    // TODO: CRLF on windows? - 2 chars!
    *sz += max(docs.len, 1) - 1;
    for (size_t i = 0; i < docs.len; i++)
        doc_len(docs.data[i], ctx);
}

void doc_len(Document* doc, size_t* len) {
    DocumentVisitor len_visitor;
    len_visitor.on_str_doc = str_doc_len;
    len_visitor.on_cat_doc = cat_doc_len;
    len_visitor.on_sep_doc = sep_doc_len;
    len_visitor.on_vsep_doc = vsep_doc_len;
    len_visitor.ctx = len;

    visit_document(doc, &len_visitor);
}

void doc_str(Document* doc, uint8_t** data);

void str_doc_str(String str, void* data) {
    void** dptr = (void*)data;
    memcpy(*dptr, str.bytes, str.memsize - 1);
    *dptr += str.memsize - 1;
}

void cat_doc_str(PtrArray docs, void* ctx) {
    for (size_t i = 0; i < docs.len; i++) {
        doc_str(docs.data[i], ctx);
    }
}

void sep_doc_str(PtrArray docs, void* ctx) {
    uint8_t** cptr = (uint8_t**)ctx;
    for (size_t i = 0; i < docs.len; i++) {
        doc_str(docs.data[i], ctx);
        if (i + 1 < docs.len) {
            **cptr = ' '; 
            *cptr += 1;
        }
    }
}

void vsep_doc_str(PtrArray docs, void* ctx) {
    uint8_t** cptr = (uint8_t**)ctx;
    for (size_t i = 0; i < docs.len; i++) {
        doc_str(docs.data[i], ctx);
        if (i + 1 < docs.len) {
            **cptr = '\n'; 
            *cptr += 1;
        }
    }
}

void doc_str(Document* doc, uint8_t** data) {
    DocumentVisitor str_visitor;
    str_visitor.on_str_doc = str_doc_str;
    str_visitor.on_cat_doc = cat_doc_str;
    str_visitor.on_sep_doc = sep_doc_str;
    str_visitor.on_vsep_doc = vsep_doc_str;
    str_visitor.ctx = data;

    visit_document(doc, &str_visitor);
}

String doc_to_str(Document* doc, Allocator* a) {
    size_t total_len = 1; // for null byte
    doc_len(doc, &total_len);
    
    String str;
    str.memsize = total_len;
    str.bytes = mem_alloc(total_len, a);
    uint8_t* mptr = str.bytes; // mptr is modified, but we don't want str.bytes modified!
    doc_str(doc, &mptr);

    return str;
}
