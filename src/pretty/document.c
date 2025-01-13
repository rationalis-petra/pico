#include <string.h>

#include "data/string.h"
#include "data/array.h" 
#include "pretty/document.h"


typedef enum {
    StringDocument,
    CatDocument,
    SepDocument,
    VSepDocument
} DocumentType;

struct Document {
    DocumentType doc_type;
    union {
        String string;
        PtrArray docs;
    } data;
};

void visit_document(const Document* doc, const DocumentVisitor* visitor) {
    switch (doc->doc_type) {
    case StringDocument:
        visitor->on_str_doc(doc->data.string, visitor->ctx);
        break;
    case CatDocument:
        visitor->on_cat_doc(doc->data.docs, visitor->ctx);
        break;
    case SepDocument:
        visitor->on_sep_doc(doc->data.docs, visitor->ctx);
        break;
    case VSepDocument:
        visitor->on_vsep_doc(doc->data.docs, visitor->ctx);
        break;
    }
}

/* The Document Constructors */
Document* mv_str_doc(const String source, Allocator* a) {
    Document* doc = mem_alloc(sizeof(Document), a);
    doc->doc_type = StringDocument;
    doc->data.string = source;
    return doc;
}

Document* mk_str_doc(const String source, Allocator* a) {
    String copy = copy_string(source, a);
    Document* doc = mem_alloc(sizeof(Document), a);
    doc->doc_type = StringDocument;
    doc->data.string = copy;
    return doc;
}

Document* mv_cat_doc(const PtrArray source, Allocator* a) {
    Document* doc = mem_alloc(sizeof(Document), a);
    doc->doc_type = CatDocument;
    doc->data.docs = source;
    return doc;
}

Document* mk_cat_doc(const PtrArray source, Allocator* a) {
    PtrArray copy = scopy_ptr_array(source, a);
    Document* doc = mem_alloc(sizeof(Document), a);
    doc->doc_type = CatDocument;
    doc->data.docs = copy;
    return doc;
}

Document* mv_sep_doc(const PtrArray source, Allocator* a) {
    Document* doc = mem_alloc(sizeof(Document), a);
    doc->doc_type = SepDocument;
    doc->data.docs = source;
    return doc;
}

Document* mk_sep_doc(const PtrArray source, Allocator* a) {
    PtrArray copy = scopy_ptr_array(source, a);
    Document* doc = mem_alloc(sizeof(Document), a);
    doc->doc_type = SepDocument;
    doc->data.docs = copy;
    return doc;
}

Document* mv_vsep_doc(const PtrArray source, Allocator* a) {
    Document* doc = mem_alloc(sizeof(Document), a);
    doc->doc_type = VSepDocument;
    doc->data.docs = source;
    return doc;
}

Document* mk_vsep_doc(const PtrArray source, Allocator* a) {
    PtrArray copy = scopy_ptr_array(source, a);
    Document* doc = mem_alloc(sizeof(Document), a);
    doc->doc_type = VSepDocument;
    doc->data.docs = copy;
    return doc;
}

Document* mk_paren_doc(const char* lhs, const char* rhs, Document* inner, Allocator* a) {
    PtrArray nodes = mk_ptr_array(3, a);
    push_ptr(mk_str_doc(mv_string(lhs), a), &nodes);
    push_ptr(inner, &nodes);
    push_ptr(mk_str_doc(mv_string(rhs),a), &nodes);
    return mv_cat_doc(nodes, a);
}

void delete_doc(Document* doc, Allocator* a) {
    switch (doc->doc_type) {
    case StringDocument:
        delete_string(doc->data.string, a);
        mem_free(doc, a);
        break;
    case CatDocument:
    case SepDocument:
    case VSepDocument:
        for (size_t i = 0; i < doc->data.docs.len; i++)
            delete_doc(doc->data.docs.data[i], a);
        mem_free(doc, a);
        break;
    }
}
