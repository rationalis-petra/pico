#include <string.h>

#include "data/string.h"
#include "data/array.h" 
#include "pretty/document.h"


typedef enum DocumentType {
    StringDocument,
    CatDocument,
    SepDocument,
    VSepDocument
} DocumentType;

struct document {
    DocumentType doc_type;
    union {
        string string;
        ptr_array docs;
    } data;
};

void visit_document(const document* doc, const document_visitor* visitor) {
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
document* mv_str_doc(const string source, allocator a) {
    document* doc = mem_alloc(sizeof(document), a);
    doc->doc_type = StringDocument;
    doc->data.string = source;
    return doc;
}

document* mk_str_doc(const string source, allocator a) {
    string copy = copy_string(source, a);
    document* doc = mem_alloc(sizeof(document), a);
    doc->doc_type = StringDocument;
    doc->data.string = copy;
    return doc;
}

document* mv_cat_doc(const ptr_array source, allocator a) {
    document* doc = mem_alloc(sizeof(document), a);
    doc->doc_type = CatDocument;
    doc->data.docs = source;
    return doc;
}

document* mk_cat_doc(const ptr_array source, allocator a) {
    ptr_array copy = scopy_ptr_array(source, a);
    document* doc = mem_alloc(sizeof(document), a);
    doc->doc_type = CatDocument;
    doc->data.docs = copy;
    return doc;
}

document* mv_sep_doc(const ptr_array source, allocator a) {
    document* doc = mem_alloc(sizeof(document), a);
    doc->doc_type = SepDocument;
    doc->data.docs = source;
    return doc;
}

document* mk_sep_doc(const ptr_array source, allocator a) {
    ptr_array copy = scopy_ptr_array(source, a);
    document* doc = mem_alloc(sizeof(document), a);
    doc->doc_type = SepDocument;
    doc->data.docs = copy;
    return doc;
}

document* mv_vsep_doc(const ptr_array source, allocator a) {
    document* doc = mem_alloc(sizeof(document), a);
    doc->doc_type = VSepDocument;
    doc->data.docs = source;
    return doc;
}

document* mk_vsep_doc(const ptr_array source, allocator a) {
    ptr_array copy = scopy_ptr_array(source, a);
    document* doc = mem_alloc(sizeof(document), a);
    doc->doc_type = VSepDocument;
    doc->data.docs = copy;
    return doc;
}

void delete_doc(document* doc, allocator a) {
    switch (doc->doc_type) {
    case StringDocument:
        delete_string(doc->data.string, a);
        mem_free(doc, a);
        break;
    case CatDocument:
    case SepDocument:
    case VSepDocument:
        delete_ptr_array(doc->data.docs, (void(*)(void*, allocator))delete_doc, a);
        mem_free(doc, a);
        break;
    }
}
