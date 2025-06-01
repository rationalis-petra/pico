#include <string.h>

#include "data/string.h"
#include "data/array.h" 
#include "pretty/document.h"


/* The Document Constructors */
Document* mv_str_doc(const String source, Allocator* a) {
    Document* doc = mem_alloc(sizeof(Document), a);
    doc->type = StringDocument;
    doc->string = source;
    return doc;
}

Document* mk_str_doc(const String source, Allocator* a) {
    String copy = copy_string(source, a);
    Document* doc = mem_alloc(sizeof(Document), a);
    doc->type = StringDocument;
    doc->string = copy;
    return doc;
}

Document* mv_cat_doc(const PtrArray source, Allocator* a) {
    Document* doc = mem_alloc(sizeof(Document), a);
    doc->type = CatDocument;
    doc->docs = source;
    return doc;
}

Document* mk_cat_doc(const PtrArray source, Allocator* a) {
    PtrArray copy = scopy_ptr_array(source, a);
    Document* doc = mem_alloc(sizeof(Document), a);
    doc->type = CatDocument;
    doc->docs = copy;
    return doc;
}

Document* mv_sep_doc(const PtrArray source, Allocator* a) {
    Document* doc = mem_alloc(sizeof(Document), a);
    doc->type = SepDocument;
    doc->docs = source;
    return doc;
}

Document* mk_sep_doc(const PtrArray source, Allocator* a) {
    PtrArray copy = scopy_ptr_array(source, a);
    Document* doc = mem_alloc(sizeof(Document), a);
    doc->type = SepDocument;
    doc->docs = copy;
    return doc;
}

Document* mv_vsep_doc(const PtrArray source, Allocator* a) {
    Document* doc = mem_alloc(sizeof(Document), a);
    doc->type = VSepDocument;
    doc->docs = source;
    return doc;
}

Document* mk_vsep_doc(const PtrArray source, Allocator* a) {
    PtrArray copy = scopy_ptr_array(source, a);
    Document* doc = mem_alloc(sizeof(Document), a);
    doc->type = VSepDocument;
    doc->docs = copy;
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
    switch (doc->type) {
    case StringDocument:
        delete_string(doc->string, a);
        mem_free(doc, a);
        break;
    case CatDocument:
    case SepDocument:
    case VSepDocument:
        for (size_t i = 0; i < doc->docs.len; i++)
            delete_doc(doc->docs.data[i], a);
        mem_free(doc, a);
        break;
    }
}
