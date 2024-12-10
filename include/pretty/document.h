#ifndef __PRETTY_DOCUMENT_H
#define __PRETTY_DOCUMENT_H

#include "data/array.h"
#include "data/string.h"


/* The Document Interaface. */

typedef struct Document Document;
typedef struct {
    void (*on_str_doc)(String str, void* ctx);
    void (*on_cat_doc)(PtrArray docs, void* ctx);
    void (*on_sep_doc)(PtrArray docs, void* ctx);
    void (*on_vsep_doc)(PtrArray docs, void* ctx);
    void* ctx;
} DocumentVisitor;
void visit_document(const Document* doc, const DocumentVisitor* visitor);

/* The Document Constructors */
Document* mv_str_doc(const String string, Allocator* a);
Document* mk_str_doc(const String string, Allocator* a);

Document* mv_cat_doc(const PtrArray docs, Allocator* a);
Document* mk_cat_doc(const PtrArray docs, Allocator* a);

Document* mv_sep_doc(const PtrArray docs, Allocator* a);
Document* mk_sep_doc(const PtrArray docs, Allocator* a);

Document* mv_vsep_doc(const PtrArray docs, Allocator* a);
Document* mk_vsep_doc(const PtrArray docs, Allocator* a);

Document* mk_paren_doc(const char* lhs, const char* rhs, Document* inner, Allocator* a);

/* The Document Destructor */
void delete_doc(Document* Document, Allocator* a);

#endif
