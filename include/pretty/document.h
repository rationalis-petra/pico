#ifndef __PRETTY_DOCUMENT_H
#define __PRETTY_DOCUMENT_H

#include "data/array.h"
#include "data/string.h"


/* The Document Interaface. */

typedef enum {
    StringDocument,
    CatDocument,
    SepDocument,
    VSepDocument
} DocumentType;

typedef struct {
    DocumentType type;
    union {
        String string;
        PtrArray docs;
    };
} Document;

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
