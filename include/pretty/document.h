#ifndef __PRETTY_DOCUMENT_H
#define __PRETTY_DOCUMENT_H

#include "data/array.h"
#include "data/string.h"


/* The Document Interaface. */

typedef struct document document;
typedef struct document_visitor {
    void (*on_str_doc)(string str, void* ctx);
    void (*on_cat_doc)(ptr_array docs, void* ctx);
    void (*on_sep_doc)(ptr_array docs, void* ctx);
    void (*on_vsep_doc)(ptr_array docs, void* ctx);
    void* ctx;
} document_visitor;
void visit_document(const document* doc, const document_visitor* visitor);

/* The Document Constructors */
document* mv_str_doc(const string string, allocator a);
document* mk_str_doc(const string string, allocator a);

document* mv_cat_doc(const ptr_array docs, allocator a);
document* mk_cat_doc(const ptr_array docs, allocator a);

document* mv_sep_doc(const ptr_array docs, allocator a);
document* mk_sep_doc(const ptr_array docs, allocator a);

document* mv_vsep_doc(const ptr_array docs, allocator a);
document* mk_vsep_doc(const ptr_array docs, allocator a);

/* The Document Destructor */
void delete_doc(document* document, allocator a);

#endif
