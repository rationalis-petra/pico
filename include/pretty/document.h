#ifndef __PRETTY_DOCUMENT_H
#define __PRETTY_DOCUMENT_H

#include "data/array.h"
#include "data/string.h"


/* The Document Interaface. */

typedef enum {
    // 'Core'
    LineDocument,
    StringDocument,
    CatDocument,
    NestDocument,
    GroupDocument,

    // Utility/extra
    SepDocument,
    VSepDocument,
} DocumentType;

typedef struct Document Document;

typedef enum {Finite , Infinite} Finitude;

typedef struct {
    Finitude fin;
    uint16_t cols;
} DocRequirement;

typedef struct {
    uint16_t indent;
    Document* inner;
} DocNested;

typedef struct {
    String sep;
    PtrArray* docs;
} DocSeparated;

struct Document {
    DocumentType type;
    DocRequirement requirement;
    union {
        String string;
        Document* group;
        DocNested nest;
        DocSeparated sep;
        PtrArray docs;
    };
};

/* The Document Constructors */
Document* mk_line_doc(Allocator* a);

Document* mv_str_doc(const String string, Allocator* a);
Document* mk_str_doc(const String string, Allocator* a);

Document* mv_cat_doc(const PtrArray docs, Allocator* a);
Document* mk_cat_doc(const PtrArray docs, Allocator* a);

Document* mv_nest_doc(size_t idx, Document* nested, Allocator* a);

Document* mv_group_doc(Document* group, Allocator* a);

Document* mv_sep_doc(const PtrArray docs, Allocator* a);
Document* mk_sep_doc(const PtrArray docs, Allocator* a);

Document* mv_vsep_doc(const PtrArray docs, Allocator* a);
Document* mk_vsep_doc(const PtrArray docs, Allocator* a);

// Smart constructors
Document* mv_grouped_sep_doc(const PtrArray docs, Allocator* a);

Document* mk_paren_doc(const char* lhs, const char* rhs, Document* inner, Allocator* a);

/* The Document Destructor */
void delete_doc(Document* Document, Allocator* a);

#endif
