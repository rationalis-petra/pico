#ifndef __COMPONENTS_PRETTY_DOCUMENT_H
#define __COMPONENTS_PRETTY_DOCUMENT_H

#include "data/array.h"
#include "data/string.h"
#include "platform/terminal/terminal.h"


/* The Document Interaface. */

typedef enum {// 'Core'
    LineDocument,
    StringDocument,
    CatDocument,
    NestDocument,
    GroupDocument,
    HookDocument,

    // Utility/extra
    SepDocument,
    HSepDocument,
    VSepDocument,

    // Styling
    StyledDocument,
} DocumentType;

typedef enum {
    HasColour = 0x1,
    HasBoldness = 0x2,
    HasItalics = 0x4,
    HasUnderline = 0x8,
} StyleOptions;

typedef struct {
    StyleOptions options;
    Colour colour;
    FontBoldness boldnes;
} DocStyle;

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

typedef struct {
    DocStyle style;
    Document* inner;
} DocStyled;

struct Document {
    DocumentType type;
    DocRequirement requirement;
    union {
        String string;
        Document* group;
        Document* hook;
        DocNested nest;
        DocSeparated sep;
        PtrArray docs;
        DocStyled styled;
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
Document* mv_hook_doc(Document* hook, Allocator* a);

Document* mv_hsep_doc(const PtrArray docs, Allocator* a);
Document* mk_hsep_doc(const PtrArray docs, Allocator* a);

Document* mv_sep_doc(const PtrArray docs, Allocator* a);
Document* mk_sep_doc(const PtrArray docs, Allocator* a);

Document* mv_vsep_doc(const PtrArray docs, Allocator* a);
Document* mk_vsep_doc(const PtrArray docs, Allocator* a);

Document* mv_style_doc(const DocStyle style, Document* inner, Allocator* a);

// Smart constructors
Document* mv_cstr_doc(const char* string, Allocator* a);
Document* mk_cstr_doc(const char* string, Allocator* a);
Document* mv_grouped_sep_doc(const PtrArray docs, Allocator* a);
Document* mv_grouped_vsep_doc(const PtrArray docs, Allocator* a);
Document* mk_paren_doc(const char* lhs, const char* rhs, Document* inner, Allocator* a);


/* The Document Destructor */
void delete_doc(Document* Document, Allocator* a);
Document* copy_doc(Document* Document, Allocator* a);

// Styles
static const DocStyle dstyle = (DocStyle){.options = 0};
DocStyle scolour(Colour c, const DocStyle base);

#endif
