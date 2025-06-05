#include <string.h>

#include "platform/signals.h"
#include "data/string.h"
#include "data/array.h" 
#include "pretty/document.h"

int requirement_leq(const DocRequirement lhs, const DocRequirement rhs) {
    switch (lhs.fin) {
    case Infinite:
        return rhs.fin == Infinite;
    case Finite:
        return rhs.fin == Infinite || lhs.cols <= rhs.cols;
    }
    panic(mv_string("Invalid requirement provided to requirement_leq"));
}

DocRequirement requirement_add(const DocRequirement lhs, const DocRequirement rhs) {
  if (lhs.fin == Finite && rhs.fin == Finite) {
      return (DocRequirement) {.fin = Finite, .cols = lhs.cols + rhs.cols};
  } else {
      return (DocRequirement) {.fin = Infinite};
  }
}

Document *mk_line_doc(Allocator *a) {
    Document* doc = mem_alloc(sizeof(Document), a);

    doc->requirement = (DocRequirement) {.fin = Infinite,};
    doc->type = LineDocument;
    return doc;
}

/* The Document Constructors */
Document* mv_str_doc(const String source, Allocator* a) {
    Document* doc = mem_alloc(sizeof(Document), a);

    // TODO (BUG): use actual string-length and not memsize
    // TODO (BUG): check for newlines and construct a 'correct' document
    doc->requirement = (DocRequirement) {
        .fin = Finite, .cols = source.memsize
    };
    doc->type = StringDocument;
    doc->string = source;
    return doc;
}

Document* mk_str_doc(const String source, Allocator* a) {
    String copy = copy_string(source, a);
    return mv_str_doc(copy, a);
}

Document* mv_cat_doc(const PtrArray source, Allocator* a) {
    Document* doc = mem_alloc(sizeof(Document), a);

    doc->type = CatDocument;
    doc->docs = source;

    DocRequirement req = (DocRequirement) {.fin = Finite, .cols = 0};
    for (size_t i = 0; i < source.len; i++) {
        Document* d = source.data[i];
        req = requirement_add(req, d->requirement);
    }
    doc->requirement = req;
    return doc;
}

Document* mk_cat_doc(const PtrArray source, Allocator* a) {
    PtrArray copy = scopy_ptr_array(source, a);
    return mv_cat_doc(copy, a);
}

Document *mv_nest_doc(size_t idx, Document *nested, Allocator *a) {
    Document* doc = mem_alloc(sizeof(Document), a);

    doc->type = NestDocument;
    doc->nest.indent = idx;
    doc->nest.inner = nested;
    doc->requirement = nested->requirement;
    return doc;
}

Document *mv_group_doc(Document* group, Allocator *a) {
    Document* doc = mem_alloc(sizeof(Document), a);
    doc->type = GroupDocument;
    doc->group = group;
    doc->requirement = group->requirement;
    return doc;
}

Document* mv_sep_doc(const PtrArray source, Allocator* a) {
    Document* doc = mem_alloc(sizeof(Document), a);

    const DocRequirement one = (DocRequirement) {.fin = Finite, .cols = 1};
    DocRequirement req = (DocRequirement) {.fin = Finite, .cols = 0};
    for (size_t i = 0; i < source.len; i++) {
        Document* d = source.data[i];
        req = requirement_add(req, d->requirement);
        req = requirement_add(req, one);
    }
    doc->type = SepDocument;
    doc->docs = source;
    doc->requirement = req;
    return doc;
}

Document* mk_hsep_doc(const PtrArray source, Allocator* a) {
    PtrArray copy = scopy_ptr_array(source, a);
    return mv_hsep_doc(copy, a);
}

Document* mv_hsep_doc(const PtrArray source, Allocator* a) {
    Document* doc = mem_alloc(sizeof(Document), a);

    const DocRequirement one = (DocRequirement) {.fin = Finite, .cols = 1};
    DocRequirement req = (DocRequirement) {.fin = Finite, .cols = 0};
    for (size_t i = 0; i < source.len; i++) {
        Document* d = source.data[i];
        req = requirement_add(req, d->requirement);
        req = requirement_add(req, one);
    }
    doc->type = HSepDocument;
    doc->docs = source;
    doc->requirement = req;
    return doc;
}

Document* mk_sep_doc(const PtrArray source, Allocator* a) {
    PtrArray copy = scopy_ptr_array(source, a);
    return mv_sep_doc(copy, a);
}

Document* mv_vsep_doc(const PtrArray source, Allocator* a) {
    Document* doc = mem_alloc(sizeof(Document), a);
    DocRequirement inf = (DocRequirement) {.fin = Infinite,};
    doc->type = VSepDocument;
    doc->docs = source;
    doc->requirement = inf;
    return doc;
}

Document* mk_vsep_doc(const PtrArray source, Allocator* a) {
    PtrArray copy = scopy_ptr_array(source, a);
    return mv_vsep_doc(copy, a);
}

Document *mv_style_doc(const DocStyle style, Document *inner, Allocator *a) {
    Document* doc = mem_alloc(sizeof(Document), a);
    doc->type = StyledDocument;
    doc->styled.style = style;
    doc->styled.inner = inner;
    doc->requirement = inner->requirement;
    return doc;
}

Document *mv_cstr_doc(const char *string, Allocator *a) {
    return mv_str_doc(mv_string(string), a);
}

Document *mk_cstr_doc(const char *string, Allocator *a) {
    return mk_str_doc(mv_string(string), a);
}

Document *mv_grouped_sep_doc(const PtrArray docs, Allocator *a) {
  for (size_t i = 0; i < docs.len; i++) {
      docs.data[i] = mv_group_doc(docs.data[i], a);
  }
  return mv_sep_doc(docs, a);
}

Document *mv_grouped_vsep_doc(const PtrArray docs, Allocator *a) {
  for (size_t i = 0; i < docs.len; i++) {
      docs.data[i] = mv_group_doc(docs.data[i], a);
  }
  return mv_vsep_doc(docs, a);
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
    case LineDocument:
    case StringDocument:
        delete_string(doc->string, a);
        break;
    case NestDocument:
        delete_doc(doc->nest.inner, a);
        break;
    case GroupDocument:
        delete_doc(doc->group, a);
        break;
    case CatDocument:
    case SepDocument:
    case VSepDocument:
        for (size_t i = 0; i < doc->docs.len; i++)
            delete_doc(doc->docs.data[i], a);
        break;
    case StyledDocument:
        delete_doc(doc->styled.inner, a);
        break;
    }
    mem_free(doc, a);
}

DocStyle scolour(Colour c, const DocStyle base) {
    DocStyle out = base;
    out.options |= HasColour;
    out.colour = c;
    return out;
}
