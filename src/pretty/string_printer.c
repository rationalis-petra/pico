#include "pretty/string_printer.h"
#include "pretty/document.h"

#include <string.h>

static size_t max(size_t a, size_t b) {
    return a < b ? b : a;
}

void doc_len(Document* doc, size_t* len) {
    switch (doc->type) {
    case StringDocument:
        *len += doc->string.memsize - 1; // subtrat null byte
        break;
    case CatDocument:
        for (size_t i = 0; i < doc->docs.len; i++)
            doc_len(doc->docs.data[i], len);
        break;
    case SepDocument:
    case VSepDocument:
        // TODO (BUG): for vsep doc, account for newline = \r\n on Windows 
        *len += max(doc->docs.len, 1) - 1;
        for (size_t i = 0; i < doc->docs.len; i++)
            doc_len(doc->docs.data[i], len);
        break;
    }
}

void doc_str(Document* doc, uint8_t** data) {
    switch (doc->type) {
    case StringDocument:
        memcpy(*data, doc->string.bytes, doc->string.memsize - 1);
        *data += doc->string.memsize - 1;
        break;
    case CatDocument:
        for (size_t i = 0; i < doc->docs.len; i++) {
            doc_str(doc->docs.data[i], data);
        }
        break;
    case SepDocument:
        for (size_t i = 0; i < doc->docs.len; i++) {
            doc_str(doc->docs.data[i], data);
            if (i + 1 < doc->docs.len) {
                **data = ' '; 
                *data += 1;
            }
        }
        break;
    case VSepDocument:
        for (size_t i = 0; i < doc->docs.len; i++) {
            doc_str(doc->docs.data[i], data);
            if (i + 1 < doc->docs.len) {
                **data = '\n'; 
                *data += 1;
            }
        }
        break;
    }
}

String doc_to_str(Document* doc, Allocator* a) {
    size_t total_len = 1; // for null byte
    doc_len(doc, &total_len);
    
    String str;
    str.memsize = total_len;
    str.bytes = mem_alloc(total_len, a);
    uint8_t* mptr = str.bytes; // mptr is modified, but we don't want str.bytes modified!
    doc_str(doc, &mptr);
    str.bytes[total_len-1] = '\0';

    return str;
}
