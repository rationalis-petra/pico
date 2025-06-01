#include "pretty/stream_printer.h"
#include "pretty/document.h"
#include "data/stream.h"

void write_doc(Document* doc, OStream* os) {
    switch (doc->type) {
    case StringDocument:
        write_string(doc->string, os);
        break;
    case CatDocument:
        for (size_t i = 0; i < doc->docs.len; i++) {
            write_doc(doc->docs.data[i], os);
        }
        break;
    case SepDocument:
        for (size_t i = 0; i < doc->docs.len; i++) {
            write_doc(doc->docs.data[i], os);
            if (i + 1 < doc->docs.len) write_impl(' ', os);
        }
        break;
    case VSepDocument:
        for (size_t i = 0; i < doc->docs.len; i++) {
            write_doc(doc->docs.data[i], os);
            if (i + 1 < doc->docs.len) write_impl('\n', os);
        }
        break;
    }
}
