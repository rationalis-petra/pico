#include "components/pretty/string_printer.h"
#include "components/pretty/stream_printer.h"
#include "components/pretty/document.h"

String doc_to_str(Document* doc, uint16_t width, Allocator* a) {
    OStream* os = mk_string_ostream(a); 
    write_doc(doc, width, os);
    String* s = current_string(os, a);
    String out = *s;
    mem_free(s, a);
    delete_ostream(os, a);
    return out;
}
