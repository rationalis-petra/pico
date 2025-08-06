#ifndef __PRETTY_STRING_PRINTER_H
#define __PRETTY_STRING_PRINTER_H

#include "components/pretty/document.h"

String doc_to_str(Document* doc, uint16_t width, Allocator* a);

#endif 
