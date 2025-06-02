#ifndef __PRETTY_STREAM_PRINTER_H
#define __PRETTY_STREAM_PRINTER_H

#include "platform/io/terminal.h"
#include "pretty/document.h"
#include "data/stream.h"

void write_doc(Document* doc, uint16_t width, OStream* os);
void write_doc_formatted(Document* doc, uint16_t width, FormattedOStream* os);

#endif 
