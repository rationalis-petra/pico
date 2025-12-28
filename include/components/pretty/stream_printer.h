#ifndef __COMPONENTS_PRETTY_STREAM_PRINTER_H
#define __COMPONENTS_PRETTY_STREAM_PRINTER_H

#include "data/stream.h"
#include "platform/terminal/terminal.h"
#include "components/pretty/document.h"

void write_doc(Document* doc, uint16_t width, OStream* os);

void write_doc_formatted(Document* doc, uint16_t width, FormattedOStream* os);

#endif 
