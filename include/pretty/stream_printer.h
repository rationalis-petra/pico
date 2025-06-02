#ifndef __PRETTY_STREAM_PRINTER_H
#define __PRETTY_STREAM_PRINTER_H

#include "pretty/document.h"
#include "data/stream.h"

void write_doc(Document* doc, uint16_t width, OStream* os);

#endif 
