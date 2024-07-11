#ifndef __PRETTY_STREAM_PRINTER_H
#define __PRETTY_STREAM_PRINTER_H

typedef struct document document;
typedef struct ostream ostream;

void write_doc(document* doc, ostream* os);

#endif 
