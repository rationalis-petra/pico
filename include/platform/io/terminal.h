#ifndef __PLATFORM_IO_TERMINAL_H
#define __PLATFORM_IO_TERMINAL_H

#include "data/string.h"
#include "platform/memory/allocator.h"
#include <stdint.h>

// from data/stream.h
typedef struct OStream OStream;

typedef struct {
    uint8_t r;
    uint8_t g;
    uint8_t b;
} Colour;

typedef enum {
    Normal,
    Bold,
    Dim,
} FontBoldness;

void init_terminal(Allocator* a);


typedef struct FormattedOStream FormattedOStream;
FormattedOStream* mk_formatted_ostream(OStream* os, Allocator* a);
void write_fstring(String string, FormattedOStream* os);
OStream* fos(FormattedOStream* os);

FormattedOStream* get_formatted_stdout();

Colour colour(uint8_t r, uint8_t g, uint8_t b);

void start_coloured_text(Colour colour, FormattedOStream* os);
void end_coloured_text(FormattedOStream* os);

void start_boldness(FontBoldness boldness, FormattedOStream* os);
void end_boldness(FormattedOStream* os);

void start_italics(FormattedOStream* os);
void end_italics(FormattedOStream* os);

#endif
