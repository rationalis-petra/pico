#ifndef __PLATFORM_IO_TERMINAL_H
#define __PLATFORM_IO_TERMINAL_H

#include "platform/memory/allocator.h"
#include <stdint.h>

typedef struct {
    uint8_t r;
    uint8_t g;
    uint8_t b;
} Colour;

typedef enum {
    Normal,
    Bold,
    Dim,
} FondBoldness;

void init_terminal(Allocator* a);

Colour colour(uint8_t r, uint8_t g, uint8_t b);

void start_coloured_text(Colour colour);
void end_coloured_text();

void set_boldness(FondBoldness boldness);

void start_italics();
void end_italics();

#endif
