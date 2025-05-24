#include "platform/io/terminal.h"
#include "platform/signals.h"

#include <stdio.h>
#include <inttypes.h>

static Allocator* terminal_allocator;
static Colour colours[128];
static size_t colour_len = 0;

const uint8_t esc_code = 0x1B;

void init_terminal(Allocator *a) {
    terminal_allocator = a;
}

Colour colour(uint8_t r, uint8_t g, uint8_t b) {
    return (Colour) {.r = r, .g = g, .b = b,};
}

void start_coloured_text(Colour colour) {
  if (colour_len == 128) {
      panic(mv_string("Don't support nesting of > 128 colours"));
  } else {
      colours[colour_len++] = colour;
      printf("\x1b[38;2;%"PRIu8";%"PRIu8";%"PRIu8"m", colour.r, colour.g, colour.b);
  }
}

void end_coloured_text() {
    // TODO (feature): check for underflow in debug mode
    colour_len--;
    if (colour_len == 0) {
        printf("\x1b[39m");
    }
}


