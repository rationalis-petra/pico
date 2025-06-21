#include "platform/io/terminal.h"
#include "platform/signals.h"
#include "platform/machine_info.h"
#include "data/stream.h"

#include <stdio.h>
#include <inttypes.h>

#if OS_FAMILY == WINDOWS 
#include <Windows.h>    
#endif

static Allocator* terminal_allocator;

struct FormattedOStream {
    OStream* os;

    Colour* colours;
    size_t colour_len;

    FontBoldness* boldness;
    size_t boldness_len;
};

static FormattedOStream form_stdout;

void init_terminal(Allocator *a) {
    terminal_allocator = a;

#if OS_FAMILY == WINDOWS 
    HANDLE consoleHanlde = GetStdHandle(STD_OUTPUT_HANDLE);
    SetConsoleMode(consoleHanlde, ENABLE_PROCESSED_OUTPUT | ENABLE_VIRTUAL_TERMINAL_PROCESSING);
    SetConsoleCP(65001);
    SetConsoleOutputCP(65001);
#endif

    static Colour stdout_colours[128];
    static FontBoldness bolds[128];
    form_stdout = (FormattedOStream) {
        .os = get_stdout_stream(),
        .colours = stdout_colours,
        .colour_len = 0,
        .boldness_len = 0,
        .boldness = bolds,
    };
}

FormattedOStream *mk_formatted_ostream(OStream *os, Allocator* a) {
    FormattedOStream* out = mem_alloc(sizeof(FormattedOStream), a);

    *out = (FormattedOStream) {
        .os = os,
        .colours = mem_alloc(128 * sizeof(Colour), a),
        .colour_len = 0,
        .boldness = mem_alloc(128 * sizeof(FontBoldness), a),
        .boldness_len = 0,
    };
    return out;
}

void delete_formatted_ostream(FormattedOStream *os, Allocator *a) {
    mem_free(os->colours, a);
    mem_free(os->boldness, a);
    mem_free(os, a);
}

OStream *fos(FormattedOStream *os) {
    return os->os;
}

void write_fstring(String string, FormattedOStream *os) {
    write_string(string, os->os);
}

FormattedOStream *get_formatted_stdout() {
    return &form_stdout;
}

Colour colour(uint8_t r, uint8_t g, uint8_t b) {
    return (Colour) {.r = r, .g = g, .b = b,};
}

void start_coloured_text(Colour colour, FormattedOStream* os) {
  if (++os->colour_len >= 128) {
      panic(mv_string("Don't support nesting of > 128 colours"));
  } else {
      os->colours[os->colour_len] = colour;
      printf("\x1b[38;2;%"PRIu8";%"PRIu8";%"PRIu8"m", colour.r, colour.g, colour.b);
  }
}

void end_coloured_text(FormattedOStream* os) {
    // TODO (feature): check for underflow in debug mode
#ifdef DEBUG
    if (os->colour_len == 0) {
        panic(mv_string("Underflow on end_coloured_text"));
    }
#endif

    os->colour_len--;
    if (os->colour_len == 0) {
        printf("\x1b[39m");
    } else {
        Colour colour = os->colours[os->colour_len];
        printf("\x1b[38;2;%"PRIu8";%"PRIu8";%"PRIu8"m", colour.r, colour.g, colour.b);
    }
}

void start_boldness(FontBoldness boldness, FormattedOStream* os) {
    if (++os->boldness_len >= 128) {
        panic(mv_string("Don't support nesting of > 128 colours"));
    } else {
        os->boldness[os->boldness_len] = boldness;
    }
    switch (boldness) {
    case Normal:
        printf("\x1b[22m");
        break;
    case Bold:
        printf("\x1b[1m");
        break;
    case Dim:
        printf("\x1b[2m");
        break;
    }
}

void end_boldness(FormattedOStream* os) {
#ifdef DEBUG
    if (os->boldness_len == 0) {
        panic(mv_string("Underflow on end_boldness"));
    }
#endif

    os->boldness_len--;
    if (os->colour_len == 0) {
        printf("\x1b[22m");
    } else {
        FontBoldness boldness = os->boldness[os->boldness_len];
        switch (boldness) {
        case Normal:
            printf("\x1b[22m");
            break;
        case Bold:
            printf("\x1b[1m");
            break;
        case Dim:
            printf("\x1b[2m");
            break;
        }
    }
}

void start_italics(FormattedOStream *os) {
    char str[8];
    snprintf(str, 8, "\x1b[3m");
    write_fstring(mv_string(str), os);
}

void end_italics(FormattedOStream *os) {
    char str[8];
    snprintf(str, 8, "\x1b[23m");
    write_fstring(mv_string(str), os);
}

void start_underline(FormattedOStream *os) {
    char str[8];
    snprintf(str, 8, "\x1b[4m");
    write_fstring(mv_string(str), os);
}

void end_underline(FormattedOStream *os) {
    char str[8];
    snprintf(str, 8, "\x1b[24m");
    write_fstring(mv_string(str), os);
}
