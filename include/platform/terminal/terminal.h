#ifndef __PLATFORM_IO_TERMINAL_H
#define __PLATFORM_IO_TERMINAL_H

#include "data/string.h"
#include "platform/memory/allocator.h"
#include <stdint.h>

// ------------------------------------------------------------ 
// 
//     Streaming Interface
// 
// ------------------------------------------------------------
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

// TODO: move these operations into a components/formattedstream, which
// wraps a 'true' interface centererd around input/output *events*
typedef struct FormattedOStream FormattedOStream;
FormattedOStream* mk_formatted_ostream(OStream* os, Allocator* a);
void delete_formatted_ostream(FormattedOStream* os, Allocator* a);
void write_fstring(String string, FormattedOStream* os);
OStream* fos(FormattedOStream* os);

FormattedOStream* get_formatted_stdout();

Colour colour(uint8_t r, uint8_t g, uint8_t b);

void start_coloured_text(Colour colour, FormattedOStream* os);
void end_coloured_text(FormattedOStream* os);

void start_bg_colour(Colour colour, FormattedOStream* os);
void end_bg_colour(FormattedOStream* os);
void set_bg_colour(Colour colour, FormattedOStream* os);

void start_boldness(FontBoldness boldness, FormattedOStream* os);
void end_boldness(FormattedOStream* os);

void start_italics(FormattedOStream* os);
void end_italics(FormattedOStream* os);

void start_underline(FormattedOStream* os);
void end_underline(FormattedOStream* os);

// ------------------------------------------------------------ 
// 
//     Input Event Stream
// 
// ------------------------------------------------------------

typedef enum {
    ITNone,
    ITChar,
} InTermEventType;

typedef struct {
  InTermEventType type;
  union {
    uint32_t codepoint;
  };
} InTermEvent;

InTermEvent poll_in_terminal_event();

// ------------------------------------------------------------ 
// 
//     Output Event Stream
// 
// ------------------------------------------------------------

typedef enum {
  OTClear,
  OTPosCursor,
} OutTermEventType;

typedef enum {
  ClearScreen,
} ClearEventData;

typedef struct {
  uint16_t row;
  uint16_t col;
} PosCursorData;

typedef struct {
  OutTermEventType type;
  union {
    ClearEventData clear;
    PosCursorData cursor_pos;
  };
} OutTermEvent;

void send_output_terminal_event(OutTermEvent);

// ------------------------------------------------------------ 
// 
//     Terminal Modes
// 
// ------------------------------------------------------------

typedef struct {
  bool echo;
  bool input_line;
  bool signals;
} TerminalSettings;

/* TerminalSettings terminal_get_settings(void); */
/* void terminal_set_settings(TerminalSettings); */

void terminal_set_raw_mode(bool is_on);

#endif
