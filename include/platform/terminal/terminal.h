#ifndef __PLATFORM_IO_TERMINAL_H
#define __PLATFORM_IO_TERMINAL_H

#include <stdint.h>

#include "data/string.h"
#include "data/result.h"
#include "data/colour.h"
#include "platform/memory/allocator.h"

// ------------------------------------------------------------ 
// 
//     Streaming Interface
// 
// ------------------------------------------------------------
struct OStream;

typedef enum {
    Normal,
    Bold,
    Dim,
} FontBoldness;
 
void init_terminal(Allocator* a);

// TODO: move these operations into a components/formattedstream, which
// wraps a 'true' interface centererd around input/output *events*
typedef struct FormattedOStream FormattedOStream;
FormattedOStream* mk_formatted_ostream(struct OStream* os, Allocator* a);
void delete_formatted_ostream(FormattedOStream* os, Allocator* a);
void write_fstring(String string, FormattedOStream* os);
struct OStream* fos(FormattedOStream* os);

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
    TKUp,
    TKDown,
    TKLeft,
    TKRight,
    TKDelete,
    TK_NUMPAD_PF1,
    TK_NUMPAD_PF2,
    TK_NUMPAD_PF3,
} TerminalKey;

typedef enum {
    ITNone,
    ITChar,
    ITKey,
    ITCursorPos,
    ITStatus,
} InTermEventType;

typedef struct {
  uint16_t row;
  uint16_t col;
} PosCursorData;

typedef struct {
  InTermEventType type;
  union {
    uint32_t codepoint;
    TerminalKey key;
    PosCursorData cursor_pos;
    Result_t status;
  };
} InTermEvent;

InTermEvent poll_in_terminal_event();
InTermEvent get_in_terminal_event();
InTermEvent wait_for_in_terminal_event(InTermEventType type);
PosCursorData get_cursor_pos();

// ------------------------------------------------------------ 
// 
//     Output Event Stream
// 
// ------------------------------------------------------------

typedef enum {
  OTClear,
  OTPosCursor,
  OTChar,
  OTQueryCursorPos,
} OutTermEventType;

typedef enum {
  ClearScreen,
  ClearToScreenEnd,
  ClearToScreenStart,
  ClearLine,
  ClearToLineEnd,
  ClearToLineStart,
} ClearEventData;

typedef struct {
  OutTermEventType type;
  union {
    ClearEventData clear;
    PosCursorData cursor_pos;
    uint32_t codepoint;
  };
} OutTermEvent;

void send_output_terminal_event(OutTermEvent);

void terminal_write_string_unbuffered(String string);

// ------------------------------------------------------------ 
// 
//     Query and Set Information about the terminal
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

typedef struct {
    int rows;
    int cols;
} TermSize;

typedef struct {
    Result_t type;
    TermSize size;
} TermSizeResult;

TermSizeResult terminal_get_size();

#endif
