#include "platform/terminal/terminal.h"
#include "platform/signals.h"
#include "platform/machine_info.h"

#include "data/meta/circle_buffer_header.h"
#include "data/meta/circle_buffer_impl.h"
#include "data/stream.h"

#include <stdio.h>
#include <inttypes.h>

#if OS_FAMILY == UNIX 
#include <sys/ioctl.h>
#include <termios.h>
#include <unistd.h>
#include <errno.h>
#elif OS_FAMILY == WINDOWS 
#include <Windows.h>    
#endif


static Allocator* terminal_allocator;

struct FormattedOStream {
    OStream* os;

    Colour* colours;
    Colour* bg_colours;
    size_t colour_len;
    size_t bg_colour_len;

    FontBoldness* boldness;
    size_t boldness_len;
};

static FormattedOStream form_stdout;

CIRCLE_BUFFER_HEADER_TYPE(InTermEvent, ITE);
CIRCLE_BUFFER_IMPL(InTermEvent, ie, ITE);

static ITECBuffer in_buffer;

// TOOD (improvement):
//   move the 'formatted stream' stuff to a create_formatted_ostream() constructor
void init_terminal(Allocator *a) {
    terminal_allocator = a;

    in_buffer = mk_ie_cbuffer(1024, a);

#if OS_FAMILY == WINDOWS 
    HANDLE consoleHanlde = GetStdHandle(STD_OUTPUT_HANDLE);
    SetConsoleMode(consoleHanlde, ENABLE_PROCESSED_OUTPUT | ENABLE_VIRTUAL_TERMINAL_PROCESSING);
    SetConsoleCP(65001);
    SetConsoleOutputCP(65001);
#endif

    static Colour stdout_colours[128];
    static Colour stdout_bg_colours[128];
    static FontBoldness bolds[128];
    form_stdout = (FormattedOStream) {
        .os = get_stdout_stream(),
        .colours = stdout_colours,
        .bg_colours = stdout_bg_colours,
        .colour_len = 0,
        .bg_colour_len = 0,
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
      //printf("\x1b[38;2;%"PRIu8";%"PRIu8";%"PRIu8"m", colour.r, colour.g, colour.b);
      char str[32];
      Colour colour = os->colours[os->colour_len];
      size_t len = snprintf(str, 31, "\x1b[38;2;%"PRIu8";%"PRIu8";%"PRIu8"m", colour.r, colour.g, colour.b);
      write_fstring((String){.bytes = (uint8_t*)str, .memsize = len + 1}, os);
  }
}

void end_coloured_text(FormattedOStream* os) {
#ifdef DEBUG
    if (os->colour_len == 0) {
        panic(mv_string("Underflow on end_coloured_text"));
    }
#endif

    os->colour_len--;
    if (os->colour_len == 0) {
        write_fstring(mv_string("\x1b[39m"), os);
    } else {
        char str[24];
        Colour colour = os->colours[os->colour_len];
        size_t len = snprintf(str, 23, "\x1b[38;2;%"PRIu8";%"PRIu8";%"PRIu8"m", colour.r, colour.g, colour.b);
        write_fstring((String){.bytes = (uint8_t*)str, .memsize = len + 1}, os);
    }
}

void start_bg_colour(Colour colour, FormattedOStream *os) {
  if (++os->bg_colour_len >= 128) {
      panic(mv_string("Don't support nesting of > 128 background colours"));
  } else {
      os->bg_colours[os->bg_colour_len] = colour;
      printf("\x1b[48;2;%"PRIu8";%"PRIu8";%"PRIu8"m", colour.r, colour.g, colour.b);
  }
}

void end_bg_colour(FormattedOStream *os) {
#ifdef DEBUG
    if (os->bg_colour_len == 0) {
        panic(mv_string("Underflow on end_bg_colour_text"));
    }
#endif

    os->bg_colour_len--;
    if (os->bg_colour_len == 0) {
        printf("\x1b[39m");
    } else {
        Colour colour = os->bg_colours[os->colour_len];
        printf("\x1b[48;2;%"PRIu8";%"PRIu8";%"PRIu8"m", colour.r, colour.g, colour.b);
    }
}

void set_bg_colour(Colour colour, FormattedOStream *os) {
    printf("\x1b[48;2;%"PRIu8";%"PRIu8";%"PRIu8"m", colour.r, colour.g, colour.b);
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

// ----------------------------------------------------------------------------- 
// 
//     Input Event Stream
// 
// -----------------------------------------------------------------------------

typedef struct {
    uint16_t code; 
    uint16_t arg_1; 
    uint16_t arg_2; 
    uint16_t arg_3; 
} BracketEscapeCode;

InTermEvent translate_code(BracketEscapeCode code) {
    switch (code.code) {
    case 'A':
        return (InTermEvent) {.type = ITKey, .key = TKUp};
        break;
    case 'B':
        return (InTermEvent) {.type = ITKey, .key = TKDown};
        break;
    case 'C':
        return (InTermEvent) {.type = ITKey, .key = TKLeft};
        break;
    case 'D':
        return (InTermEvent) {.type = ITKey, .key = TKRight};
        break;
    case 'R':
        return (InTermEvent) {
            .type = ITCursorPos,
            .cursor_pos = (PosCursorData){.row = code.arg_1, .col = code.arg_2},
        };
        break;
    case 'n':
        return (InTermEvent) {.type = ITStatus, .status = code.arg_1 == 0 ? Ok : Err};
        break;
    }
    panic(mv_string("TODO: Handle this terminal escape code"));
}

InTermEvent parse_escape_code() {
    // TODO: account for wait/poll dichotomy
    char c = getchar();
    BracketEscapeCode out = {};
    switch (c) {
    case 'R':
        return (InTermEvent){.type = ITKey, .key = TK_NUMPAD_PF3 };
      break;
    case '[':
      break;
    default:
        panic(mv_string("Unhahdled escape code"));
    }

    bool parsing = true;
    size_t idx = 0;
    while (parsing) { 
        bool parsing_num = true;

        // TODO: do research into max. length of parameters   
        uint16_t digits[8]; 
        size_t ndigits = 0; 
        while (parsing_num) { 
            c = getchar();
            if (ndigits > 4) {
                panic(mv_string("TODO: expand size of parameter base"));
            }
            if ('0' <= c && c <= '9') {
                digits[ndigits] = (c - '0');
                ndigits++;
            } else if (c == ';') {
                parsing_num = false;
            } else {
                parsing = false;
                parsing_num = false;
            }
        }
        uint16_t current = 0;
        uint16_t base = 1;
        for (size_t i = 0; i < ndigits; i++) { 
            current += base * digits[ndigits - (i + 1)];
            base *= 10;
        }
        ((uint16_t*)&out)[idx + 1] = current;
        idx++;
        // TODO: there can more than 3 parameters
        if (idx == 3)
            parsing = false;
    }
    out.code = c;
    return translate_code(out);
}

InTermEvent unbuffered_poll_in_terminal_event() {
#if OS_FAMILY == UNIX
    char c = '\0';
    int nread = read(STDIN_FILENO, &c, 1);
    if (nread == -1 && errno != EAGAIN) {
        // TODO: there has been a significant error; report this!
    }

    if (nread == 0 || nread == -1) {
        return (InTermEvent) {.type = ITNone};
    } else {
        if (c == '\x1b') {
            return parse_escape_code();
        } else {
            // TODO (BUG): implement utf-8 decoding
            return (InTermEvent) {.type = ITChar, .codepoint = c};
        }
    }

#elif OS_FAMILY == WINDOWS
    HANDLE std_cin = GetStdHandle(STD_INPUT_HANDLE);
    INPUT_RECORD records[1];
    DWORD nread = 0;
    if (!PeekConsoleInput(std_cin, records, 1, &nread)) {
        // TODO: there has been an error; report!
    }

    if (nread != 0) {
        // TODO: surely there's a better way peek & read?
        if (!ReadConsoleInput(std_cin, records, 1, &nread)) {
            // TODO: there has been an error; report!
        }
        switch (records[0].EventType) {
        case KEY_EVENT: {
            KEY_EVENT_RECORD key_event = records[0].Event.KeyEvent;
            if (key_event.bKeyDown) {
                
                if (key_event.uChar.UnicodeChar == '\x1b') {
                    return parse_escape_code();
                } else {
                    return (InTermEvent){.type = ITChar, .codepoint = key_event.uChar.UnicodeChar };
                }
            } else {
                return (InTermEvent){.type = ITNone};
            }
            break;
        }
        default:
            // TODO: handle other events!
            return (InTermEvent){.type = ITNone};
        }
    } else {
        return (InTermEvent){.type = ITNone};
    }

#else
#error "Unsupported OS_FAMILY in poll_in_terminal_event"
#endif
}
   
InTermEvent unbuffered_get_in_terminal_event() {
#if OS_FAMILY == UNIX

    struct termios tsettings;
    struct termios tsettings_old;
    tcgetattr(STDIN_FILENO, &tsettings_old);
    tsettings = tsettings_old;
    tsettings.c_cc[VMIN] = 1;
    tcsetattr(STDIN_FILENO, 0, &tsettings);
    char c;
    if (read(STDIN_FILENO, &c, 1) != 1) {
        panic(mv_string("TODO: determine what to do in this sitch"));
    }

    if (c == '\x1b') {
        InTermEvent evt = parse_escape_code();
        tcsetattr(STDIN_FILENO, 0, &tsettings_old);
        return evt;
    } else {
        // TODO (BUG): implement utf-8 decoding
        tcsetattr(STDIN_FILENO, 0, &tsettings_old);
        return (InTermEvent) {.type = ITChar, .codepoint = c};
    }

#elif OS_FAMILY == WINDOWS
    HANDLE std_cin = GetStdHandle(STD_INPUT_HANDLE);
    INPUT_RECORD records[1];
    DWORD nread = 0;

    if (!ReadConsoleInput(std_cin, records, 1, &nread)) {
        // TODO: there has been an error; report!
    }
    switch (records[0].EventType) {
    case KEY_EVENT: {
        KEY_EVENT_RECORD key_event = records[0].Event.KeyEvent;
        if (key_event.bKeyDown) {
            if (key_event.uChar.UnicodeChar == '\x1b') {
                return parse_escape_code();
            } else {
                return (InTermEvent){.type = ITChar, .codepoint = key_event.uChar.UnicodeChar };
            }
        } else {
            return (InTermEvent){.type = ITNone};
        }
        break;
    }
    default:
        // TODO: handle other events!
        return (InTermEvent){.type = ITNone};
    }

#else
#error "Unsupported OS_FAMILY in poll_in_terminal_event"
#endif
}

InTermEvent poll_in_terminal_event() {
  if (ie_not_empty(in_buffer)) {
    return pop_ie_cbuffer(&in_buffer);
  }
  return unbuffered_poll_in_terminal_event();
}

InTermEvent get_in_terminal_event() {
  if (ie_not_empty(in_buffer)) {
    return pop_ie_cbuffer(&in_buffer);
  }
  return unbuffered_get_in_terminal_event();
}

InTermEvent wait_for_in_terminal_event(InTermEventType type) {
  size_t num_items = ie_len(in_buffer);
  for (size_t i = 0; i < num_items; i++) {
    size_t idx = (in_buffer.start + i) % in_buffer.size;
    if (in_buffer.data[idx].type == type) 
      panic(mv_string("TODO: add ability to target removals of specific elements of cbuffer"));
  }
  while (true) {
    InTermEvent evt = unbuffered_get_in_terminal_event();
    if (evt.type == type) 
      return evt;
    else
      push_ie_cbuffer(evt, &in_buffer);
  }
}

PosCursorData get_cursor_pos() {
  OutTermEvent query = {
      .type = OTQueryCursorPos,
  };
  send_output_terminal_event(query);
  InTermEvent out = wait_for_in_terminal_event(ITCursorPos);
  // TODO: add an input queue to the internal api and search through for input event
  if (out.type != ITCursorPos) {
      panic(mv_string("expected a cursor pos event"));
  }
  return out.cursor_pos;
}

// ----------------------------------------------------------------------------- 
// 
//     Output Event Stream
// 
// -----------------------------------------------------------------------------


void send_output_terminal_event(OutTermEvent event) {
  switch (event.type) {
  case OTClear:
      switch (event.clear) {
      case ClearScreen:
          terminal_write_string_unbuffered(mv_string("\x1b[2J"));
          break;
      case ClearToScreenEnd:
          terminal_write_string_unbuffered(mv_string("\x1b[0J"));
          break;
      case ClearToScreenStart:
          terminal_write_string_unbuffered(mv_string("\x1b[1J"));
          break;
      case ClearLine:
          terminal_write_string_unbuffered(mv_string("\x1b[2K"));
          break;
      case ClearToLineEnd:
          terminal_write_string_unbuffered(mv_string("\x1b[0K"));
          break;
      case ClearToLineStart:
          terminal_write_string_unbuffered(mv_string("\x1b[21"));
          break;
      }
      break;
  case OTPosCursor: {
      char str[16];
      size_t len = snprintf(str, 16, "\x1b[%" PRIu16 ";%" PRIu16 "H", event.cursor_pos.row, event.cursor_pos.col);
      terminal_write_string_unbuffered((String){.bytes = (uint8_t*)str, .memsize = len + 1});
      break;
  }
  case OTChar: {
      // TODO: check return code for error, report in signature?
      putchar(event.codepoint);
      break;
  }
  case OTQueryCursorPos: {
      terminal_write_string_unbuffered(mv_string("\x1b[6n"));
      break;
  }
  }
}

void terminal_write_string_unbuffered(String string) {
#if OS_FAMILY == UNIX
    fflush(stdout);
    write(STDOUT_FILENO, string.bytes, string.memsize);
#elif OS_FAMILY == WINDOWS
    fflush(stdout);
    HANDLE std_cout = GetStdHandle(STD_OUTPUT_HANDLE);
    DWORD num_chars_written;

    // TODO: check for error code? return error on error code
    WriteConsole(std_cout, 
        string.bytes,
        string.memsize - 1, // TODO : this arg may be write as is 'number of chars to write' (uft-8)?
        &num_chars_written,
        NULL);
#else
#error "terminal_write_string_unbuffered not implemented on this system"
#endif
}


// -----------------------------------------------------------------------------
// 
//     Terminal Modes
// 
// -----------------------------------------------------------------------------

/* TerminalSettings terminal_get_settings(void) { */
/* #if OS_FAMILY == UNIX  */
/*   struct termios platform_settings; */
/*   tcgetattr(STDIN_FILENO, &platform_settings); */
/*   return (TerminalSettings) { */
/*     .echo = ECHO && platform_settings.c_lflag, */
/*     .input_line = ICANON && platform_settings.c_lflag, */
/*     .signals =  */
/*   } */
/* #elif OS_FAMILY == WINDOWS  */
/* #error "not implemented: terminal_get_settings in windows" */
/* #endif */
/* } */

/* void terminal_set_settings(TerminalSettings settings) { */
/* #if OS_FAMILY == UNIX  */
/*   struct termios platform_settings; */
/*   tcgetattr(STDIN_FILENO, &platform_settings); */

/*   if (settings.echo) */
/*       platform_settings.c_lflag |= (ECHO); */
/*   else  */
/*       platform_settings.c_lflag &= ~(ECHO); */

/*   if (settings.input_line) */
/*       platform_settings.c_lflag |= (ICANON); */
/*   else  */
/*       platform_settings.c_lflag &= ~(ICANON); */
  
/*   if (settings.signals) */
/*       platform_settings.c_lflag |= (ICANON); */
/*   else  */
/*       platform_settings.c_lflag &= ~(ICANON); */


/*   tcsetattr(STDIN_FILENO, TCSAFLUSH, &platform_settings); */
/* #elif OS_FAMILY == WINDOWS  */
/* #error "not implemented: terminal_set_settings in windows" */
/* #endif */
/* } */

void terminal_set_raw_mode(bool is_on) {
    // TODO (BUG): come up with a better API where we can restore old settings...
#if OS_FAMILY == UNIX
    if (is_on) {
        struct termios tsettings;
        tcgetattr(STDIN_FILENO, &tsettings);
        tsettings.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
        tsettings.c_oflag &= ~(OPOST);
        tsettings.c_cflag |= (CS8);
        tsettings.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);

        tsettings.c_cc[VMIN] = 0;
        tsettings.c_cc[VTIME] = 0;
        if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &tsettings) == -1) {
            panic(mv_string("failed to enter raw mode: error code returned!"));
        }

    } else {
        struct termios tsettings;
        tcgetattr(STDIN_FILENO, &tsettings);
        tsettings.c_iflag |= (BRKINT | ICRNL | INPCK | ISTRIP | IXON);
        tsettings.c_oflag |= (OPOST);
        tsettings.c_lflag |= (ECHO | ICANON | IEXTEN | ISIG);
        tcsetattr(STDIN_FILENO, TCSAFLUSH, &tsettings);
    }
#elif OS_FAMILY == WINDOWS
    HANDLE std_cin = GetStdHandle(STD_INPUT_HANDLE);
    HANDLE std_cout = GetStdHandle(STD_OUTPUT_HANDLE);
    if (is_on) {
        // TODO (BUG) - report error!
        // TODO : enable remembering of state.
        SetConsoleMode(std_cin, ENABLE_MOUSE_INPUT | ENABLE_WINDOW_INPUT | ENABLE_VIRTUAL_TERMINAL_INPUT);
        SetConsoleMode(std_cout, ENABLE_PROCESSED_OUTPUT | ENABLE_VIRTUAL_TERMINAL_PROCESSING | DISABLE_NEWLINE_AUTO_RETURN | ENABLE_LVB_GRID_WORLDWIDE);
    } else {
        SetConsoleMode(std_cin, ENABLE_ECHO_INPUT | ENABLE_INSERT_MODE | ENABLE_LINE_INPUT | ENABLE_PROCESSED_INPUT | ENABLE_VIRTUAL_TERMINAL_INPUT);
        SetConsoleMode(std_cout, ENABLE_PROCESSED_OUTPUT | ENABLE_WRAP_AT_EOL_OUTPUT | ENABLE_VIRTUAL_TERMINAL_PROCESSING | ENABLE_LVB_GRID_WORLDWIDE);
    }
#else
#error "set raw mode not implemented for this system"
#endif
}

TermSizeResult terminal_get_size() {
#if OS_FAMILY == UNIX
    struct winsize ws;
    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
        return (TermSizeResult) {.type = Err,};
    } else {
        return (TermSizeResult) {
            .type = Ok,
            .size.rows = ws.ws_row,
            .size.cols = ws.ws_col,
        };
    }
#elif OS_FAMILY == WINDOWS
    CONSOLE_SCREEN_BUFFER_INFO csbi;
    // TODO: check for error?
    GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &csbi);
    return (TermSizeResult) {
        .type = Ok,
        .size.rows = csbi.srWindow.Bottom - csbi.srWindow.Top + 1,
        .size.cols = csbi.srWindow.Right - csbi.srWindow.Left + 1,
    };
#else
#error ""
#endif
}
