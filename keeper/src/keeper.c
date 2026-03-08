#include "data/string.h"
#include "data/stream.h"

#include "platform/memory/std_allocator.h"
#include "platform/terminal/terminal.h"
#include "platform/thread.h"
#include "platform/signals.h"

#include <stdio.h>


void write_centered(String string, TermSize termsize, Allocator* a) {
    
    // TODO: this is utf-8, 'len' is wrong!
    size_t len = string.memsize - 1;
    size_t leftpad_len = (termsize.cols - len) / 2;
    String leftpad = (String){.memsize = leftpad_len + 1, .bytes = mem_alloc(leftpad_len + 1, a)};
    for (size_t i = 0; i < leftpad_len; i++) {
        leftpad.bytes[i] = ' ';
    }
    leftpad.bytes[leftpad_len] = '\0';
  terminal_write_string_unbuffered(leftpad);
  terminal_write_string_unbuffered(string);
}

void write_header(TermSize termsize, Allocator* a) {
  size_t leftpad_len = (termsize.cols - 47) / 2;
  String leftpad = (String){.memsize = leftpad_len + 1, .bytes = mem_alloc(leftpad_len + 1, a)};
  for (size_t i = 0; i < leftpad_len; i++) {
      leftpad.bytes[i] = ' ';
  }
  leftpad.bytes[leftpad_len] = '\0';

  OStream* cout = get_stdout_stream();
  write_string(leftpad, cout);
  write_string(mv_string("██╗  ██╗███████╗███████╗██████╗ ███████╗██████╗ \r\n"), cout);
  write_string(leftpad, cout);
  write_string(mv_string("██║ ██╔╝██╔════╝██╔════╝██╔══██╗██╔════╝██╔══██╗\r\n"), cout);
  write_string(leftpad, cout);
  write_string(mv_string("█████╔╝ █████╗  █████╗  ██████╔╝█████╗  ██████╔╝\r\n"), cout);
  write_string(leftpad, cout);
  write_string(mv_string("██╔═██╗ ██╔══╝  ██╔══╝  ██╔═══╝ ██╔══╝  ██╔══██╗\r\n"), cout);
  write_string(leftpad, cout);
  write_string(mv_string("██║  ██╗███████╗███████╗██║     ███████╗██║  ██║\r\n"), cout);
  write_string(leftpad, cout);
  write_string(mv_string("╚═╝  ╚═╝╚══════╝╚══════╝╚═╝     ╚══════╝╚═╝  ╚═╝\r\n"), cout);
                                                
  mem_free(leftpad.bytes, a);
}

int main(int argc, char **argv) {
  Allocator* a = get_std_allocator();
  init_terminal(a);
  terminal_set_raw_mode(true);

  // Keeper is expecting documentation to be located in 
  // ~/.local/pico on Unix System; and
  // %LOCALAPPDATA%/Programs/pico on Windows
  // TOOD: add directories if stored system-wide?

  send_output_terminal_event((OutTermEvent){.type = OTClear, .clear = ClearScreen});
  send_output_terminal_event((OutTermEvent){.type = OTPosCursor, .cursor_pos = (PosCursorData){.row = 3, .col = 1}});

  TermSizeResult tsr = terminal_get_size();
  TermSize termsize = tsr.size;
  if (tsr.type == Err) panic(mv_string("Failed to get terminal size!"));
  write_header(tsr.size, a);
  write_centered(mv_string("Hello. I am the Relic Keeper Documentation Archivist and Assistant."), tsr.size, a);

  send_output_terminal_event((OutTermEvent){.type = OTPosCursor, .cursor_pos = (PosCursorData){.row = 40, .col = 1}});
  write_centered(mv_string("Press 'q' to Quit\r\n"), tsr.size, a);
  send_output_terminal_event((OutTermEvent){.type = OTPosCursor, .cursor_pos = (PosCursorData){.row = 12, .col = termsize.cols / 2}});

  InTermEvent evt = (InTermEvent){.type = ITNone};
  while (true) {
    if (evt.type == ITChar && evt.codepoint == 'q') {
      break;
    } 
    evt = poll_in_terminal_event();
    sleep_for((Microseconds){.us = 100.0});
  }

  send_output_terminal_event((OutTermEvent){.type = OTClear, .clear = ClearScreen});
  send_output_terminal_event((OutTermEvent){.type = OTPosCursor, .cursor_pos = (PosCursorData){.row = 1, .col = 1}});

  terminal_set_raw_mode(false);
  return 0;
}
