#include "data/string.h"
#include "data/stream.h"

#include "platform/memory/std_allocator.h"
#include "platform/terminal/terminal.h"
#include "platform/thread.h"

#include <stdio.h>

int main(int argc, char **argv) {
  Allocator* a = get_std_allocator();
  init_terminal(a);
  terminal_set_raw_mode(true);
  OStream* cout = get_stdout_stream();

  // Keeper is expecting documentation to be located in 
  // ~/.local/pico on Unix System; and
  // %LOCALAPPDATA%/Programs/pico on Windows
  // TOOD: add directories if stored system-wide?

  send_output_terminal_event((OutTermEvent){.type = OTClear, .clear = ClearScreen});
  send_output_terminal_event((OutTermEvent){.type = OTPosCursor, .cursor_pos = (PosCursorData){.row = 1, .col = 1}});

  write_string(mv_string("Hello. I am the Relic Keeper Documentation Archivist and Assistant.\r\n"), cout);
  for (size_t i = 0; i < 20; i++) write_string(mv_string("\r\n"), cout);
  write_string(mv_string("Press 'q' to Quit\r\n"), cout);

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
