#include "data/string.h"
#include "data/stream.h"

#include "platform/terminal/terminal.h"

int main(int argc, char **argv) {
    terminal_set_raw_mode(true);
    OStream* cout = get_stdout_stream();

    // Keeper is expecting documentation to be located in 
    // ~/.local/pico on Unix System; and
    // %LOCALAPPDATA%/Programs/pico on Windows
    // TOOD: add directories if stored system-wide?

    send_output_terminal_event((OutTermEvent){.type = OTClear, .clear = ClearScreen});
    send_output_terminal_event((OutTermEvent){.type = OTPosCursor, .cursor_pos = (PosCursorData){.row = 1, .col = 1}});

    write_string(mv_string("Hello. I am the Relic Keeper Documentation Archivist and Assistant.\r\n"), cout);

    terminal_set_raw_mode(false);
    return 0;
}
