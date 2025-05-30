#include "platform/signals.h"
#include "platform/io/terminal.h"
#include "data/stream.h"

#include <stdlib.h>

_Noreturn void panic(String message) {
    start_coloured_text(colour(200, 0, 0));
    write_string(mv_string("Program Panicked! \n\n"), get_stdout_stream());
    end_coloured_text();

    write_string(mv_string("Message:\n"), get_stdout_stream());

    start_coloured_text(colour(255, 200, 0));
    write_string(message, get_stdout_stream());
    end_coloured_text();

    start_coloured_text(colour(150, 150, 150));
    write_string(mv_string("\n\n"), get_stdout_stream());
    write_string(mv_string("A program panic is used to signal that there is a problem with the implementation\n"), get_stdout_stream());
    write_string(mv_string("of pico, rather than an issue with program being run.\n"), get_stdout_stream());
    end_coloured_text();

    exit(1); // Exit to ensure buffers are flushed
}
