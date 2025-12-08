#include "platform/signals.h"
#include "platform/terminal/terminal.h"
#include "data/stream.h"

#include <stdlib.h>

_Noreturn void panic(String message) {
    FormattedOStream* stdout = get_formatted_stdout();

    start_coloured_text(colour(200, 0, 0), stdout);
    write_string(mv_string("Program Panicked! \n\n"), get_stdout_stream());
    end_coloured_text(stdout);

    write_string(mv_string("Message:\n"), get_stdout_stream());

    start_coloured_text(colour(255, 200, 0), stdout);
    write_string(message, get_stdout_stream());
    end_coloured_text(stdout);

    start_coloured_text(colour(150, 150, 150), stdout);
    write_string(mv_string("\n\n"), get_stdout_stream());
    write_string(mv_string("A program panic is used to signal that there is a problem with the implementation\n"), get_stdout_stream());
    write_string(mv_string("of pico, rather than an issue with program being run.\n"), get_stdout_stream());
    end_coloured_text(stdout);

    // When in debug, abort prevents the leak checker from producing a leak
    // report, which is undesirable in a crash/panic situation
    // When in release, we don't really care about cleaning up in a panic, so
    // quick_exit is fine anyways.
    abort(); 
}
