#include "platform/signals.h"
#include "platform/terminal/terminal.h"
#include "platform/machine_info.h"

#include <stdlib.h>

#if OS_FAMILY == UNIX
#include <signal.h>
#endif

_Noreturn void panic(String message) {
    FormattedOStream* stdout = get_formatted_stdout();

    start_coloured_text(colour(200, 0, 0), stdout);
    write_fstring(mv_string("Program Panicked! \n\n"), stdout);
    end_coloured_text(stdout);

    write_fstring(mv_string("Message:\n"), stdout);

    start_coloured_text(colour(255, 200, 0), stdout);
    write_fstring(message, stdout);
    end_coloured_text(stdout);

    start_coloured_text(colour(150, 150, 150), stdout);
    write_fstring(mv_string("\n\n"), stdout);
    write_fstring(mv_string("A program panic is used to signal that there is a problem with the implementation\n"), stdout);
    write_fstring(mv_string("of pico, rather than an issue with program being run.\n"), stdout);
    end_coloured_text(stdout);

    // When in debug, abort prevents the leak checker from producing a leak
    // report, which is undesirable in a crash/panic situation
    // When in release, we don't really care about cleaning up in a panic, so
    // quick_exit is fine anyways.
    abort(); 
}

void debug_break() {  
#if OS_FAMILY == WINDOWS
    __debugbreak();
#elif OS_FAMILY == UNIX
    raise(SIGTRAP);
#endif
}
