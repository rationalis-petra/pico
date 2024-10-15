#include "platform/signals.h"
#include "data/stream.h"

#include <stdlib.h>

_Noreturn void panic(String message) {
    write_string(message, get_stdout_stream());
    write_string(mv_string("\n"), get_stdout_stream());
    exit(1); // Exit to ensure buffers are flushed
}
