#include "platform/signals.h"
#include "data/stream.h"

#include <stdlib.h>

void panic(String message) {
    write_string(message, get_stdout_stream());
    abort();
}
