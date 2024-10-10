#include "platform/invariant.h"
#include "data/stream.h"

#include <assert.h>

void invariant(bool cond, String message) {
    if (!cond) {
        OStream* stdout = get_stdout_stream();
        write_string(mv_string("assertion failed:"), stdout);
        write_string(message, stdout);
        assert(false);
    }
}
