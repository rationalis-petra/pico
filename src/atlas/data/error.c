#include "atlas/data/error.h"

_Noreturn void throw_at_error(AtErrorPoint* point, AtlasError err) {
    MultiError error = {
        .has_many = false,
        .error = (PicoError) {
            .message = err.message,
            .range = err.range,
        },
    };

    AtlasMultiError multi = {
        .error = error,
        .filename = err.filename,
        .captured_file = err.captured_file,
    };
    point->error = multi;
    long_jump(point->buf, 1);
}

_Noreturn void throw_at_multi_error(AtErrorPoint* point, AtlasMultiError err) {
    point->error = err;
    long_jump(point->buf, 1);
}
