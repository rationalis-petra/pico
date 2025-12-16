#include "atlas/data/error.h"

_Noreturn void throw_at_error(AtErrorPoint* point, AtlasError err) {
    point->error = err;
    long_jump(point->buf, 1);
}
