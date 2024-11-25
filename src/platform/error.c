#include "platform/error.h"

void throw_error(ErrorPoint* point, String err) {
    point->error_message = err;
    long_jump(point->buf, 1);
}
