#include "platform/error.h"

void throw_error(ErrorPoint* point, String message) {
    point->error_message = message;
    long_jump(point->buf, 1);
}
