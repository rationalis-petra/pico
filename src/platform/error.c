#include "platform/error.h"

void throw_error(ErrorPoint* point, String err) {
    point->error_message = err;
    longjmp(point->buf, 1);
}
