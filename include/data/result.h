#ifndef __DATA_RESULT_H
#define __DATA_RESULT_H

#include <stdint.h>
#include "data/string.h"

typedef enum : uint64_t {
    Ok,
    Err
} Result_t;

typedef struct Result {
    Result_t type;
    String error_message;
} Result;

#endif
