#ifndef __DATA_RESULT_H
#define __DATA_RESULT_H

#include "data/string.h"

typedef enum Result_t {
    Ok,
    Err
} Result_t;

typedef struct result {
    Result_t type;
    string error_message;
} result;

#endif
