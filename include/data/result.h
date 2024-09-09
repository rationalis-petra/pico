#ifndef __DATA_RESULT_H
#define __DATA_RESULT_H

#include "data/string.h"

typedef enum Result_t {
    Ok,
    Err
} Result_t;

typedef struct Result {
    Result_t type;
    String error_message;
} Result;

#endif
