#ifndef __PLATFORM_ERROR_H
#define __PLATFORM_ERROR_H

#include "platform/jump.h"
#include "data/string.h"

typedef struct {
    volatile String error_message; 
    jump_buf buf; 
} ErrorPoint;

#define catch_error(error) set_jump(error.buf)

_Noreturn void throw_error(ErrorPoint* point, String err); 

#endif
