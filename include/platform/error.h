#ifndef __PLATFORM_ERROR_H
#define __PLATFORM_ERROR_H

#include <setjmp.h>
#include "data/string.h"

typedef struct {
    volatile String error_message; 
    jmp_buf buf; 
} ErrorPoint;

#define catch_error(error) setjmp(error.buf)

_Noreturn void throw_error(ErrorPoint* point, String err); 

#endif
