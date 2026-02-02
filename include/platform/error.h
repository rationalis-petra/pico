#ifndef __PLATFORM_ERROR_H
#define __PLATFORM_ERROR_H

#include "platform/jump.h"

#include "components/pretty/document.h"

typedef struct {
    Document* volatile error_message; 
    jump_buf buf; 
} ErrorPoint;

#define catch_error(error) set_jump(error.buf)

_Noreturn void throw_error(ErrorPoint* point, Document* err); 

#endif
