#ifndef __PICO_DATA_ERROR_H
#define __PICO_DATA_ERROR_H

#include "platform/jump.h"
#include "data/string.h"
#include "data/stream.h"

#include "pico/data/range.h"

typedef struct {
    String message;
    Range range;
} PicoError;

typedef struct {
    volatile PicoError error; 
    jump_buf buf; 
} PiErrorPoint;

_Noreturn void throw_pi_error(PiErrorPoint* point, PicoError err); 

void display_error(PicoError error, IStream *is, OStream* cout, Allocator* a);

#endif
