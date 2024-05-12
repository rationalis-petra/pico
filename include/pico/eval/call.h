#ifndef __PICO_EVAL_CALL_H
#define __PICO_EVAL_CALL_H
#include <stdint.h>


// Utilities for calling Pico functions from C (adapt to the different calling convention)
// Note: currently cannot proviide args/require no arguments!

int64_t pico_call(void* entry);

#endif
