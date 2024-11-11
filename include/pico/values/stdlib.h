#ifndef __PICO_VALUES_STDLIB_H
#define __PICO_VALUES_STDLIB_H

#include "pico/values/modular.h"

// We use our own setjmp/longjmp because the Windows implementation crashes the
// program if it has to jump over a pico stackframe.

typedef void *pi_jmp_buf[17];
int pi_setjmp(pi_jmp_buf buf);
void pi_longjmp(pi_jmp_buf buf, int val);

// The (exit) function in the base module will pi_longjmp to the jump buffer set here!
void set_exit_callback(pi_jmp_buf* buf);

Module* base_module(Assembler* ass, Allocator* a);

#endif
