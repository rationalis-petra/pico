#ifndef __PICO_VALUES_STDLIB_H
#define __PICO_VALUES_STDLIB_H

#include "data/stream.h"
#include "pico/values/modular.h"

// We use our own setjmp/longjmp because the Windows implementation crashes the
// program if it has to jump over a pico stackframe.

typedef void *pi_jmp_buf[17];
int pi_setjmp(pi_jmp_buf buf);
void pi_longjmp(pi_jmp_buf buf, int val);

// Set the default value of dynamic variables
void set_exit_callback(pi_jmp_buf* buf);
void set_current_module(Module* current);
void set_current_package(Package* current);
void set_std_istream(IStream* current);
void set_std_ostream(OStream* current);

Package* base_package(Assembler* ass, Allocator* a);

#endif
