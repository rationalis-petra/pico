#ifndef __PICO_TYPECHECK_TYPE_ERRORS_H
#define __PICO_TYPECHECK_TYPE_ERRORS_H

#include "pico/typecheck/typecheck.h"

_Noreturn void type_error_unexpected_module(TypeCheckContext ctx, Syntax* syn, Module* module);

#endif
