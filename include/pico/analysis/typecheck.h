#ifndef __PICO_ANALYSIS_TYPECHECK_H
#define __PICO_ANALYSIS_TYPECHECK_H

#include "data/result.h"
#include "pico/binding/environment.h"
#include "pico/syntax/syntax.h"

Result type_check(TopLevel* untyped, Environment* env, Allocator* a);

#endif
