#ifndef __PICO_ANALYSIS_ABSTRACTION_H
#define __PICO_ANALYSIS_ABSTRACTION_H

#include "pico/data/error.h"
#include "pico/binding/environment.h"
#include "pico/syntax/concrete.h"
#include "pico/syntax/syntax.h"
#include "pico/syntax/header.h"

TopLevel abstract(RawTree raw, Environment* env, Allocator* a, PiErrorPoint* point);
Syntax* abstract_expr(RawTree raw, Environment* env, Allocator* a, PiErrorPoint* point);
ModuleHeader* abstract_header(RawTree raw, Allocator* a, PiErrorPoint* point);

#endif
