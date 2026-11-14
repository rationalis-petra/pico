#ifndef __ATLAS_ATLAS_H
#define __ATLAS_ATLAS_H

#include "platform/terminal/terminal.h"
#include "pico/data/string_array.h"
#include "pico/values/modular.h"

/**
 * Atlas Overview
 * ================
 * The atlas build system is inspired by the shake build system: each build
 * target is in effect a monad paired with metadata about what it provides.
 * A the 'monad' in a target is in effect a program that may run, potentially
 * suspending and requesting additional dependencies. Thus, a target may.
 *   • Request additionaly dependencies. This will suspend the current
 *     computation until the dependencies are met.
 *   • Abort with an error
 *   • Complete.  
 * 
 *
 * Targets:
 *  A target containts the following:
 * 
 */

void run_atlas(Package* base, StringArray args, FormattedOStream* out);

#endif
