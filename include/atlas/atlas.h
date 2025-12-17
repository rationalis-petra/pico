#ifndef __ATLAS_ATLAS_H
#define __ATLAS_ATLAS_H

#include "platform/terminal/terminal.h"
#include "pico/data/string_array.h"
#include "pico/values/modular.h"

void run_atlas(Package* base, StringArray args, FormattedOStream* out);

#endif
