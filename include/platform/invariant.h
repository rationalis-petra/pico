#ifndef __PLATFORM_INVARIANT_H
#define __PLATFORM_INVARIANT_H

#include <stdbool.h>
#include "data/string.h"

void invariant(bool cond, String message);

#endif
