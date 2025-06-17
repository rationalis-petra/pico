#ifndef __PLATFORM_HEDRON_HEDRON_H
#define __PLATFORM_HEDRON_HEDRON_H

#include "platform/memory/allocator.h"
#include <stdbool.h>

bool is_hedron_supported();

int init_hedron(Allocator* a);
void teardown_hedron();

#endif
