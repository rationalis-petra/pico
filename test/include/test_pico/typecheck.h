#ifndef __TEST_PICO_TYPECHECK_H
#define __TEST_PICO_TYPECHECK_H

#include "platform/memory/region.h"
#include "test/test_log.h"

void run_pico_typecheck_tests(TestLog* log, Target target, RegionAllocator* region);

#endif
