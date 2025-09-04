#ifndef __TEST_PVM_HELPER_H
#define __TEST_PVM_HELPER_H

#include "components/pvm/pvm.h"
#include <stddef.h>

void check_pvm_eval(void* expected, size_t expected_size, PVMTerm* term, Allocator* a);

#endif
