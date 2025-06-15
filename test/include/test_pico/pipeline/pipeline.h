#ifndef __TEST_PIPELINE_PIPELINE_H
#define __TEST_PIPELINE_PIPELINE_H

#include "pico/values/modular.h"

#include "test/suite.h"

void run_pico_pipeline_tests(RunDescriptor to_run, TestLog* log, Allocator* a);

#endif
