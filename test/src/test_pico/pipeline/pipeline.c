#include "test_pico/pipeline/pipeline.h"

void run_pico_pipeline_tests(RunDescriptor to_run, TestLog* log, Allocator* a) {
    test_log_error(log, mv_string("pipeline test not implemented!"));
    test_log_pass(log, mv_string("pipeline tests passed!"));
}
