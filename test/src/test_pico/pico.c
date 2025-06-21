#include "data/amap.h"

#include "test_pico/parse/parse.h"
#include "test_pico/stdlib/stdlib.h"

#include "test_pico/pico.h"


void run_pico_tests(TestLog* log, Allocator* a) {
    if (suite_start(log, mv_string("parse"))) {
        run_pico_parse_tests(log, a);
        suite_end(log);
    }

    if (suite_start(log, mv_string("pipeline"))) {
        run_pico_stdlib_tests(log, a);
        suite_end(log);
    }
}
