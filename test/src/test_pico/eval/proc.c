#include "test_pico/eval/components.h"
#include "test_pico/helper.h"

void run_pico_eval_proc_tests(TestLog *log, Module* module,Allocator *a) {
    // check evaluation of procedure with 
    if (test_start(log, mv_string("int-literal"))) {
        int64_t expected = -10;
        test_toplevel_eq("-10", &expected, module, log, a) ;
    }
}
