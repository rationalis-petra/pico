#include "test_pico/stdlib/components.h"
#include "test_pico/helper.h"

void run_pico_stdlib_data_string_tests(TestLog *log, Module* module, Environment* env, Target target, RegionAllocator* region) {
    TestContext context = (TestContext) {
        .env = env,
        .region = region,
        .log = log,
        .target = target,
    };

    if (test_start(log, mv_string("str-memsize"))) {
        uint64_t expected = 4;
        TEST_EQ("\"test\".memsize");
    }

    if (test_start(log, mv_string("pair-fn"))) {
        uint8_t expected = 'c';
        TEST_EQ("(string.nth-byte 2 \"lack\")");
    }

    if (test_start(log, mv_string("eql-null"))) {
        bool expected = true;
        TEST_EQ("(string.= \"\" \"\")");
    }
    
    if (test_start(log, mv_string("eql-true"))) {
        bool expected = true;
        TEST_EQ("(string.= \"str1\" \"str1\")");
    }

    if (test_start(log, mv_string("eql-prefix-false"))) {
        bool expected = false;
        TEST_EQ("(string.= \"str1\" \"str123\")");
    }

    if (test_start(log, mv_string("eql-same-len-false"))) {
        bool expected = false;
        TEST_EQ("(string.= \"test\" \"lack\")");
    }

    if (test_start(log, mv_string("not-eql-false"))) {
        bool expected = false;
        TEST_EQ("(string.!= \"str1\" \"str1\")");
    }

    if (test_start(log, mv_string("not-eql-true"))) {
        bool expected = true;
        TEST_EQ("(string.!= \"test\" \"lack\")");
    }

    if (test_start(log, mv_string("subview-whole-string"))) {
        bool expected = true;
        TEST_EQ("(string.= (string.subview 0 4 \"test\") \"test\")");
    }

    if (test_start(log, mv_string("subview-first-2"))) {
        bool expected = true;
        TEST_EQ("(string.= (string.subview 0 2 \"test\") \"te\")");
    }

    if (test_start(log, mv_string("subview-last-2"))) {
        bool expected = true;
        TEST_EQ("(string.= (string.subview 2 4 \"test\") \"st\")");
    }
}
