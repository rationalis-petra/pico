#ifndef __TEST_SUITE_H
#define __TEST_SUITE_H

#include "data/amap.h"
#include "test/test_log.h"

typedef enum {
    RunAll, 
    RunOnly, 
    RunExcept, 
} RunType_t;

typedef struct {
    String name;
    PtrArray children;
} TestPath;

typedef struct {
    RunType_t type;
    PtrArray tests;
} RunDescriptor;

typedef void (TestSuite)(RunDescriptor to_run, TestLog* log, Allocator* a);

void run_tests(StrPtrAMap tests, RunDescriptor to_run, TestLog* log, Allocator* a);

#endif
