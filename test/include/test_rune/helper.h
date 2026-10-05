#ifndef __TEST_RUNE_HELPER_H
#define __TEST_RUNE_HELPER_H

#include "platform/memory/region.h"

#include "rune/syntax/expression.h"

#include "test/test_log.h"

typedef struct {
    TestLog *log;
    RegionAllocator *region;
} TestContext;

void test_rune_toplevel_eq(const char *string, ValRef expected, TestContext context);
void assert_rune_toplevel_eq(const char *string, ValRef expected, TestContext context);

#define TEST_EQ(str) test_rune_toplevel_eq(str, expected, context)

#endif
