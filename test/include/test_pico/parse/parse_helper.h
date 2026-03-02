#ifndef __TEST_PICO_PARSE_HELPER_H
#define __TEST_PICO_PARSE_HELPER_H

#include "pico/syntax/concrete.h"
#include "pico/binding/environment.h"

#include "test_pico/helper.h"

void test_parse_eq(const char *string, RawTree expected, TestContext context);

bool rawtree_eql(RawTree lhs, RawTree rhs);

RawTree int_atom(int64_t val);
RawTree bool_atom(bool val);
RawTree symbol_atom(const char* vals);

RawTree expr_branch(PiAllocator* a, uint64_t len, ...);

#endif
