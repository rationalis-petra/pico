#include "platform/signals.h"
#include "platform/memory/region.h"

#include "pico/stdlib/stdlib.h"
#include "pico/binding/environment.h"

#include "test_pico/parse/parse.h"
#include "test_pico/parse/parse_helper.h"


#undef TEST_EQ
#define TEST_EQ(str) test_parse_eq(str, expected, context)

void run_pico_parse_tests(TestLog* log, RegionAllocator* region) {
  Allocator gpa = ra_to_gpa(region);
  Allocator* a = &gpa;
  PiAllocator pia = convert_to_pallocator(a);

  ErrorPoint point;
  if (catch_error(point)) {
    panic(mv_string("Error in tests: test_pico/typecheck.c"));
  }

  TestContext context = (TestContext) {
    .region = region,
    .log = log,
  };

  if (test_start(log, mv_string("parse-num"))) {
    RawTree expected = int_atom(1080);
    TEST_EQ("1080");
  }

  if (test_start(log, mv_string("parse-neg-num"))) {
    RawTree expected = int_atom(-1080);
    TEST_EQ("-1080");
  }

  if (test_start(log, mv_string("parse-char"))) {
    RawTree expected = int_atom('x');
    TEST_EQ("#x");
  }

  if (test_start(log, mv_string("parse-char-unicode"))) {
    RawTree expected = int_atom(8592);
    TEST_EQ("#←");
  }

  if (test_start(log, mv_string("parse-binary-num"))) {
    RawTree expected = int_atom(6);
    TEST_EQ("#b_110");
  }

  if (test_start(log, mv_string("parse-octal-num"))) {
    RawTree expected = int_atom(97);
    TEST_EQ("#o_141");
  }

  if (test_start(log, mv_string("parse-hex-simple"))) {
    RawTree expected = int_atom(10);
    TEST_EQ("#x_a");
  }

  if (test_start(log, mv_string("parse-hex-num"))) {
    RawTree expected = int_atom(31);
    TEST_EQ("#x_1f");
  }

  if (test_start(log, mv_string("parse-symbol"))) {
    RawTree expected = symbol_atom("x");
    TEST_EQ("x");
  }

  if (test_start(log, mv_string("parse-^-symbol"))) {
    RawTree expected = symbol_atom("^");
    TEST_EQ("^");
  }

  if (test_start(log, mv_string("parse-.-symbol"))) {
    RawTree expected = symbol_atom(".");
    TEST_EQ(".");
  }

  if (test_start(log, mv_string("parse-:-symbol"))) {
    RawTree expected = symbol_atom(":");
    TEST_EQ(":");
  }

  if (test_start(log, mv_string("parse-^-prefix-eos"))) {
    RawTree expected = expr_branch(&pia, 2, symbol_atom("^"), symbol_atom("ref"));
    TEST_EQ("^ref");
  }

  if (test_start(log, mv_string("parse-^-prefix-no-eos"))) {
    RawTree expected = expr_branch(&pia, 2, symbol_atom("^"), symbol_atom("ref"));
    TEST_EQ(" ^ref ");
  }

  if (test_start(log, mv_string("parse-^-in-place"))) {
    RawTree expected = expr_branch(&pia, 3, symbol_atom("set"), symbol_atom("^"), symbol_atom("ref"));
    TEST_EQ("(set ^ ref)");
  }

  if (test_start(log, mv_string("parse-^-complex"))) {
    RawTree rhs =
      expr_branch(&pia, 2, symbol_atom("foo"), symbol_atom("bar"));
    RawTree expected = expr_branch(&pia, 2, symbol_atom("^"), rhs);
    TEST_EQ("^(foo bar) ");
  }

  if (test_start(log, mv_string("parse-.-prefix"))) {
    RawTree expected = expr_branch(&pia, 2, symbol_atom("."), symbol_atom("ref"));
    TEST_EQ(".ref");
  }

  if (test_start(log, mv_string("parse-.-infix-complex-rhs"))) {
    RawTree rhs =
      expr_branch(&pia, 2, symbol_atom("bar"), symbol_atom("baz"));
    RawTree expected =
      expr_branch(&pia, 3, symbol_atom("."), rhs, symbol_atom("foo"));
    TEST_EQ("foo.(bar baz)");
  }

  if (test_start(log, mv_string("parse-.-infix-complex-lhs"))) {
    RawTree lhs =
      expr_branch(&pia, 2, symbol_atom("bar"), symbol_atom("baz"));
    RawTree expected =
      expr_branch(&pia, 3, symbol_atom("."), symbol_atom("foo"), lhs);
    TEST_EQ("(bar baz).foo");
  }

  if (test_start(log, mv_string("parse-:-prefix"))) {
    RawTree expected = expr_branch(&pia, 2, symbol_atom(":"), symbol_atom("ref"));
    TEST_EQ(":ref");
  }

  if (test_start(log, mv_string("parse-.-infix"))) {
    RawTree expected =
      expr_branch(&pia, 3, symbol_atom("."), symbol_atom("ref"), symbol_atom("foo"));
    TEST_EQ("foo.ref");
  }

  if (test_start(log, mv_string("parse-:-infix"))) {
    RawTree expected =
      expr_branch(&pia, 3, symbol_atom(":"), symbol_atom("ref"), symbol_atom("foo"));
    TEST_EQ("foo:ref");
  }

  if (test_start(log, mv_string("parse-char-literal-adjacent-to-paren"))) {
    RawTree expected =
      expr_branch(&pia, 2, symbol_atom("foo"), int_atom(','));
    TEST_EQ("(foo #,)");
  }
}
