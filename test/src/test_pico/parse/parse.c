#include "platform/signals.h"
#include "platform/memory/region.h"

#include "pico/stdlib/stdlib.h"
#include "pico/binding/environment.h"

#include "test_pico/parse/parse.h"
#include "test_pico/parse/parse_helper.h"


//void test_parse_eq(const char *string, RawTree expected, Environment* env, TestContext context);
#define TEST_EQ(str) test_parse_eq(str, expected, context)

void run_pico_parse_tests(TestLog* log, RegionAllocator* region) {
  Allocator gpa = ra_to_gpa(region);
  Allocator* a = &gpa;
  PiAllocator pia = convert_to_pallocator(a);

  Package* base = get_base_package();

  Imports imports = (Imports) {
    .clauses = mk_import_clause_array(8, a),
  };
  add_import_all(&imports.clauses, a, 1, "core");
  add_import_all(&imports.clauses, a, 1, "num");
  add_import_all(&imports.clauses, a, 1, "extra");
  add_import_all(&imports.clauses, a, 1, "data");
  add_import_all(&imports.clauses, a, 2, "platform", "memory");

  Exports exports = (Exports) {
    .export_all = true,
    .clauses = mk_export_clause_array(0, a),
  };
  ModuleHeader header = (ModuleHeader) {
    .name = string_to_symbol(mv_string("pipeline-test-module")),
    .imports = imports,
    .exports = exports,
  };
  Module* module = mk_module(header, base, NULL);

  ErrorPoint point;
  if (catch_error(point)) {
    panic(mv_string("Error in tests: test_pico/typecheck.c"));
  }

  Environment* env = env_from_module(module, &point, a);
  delete_module_header(header);
  TestContext context = (TestContext) {
    .env = env,
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

  if (test_start(log, mv_string("parse-i64"))) {
    RawTree expected = int_atom(-1080);
    TEST_EQ("-1080");
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

  if (test_start(log, mv_string("parse-^-prefix"))) {
    RawTree expected = expr_branch(&pia, 2, symbol_atom("^"), symbol_atom("ref"));
    TEST_EQ("^ref");
  }

  if (test_start(log, mv_string("parse-^-in-place"))) {
    RawTree expected = expr_branch(&pia, 3, symbol_atom("set"), symbol_atom("^"), symbol_atom("ref"));
    TEST_EQ("(set ^ ref)");
  }

  if (test_start(log, mv_string("parse-.-prefix"))) {
    RawTree expected = expr_branch(&pia, 2, symbol_atom("."), symbol_atom("ref"));
    TEST_EQ(".ref");
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

  delete_module(module);
}
