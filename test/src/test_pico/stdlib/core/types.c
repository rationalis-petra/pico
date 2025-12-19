#include "pico/stdlib/core.h"

#include "test_pico/stdlib/components.h"
#include "test_pico/helper.h"

#define RUN(str) run_toplevel(str, module, context); refresh_env(env)
#define TEST_EQ(str) test_toplevel_eq(str, &expected, module, context); reset_subregion(region)

void run_pico_stdlib_core_type_tests(TestLog *log, Module* module, Environment* env, Target target, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);
    PiAllocator pico_region = convert_to_pallocator(&ra);
    PiAllocator* pia = &pico_region;

    TestContext context = (TestContext) {
        .env = env,
        .region = region,
        .pia = pia,
        .log = log,
        .target = target,
    };

    // -----------------------------------------------------
    // 
    //      Type Constructors
    // 
    // -----------------------------------------------------

    if (test_start(log, mv_string("I64"))) {
        PiType* expected = mk_prim_type(pia, Int_64);
        TEST_EQ("I64");
    }

    if (test_start(log, mv_string("proc-const"))) {
        PiType* expected = mk_proc_type(pia, 2, mk_prim_type(pia, Int_64), mk_prim_type(pia, Int_64), mk_prim_type(pia, Int_64));
        TEST_EQ("(Proc [I64 I64] I64)");
    }

    if (test_start(log, mv_string("enum-basic"))) {
        PiType* expected = mk_sz_enum_type(pia, 64, 2, "x", 1, mk_prim_type(pia, Int_64), "y", 1, mk_prim_type(pia, Int_64));
        TEST_EQ("(Enum [:x I64] [:y I64])");
    }

    if (test_start(log, mv_string("enum-with-size"))) {
        PiType* expected = mk_sz_enum_type(pia, 8, 2, "x", 1, mk_prim_type(pia, Int_64), "y", 1, mk_prim_type(pia, Int_64));
        TEST_EQ("(Enum 8 [:x I64] [:y I64])");
    }

    if (test_start(log, mv_string("proc-const"))) {
        PiType* expected = mk_proc_type(pia, 2, mk_prim_type(pia, Int_64), mk_prim_type(pia, Int_64), mk_prim_type(pia, Int_64));
        TEST_EQ("(Proc [I64 I64] I64)");
    }

    if (test_start(log, mv_string("all-type"))) {
        PiType* expected = mk_all_type(pia, 1, "A", mk_var_type(pia, "A"));
        TEST_EQ("(All [A] A)");
    }

    if (test_start(log, mv_string("sealed-type"))) {
        PiType* expected = mk_sealed_type(pia, 1, "A", 0, mk_var_type(pia, "A"));
        TEST_EQ("(Sealed [A] A)");
    }

    if (test_start(log, mv_string("exists-with-implicits"))) {
        RUN("(def Unital Trait [A] [.val A])");
        PiType* trait = mk_trait_type(pia, 1, "A", 1,
                                      "val", mk_var_type(pia, "A"));
        trait->trait.id--;
        PiType* var_type = mk_var_type(pia, "A");
        PiType* instance = mk_app_type(pia, trait, var_type);
        // TODO: update this to get the defined type, rather than this hack

        PiType* expected = mk_sealed_type(pia, 1, "A", 1, instance, mk_var_type(pia, "A"));
        TEST_EQ("(Sealed [A] {(Unital A)} A)");
    }

    if (test_start(log, mv_string("recursive-named"))) {
        PiType* vty = mk_var_type(pia, "Element");
        PiType* lty = mk_app_type(pia, get_list_type(), vty);
        PiType* expected = mk_named_type(pia, "Element",
                                         mk_struct_type(pia, 1, "children", lty));
        TEST_EQ("(Named Element Struct [.chidren (List Element)])");
    }
}
