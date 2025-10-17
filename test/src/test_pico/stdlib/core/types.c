#include "pico/stdlib/core.h"

#include "test_pico/stdlib/components.h"
#include "test_pico/helper.h"

#define RUN(str) run_toplevel(str, module, context); refresh_env(env, a)
#define TEST_EQ(str) test_toplevel_eq(str, &expected, module, context)

void run_pico_stdlib_core_type_tests(TestLog *log, Module* module, Environment* env, Target target, Allocator *a) {
    TestContext context = (TestContext) {
        .env = env,
        .a = a,
        .log = log,
        .target = target,
    };

    // -----------------------------------------------------
    // 
    //      Type Constructors
    // 
    // -----------------------------------------------------

    if (test_start(log, mv_string("I64"))) {
        PiType* expected = mk_prim_type(a, Int_64);
        TEST_EQ("I64");
        delete_pi_type_p(expected, a);
    }

    if (test_start(log, mv_string("proc-const"))) {
        PiType* expected = mk_proc_type(a, 2, mk_prim_type(a, Int_64), mk_prim_type(a, Int_64), mk_prim_type(a, Int_64));
        TEST_EQ("(Proc [I64 I64] I64)");
        delete_pi_type_p(expected, a);
    }

    if (test_start(log, mv_string("all-type"))) {
        PiType* expected = mk_all_type(a, 1, "A", mk_var_type(a, "A"));
        TEST_EQ("(All [A] A)");
        delete_pi_type_p(expected, a);
    }

    if (test_start(log, mv_string("sealed-type"))) {
        PiType* expected = mk_sealed_type(a, 1, "A", 0, mk_var_type(a, "A"));
        TEST_EQ("(Sealed [A] A)");
        delete_pi_type_p(expected, a);
    }

    if (test_start(log, mv_string("exists-with-implicits"))) {
        RUN("(def Unital Trait [A] [.val A])");
        PiType* trait = mk_trait_type(a, 1, "A", 1,
                                      "val", mk_var_type(a, "A"));
        trait->trait.id--;
        PiType* var_type = mk_var_type(a, "A");
        PiType* instance = mk_app_type(a, trait, var_type);
        // TODO: update this to get the defined type, rather than this hack

        PiType* expected = mk_sealed_type(a, 1, "A", 1, instance, mk_var_type(a, "A"));
        TEST_EQ("(Sealed [A] {(Unital A)} A)");
        delete_pi_type_p(var_type, a);
        delete_pi_type_p(expected, a);
        delete_pi_type_p(trait, a);
    }

    if (test_start(log, mv_string("recursive-named"))) {
        PiType* vty = mk_var_type(a, "Element");
        PiType* lty = mk_app_type(a, get_list_type(), vty);
        PiType* expected = mk_named_type(a, "Element",
                                         mk_struct_type(a, 1, "children", lty));
        TEST_EQ("(Named Element Struct [.chidren (List Element)])");
        delete_pi_type_p(expected, a);
        delete_pi_type_p(vty, a);
    }
}
