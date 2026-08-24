#include "test_pico/eval/components.h"
#include "test_pico/helper.h"

#include "pico/stdlib/core/kernel.h"

#define TEST_EQ(str) test_toplevel_eq(str, &expected, module, context)

void run_pico_eval_types_tests(TestLog *log, Module* module, Environment* env, Target target, RegionAllocator* region) {
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
    //      Type Formers and Primitives
    // 
    // -----------------------------------------------------

    if (test_start(log, mv_string("I64"))) {
        PiType* expected = mk_prim_type(pia, Int_64);
        TEST_EQ("I64");
    }

    //  Proc
    // -----------------------------------------------------
    if (test_start(log, mv_string("proc-const"))) {
        PiType* expected = mk_proc_type(pia, 2, mk_prim_type(pia, Int_64), mk_prim_type(pia, Int_64), mk_prim_type(pia, Int_64));
        TEST_EQ("(Proc [I64 I64] I64)");
    }

    if (test_start(log, mv_string("proc-implicits"))) {
        RUN("(def Hab Trait Hab [A] [.val A])");
        PiType *type;
        GET_TYPE(type, "Hab");

        PiType* mk_type_app(PiAllocator* pia, PiType* fam, ...);
        PiType* instance = mk_type_app (pia, type, mk_prim_type(pia, Int_64));
        PiType *expected = mk_proc_impl_type(
            pia, 1, 2,
            instance,
            mk_prim_type(pia, Int_64),
            mk_prim_type(pia, Int_64),
            mk_prim_type(pia, Int_64));
        TEST_EQ("(Proc {(Hab I64)} [I64 I64] I64)");
    }

    //  Array
    // -----------------------------------------------------
    if (test_start(log, mv_string("1d-array-no-backet"))) {
        PiType* expected = mk_array_type(pia, 1, 4, mk_prim_type(pia, Int_64));
        TEST_EQ("(Array 4 I64)");
    }

    if (test_start(log, mv_string("1d-array"))) {
        PiType* expected = mk_array_type(pia, 1, 4, mk_prim_type(pia, Int_64));
        TEST_EQ("(Array [4] I64)");
    }

    if (test_start(log, mv_string("2d-array"))) {
        PiType* expected = mk_array_type(pia, 2, 4, 2, mk_prim_type(pia, Int_64));
        TEST_EQ("(Array [4 2] I64)");
    }

    //  Structure
    // -----------------------------------------------------
    if (test_start(log, mv_string("struct-basic"))) {
        PiType* expected = mk_struct_type(pia, 2, "x", mk_prim_type(pia, Int_64), "y", mk_prim_type(pia, Int_64));
        TEST_EQ("(Struct [.x I64] [.y I64])");
    }

    if (test_start(log, mv_string("struct-packed"))) {
        PiType* expected = mk_struct_packed_type(pia, true, 2, "x", mk_prim_type(pia, Int_64), "y", mk_prim_type(pia, Int_64));
        TEST_EQ("(Struct packed [.x I32] [.y I64])");
    }

    //  Enumeration
    // -----------------------------------------------------
    if (test_start(log, mv_string("enum-basic"))) {
        PiType* expected = mk_sz_enum_type(pia, 64, 2, "x", 1, mk_prim_type(pia, Int_64), "y", 1, mk_prim_type(pia, Int_64));
        TEST_EQ("(Enum [:x I64] [:y I64])");
    }

    if (test_start(log, mv_string("enum-with-size"))) {
        PiType* expected = mk_sz_enum_type(pia, 8, 2, "x", 1, mk_prim_type(pia, Int_64), "y", 1, mk_prim_type(pia, Int_64));
        TEST_EQ("(Enum 8 [:x I64] [:y I64])");
    }

    //  Procedures and Polymorphic Procedures
    // -----------------------------------------------------
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
        RUN("(def Unital Trait Unital [A] [.val A])");
        PiType* trait = mk_trait_type(pia, 1, "A", 0, 1,
                                      "val", mk_var_type(pia, "A"));
        trait->trait.id--;
        PiType* var_type = mk_var_type(pia, "A");
        PiType* instance = mk_type_app(pia, trait, var_type);
        // TODO: update this to get the defined type, rather than this hack

        PiType* expected = mk_sealed_type(pia, 1, "A", 1, instance, mk_var_type(pia, "A"));
        TEST_EQ("(Sealed [A] {(Unital A)} A)");
    }

    if (test_start(log, mv_string("recursive-named"))) {
        PiType* vty = mk_var_type(pia, "Element");
        PiType* lty = mk_type_app(pia, get_list_type(), vty);
        PiType* expected = mk_named_type(pia, "Element",
                                         mk_struct_type(pia, 1, "children", lty));
        TEST_EQ("(Named Element Struct [.chidren (List Element)])");
    }

    if (test_start(log, mv_string("kind-type"))) {
        PiType* expected = mk_type_type(pia);
        TEST_EQ("Type");
    }

    if (test_start(log, mv_string("kind-family"))) {
        PiType* expected = mk_type_kind(pia, 1, mk_type_type(pia), mk_type_type(pia));
        TEST_EQ("(Kind [Type] Type)");
    }
}
