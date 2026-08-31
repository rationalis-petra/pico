#include "platform/signals.h"
#include "platform/memory/executable.h"
#include "platform/memory/region.h"

#include "pico/stdlib/stdlib.h"
#include "pico/stdlib/platform/submodules.h"
#include "pico/binding/environment.h"

#include "test_pico/helper.h"
#include "test_pico/typecheck.h"

/**
 * TODO: Typechecking Tests To Add
 * ================================
 *  - Check that all types are checked as types (with appropriate kind) (named, sealed, prim etc.)
 * 
 */

void run_pico_typecheck_tests(TestLog* log, Target target, RegionAllocator* region) {
    Allocator gpa = ra_to_gpa(region);
    Allocator* a = &gpa;

    Allocator exalloc = mk_executable_allocator(&gpa);

    PiAllocator pregion = convert_to_pallocator(&gpa);
    Assembler* ass = mk_assembler(current_cpu_feature_flags(), &exalloc);
    Package* base = get_base_package();

    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(4, a),
    };
    add_import_all(&imports.clauses, a, 2, "core", "kernel");
    add_import_all(&imports.clauses, a, 2, "core", "prim");

    ReExports re_exports = (ReExports) {
        .clauses = mk_import_clause_array(0, a),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, a),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_name(mv_string("typecheck-test-module")),
        .imports = imports,
        .re_exports = re_exports,
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
        .target = target,
    };
    if (test_start(log, mv_string("with-creates-tile"))) {
        PiType* expected = mk_tile_type(&pregion, 2, 2, 4, mk_prim_type(&pregion, UInt_64));
        TEST_TYPE("(with [i j] [2 4] j)");
    }

    if (test_start(log, mv_string("UVar through all"))) {
        PiType* expected = mk_prim_type(&pregion, Int_64);
        TEST_TYPE("((all [A] -28) {Unit})");
    }

    if (test_start(log, mv_string("substitution-through-uvar"))) {
        RUN("(def id all [A] proc [(x A)] x)");
        PiType* expected = mk_prim_type(&pregion, Int_64);
        TEST_TYPE("((all [B] proc [(x B)] (id x)) 77)");
    }

    if (test_start(log, mv_string("instantiate-call-poly-fn"))) {
        RUN("(def int-id proc [(x I64)] x)");
        PiType* expected = mk_prim_type(&pregion, Int_64);
        TEST_TYPE("((all [A] proc [(a A) (fn (Proc [A] I64))] (fn a)) 89 int-id)");
    }

    if (test_start(log, mv_string("Instnatiate Implicit with Default UVar"))) {
        RUN("(def Ptr Named Ptr Family [A] Address)");
        RUN("(def pinit all [A] (name (Ptr A) (address.num-to-address 0)))");
        RUN("(def pset all [A] proc [(p Ptr A) (x A)] :unit)");
        RUN("(def pget all [A] proc [(x Ptr A)] (address.load {A} (unname x)))");
        PiAllocator current_old = get_std_current_allocator();
        set_std_current_allocator(pregion);
        PiType* expected = mk_prim_type(&pregion, Int_64);
        TEST_TYPE("(seq [let! ptr (pinit)] (pset ptr 10) (pget ptr))");
        set_std_current_allocator(current_old);
    }

    //  Structure Typechecking
    // -------------------------
    if (test_start(log, mv_string("Default struct from field constraints"))) {
        RUN("(def i64-fn proc [(x I64) (y I64)] x)");
        PiAllocator current_old = get_std_current_allocator();
        set_std_current_allocator(pregion);
        PiType *expected =
            mk_proc_type(&pregion, 1,
                         mk_struct_type(&pregion, 2, "x", mk_prim_type(&pregion, Int_64),
                                        "y", mk_prim_type(&pregion, Int_64)), mk_prim_type(&pregion, Int_64));
        TEST_TYPE("(proc [point] (i64-fn point.x point.y))");
        set_std_current_allocator(current_old);
    }

    //  Variant/Match Typechecking
    // -------------------------
    if (test_start(log, mv_string("Un-annotated variant in match"))) {
        // We deduce that Right A has A = address (from use of address-to-num)
        // We deduce that Left V  has V = U64 (as must be same return type as right)
        PiAllocator current_old = get_std_current_allocator();
        set_std_current_allocator(pregion);
        PiType *expected =
            mk_proc_type(&pregion, 1,
                         mk_enum_type(&pregion, 2, "left", 1, mk_prim_type(&pregion, UInt_64),
                                      "right", 1, mk_prim_type(&pregion, Address)), mk_prim_type(&pregion, UInt_64));
        TEST_TYPE("(proc [either] match either [[:left v] v] [[:right x] (address.address-to-num x)])");
        set_std_current_allocator(current_old);
    }

    if (test_start(log, mv_string("Nested-variables-in-match"))) {
        PiAllocator current_old = get_std_current_allocator();
        set_std_current_allocator(pregion);
        PiType *expected =
            mk_struct_type(&pregion, 3,
                           "x", mk_prim_type(&pregion, Int_64),
                           "y", mk_prim_type(&pregion, Int_64),
                           "z", mk_prim_type(&pregion, Int_64));
        TEST_TYPE("(seq \n"
                  "[let! my-struct struct [.x 12] [.y 14] [.z 24]]\n"
                  "[let! v match (:left my-struct)  \n"
                  "          [[:left my-struct] my-struct] \n"
                  "          [[:right my-struct] my-struct]] \n"
                  "v)");
        set_std_current_allocator(current_old);
    }

    if (test_start(log, mv_string("enum from variant constraints"))) {
        PiAllocator current_old = get_std_current_allocator();
        set_std_current_allocator(pregion);
        PiType *expected = mk_proc_type(&pregion, 1, mk_prim_type(&pregion, Bool),
                                        mk_enum_type(&pregion, 2,
                                                     "left", 1, mk_prim_type(&pregion, Int_64),
                                                     "right", 1, mk_prim_type(&pregion, Address)));
        TEST_TYPE("(proc [which] if which (:left 10) (:right (address.num-to-address 8)))") ;
        set_std_current_allocator(current_old);
    }

    if (test_start(log, mv_string("declaration"))) {
        PiAllocator current_old = get_std_current_allocator();
        set_std_current_allocator(pregion);
        PiType *expected = mk_proc_type(&pregion, 1, mk_prim_type(&pregion, UInt_64),
                                        mk_prim_type(&pregion, UInt_64));
        RUN("(declare id [.type Proc [U64] U64])");
        RUN("(def id proc [x] x)");
        TEST_TYPE("id");
        set_std_current_allocator(current_old);
    }

    if (test_start(log, mv_string("kinds-1"))) {
        PiAllocator current_old = get_std_current_allocator();
        set_std_current_allocator(pregion);
        PiType ty = *mk_type_kind(&pregion, 1, mk_type_type(&pregion), mk_type_type(&pregion));
        PiType* expected = &ty;
        TEST_TYPE("(Family [A] A)");
        set_std_current_allocator(current_old);
    }

    if (test_start(log, mv_string("family-must-have-args"))) {
        TEST_TYPE_FAIL("(Family [] Address)");
    }


    if (test_start(log, mv_string("implicit-must-be-instance"))) {
        TEST_TYPE_FAIL("(Proc {I64} [I64] I64)");
    }

    if (test_start(log, mv_string("cannot-apply-non-family-types"))) {
        TEST_TYPE_FAIL("(U8)");
    }

    if (test_start(log, mv_string("can-coerce-to-named"))) {
        PiAllocator current_old = get_std_current_allocator();
        set_std_current_allocator(pregion);
        PiType* expected = mk_named_type(&pregion, "NF", mk_prim_type(&pregion, Float_32));
        RUN("(def NF Named NF F32)");
        RUN("(def id-nf proc [(nf NF)] nf)");
        TEST_TYPE("(id-nf 3.5)");
        set_std_current_allocator(current_old);
    }

    if (test_start(log, mv_string("cannot-coerce-to-disticnt"))) {
        RUN("(def DF Distinct DF F32)");
        RUN("(def id-df proc [(df DF)] df)");
        TEST_TYPE_FAIL("(id-df 3.5)");
    }

    if (test_start(log, mv_string("unsolved-var-errors"))) {
        TEST_TYPE_FAIL("(def vec2 all [A] proc [x y] name (Vec2 A) array {2} [x y])");
    }

    if (test_start(log, mv_string("instance-find"))) {
        RUN("(def Habit Trait Habit [A] [.val A])");
        RUN("(def habit-i64 instance (Habit I64) [.val 3])");
        RUN("(def val all [A] proc {(habit (Habit A))} [] habit.val)");

        PiType* expected = mk_prim_type(&pregion, Int_64);
        TEST_TYPE("(val {I64})");
    }

    if (test_start(log, mv_string("instance-find"))) {
        RUN("(def Habit Trait Habit [A] [.val A])");
        RUN("(def habit-i64 instance (Habit I64) [.val 3])");
        RUN("(def val all [A] proc {(habit (Habit A))} [] habit.val)");

        PiType* expected = mk_prim_type(&pregion, Int_64);
        TEST_TYPE("(val {I64})");
    }

    if (test_start(log, mv_string("instance-arg-find"))) {
        RUN("(def Habit Trait Habit [A] [.val A])");
        RUN("(def habit-i64 instance (Habit I64) [.val 3])");
        RUN("(def val all [A] proc {(habit (Habit A))} [] habit.val)");
        RUN("(def val-2 all [A] proc {(habit (Habit A))} [] (val {A}))");
        PiType* expected = mk_prim_type(&pregion, Int_64);
        TEST_TYPE("(val-2 {I64})");
    }

    /* TODO: fix me!
    if (test_start(log, mv_string("instance-instance-find"))) {
        RUN("(def InHab Trait InHab [A] [.val A])");
        RUN("(def inhab-i64 instance (InHab I64) [.val 3])");

        RUN("(def val all [A] proc {(habit (InHab A))} [] habit.val)");
        RUN("(def Wrap Named Wrap Family [A] Struct [.inner A])");
        RUN("(def habit-wrap instance [A] {(h (InHab A))} (InHab (Wrap A))\n"
            "  [.val struct (Wrap A) [.inner (val)]])");
        RUN("(def val-2 all [A] proc {(habit (InHab A))} [] (val {A}))");
        PiType* expected = mk_prim_type(&pregion, Int_64);
        TEST_TYPE("(val-2 {(Wrap I64)})");
    }
    */

    delete_env(env, a);
    remove_module(base, string_to_name(mv_string("typecheck-test-module")));
    delete_assembler(ass);
    release_executable_allocator(exalloc);
}
