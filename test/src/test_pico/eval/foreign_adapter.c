#include "platform/memory/executable.h"

#include "components/pretty/string_printer.h"

#include "pico/codegen/codegen.h"
#include "pico/values/ctypes.h"
#include "pico/values/types.h"

#include "test_pico/eval/components.h"
#include "test_pico/helper.h"

typedef struct {
    int64_t a; 
    int64_t b; 
    int64_t c; 
    int64_t d; 
} Struct4Words;

typedef struct {
    float data[4];
} Float4;

int64_t foreign_add_10(int64_t val) {
    return val + 10;
}

bool examine_two_structs(Struct4Words st, Struct4Words st2) {
    bool out = false;
    if (st.a == 0 && st.b == -1 && st.c == 2 && st.d == -3
        && st2.a == 0 && st2.b == 1 && st2.c == -2 && st2.d == 3) {
        out = true;
    }
    return out;
}

bool examine_struct(Struct4Words st) {
    bool out = false;
    if (st.a == 0 && st.b == -1 && st.c == 2 && st.d == -3) {
        out = true;
    }
    return out;
}

bool examine_float4(Float4 tile) {
    return tile.data[0] == 1.0f && tile.data[1] == -2.5f
        && tile.data[2] == 3.25f && tile.data[3] == 4.5f;
}

bool expect_polymorphic_minus_90(uint64_t type_data, void* data) {
    uint64_t size = (type_data >> 28) & 0xFFFFFFF;
    uint64_t stack_size = type_data & 0xFFFFFFF;
    uint64_t alignment = type_data >> 56;

    if (size == sizeof(int64_t) && stack_size == sizeof(int64_t) && alignment == _Alignof(int64_t)) {
        return*(int64_t*)data == -90;
    }

    return false;
}

bool expect_polymorphic_struct(uint64_t type_data, void* data) {
    uint64_t size = (type_data >> 28) & 0xFFFFFFF;
    uint64_t stack_size = type_data & 0xFFFFFFF;
    uint64_t alignment = type_data >> 56;

    if (size == sizeof(Struct4Words) && stack_size == sizeof(Struct4Words) && alignment == _Alignof(Struct4Words)) {
        Struct4Words* value = data;
        return value->a == 0 && value->b == -1 && value->c == 2 && value->d == -3;;
    }

    return false;
}

void run_pico_eval_foreign_adapter_tests(TestLog *log, Module *module, Environment* env, Target target, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);
    PiAllocator pico_allocator = convert_to_pallocator(&ra);
    PiAllocator* pia = &pico_allocator;
    TestContext context = (TestContext) {
        .env = env,
        .region = region,
        .pia = pia,
        .log = log,
        .target = target,
    };
    Allocator exec = mk_executable_allocator(&ra);
    Assembler* ass = mk_assembler(current_cpu_feature_flags(), &exec);
    Segments prepped;
    Segments fn_segments = {.data = mk_u8_array(0, &ra),};
    Segments null_segments = (Segments) {
        .code = mk_u8_array(0, &ra),
        .data = mk_u8_array(0, &ra),
    };

    if (test_start(log, mv_string("add-10"))) {
        ErrorPoint point;
        if (catch_error(point)) {
            test_log_error(log, doc_to_str(point.error_message, 120, &ra));
            test_fail(log);
        }

        CType ctype = mk_fn_ctype(pia, 1, "4word-struct", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Signed}),
                                  mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Signed}));

        PiType* ptype = mk_proc_type(pia, 1, mk_prim_type(pia, Int_64), mk_prim_type(pia, Int_64));
        convert_c_fn(foreign_add_10, &ctype, ptype, ass, &ra, &point); 

        Name name = string_to_name(mv_string("foreign-add-10"));
        fn_segments.code = get_instructions(ass);
        prepped = prep_target(module, fn_segments, ass, NULL);
        add_def(module, name, *ptype, &prepped.code.data, prepped, NULL);
        clear_assembler(ass);
        delete_pi_type_p(ptype, pia);
        delete_c_type(ctype, pia);
        
        refresh_env(env);
        int64_t expected = -90;
        TEST_EQ("(foreign-add-10 -100)\n");
    }
        
    if (test_start(log, mv_string("4word-struct"))) {
        ErrorPoint point;
        if (catch_error(point)) {
            test_log_error(log, doc_to_str(point.error_message, 120, &ra));
            test_fail(log);
        }

        CType struct_ctype = mk_struct_ctype(pia, 4, 
                                                    "a", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Signed}),
                                                    "b", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Signed}),
                                                    "c", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Signed}),
                                                    "d", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Signed}));
        CType ctype = mk_fn_ctype(pia, 1, "4word-struct", struct_ctype,
                                  mk_primint_ctype((CPrimInt){.prim = CChar, .is_signed = Unsigned}));

        PiType* struct_type = mk_struct_type(pia, 4,
                                                   "a", mk_prim_type(pia, Int_64),
                                                   "b", mk_prim_type(pia, Int_64),
                                                   "c", mk_prim_type(pia, Int_64),
                                                   "d", mk_prim_type(pia, Int_64));
        PiType* ptype = mk_proc_type(pia, 1, struct_type, mk_prim_type(pia, Bool));
        convert_c_fn(examine_struct, &ctype, ptype, ass, &ra, &point); 

        Name name = string_to_name(mv_string("examine-4word-struct"));
        fn_segments.code = get_instructions(ass);
        prepped = prep_target(module, fn_segments, ass, NULL);
        add_def(module, name, *ptype, &prepped.code.data, prepped, NULL);
        clear_assembler(ass);
        delete_pi_type_p(ptype, pia);
        delete_c_type(ctype, pia);
        
        refresh_env(env);
        bool expected = true;
        TEST_EQ("(examine-4word-struct (struct [.a 0] [.b -1] [.c 2] [.d -3]) )\n") ;
    }

    if (test_start(log, mv_string("2-4word-structs"))) {
        ErrorPoint point;
        if (catch_error(point)) {
            test_log_error(log, doc_to_str(point.error_message, 120, &ra));
            test_fail(log);
        }

        CType struct_ctype = mk_struct_ctype(pia, 4, 
                                                    "a", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Signed}),
                                                    "b", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Signed}),
                                                    "c", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Signed}),
                                                    "d", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Signed}));
        CType ctype = mk_fn_ctype(pia, 2, "4word-struct", struct_ctype, "arg2", copy_c_type(struct_ctype, pia),
                                  mk_primint_ctype((CPrimInt){.prim = CChar, .is_signed = Unsigned}));

        PiType* struct_type = mk_struct_type(pia, 4,
                                                   "a", mk_prim_type(pia, Int_64),
                                                   "b", mk_prim_type(pia, Int_64),
                                                   "c", mk_prim_type(pia, Int_64),
                                                   "d", mk_prim_type(pia, Int_64));
        PiType* ptype = mk_proc_type(pia, 2, struct_type, copy_pi_type_p(struct_type, pia), mk_prim_type(pia, Bool));
        convert_c_fn(examine_two_structs, &ctype, ptype, ass, &ra, &point); 

        Name name = string_to_name(mv_string("examine-two-4word-structs"));
        fn_segments.code = get_instructions(ass);
        prepped = prep_target(module, fn_segments, ass, NULL);
        add_def(module, name, *ptype, &prepped.code.data, prepped, NULL);
        clear_assembler(ass);
        delete_pi_type_p(ptype, pia);
        delete_c_type(ctype, pia);
        
        refresh_env(env);
        bool expected = true;
        TEST_EQ("(examine-two-4word-structs \n"
                         "(struct [.a 0] [.b -1] [.c 2] [.d -3]) (struct [.a 0] [.b 1] [.c -2] [.d 3]))\n") ;
    }

    if (test_start(log, mv_string("float4-tile-as-struct"))) {
        ErrorPoint point;
        if (catch_error(point)) {
            test_log_error(log, doc_to_str(point.error_message, 120, &ra));
            test_fail(log);
        }

        CType float4_ctype = mk_struct_ctype(pia, 1,
                                             "data", mk_array_ctype(pia, 4, (CType){.sort = CSFloat}));
        CType ctype = mk_fn_ctype(pia, 1, "tile", float4_ctype,
                                  mk_primint_ctype((CPrimInt){.prim = CChar, .is_signed = Unsigned}));
        PiType* tile_type = mk_tile_type(pia, 1, 4, mk_prim_type(pia, Float_32));
        PiType* ptype = mk_proc_type(pia, 1, tile_type, mk_prim_type(pia, Bool));
        convert_c_fn(examine_float4, &ctype, ptype, ass, &ra, &point);

        Name name = string_to_name(mv_string("examine-float4"));
        fn_segments.code = get_instructions(ass);
        prepped = prep_target(module, fn_segments, ass, NULL);
        add_def(module, name, *ptype, &prepped.code.data, prepped, NULL);
        clear_assembler(ass);
        delete_pi_type_p(ptype, pia);
        delete_c_type(ctype, pia);

        refresh_env(env);
        bool expected = true;
        TEST_EQ("(examine-float4 (tile [(is F32 1.0) (is F32 -2.5) "
                 "(is F32 3.25) (is F32 4.5)]))");
    }

    if (test_start(log, mv_string("polymorphic-value"))) {
        ErrorPoint point;
        if (catch_error(point)) {
            test_log_error(log, doc_to_str(point.error_message, 120, &ra));
            test_fail(log);
        }

        CType ctype = mk_fn_ctype(pia, 2,
                                  "type-data", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                                                                    "data", mk_voidptr_ctype(pia),
                                                                    (CType){.sort = CSVoid});
        PiType* ptype = mk_all_type(pia, 1, "A",
                                                                        mk_proc_type(pia, 1, mk_var_type(pia, "A"), mk_prim_type(pia, Unit)));
        convert_c_fn(expect_polymorphic_minus_90, &ctype, ptype, ass, &ra, &point);

        Name name = string_to_name(mv_string("expect-minus-90"));
        fn_segments.code = get_instructions(ass);
        prepped = prep_target(module, fn_segments, ass, NULL);
        add_def(module, name, *ptype, &prepped.code.data, prepped, NULL);
        clear_assembler(ass);
        delete_pi_type_p(ptype, pia);
        delete_c_type(ctype, pia);

        refresh_env(env);
        bool expected = true;
        TEST_EQ("(expect-minus-90 -90)\n");
    }

    if (test_start(log, mv_string("polymorphic-struct-value"))) {
        ErrorPoint point;
        if (catch_error(point)) {
            test_log_error(log, doc_to_str(point.error_message, 120, &ra));
            test_fail(log);
        }

        CType ctype = mk_fn_ctype(pia, 2,
                                  "type-data", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                                                                    "data", mk_voidptr_ctype(pia),
                                                                    (CType){.sort = CSVoid});
        PiType* ptype = mk_all_type(pia, 1, "A",
                                                                        mk_proc_type(pia, 1, mk_var_type(pia, "A"), mk_prim_type(pia, Unit)));
        convert_c_fn(expect_polymorphic_struct, &ctype, ptype, ass, &ra, &point);

        Name name = string_to_name(mv_string("expect-struct"));
        fn_segments.code = get_instructions(ass);
        prepped = prep_target(module, fn_segments, ass, NULL);
        add_def(module, name, *ptype, &prepped.code.data, prepped, NULL);
        clear_assembler(ass);
        delete_pi_type_p(ptype, pia);
        delete_c_type(ctype, pia);

        refresh_env(env);
        bool expected = true;
        TEST_EQ("(expect-struct (struct [.a 0] [.b -1] [.c 2] [.d -3]))");
    }

    sdelete_u8_array(fn_segments.data);
    sdelete_u8_array(null_segments.data);
    sdelete_u8_array(null_segments.code);
    delete_assembler(ass);
    release_executable_allocator(exec);
}
