#include "platform/memory/arena.h"

#include "pico/codegen/foreign_adapters.h"
#include "pico/values/ctypes.h"
#include "pico/values/types.h"
#include "pico/data/error.h"

#include "test_pico/eval/components.h"
#include "test_pico/helper.h"

typedef struct {
    int64_t a; 
    int64_t b; 
    int64_t c; 
    int64_t d; 
} Struct4Words;

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

void run_pico_eval_foreign_adapter_tests(TestLog *log, Module *module, Allocator *a) {
    Assembler* ass = mk_assembler(a);
    Segments prepped;
    Segments fn_segments = {.data = mk_u8_array(0, a),};
    Segments null_segments = (Segments) {
        .code = mk_u8_array(0, a),
        .data = mk_u8_array(0, a),
    };

    if (test_start(log, mv_string("add-10"))) {
        ErrorPoint point;
        if (catch_error(point)) {
            test_log_error(log, point.error_message);
            test_fail(log);
        }

        CType ctype = mk_fn_ctype(a, 1, "4word-struct", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Signed}),
                                  mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Signed}));

        PiType* ptype = mk_proc_type(a, 1, mk_prim_type(a, Int_64), mk_prim_type(a, Int_64));
        convert_c_fn(foreign_add_10, &ctype, ptype, ass, a, &point); 

        Symbol sym = string_to_symbol(mv_string("foreign-add-10"));
        fn_segments.code = get_instructions(ass);
        prepped = prep_target(module, fn_segments, ass, NULL);
        add_def(module, sym, *ptype, &prepped.code.data, prepped, NULL);
        clear_assembler(ass);
        delete_pi_type_p(ptype, a);
        delete_c_type(ctype, a);
        
        int64_t expected = -90;
        test_toplevel_eq("(foreign-add-10 -100)\n", &expected, module, log, a) ;
    }
        
    if (test_start(log, mv_string("4word-struct"))) {
        ErrorPoint point;
        if (catch_error(point)) {
            test_log_error(log, point.error_message);
            test_fail(log);
        }

        CType struct_ctype = mk_struct_ctype(a, 4, 
                                                    "a", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Signed}),
                                                    "b", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Signed}),
                                                    "c", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Signed}),
                                                    "d", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Signed}));
        CType ctype = mk_fn_ctype(a, 1, "4word-struct", struct_ctype,
                                  mk_primint_ctype((CPrimInt){.prim = CChar, .is_signed = Unsigned}));

        PiType* struct_type = mk_struct_type(a, 4,
                                                   "a", mk_prim_type(a, Int_64),
                                                   "b", mk_prim_type(a, Int_64),
                                                   "c", mk_prim_type(a, Int_64),
                                                   "d", mk_prim_type(a, Int_64));
        PiType* ptype = mk_proc_type(a, 1, struct_type, mk_prim_type(a, Bool));
        convert_c_fn(examine_struct, &ctype, ptype, ass, a, &point); 

        Symbol sym = string_to_symbol(mv_string("examine-4word-struct"));
        fn_segments.code = get_instructions(ass);
        prepped = prep_target(module, fn_segments, ass, NULL);
        add_def(module, sym, *ptype, &prepped.code.data, prepped, NULL);
        clear_assembler(ass);
        delete_pi_type_p(ptype, a);
        delete_c_type(ctype, a);
        
        bool expected = true;
        test_toplevel_eq("(examine-4word-struct (struct [.a 0] [.b -1] [.c 2] [.d -3]) )\n",
                        &expected, module, log, a) ;
    }

    if (test_start(log, mv_string("2-4word-structs"))) {
        ErrorPoint point;
        if (catch_error(point)) {
            test_log_error(log, point.error_message);
            test_fail(log);
        }

        CType struct_ctype = mk_struct_ctype(a, 4, 
                                                    "a", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Signed}),
                                                    "b", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Signed}),
                                                    "c", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Signed}),
                                                    "d", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Signed}));
        CType ctype = mk_fn_ctype(a, 2, "4word-struct", struct_ctype, "arg2", copy_c_type(struct_ctype, a),
                                  mk_primint_ctype((CPrimInt){.prim = CChar, .is_signed = Unsigned}));

        PiType* struct_type = mk_struct_type(a, 4,
                                                   "a", mk_prim_type(a, Int_64),
                                                   "b", mk_prim_type(a, Int_64),
                                                   "c", mk_prim_type(a, Int_64),
                                                   "d", mk_prim_type(a, Int_64));
        PiType* ptype = mk_proc_type(a, 2, struct_type, copy_pi_type_p(struct_type, a), mk_prim_type(a, Bool));
        convert_c_fn(examine_two_structs, &ctype, ptype, ass, a, &point); 

        Symbol sym = string_to_symbol(mv_string("examine-two-4word-structs"));
        fn_segments.code = get_instructions(ass);
        prepped = prep_target(module, fn_segments, ass, NULL);
        add_def(module, sym, *ptype, &prepped.code.data, prepped, NULL);
        clear_assembler(ass);
        delete_pi_type_p(ptype, a);
        delete_c_type(ctype, a);
        
        bool expected = true;
        test_toplevel_eq("(examine-two-4word-structs \n"
                         "(struct [.a 0] [.b -1] [.c 2] [.d -3]) (struct [.a 0] [.b 1] [.c -2] [.d 3]))\n",
                        &expected, module, log, a) ;
    }

    
    sdelete_u8_array(fn_segments.data);
    sdelete_u8_array(null_segments.data);
    sdelete_u8_array(null_segments.code);
    delete_assembler(ass);
}
