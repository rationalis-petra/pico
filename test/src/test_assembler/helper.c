#include <inttypes.h>
#include <stdio.h>

#include "pretty/string_printer.h"
#include "test_assembler/helper.h"

Document* pretty_expected(uint8_t* expected, Allocator* a) { 
    PtrArray nodes = mk_ptr_array(4, a);

    while (*expected != 0x90) {
        int len = snprintf(NULL, 0, "%02x", *expected) + 1;
        char* str = (char*)mem_alloc(sizeof(char) * len, a);
        snprintf(str, len, "%02" PRIx8, *expected);
        Document* arg = mv_cstr_doc(str, a);

        push_ptr(arg, &nodes);
        expected++;
    }

    return mv_hsep_doc(nodes, a);
}

void check_asm_eq(uint8_t *expected, Assembler *ass, Allocator* a, TestLog *log) {
    U8Array arr = get_instructions(ass);
    size_t i = 0;
    for (i = 0; i < arr.len; i++) {
        if (expected[i] == 0x90 || arr.data[i] != expected[i]) {
            goto on_error;
        }
    }

    if (expected[i] == 0x90) {
        test_pass(log);
    } else {
        goto on_error;
    }
    return;

 on_error: {
        PtrArray nodes = mk_ptr_array(8, a);
        push_ptr(mk_cstr_doc("Assembler test failure: expected:", a), &nodes);
        push_ptr(pretty_expected(expected, a), &nodes);
        push_ptr(mk_cstr_doc("but got:", a), &nodes);
        push_ptr(pretty_assembler(ass, a), &nodes);
        Document* out = mv_sep_doc(nodes, a);
        String message = doc_to_str(out, 120, a);
        test_log_error(log, message);
        delete_doc(out, a);
        test_fail(log);
    }
}
