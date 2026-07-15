#include "platform/signals.h"

#include "components/pretty/string_printer.h"

#include "pico/stdlib/helpers.h"
#include "pico/stdlib/data/submodules.h"

void add_string_module(Target target, Module *data, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);

    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(8, &ra),
    };
    add_import_all(&imports.clauses, &ra, 1, "core");
    add_import_all(&imports.clauses, &ra, 1, "num");
    add_import_all(&imports.clauses, &ra, 1, "extra");
    add_import_all(&imports.clauses, &ra, 2, "data", "pointer");
    add_import(&imports.clauses, &ra, 2, "data", "slice");
    add_import(&imports.clauses, &ra, 2, "data", "list");
    add_import(&imports.clauses, &ra, 2, "platform", "memory");

    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, &ra),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("string")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(data), NULL);
    delete_module_header(header);

    PiErrorPoint pi_point;
    if (catch_error(pi_point)) {
        panic(mv_string("pi error in string.c"));
    }

    ErrorPoint point;
    if (catch_error(point)) {
        panic(doc_to_str(point.error_message, 120, &ra));
    }

    const char* str_type = "(def String Named String (slice.Slice U8))";
    compile_toplevel(str_type, module, target, &point, &pi_point, region);

    const char *str_nth_byte =
        "(def nth-byte proc [(idx U64) (string String)] \n"
        "  slice.elt idx (unname string))";
    compile_toplevel(str_nth_byte, module, target, &point, &pi_point, region);

    const char *str_subview =
        "(def subview proc [(start U64) (end U64) (string String)] \n"
        "  (struct String [.addr (num-to-address (u64.+ start (address-to-num string.addr)))] [.len (u64.- end start)]))";
    compile_toplevel(str_subview, module, target, &point, &pi_point, region);

    const char *str_join =
        "(def join proc [(s1 String) (s2 String)] \n"
        "  name String slice.join (unname s1) (unname s2))";
    compile_toplevel(str_join, module, target, &point, &pi_point, region);

    const char *from_ascii =
        "(def from-ascii proc [(ascii (list.List U8))] seq\n"
        "  [let! new-bytes (memory.alloc ascii.len)]\n"
        "  (loop [for i from 0 below ascii.len]\n"
        "    [let! byte (list.elt i ascii)]\n"
        "    [let! dest-address (num-to-address (u64.+ i (address-to-num new-bytes)))]\n"
        "    (store dest-address byte))\n"
        "  (store {U8} (num-to-address (u64.+ ascii.len (address-to-num new-bytes))) 0)\n"
        "  (struct String [.addr new-bytes] [.len (u64.+ ascii.len 1)]))";
    compile_toplevel(from_ascii, module, target, &point, &pi_point, region);

    const char *str_delete =
        "(def delete proc [(lhs String)] \n"
        "  memory.free lhs.addr)";
    compile_toplevel(str_delete, module, target, &point, &pi_point, region);

    const char *str_eql =
        "(def = proc [(lhs String) (rhs String)] \n"
        "  (if (u64.!= lhs.len rhs.len)  \n"
        "    :false                              \n"
        "    (seq                                \n"
        "      [let! idx (local 0)]              \n"
        "      (loop [for i from 0 below lhs.len] \n"
        "            [while (u8.= (nth-byte i lhs) (nth-byte i rhs))] \n"
        "        (set idx i))                    \n"
        "      (if (u64.= 0 lhs.len)         \n "
        "          :true                         \n "
        "          (u64.= (get idx) (u64.- lhs.len 1))))))";
    compile_toplevel(str_eql, module, target, &point, &pi_point, region);

    const char *str_not_eql =
        "(def != proc [(lhs String) (rhs String)] \n"
        "  bool.not (= lhs rhs))";
    compile_toplevel(str_not_eql, module, target, &point, &pi_point, region);

    Result r = add_module_def(data, string_to_symbol(mv_string("string")), module);
    if (r.type == Err) panic(r.error_message);
}
