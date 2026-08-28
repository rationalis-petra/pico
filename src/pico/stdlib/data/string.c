#include "platform/signals.h"

#include "components/pretty/string_printer.h"

#include "pico/stdlib/helpers.h"
#include "pico/stdlib/data/submodules.h"

void add_string_module(Target target, Module *data, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);

    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(8, &ra),
    };
    add_import_all(&imports.clauses, &ra, 2, "lang", "relic");
    add_import_all(&imports.clauses, &ra, 2, "core", "prim");
    add_import_all(&imports.clauses, &ra, 2, "data", "pointer");
    add_import(&imports.clauses, &ra, 2, "data", "slice");
    add_import(&imports.clauses, &ra, 2, "data", "list");
    add_import(&imports.clauses, &ra, 2, "platform", "memory");
    add_import(&imports.clauses, &ra, 2, "core", "debug");

    add_import_flags(&imports.clauses, &ra, ImportTypes | ImportInstances,
                     2, seg_name("num"), seg_wild());
    add_import_all(&imports.clauses, &ra, 2, "abs", "equality");
    add_import_all(&imports.clauses, &ra, 2, "abs", "order");
    add_import_all(&imports.clauses, &ra, 2, "abs", "numeric");
    add_import_all(&imports.clauses, &ra, 2, "abs", "lifetime");

    ReExports re_exports = (ReExports) {
        .clauses = mk_import_clause_array(0, &ra),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, &ra),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_name(mv_string("string")),
        .imports = imports,
        .re_exports = re_exports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(data), data);
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

    const char *num_uft8_bytes =
        "(def num-utf8-bytes proc [(byte U8)] \n"
        "  cond \n"
        "    [(u8.= #b_11000000 (u8.and byte #b_11100000)) 2] \n"
        "    [(u8.= #b_11100000 (u8.and byte #b_11110000)) 3] \n"
        "    [(u8.= #b_11110000 (u8.and byte #b_11111000)) 4] \n"
        "    [:true (is U8 1)])";
    compile_toplevel(num_uft8_bytes, module, target, &point, &pi_point, region);

    const char *decode_uft8_bytes =
        "(def decode-utf8-point proc [(s (slice.Slice U8))] seq\n"
        "  [let! num-bytes (num-utf8-bytes (slice.elt 0 s))]"
        "  (cond \n"
        "    [(u8.= num-bytes 1)"
        "     (widen U32 (slice.elt 0 s))] \n"
        "    [(u8.= num-bytes 2)\n"
        "     (->> (widen U32 u8.and (slice.elt 0 s) #x_1f)\n"
        "          (u32.shl 6)\n"
        "          (u32.or (widen U32 u8.and (slice.elt 1 s) #x_1f)))]\n"
        "    [(u8.= num-bytes 3) \n"
        "     (->> (widen U32 u8.and (slice.elt 0 s) #x_f)\n"
        "          (u32.shl 6)\n"
        "          (u32.or (widen U32 u8.and (slice.elt 1 s) #x_1f)) \n"
        "          (u32.shl 6)\n"
        "          (u32.or (widen U32 u8.and (slice.elt 2 s) #x_1f)))] \n"
        "    [:true\n"
        "     (->> (widen U32 u8.and (slice.elt 0 s) #x_7)\n"
        "          (u32.shl 6)\n"
        "          (u32.or (widen U32 u8.and (slice.elt 1 s) #x_1f)) \n"
        "          (u32.shl 6)\n"
        "          (u32.or (widen U32 u8.and (slice.elt 2 s) #x_1f)) \n"
        "          (u32.shl 6)\n"
        "          (u32.or (widen U32 u8.and (slice.elt 3 s) #x_1f)))]))";
    compile_toplevel(decode_uft8_bytes, module, target, &point, &pi_point, region);

    const char *str_elt =
        "(def elt proc [(idx U64) (string String)] seq\n"
        "  [let! offset (local 0)]\n"
        "  [let! index (local 0)]\n"
        "  (loop [while (u64.< ^offset string.len)] \n"
        "        [while (u64.< ^index idx)]\n"
        "    [let! byte (slice.elt ^offset (unname string))]\n"
        "    [let! len (widen U64 (num-utf8-bytes byte))]"
        "    (set index  (u64.+ 1 ^index))\n"
        "    (set offset (u64.+ len ^offset)))\n"
        "  \n"
        "  (when (u64.!= idx ^index) (panic \"string.elt: index out of range\")) \n"
        "  (decode-utf8-point (slice.subview ^offset string.len (unname string))))";
    compile_toplevel(str_elt, module, target, &point, &pi_point, region);

    const char *str_len =
        "(def len proc [(string String)] seq\n"
        "  [let! offset (local 0)]\n"
        "  [let! index (local 0)]\n"
        "  (loop [while (u64.< ^offset string.len)] \n"
        "    [let! byte (slice.elt ^offset (unname string))]\n"
        "    [let! len (widen U64 (num-utf8-bytes byte))]"
        "    (set index  (u64.+ 1 ^index))\n"
        "    (set offset (u64.+ len ^offset)))\n"
        "  \n"
        "  ^index) \n";
    compile_toplevel(str_len, module, target, &point, &pi_point, region);

    const char *str_subview =
        "(def subview proc [(start U64) (end U64) (string String)] seq \n"
        "  [let! start-byte (local start)]\n"
        "  [let! end-byte (local end)]\n"
        "  [let! offset (local 0)]\n"
        "  [let! index (local 0)]\n"
        //"  (debug.debug-break)"
        //"  (when (= start ^index) (set start-byte ^offset))"
        //"  (when (= end ^index) (set end-byte ^offset))"
        //"  (debug.debug-break)"
        "  (loop [while (u64.< ^offset string.len)] \n"
        "    (when (u64.= start ^index) (set start-byte ^offset))"
        "    (when (u64.= end ^index) (set end-byte ^offset))"
        "    [let! byte (slice.elt ^offset (unname string))]\n"
        "    [let! len (widen U64 (num-utf8-bytes byte))]"
        "    (set index  (u64.+ 1 ^index))\n"
        "    (set offset (u64.+ len ^offset)))\n"
        "  \n"
        "  (struct String [.addr (address.num-to-address (u64.+ ^start-byte "
        "                           (address.address-to-num string.addr)))]"
        "                 [.len (u64.- ^end-byte ^start-byte)]))";
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
        "    [let! dest-address (address.num-to-address (u64.+ i (address.address-to-num new-bytes)))]\n"
        "    (address.store dest-address byte))\n"
        "  (address.store {U8} (address.num-to-address (u64.+ ascii.len (address.address-to-num new-bytes))) 0)\n"
        "  (struct String [.addr new-bytes] [.len (u64.+ ascii.len 1)]))";
    compile_toplevel(from_ascii, module, target, &point, &pi_point, region);

    const char *str_delete =
        "(def string-delete instance (Delete String) \n"
        "  [.delete proc [str] memory.free str.addr])";
    compile_toplevel(str_delete, module, target, &point, &pi_point, region);

    const char *str_eql =
        "(def string= proc [(lhs String) (rhs String)] \n"
        "  (if (u64.!= lhs.len rhs.len)  \n"
        "    :false                              \n"
        "    (seq                                \n"
        "      [let! idx (local 0)]              \n"
        "      (loop [for i from 0 below lhs.len] \n"
        "            [while (u8.= (nth-byte i lhs) (nth-byte i rhs))] \n"
        "        (set idx i))                    \n"
        "      (if (u64.= 0 lhs.len)         \n "
        "          :true                         \n "
        "          (u64.= ^idx (u64.- lhs.len 1))))))";
    compile_toplevel(str_eql, module, target, &point, &pi_point, region);

    const char *str_eq =
        "(def string-eq instance (Eq String) [.= string=] [.!= proc [l r] bool.not (string= l r)] )\n";
    compile_toplevel(str_eq, module, target, &point, &pi_point, region);
}
