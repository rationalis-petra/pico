#include "platform/signals.h"
#include "platform/memory/arena.h"

#include "pico/data/range.h"
#include "pico/syntax/concrete.h"
#include "pico/values/modular.h"
#include "pico/codegen/foreign_adapters.h"
#include "pico/stdlib/core.h"
#include "pico/stdlib/extra.h"
#include "pico/stdlib/meta/submodules.h"

#include "app/module_load.h"

// C implementation (called from pico!)
Result load_module_c_fun(String filename) {
    // (ann load-module String → Unit) : load the module/code located in file! 
    
    Allocator* a = get_std_allocator();
    IStream* sfile = open_file_istream(filename, a);
    if (!sfile) {
      return (Result) {
        .type = Err, .error_message = mv_string("failed to open file!"),
      };
    }

    OStream* current_ostream = get_std_ostream();
    Package* current_package = get_current_package();
    FormattedOStream* os = mk_formatted_ostream(current_ostream, a);

    Module* parent = get_std_current_module();
    load_module_from_istream(sfile, os, current_package, parent, a);
    delete_istream(sfile, a);
    delete_formatted_ostream(os, a);
    return (Result) {.type = Ok};
}

void build_load_module_fun(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType result_ctype = mk_struct_ctype(a, 2,
                                         "type", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                                         "error_message", copy_c_type(mk_string_ctype(a), a));


    CType fn_ctype = mk_fn_ctype(a, 1, "filename", mk_string_ctype(a), result_ctype);

    convert_c_fn(load_module_c_fun, &fn_ctype, type, ass, a, point); 
}

Result run_script_c_fun(String filename) {
    // (ann load-module String → Unit) : load the module/code located in file! 
    
    Allocator* a = get_std_allocator();
    IStream* sfile = open_file_istream(filename, a);
    if (!sfile) {
      return (Result) {
        .type = Err, .error_message = mv_string("failed to open file!"),
      };
    }
    Module* current_module = get_std_current_module();
    OStream* current_ostream = get_std_ostream();
    FormattedOStream* os = mk_formatted_ostream(current_ostream, a);
    run_script_from_istream(sfile, os, current_module, a);
    delete_istream(sfile, a);
    delete_formatted_ostream(os, a);
    return (Result) {.type = Ok};
}

void build_run_script_fun(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    // Proc type
    // TODO: this does not accurately represent the result type!
    CType result_ctype = mk_struct_ctype(a, 2,
                                         "type", mk_primint_ctype((CPrimInt){.prim = CLongLong, .is_signed = Unsigned}),
                                         "error_message", copy_c_type(mk_string_ctype(a), a));


    CType fn_ctype = mk_fn_ctype(a, 1, "filename", mk_string_ctype(a), result_ctype);

    convert_c_fn(run_script_c_fun, &fn_ctype, type, ass, a, point); 
}


void add_refl_module(Assembler* ass, Module* base, Allocator* a) {
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(0, a),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, a),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("refl")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, get_package(base), NULL, a);
    delete_module_header(header);
    Symbol sym;

    PiType type;
    PiType* typep;
    ErrorPoint point;
    if (catch_error(point)) {
        panic(point.error_message);
    }

    // TODO: we use int64_t as it has the requisite size (8 bytes)
    // for pico values: currently don't support non-64 bit values 
    TermFormer former;
    // TermFormer former;
    type.sort = TPrim;
    type.prim = TFormer;

    Segments null_segments = (Segments) {
        .code = mk_u8_array(0, a),
        .data = mk_u8_array(0, a),
    };

    // Now that we have setup appropriately, override the allocator
    Allocator arena = mk_arena_allocator(16384, a);
    a = &arena;

    // ------------------------------------------------------------------------
    // Term Formers
    // ------------------------------------------------------------------------

    // ------------------------------------------------------------------------
    // Types 
    // ------------------------------------------------------------------------

    // ------------------------------------------------------------------------
    // Values 
    // ------------------------------------------------------------------------

    type = (PiType) {
        .sort = TKind,
        .kind.nargs = 0,
    };

    Segments fn_segments = (Segments) {.data = mk_u8_array(0, a),};
    Segments prepped;    // load-module : Proc [String] Unit
    typep = mk_proc_type(a, 1, mk_string_type(a), mk_enum_type(a, 2, "Ok", 0, "Err", 1, mk_string_type(a)));
    build_load_module_fun(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("load-module"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    // run-script : Proc [String] Result
    typep = mk_proc_type(a, 1, mk_string_type(a), mk_enum_type(a, 2, "Ok", 0, "Err", 1, mk_string_type(a)));
    build_run_script_fun(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("run-script"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);



    // load-module : Proc [String] Unit
    typep = mk_proc_type(a, 1, mk_string_type(a), mk_enum_type(a, 2, "Ok", 0, "Err", 1, mk_string_type(a)));
    build_load_module_fun(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("load-module"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    // run-script : Proc [String] Result
    typep = mk_proc_type(a, 1, mk_string_type(a), mk_enum_type(a, 2, "Ok", 0, "Err", 1, mk_string_type(a)));
    build_run_script_fun(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("run-script"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);


    add_module_def(base, string_to_symbol(mv_string("refl")), module);

    sdelete_u8_array(null_segments.code);
    sdelete_u8_array(null_segments.data);
    // Note: we do NOT delete the 'fn_segments.code' because it is the
    // assembler, and needs to be used later!
    sdelete_u8_array(fn_segments.data);
    release_arena_allocator(arena);
}
