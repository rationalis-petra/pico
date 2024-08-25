#include "memory/std_allocator.h"
#include "memory/executable.h"
#include "memory/arena.h"

#include "data/string.h"
#include "data/stream.h"

#include "assembler/assembler.h"
#include "pretty/stream_printer.h"
#include "pretty/document.h"

#include "pico/syntax/concrete.h"
#include "pico/parse/parse.h"
#include "pico/binding/environment.h"
#include "pico/analysis/abstraction.h"
#include "pico/analysis/typecheck.h"
#include "pico/codegen/codegen.h"
#include "pico/eval/call.h"
#include "pico/values/types.h"

typedef struct repl_opts {
    bool debug_print;
} repl_opts;

pi_type mk_binop_type(allocator a, prim_type a1, prim_type a2, prim_type r) {
    pi_type* i1 = mem_alloc(sizeof(pi_type), a);
    pi_type* i2 = mem_alloc(sizeof(pi_type), a);
    pi_type* i3 = mem_alloc(sizeof(pi_type), a);

    i1->sort = TPrim;
    i1->prim = a1;
    i2->sort = TPrim;
    i2->prim = a2;
    i3->sort = TPrim;
    i3->prim = r;

    pi_type type;
    type.sort = TProc;
    ptr_array args = mk_ptr_array(2, a);
    push_ptr(i1, &args, a);
    push_ptr(i2, &args, a);

    type.proc.args = args;
    type.proc.ret = i3;

    return type;
}

void build_binary_fun(assembler* ass, binary_op op, allocator a) {

    build_unary_op (ass, Pop, reg(RCX),a );
    build_unary_op (ass, Pop, reg(RBX), a);
    build_unary_op (ass, Pop, reg(RAX), a);
    build_binary_op (ass, op, reg(RAX), reg(RBX), a);
    build_unary_op (ass, Push, reg(RAX), a);
    build_unary_op (ass, Push, reg(RCX), a);
    build_nullary_op (ass, Ret, a);
}

void build_comp_fun(assembler* ass, unary_op op, allocator a) {
    build_unary_op (ass, Pop, reg(RCX),a );
    build_unary_op (ass, Pop, reg(RBX), a);
    build_unary_op (ass, Pop, reg(RAX), a);
    build_binary_op (ass, Cmp, reg(RAX), reg(RBX), a);
    build_unary_op (ass, op, reg(RAX), a);
    build_unary_op (ass, Push, reg(RAX), a);
    build_unary_op (ass, Push, reg(RCX), a);
    build_nullary_op (ass, Ret, a);
}

pi_module* base_module(assembler* ass, allocator a) {
    pi_module* module = mk_module(a);
    pi_symbol sym;

    pi_type type;
    pi_type type_val;
    type = mk_prim_type(TType);
    type_val = mk_prim_type(Int_64);
    pi_type* type_data = &type_val;

    sym = string_to_symbol(mv_string("I64"));
    add_def(module, sym, type, &type_data);

    type_val = mk_prim_type(Bool);
    sym = string_to_symbol(mv_string("Bool"));
    add_def(module, sym, type, &type_data);

    build_binary_fun(ass, Add, a);
    type = mk_binop_type(a, Int_64, Int_64, Int_64);
    sym = string_to_symbol(mv_string("+"));
    add_fn_def(module, sym, type, ass, NULL);
    clear_assembler(ass);

    build_binary_fun(ass, Sub, a);
    sym = string_to_symbol(mv_string("-"));
    add_fn_def(module, sym, type, ass, NULL);
    clear_assembler(ass);
    delete_pi_type(type, a);

    build_comp_fun(ass, SetL, a);
    type = mk_binop_type(a, Int_64, Int_64, Bool);
    sym = string_to_symbol(mv_string("<"));
    add_fn_def(module, sym, type, ass, NULL);
    clear_assembler(ass);

    build_comp_fun(ass, SetG, a);
    sym = string_to_symbol(mv_string(">"));
    add_fn_def(module, sym, type, ass, NULL);
    clear_assembler(ass);

    build_comp_fun(ass, SetE, a);
    sym = string_to_symbol(mv_string("="));
    add_fn_def(module, sym, type, ass, NULL);
    clear_assembler(ass);
    delete_pi_type(type, a);

    pi_term_former_t former;
    type.sort = TPrim;
    type.prim = TFormer;

    former = FProcedure;
    sym = string_to_symbol(mv_string("proc"));
    add_def(module, sym, type, &former);

    former = FApplication;
    sym = string_to_symbol(mv_string("$"));
    add_def(module, sym, type, &former);

    former = FProjector;
    sym = string_to_symbol(mv_string("."));
    add_def(module, sym, type, &former);

    former = FStructure;
    sym = string_to_symbol(mv_string("struct"));
    add_def(module, sym, type, &former);

    former = FVariant;
    sym = string_to_symbol(mv_string(":"));
    add_def(module, sym, type, &former);

    former = FMatch;
    sym = string_to_symbol(mv_string("match"));
    add_def(module, sym, type, &former);

    former = FIf;
    sym = string_to_symbol(mv_string("if"));
    add_def(module, sym, type, &former);

    former = FLet;
    sym = string_to_symbol(mv_string("let"));
    add_def(module, sym, type, &former);

    former = FDefine;
    sym = string_to_symbol(mv_string("def"));
    add_def(module, sym, type, &former);

    // Types 
    former = FProcType;
    sym = string_to_symbol(mv_string("Proc"));
    add_def(module, sym, type, &former);

    former = FStructType;
    sym = string_to_symbol(mv_string("Struct"));
    add_def(module, sym, type, &former);

    former = FEnumType;
    sym = string_to_symbol(mv_string("Enum"));
    add_def(module, sym, type, &former);

    return module;
}

bool repl_iter(istream* cin, ostream* cout, allocator a, assembler* ass, pi_module* module, repl_opts opts) {
    // Create an arena allocator to use in this iteration.
    allocator arena = mk_arena_allocator(4096, a);

    clear_assembler(ass);
    environment* env = env_from_module(module, arena);

    parse_result res = parse_rawtree(cin, arena);
    if (res.type == ParseFail) {
        write_string(mv_string("Parse Failed :(\n"), cout);
        release_arena_allocator(arena);
        return false;
    }
    if (res.type != ParseSuccess) {
        write_string(mv_string("Parse Returned Invalid Result!\n"), cout);
        release_arena_allocator(arena);
        return false;
    }

    document* doc;
    if (opts.debug_print) {
        doc = pretty_rawtree(res.data.result, arena);
        write_string(mv_string("Pretty Printing Raw Syntax\n"), cout);
        write_doc(doc, cout);
        write_string(mv_string("\n"), cout);
    }

    // -------------------------------------------------------------------------
    // Resolution
    // -------------------------------------------------------------------------

    abs_result abs = abstract(res.data.result, env, arena);
    if (abs.type == Err) {
        write_string(mv_string("Abstract Faled :(\n"), cout);
        write_string(abs.error_message, cout);
        write_string(mv_string("\n"), cout);
        release_arena_allocator(arena);
        return false;
    }
    if (abs.type != Ok) {
        write_string(mv_string("Resolve Returned invalid result!\n"), cout);
        release_arena_allocator(arena);
        return false;
    }

    if (opts.debug_print) {
        write_string(mv_string("Pretty Printing Resovled Syntax:\n"), cout);
        doc = pretty_toplevel(&abs.out, arena);
        write_doc(doc, cout);
        write_string(mv_string("\n"), cout);
    }

    // -------------------------------------------------------------------------
    // Type Checking
    // -------------------------------------------------------------------------

    // Note: typechecking annotates the syntax tree with types, but doesn't have
    // an output.
    result tc_res = type_check(&abs.out, env, arena);
    if (tc_res.type == Err) {
        write_string(mv_string("Typechecking Failed\n"), cout);
        write_string(tc_res.error_message, cout);
        write_string(mv_string("\n"), cout);
        release_arena_allocator(arena);
        return false;
    }
    if (tc_res.type != Ok) {
        write_string(mv_string("Typechecking returned an invalid result\n"), cout);
        release_arena_allocator(arena);
        return false;
    }

    if (opts.debug_print) {
        write_string(mv_string("Pretty Printing Inferred Type\n"), cout);
        doc = pretty_type(toplevel_type(abs.out), arena);
        write_doc(doc, cout);
        write_string(mv_string("\n"), cout);
    }

    // -------------------------------------------------------------------------
    // Code Generation
    // -------------------------------------------------------------------------

    gen_result gen_res = generate_toplevel(abs.out, env, ass, arena);

    if (gen_res.type == Err) {
        write_string(mv_string("Codegen Failed\n"), cout);
        write_string(gen_res.error_message, cout);
        write_string(mv_string("\n"), cout);
        release_arena_allocator(arena);
        return false;
    }
    if (gen_res.type != Ok) {
        write_string(mv_string("Codegen returned an invalid result\n"), cout);
        release_arena_allocator(arena);
        return false;
    }

    if (opts.debug_print) {
        write_string(mv_string("Pretty Printing Binary\n"), cout);
        doc = pretty_assembler(ass, arena);
        write_doc(doc, cout);
        write_string(mv_string("\n"), cout);
    }

    // -------------------------------------------------------------------------
    // Evaluation
    // -------------------------------------------------------------------------

    eval_result call_res = pico_run_toplevel(abs.out, ass, &(gen_res.backlinks), module, arena);
    if (opts.debug_print) {
        write_string(mv_string("Pretty Printing Evaluation Result\n"), cout);
    }

    doc = pretty_res(call_res, arena);

    write_doc(doc, cout);
    write_string(mv_string("\n"), cout);

    release_arena_allocator(arena);
    return true;
}

int cstrcmp (const char* lhs, const char* rhs) {
    int result = 0; 
    while (result == 0 && !lhs && !rhs) {
        if (*lhs < *rhs) {
            result = -1;
        } else if (*lhs > *rhs) {
            result = 1;
        }
        lhs++;
        rhs++;
    }
    return result;
}

int main(int argc, char** argv) {
    // Argument parsing
    repl_opts opts;
    opts.debug_print = false;
    if (argc > 1) {
        for (int i = 1; i < argc; i++) {
            if (cstrcmp("-d", argv[i]) == 0) {
                opts.debug_print = true;
            }
        }
    }

    // Setup
    asm_init();

    allocator stdalloc = get_std_allocator();
    istream* cin = get_stdin_stream();
    ostream* cout = get_stdout_stream();
    allocator exalloc = mk_executable_allocator(stdalloc);
    assembler* ass = mk_assembler(exalloc);
    assembler* ass_base = mk_assembler(exalloc);
    pi_module* module = base_module(ass_base, stdalloc);

    // Main Loop
    while (repl_iter(cin, cout, stdalloc, ass, module, opts));

    // Cleanup
    delete_module(module);
    delete_assembler(ass_base);
    delete_assembler(ass);
    clear_symbols();
    release_executable_allocator(exalloc);

    return 0;
}
