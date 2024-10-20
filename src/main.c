﻿#include "platform/error.h"
#include "platform/signals.h"

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

PiType mk_binop_type(Allocator* a, PrimType a1, PrimType a2, PrimType r) {
    PiType* i1 = mem_alloc(sizeof(PiType), a);
    PiType* i2 = mem_alloc(sizeof(PiType), a);
    PiType* i3 = mem_alloc(sizeof(PiType), a);

    i1->sort = TPrim;
    i1->prim = a1;
    i2->sort = TPrim;
    i2->prim = a2;
    i3->sort = TPrim;
    i3->prim = r;

    PiType type;
    type.sort = TProc;
    PtrArray args = mk_ptr_array(2, a);
    push_ptr(i1, &args);
    push_ptr(i2, &args);

    type.proc.args = args;
    type.proc.ret = i3;

    return type;
}

void build_binary_fun(Assembler* ass, BinaryOp op, Allocator* a, ErrorPoint* point) {
    build_unary_op (ass, Pop, reg(RCX), a, point);
    build_unary_op (ass, Pop, reg(RBX), a, point);
    build_unary_op (ass, Pop, reg(RAX), a, point);
    build_binary_op (ass, op, reg(RAX), reg(RBX), a, point);
    build_unary_op (ass, Push, reg(RAX), a, point);
    build_unary_op (ass, Push, reg(RCX), a, point);
    build_nullary_op (ass, Ret, a, point);
}

void build_comp_fun(Assembler* ass, UnaryOp op, Allocator* a, ErrorPoint* point) {
    build_unary_op (ass, Pop, reg(RCX), a, point);
    build_unary_op (ass, Pop, reg(RBX), a, point);
    build_unary_op (ass, Pop, reg(RAX), a, point);
    build_binary_op (ass, Cmp, reg(RAX), reg(RBX), a, point);
    build_unary_op (ass, op, reg(RAX), a, point);
    build_unary_op (ass, Push, reg(RAX), a, point);
    build_unary_op (ass, Push, reg(RCX), a, point);
    build_nullary_op (ass, Ret, a, point);
}

Module* base_module(Assembler* ass, Allocator* a) {
    Module* module = mk_module(a);
    Symbol sym;

    PiType type;
    PiType type_val;
    PiType* type_data = &type_val;
    type = (PiType) {
        .sort = TKind,
        .kind.nargs = 0,
    };
    ErrorPoint point;
    if (catch_error(point)) {
        panic(point.error_message);
    }

    type_val = mk_prim_type(Unit);
    sym = string_to_symbol(mv_string("Unit"));
    add_def(module, sym, type, &type_data);

    type_val = mk_prim_type(Bool);
    sym = string_to_symbol(mv_string("Bool"));
    add_def(module, sym, type, &type_data);

    type_val = mk_prim_type(Address);
    sym = string_to_symbol(mv_string("Address"));
    add_def(module, sym, type, &type_data);

    type_val = mk_prim_type(Int_64);
    sym = string_to_symbol(mv_string("I64"));
    add_def(module, sym, type, &type_data);

    type_val = mk_prim_type(Int_32);
    sym = string_to_symbol(mv_string("I32"));
    add_def(module, sym, type, &type_data);

    type_val = mk_prim_type(Int_16);
    sym = string_to_symbol(mv_string("I16"));
    add_def(module, sym, type, &type_data);

    type_val = mk_prim_type(Int_8);
    sym = string_to_symbol(mv_string("I8"));
    add_def(module, sym, type, &type_data);

    build_binary_fun(ass, Add, a, &point);
    type = mk_binop_type(a, Int_64, Int_64, Int_64);
    sym = string_to_symbol(mv_string("+"));
    add_fn_def(module, sym, type, ass, NULL);
    clear_assembler(ass);

    build_binary_fun(ass, Sub, a, &point);
    sym = string_to_symbol(mv_string("-"));
    add_fn_def(module, sym, type, ass, NULL);
    clear_assembler(ass);
    delete_pi_type(type, a);

    build_comp_fun(ass, SetL, a, &point);
    type = mk_binop_type(a, Int_64, Int_64, Bool);
    sym = string_to_symbol(mv_string("<"));
    add_fn_def(module, sym, type, ass, NULL);
    clear_assembler(ass);

    build_comp_fun(ass, SetG, a, &point);
    sym = string_to_symbol(mv_string(">"));
    add_fn_def(module, sym, type, ass, NULL);
    clear_assembler(ass);

    build_comp_fun(ass, SetE, a, &point);
    sym = string_to_symbol(mv_string("="));
    add_fn_def(module, sym, type, ass, NULL);
    clear_assembler(ass);
    delete_pi_type(type, a);

    TermFormer former;
    type.sort = TPrim;
    type.prim = TFormer;

    former = FDefine;
    sym = string_to_symbol(mv_string("def"));
    add_def(module, sym, type, &former);

    former = FDefine;
    sym = string_to_symbol(mv_string("declare"));
    add_def(module, sym, type, &former);

    former = FProcedure;
    sym = string_to_symbol(mv_string("proc"));
    add_def(module, sym, type, &former);

    former = FAll;
    sym = string_to_symbol(mv_string("all"));
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

    former = FIs;
    sym = string_to_symbol(mv_string("is"));
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

    former = FAllType;
    sym = string_to_symbol(mv_string("All"));
    add_def(module, sym, type, &former);

    return module;
}

bool repl_iter(IStream* cin, OStream* cout, Allocator* a, Assembler* ass, Module* module, repl_opts opts) {
    // TODO (TAGS: UB BUG INVESTIGATE): Possibly need to add volatile qualifier to arena?
    // however, we are not expecting the arena to be mutated, so...
    // Create an arena allocator to use in this iteration.
    Allocator arena = mk_arena_allocator(4096, a);

    clear_assembler(ass);
    Environment* env = env_from_module(module, &arena);

    ErrorPoint point;
    if (catch_error(point)) goto on_error;

    write_string(mv_string("> "), cout);
    ParseResult res = parse_rawtree(cin, &arena);
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

    Document* doc;
    if (opts.debug_print) {
        doc = pretty_rawtree(res.data.result, &arena);
        write_string(mv_string("Pretty Printing Raw Syntax\n"), cout);
        write_doc(doc, cout);
        write_string(mv_string("\n"), cout);
    }

    // -------------------------------------------------------------------------
    // Resolution
    // -------------------------------------------------------------------------

    TopLevel abs = abstract(res.data.result, env, &arena, &point);

    if (opts.debug_print) {
        write_string(mv_string("Pretty Printing Resovled Syntax:\n"), cout);
        doc = pretty_toplevel(&abs, &arena);
        write_doc(doc, cout);
        write_string(mv_string("\n"), cout);
    }

    // -------------------------------------------------------------------------
    // Type Checking
    // -------------------------------------------------------------------------

    // Note: typechecking annotates the syntax tree with types, but doesn't have
    // an output.
    type_check(&abs, env, &arena, &point);

    if (opts.debug_print) {
        write_string(mv_string("Pretty Printing Inferred Type\n"), cout);
        doc = pretty_type(toplevel_type(abs), &arena);
        write_doc(doc, cout);
        write_string(mv_string("\n"), cout);
    }

    // -------------------------------------------------------------------------
    // Code Generation
    // -------------------------------------------------------------------------

    GenResult gen_res = generate_toplevel(abs, env, ass, &arena, &point);

    if (opts.debug_print) {
        write_string(mv_string("Pretty Printing Binary\n"), cout);
        doc = pretty_assembler(ass, &arena);
        write_doc(doc, cout);
        write_string(mv_string("\n"), cout);
    }

    // -------------------------------------------------------------------------
    // Evaluation
    // -------------------------------------------------------------------------

    EvalResult call_res = pico_run_toplevel(abs, ass, &(gen_res.backlinks), module, &arena, &point);
    if (opts.debug_print) {
        write_string(mv_string("Pretty Printing Evaluation Result\n"), cout);
    }

    doc = pretty_res(call_res, &arena);

    write_doc(doc, cout);
    write_string(mv_string("\n"), cout);

    release_arena_allocator(arena);
    return true;

 on_error:
    write_string(point.error_message, cout);
    write_string(mv_string("\n"), cout);
    release_arena_allocator(arena);
    return false;
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

    Allocator* stdalloc = get_std_allocator();
    IStream* cin = get_stdin_stream();
    OStream* cout = get_stdout_stream();
    Allocator exalloc = mk_executable_allocator(stdalloc);
    Assembler* ass = mk_assembler(&exalloc);
    Assembler* ass_base = mk_assembler(&exalloc);
    Module* module = base_module(ass_base, stdalloc);

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
