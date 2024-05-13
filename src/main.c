#include "assembler/assembler.h"
#include "data/string.h"
#include "data/stream.h"
#include "memory/std_allocator.h"
#include "pretty/stream_printer.h"
#include "pretty/standard_types.h"
#include "pretty/document.h"

#include "pico/syntax/concrete.h"
#include "pico/parse/parse.h"
#include "pico/binding/environment.h"
#include "pico/analysis/name_resolution.h"
#include "pico/codegen/codegen.h"
#include "pico/eval/call.h"

environment* default_env(allocator a) {
    environment* env = env_empty(a);
    pi_value val;
    pi_symbol sym;

    val.type = VPrimOp;
    val.term.primop = AddI64;
    sym = string_to_symbol(mv_string("+"));
    env_insert_inplace(sym, val, env, a);

    val.term.primop = SubI64;
    sym = string_to_symbol(mv_string("-"));
    env_insert_inplace(sym, val, env, a);

    val.term.primop = MulI64;
    sym = string_to_symbol(mv_string("*"));
    env_insert_inplace(sym, val, env, a);

    val.term.primop = QuotI64;
    sym = string_to_symbol(mv_string("/"));
    env_insert_inplace(sym, val, env, a);

    val.type = VFormer;
    val.term.former = FProcedure;
    sym = string_to_symbol(mv_string("proc"));
    env_insert_inplace(sym, val, env, a);

    val.term.former = FApplication;
    sym = string_to_symbol(mv_string("$"));
    env_insert_inplace(sym, val, env, a);

    val.term.former = FConstructor;
    sym = string_to_symbol(mv_string(":"));
    env_insert_inplace(sym, val, env, a);

    val.term.former = FRecursor;
    sym = string_to_symbol(mv_string("match"));
    env_insert_inplace(sym, val, env, a);

    val.term.former = FProjector;
    sym = string_to_symbol(mv_string("."));
    env_insert_inplace(sym, val, env, a);

    val.term.former = FStructure;
    sym = string_to_symbol(mv_string("struct"));
    env_insert_inplace(sym, val, env, a);

    val.term.former = FIf;
    sym = string_to_symbol(mv_string("if"));
    env_insert_inplace(sym, val, env, a);

    val.term.former = FLet;
    sym = string_to_symbol(mv_string("let"));
    env_insert_inplace(sym, val, env, a);

    return env;
}

bool repl_iter(istream* cin, ostream* cout, allocator a, assembler* ass) {
    clear_assembler(ass);

    parse_result res = parse_rawtree(cin, a);
    if (res.type == ParseFail) {
        write_string(mv_string("Parse Failed :(\n"), cout);
        return false;
    }
    if (res.type != ParseSuccess) {
        write_string(mv_string("Parse Returned Invalid Result!\n"), cout);
        return false;
    }
    document* doc = pretty_rawtree(res.data.result, a);
    write_string(mv_string("Pretty Printing Raw Syntax\n"), cout);
    write_doc(doc, cout);
    write_string(mv_string("\n"), cout);
    delete_doc(doc, a);

    // -------------------------------------------------------------------------
    // Resolution
    // -------------------------------------------------------------------------

    environment* env = default_env(a);
    resolve_result rlve = resolve_dynamic(res.data.result, env, a);
    delete_rawtree(res.data.result, a);
    if (rlve.type == Err) {
        delete_env(env, a);
        write_string(mv_string("Resolve Faled :(\n"), cout);
        write_string(rlve.data.error_message, cout);
        delete_string(rlve.data.error_message, a);
        return false;
    }
    if (rlve.type != Ok) {
        delete_env(env, a);
        write_string(mv_string("Resolve Returned invalid result!\n"), cout);
        return false;
    }
    write_string(mv_string("Pretty Printing Resovled Syntax:\n"), cout);
    doc = pretty_syntax(&rlve.data.out, a);
    write_doc(doc, cout);
    delete_doc(doc, a);
    write_string(mv_string("\n"), cout);

    // -------------------------------------------------------------------------
    // Code Generation
    // -------------------------------------------------------------------------

    result gen_res = generate(rlve.data.out, env, ass, a);
    if (gen_res.type == Ok) {
        // genereate a return call
        gen_res = build_unary_op(ass, Pop, reg(RAX), a);

        if (gen_res.type == Ok) {
            gen_res = build_nullary_op(ass, Ret, a);
        }
    }
    delete_syntax(rlve.data.out, a);
    delete_env(env, a);

    if (gen_res.type == Err) {
        write_string(mv_string("Codegen Failed\n"), cout);
        write_string(gen_res.error_message, cout);
        delete_string(gen_res.error_message, a);
        write_string(mv_string("\n"), cout);
        return false;
    }
    if (gen_res.type != Ok) {
        write_string(mv_string("Codegen returned an invalid result\n"), cout);
        return false;
    }

    write_string(mv_string("Pretty Printing Assembler\n"), cout);
    doc = pretty_assembler(ass, a);
    write_doc(doc, cout);
    write_string(mv_string("\n"), cout);
    delete_doc(doc, a);

    // -------------------------------------------------------------------------
    // Evaluation
    // -------------------------------------------------------------------------

    write_string(mv_string("Pretty Printing Evaluation Result\n"), cout);
    int64_t call_res = pico_run_expr(ass->data);
    doc = pretty_i64(call_res, a);
    write_doc(doc, cout);
    write_string(mv_string("\n"), cout);
    delete_doc(doc, a);

    return true;
}


int main(int argc, char** argv) {
    // Setup
    allocator stdalloc = get_std_allocator();
    istream* cin = get_stdin_stream();
    ostream* cout = get_stdout_stream();
    assembler* ass = mk_assembler(stdalloc);

    while (repl_iter(cin, cout, stdalloc, ass));

    // Cleanup
    delete_assembler(ass, stdalloc);
    clear_symbols();

    return 0;
}
