// main.c
//

#include <stdio.h>
#include <string.h>
#include <encodings/utf8.h>

#include "data/string.h"
#include "data/stream.h"
#include "memory/std_allocator.h"
#include "pretty/stream_printer.h"
#include "pretty/document.h"
#include "pico/syntax/concrete.h"
#include "pico/parse/parse.h"
#include "pico/binding/environment.h"
#include "pico/analysis/name_resolution.h"
#include "pico/eval/expr.h"

environment* default_env(allocator a) {
    environment* env = env_empty(a);
    ob_value val;
    ob_symbol sym;

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

bool repl_iter(istream* cin, ostream* cout, allocator a) {
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

    // Evaluate!
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

    eval_result evl = eval_expr(rlve.data.out, env, a);
    delete_syntax(rlve.data.out, a);
    delete_env(env, a);

    if (evl.type == Err) {
        write_string(mv_string("Eval Failed\n"), cout);
        write_string(evl.data.error_message, cout);
        delete_string(evl.data.error_message, a);
        return false;
    }
    if (evl.type != Ok) {
        write_string(mv_string("Eval Returned invalid result\n"), cout);
        return false;
    }
    write_string(mv_string("Pretty Printing Evaluated Result:\n"), cout);
    doc = pretty_value(evl.data.out, a);
    write_doc(doc, cout);
    delete_doc(doc, a);
    write_string(mv_string("\n"), cout);
}


int main()
{
    allocator stdalloc = get_std_allocator();
    istream* cin = get_stdin_stream();
    ostream* cout = get_stdout_stream();

    while (repl_iter(cin, cout, stdalloc));

    clear_symbols();
    return 0;
}

