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
#include "pico/values/types.h"

pi_type mk_int_binop_type(allocator a) {
    pi_type* i1 = mem_alloc(sizeof(pi_type), a);
    pi_type* i2 = mem_alloc(sizeof(pi_type), a);
    pi_type* i3 = mem_alloc(sizeof(pi_type), a);

    pi_type type;
    type.sort = TProc;
    ptr_array args = mk_ptr_array(2, a);
    push_ptr(i1, &args, a);
    push_ptr(i2, &args, a);

    type.proc.args = args;
    type.proc.ret = i3;

    return type;
}

void* build_binary_fun(assembler* ass, binary_op op, allocator a) {
    void* begin = ass->data + ass->len;
    build_unary_op (ass, Pop, reg(RBP), a);
    build_unary_op (ass, Pop, reg(RBX), a);
    build_unary_op (ass, Pop, reg(RAX), a);
    build_binary_op (ass, op, reg(RAX), reg(RBX), a);
    build_unary_op (ass, Push, reg(RAX), a);
    build_unary_op (ass, Push, reg(RBP), a);
    build_nullary_op (ass, Ret, a);

    return begin;
}

pi_module* base_module(assembler* ass, allocator a) {
    pi_module* module = mk_module(a);
    pi_symbol sym;

    pi_type type;
    type.sort = TFormer;

    void* assembly;

    assembly = build_binary_fun(ass, Add, a);
    type = mk_int_binop_type(a);
    sym = string_to_symbol(mv_string("+"));
    add_def(module, sym, type, assembly, a);

    assembly = build_binary_fun(ass, Sub, a);
    type = mk_int_binop_type(a);
    sym = string_to_symbol(mv_string("-"));
    add_def(module, sym, type, assembly, a);

    /* sym = string_to_symbol(mv_string("*")); */
    /* add_def(module, sym, type, assembly, a); */

    /* sym = string_to_symbol(mv_string("/")); */
    /* add_def(module, sym, type, assembly, a); */

    pi_term_former_t* former;
    type.sort = TFormer;

    former = mem_alloc(sizeof(pi_term_former_t), a);
    *former = FProcedure;
    sym = string_to_symbol(mv_string("proc"));
    add_def(module, sym, type, former, a);

    former = mem_alloc(sizeof(pi_term_former_t), a);
    *former = FApplication;
    sym = string_to_symbol(mv_string("$"));
    add_def(module, sym, type, former, a);

    former = mem_alloc(sizeof(pi_term_former_t), a);
    *former = FConstructor;
    sym = string_to_symbol(mv_string(":"));
    add_def(module, sym, type, former, a);

    former = mem_alloc(sizeof(pi_term_former_t), a);
    *former = FRecursor;
    sym = string_to_symbol(mv_string("match"));
    add_def(module, sym, type, former, a);

    former = mem_alloc(sizeof(pi_term_former_t), a);
    *former = FProjector;
    sym = string_to_symbol(mv_string("."));
    add_def(module, sym, type, former, a);

    former = mem_alloc(sizeof(pi_term_former_t), a);
    *former = FStructure;
    sym = string_to_symbol(mv_string("struct"));
    add_def(module, sym, type, former, a);

    former = mem_alloc(sizeof(pi_term_former_t), a);
    *former = FIf;
    sym = string_to_symbol(mv_string("if"));
    add_def(module, sym, type, former, a);

    former = mem_alloc(sizeof(pi_term_former_t), a);
    *former = FLet;
    sym = string_to_symbol(mv_string("let"));
    add_def(module, sym, type, former, a);

    return module;
}

bool repl_iter(istream* cin, ostream* cout, allocator a, assembler* ass, pi_module* module) {
    clear_assembler(ass);
    environment* env = env_from_module(module, a);

    parse_result res = parse_rawtree(cin, a);
    if (res.type == ParseFail) {
        write_string(mv_string("Parse Failed :(\n"), cout);
        delete_env(env, a);
        return false;
    }
    if (res.type != ParseSuccess) {
        write_string(mv_string("Parse Returned Invalid Result!\n"), cout);
        delete_env(env, a);
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

    resolve_result rlve = resolve_dynamic(res.data.result, env, a);
    delete_rawtree(res.data.result, a);
    if (rlve.type == Err) {
        write_string(mv_string("Resolve Faled :(\n"), cout);
        write_string(rlve.data.error_message, cout);
        write_string(mv_string("\n"), cout);
        delete_string(rlve.data.error_message, a);
        delete_env(env, a);
        return false;
    }
    if (rlve.type != Ok) {
        write_string(mv_string("Resolve Returned invalid result!\n"), cout);
        delete_env(env, a);
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

    result gen_res = generate_toplevel(rlve.data.out, env, ass, a);
    delete_syntax(rlve.data.out, a);

    if (gen_res.type == Err) {
        write_string(mv_string("Codegen Failed\n"), cout);
        write_string(gen_res.error_message, cout);
        delete_string(gen_res.error_message, a);
        write_string(mv_string("\n"), cout);
        delete_env(env, a);
        return false;
    }
    if (gen_res.type != Ok) {
        write_string(mv_string("Codegen returned an invalid result\n"), cout);
        delete_env(env, a);
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
    delete_env(env, a);

    return true;
}


int main(int argc, char** argv) {
    // Setup
    allocator stdalloc = get_std_allocator();
    istream* cin = get_stdin_stream();
    ostream* cout = get_stdout_stream();
    assembler* ass = mk_assembler(stdalloc);
    assembler* ass_base = mk_assembler(stdalloc);
    pi_module* module = base_module(ass_base, stdalloc);

    while (repl_iter(cin, cout, stdalloc, ass, module));

    // Cleanup
    delete_assembler(ass, stdalloc);
    delete_assembler(ass_base, stdalloc);
    delete_module(module, stdalloc);
    clear_symbols();

    return 0;
}
