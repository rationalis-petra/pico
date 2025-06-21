#include <stdio.h>
#include <stdlib.h>

#include "platform/machine_info.h"
#include "platform/signals.h"
#include "platform/signals.h"
#include "platform/memory/arena.h"

#include "pico/stdlib/core.h"
#include "pico/stdlib/extra.h"
#include "pico/stdlib/meta.h"
#include "pico/syntax/concrete.h"
#include "pico/analysis/abstraction.h"
#include "pico/codegen/foreign_adapters.h"

#include "app/module_load.h"

static jump_buf* m_buf;
void set_exit_callback(jump_buf* buf) { m_buf = buf; }

static uint64_t std_current_allocator; 
static uint64_t std_perm_allocator; 
//static uint64_t std_region_allocator; 
//static uint64_t std_comptime_allocator; 
static uint64_t std_temp_allocator; 

static Package* current_package;
void set_current_package(Package* current) { current_package = current; }

static IStream* current_istream;
void set_std_istream(IStream* current) { current_istream = current; }

static OStream* current_ostream;
OStream *set_std_ostream(OStream *current) {
    OStream* old = current_ostream;
    current_ostream = current;
    return old;
}

Allocator get_std_current_allocator() {
    Allocator** data = get_dynamic_memory();
    Allocator* dyn = data[std_current_allocator]; 
    return *dyn;
}

Allocator set_std_current_allocator(Allocator al) {
    Allocator** data = get_dynamic_memory();
    Allocator* dyn = data[std_current_allocator]; 
    Allocator old = *dyn;
    *dyn = al;
    return old;
}

Allocator get_std_perm_allocator() {
    Allocator** data = get_dynamic_memory();
    Allocator* dyn = data[std_perm_allocator]; 
    return *dyn;
}

Allocator set_std_perm_allocator(Allocator al) {
    Allocator** data = get_dynamic_memory();
    Allocator* dyn = data[std_perm_allocator]; 
    Allocator old = *dyn;
    *dyn = al;
    return old;
}

Allocator* get_std_temp_allocator() {
    void** data = get_dynamic_memory();
    Allocator** dyn = data[std_temp_allocator]; 
    return *dyn;
}

Allocator* set_std_temp_allocator(Allocator* al) {
    void** data = get_dynamic_memory();
    Allocator** dyn = data[std_temp_allocator]; 
    Allocator* old = *dyn;
    *dyn = al;
    return old;
}

static uint64_t std_current_module;
Module* get_std_current_module() {
    void** data = get_dynamic_memory();
    Module** dyn = data[std_current_module]; 
    return *dyn;
}

Module* set_std_current_module(Module* md) {
    void** data = get_dynamic_memory();
    Module** mdle = data[std_current_module]; 
    Module* old = *mdle;
    *mdle = md;
    return old;
}

void build_realloc_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    // realloc : Proc (Address U64) Address
    build_unary_op(ass, Pop, reg(RAX, sz_64), a, point);

#if ABI == SYSTEM_V_64
    // realloc (ptr = rdi, size = rsi)
    // copy size into RDX
    build_unary_op(ass, Pop, reg(RSI, sz_64), a, point);
    build_unary_op(ass, Pop, reg(RDI, sz_64), a, point);
    build_unary_op(ass, Push, reg(RAX, sz_64), a, point);

#elif ABI == WIN_64
    // realloc (ptr = RCX, size = RDX)
    build_unary_op(ass, Pop, reg(RDX, sz_64), a, point);
    build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);
    build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
    build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(32), a, point);
#endif


    build_binary_op(ass, Mov, reg(RAX, sz_64), imm64((uint64_t)&realloc),  a, point);
    build_unary_op(ass, Call, reg(RAX, sz_64), a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(32), a, point);
#endif

    build_unary_op(ass, Pop, reg(R9, sz_64), a, point);
    build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
    build_unary_op(ass, Push, reg(R9, sz_64), a, point);

    build_nullary_op(ass, Ret, a, point);
}

void *relic_malloc(uint64_t size) {
    Allocator a = get_std_current_allocator();
    return mem_alloc(size, &a);
}

void build_malloc_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    // malloc : Proc (U64) Address
    build_unary_op(ass, Pop, reg(RAX, sz_64), a, point);

#if ABI == SYSTEM_V_64
    // memcpy (dest = rdi, src = rsi, size = rdx)
    // copy size into RDX
    build_unary_op(ass, Pop, reg(RDI, sz_64), a, point);
    build_unary_op(ass, Push, reg(RAX, sz_64), a, point);

#elif ABI == WIN_64
    build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);
    build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
    build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(32), a, point);
#endif

    build_binary_op(ass, Mov, reg(RAX, sz_64), imm64((uint64_t)&relic_malloc),  a, point);
    build_unary_op(ass, Call, reg(RAX, sz_64), a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(32), a, point);
#endif

    build_unary_op(ass, Pop, reg(R9, sz_64), a, point);
    build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
    build_unary_op(ass, Push, reg(R9, sz_64), a, point);

    build_nullary_op(ass, Ret, a, point);
}

void build_free_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    // free : Proc (Address) Unit
    build_unary_op(ass, Pop, reg(RAX, sz_64), a, point);

#if ABI == SYSTEM_V_64
    // free (dest = rdi)
    // copy address into RDI
    build_unary_op(ass, Pop, reg(RDI, sz_64), a, point);
    build_unary_op(ass, Push, reg(RAX, sz_64), a, point);

#elif ABI == WIN_64
    // free (addr = rcx)
    // copy address into RCX
    build_unary_op(ass, Pop, reg(RCX, sz_64), a, point);
    build_unary_op(ass, Push, reg(RAX, sz_64), a, point);
    build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(32), a, point);
#endif

    build_binary_op(ass, Mov, reg(RAX, sz_64), imm64((uint64_t)&free),  a, point);
    build_unary_op(ass, Call, reg(RAX, sz_64), a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(32), a, point);
#endif

    build_nullary_op(ass, Ret, a, point);
}

void build_panic_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    // Pop return address (we don't need it)
    build_binary_op(ass, Add, reg(RSP, sz_64), imm8(8), a, point);

    // panic : All [A] Proc (String) A
#if ABI == SYSTEM_V_64
    // panic (str.memsize = rdi, str.bytes = rsi)
    // 
    // copy address into RDI
    build_unary_op(ass, Pop, reg(RDI, sz_64), a, point);
    build_unary_op(ass, Pop, reg(RSI, sz_64), a, point);

#elif ABI == WIN_64
    // panic (&str = rcx)
    // copy address into RCX
    build_binary_op(ass, Mov, reg(RCX, sz_64), reg(RSP, sz_64), a, point);
    build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(32), a, point);
#endif
    build_binary_op(ass, Mov, reg(RAX, sz_64), imm64((uint64_t)&panic),  a, point);
    build_unary_op(ass, Call, reg(RAX, sz_64), a, point);
    // Panic does not return
}

PiType* build_panic_fn_ty(Allocator* a) {
    PiType* proc_ty = mk_proc_type(a, 1, mk_string_type(a), mk_var_type(a, "A"));

    SymbolArray types = mk_symbol_array(1, a);
    push_symbol(string_to_symbol(mv_string("A")), &types);

    PiType* out_ty = mem_alloc(sizeof(PiType), a);
    *out_ty =  (PiType) {.sort = TAll, .binder.vars = types, .binder.body = proc_ty};
    return out_ty;
}

void exit_callback() {
    long_jump(*m_buf, 1);
}

void build_exit_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    build_binary_op(ass, Mov, reg(RAX, sz_64), imm64((uint64_t)exit_callback), a, point);
    build_unary_op(ass, Call, reg(RAX, sz_64), a, point);
}

void relic_print_fn(String s) {
    write_string(s, current_ostream);
}

void relic_println_fn(String s) {
    puts((char*)s.bytes);
}

void build_print_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(a, 1, "string", mk_string_ctype(a), (CType){.sort = CSVoid});
    convert_c_fn(relic_print_fn, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, a);
}

void build_println_fn(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(a, 1, "string", mk_string_ctype(a), (CType){.sort = CSVoid});
    convert_c_fn(relic_println_fn, &fn_ctype, type, ass, a, point); 
    delete_c_type(fn_ctype, a);
}

// C implementation (called from pico!)
void load_module_c_fun(String filename) {
    // (ann load-module String → Unit) : load the module/code located in file! 
    
    Allocator* a = get_std_allocator();
    IStream* sfile = open_file_istream(filename, a);
    load_module_from_istream(sfile, mk_formatted_ostream(current_ostream, a), current_package, NULL, a);
    delete_istream(sfile, a);
}

void build_load_module_fun(Assembler* ass, Allocator* a, ErrorPoint* point) {
    // (ann load-module String → Unit) : load the module/code located in file! 

#if ABI == SYSTEM_V_64
    // load_module_c_fun (struct on stack)
    // pass in platform/memory/on stack(?)
    build_unary_op (ass, Push, imm32(0), a, point);
    build_unary_op (ass, Push, rref8(RSP, 24, sz_64), a, point);
    // note: use 24 twice as RSP grows with push! 
    build_unary_op (ass, Push, rref8(RSP, 24, sz_64), a, point);

#elif ABI == WIN_64
    // load_module_c_fun: push struct
    // pass in platform/memory/on stack(?)
    build_unary_op (ass, Push, imm32(0), a, point);
    build_unary_op (ass, Push, rref8(RSP, 24, sz_64), a, point);
    // note: use 24 twice as RSP grows with push! 
    build_unary_op (ass, Push, rref8(RSP, 24, sz_64), a, point);

    // store ptr to struct in rcx
    build_binary_op(ass, Mov, reg(RCX, sz_64), reg(RSP, sz_64), a, point);
    build_binary_op(ass, Sub, reg(RSP, sz_64), imm32(32), a, point);
#else
#error "Unknown calling convention"
#endif

    build_binary_op(ass, Mov, reg(RAX, sz_64), imm64((uint64_t)&load_module_c_fun), a, point);
    build_unary_op(ass, Call, reg(RAX, sz_64), a, point);

#if ABI == WIN_64
    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(32), a, point);
#endif

    // To return:
    // + pop argument we pushed onto stack
    // + stash ret addr
    // + pop argument we were called with
    // + push ret addr & return
    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(24), a, point);
    build_unary_op (ass, Pop, reg(RAX, sz_64), a, point);
    build_binary_op(ass, Add, reg(RSP, sz_64), imm32(16), a, point);
    build_unary_op (ass, Push, reg(RAX, sz_64), a, point);
    build_nullary_op (ass, Ret, a, point);
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

RawTree atom_symbol(const char *str) {
    return (RawTree) {
        .type = RawAtom,
        .atom.type = ASymbol,
        .atom.symbol = string_to_symbol(mv_string(str)),
    };
}

typedef enum {UpTo, Below, Above, DownTo} RangeType; 

typedef struct {
    RangeType type;
    RawTree name;
    RawTree from;
    RawTree to;
} ForRange;

RawTree mk_condition(ForRange range, Allocator *a) {
    const char* comparator = NULL;
    switch (range.type) {
    case UpTo: comparator = "<="; break;
    case Below: comparator = "<"; break;
    case Above: comparator = ">"; break;
    case DownTo: comparator = ">="; break;
    }
    
    RawTreeArray proj_nodes = mk_rawtree_array(3, a);
    push_rawtree(atom_symbol("."), &proj_nodes);
    push_rawtree(atom_symbol(comparator), &proj_nodes);
    push_rawtree(atom_symbol("u64"), &proj_nodes);

    RawTree proj = (RawTree) {
        .type = RawBranch,
        .branch.hint = HExpression,
        .branch.nodes = proj_nodes,
    };

    RawTreeArray comp_nodes = mk_rawtree_array(3, a);
    push_rawtree(proj, &comp_nodes);
    push_rawtree(range.name, &comp_nodes);
    push_rawtree(range.to, &comp_nodes);

    return (RawTree) {
        .type = RawBranch,
        .branch.hint = HExpression,
        .branch.nodes = comp_nodes,
    };
}

MacroResult loop_macro(RawTreeArray nodes) {
    // TODO (BUGS):
    //   non-hygenic: labels loop-exit and loop-continue
    //   
    // (loop 
    //   [for var from i upto j]
    //   [for var from i above j]
    //   [for var from i downto j]
    //   [for var from i below j]
    //   (c1)
    //   (c2)
    //   (c3)
    // )
    Allocator a = get_std_current_allocator();

    RawTreeArray loop_body_nodes = mk_rawtree_array(nodes.len + 1, &a);
    push_rawtree(atom_symbol("seq"), &loop_body_nodes);

    // We push a 'null' rawtree because this will later be assigned to the
    // loop-check 
    push_rawtree((RawTree){}, &loop_body_nodes);
    PtrArray loop_fors = mk_ptr_array(2, &a);
    PtrArray loop_whiles = mk_ptr_array(2, &a);
    for (size_t i = 1; i < nodes.len; i++) {
        RawTree branch = nodes.data[i];
        if (branch.type == RawBranch && branch.branch.hint == HSpecial) {
            if (branch.branch.nodes.len == 0) {
                return (MacroResult) {
                    .result_type = Left,
                    .err.message = mv_string("Malformed loop clause no terms provided"),
                    .err.range = branch.range,
                };
            }
            RawTree clause_type = branch.branch.nodes.data[0];
            if (!is_symbol(clause_type)) {
                return (MacroResult) {
                    .result_type = Left,
                    .err.message = mv_string("Malformed loop clause: expected first term to be a symbol"),
                    .err.range = branch.branch.nodes.data[0].range,
                };
            }
            if (eq_symbol(&branch.branch.nodes.data[0], string_to_symbol(mv_string("for")))) {
                ForRange range;
                if (branch.branch.nodes.len != 6) {
                    return (MacroResult) {
                        .result_type = Left,
                        .err.message = mv_string("Malformed for clause: incorrect number of terms - expecting 6"),
                        .err.range = branch.range,
                    };
                }

                if (!is_symbol(branch.branch.nodes.data[1])) {
                    return (MacroResult) {
                        .result_type = Left,
                        .err.message = mv_string("For loop expects variable"),
                        .err.range = branch.branch.nodes.data[1].range,
                    };
                }

                range.name = branch.branch.nodes.data[1];

                if (!eq_symbol(&branch.branch.nodes.data[2], string_to_symbol(mv_string("from")))) {
                    return (MacroResult) {
                        .result_type = Left,
                        .err.message = mv_string("For loop expects 'from'"),
                        .err.range = branch.branch.nodes.data[2].range,
                    };
                }

                range.from = branch.branch.nodes.data[3];

                if (!is_symbol(branch.branch.nodes.data[4])) {
                    return (MacroResult) {
                        .result_type = Left,
                        .err.message = mv_string("For loop expects one of 'upto', 'below', 'downto', 'above'"),
                        .err.range = branch.branch.nodes.data[4].range,
                    };
                } else {
                    Symbol s = branch.branch.nodes.data[4].atom.symbol;
                    if (symbol_eq(s, string_to_symbol(mv_string("upto")))) {
                        range.type = UpTo;
                    } else if (symbol_eq(s, string_to_symbol(mv_string("below")))) {
                        range.type = Below;
                    } else if (symbol_eq(s, string_to_symbol(mv_string("downto")))) {
                        range.type = DownTo;
                    } else if (symbol_eq(s, string_to_symbol(mv_string("above")))) {
                        range.type = Above;
                    } else {
                        return (MacroResult) {
                            .result_type = Left,
                            .err.message = mv_string("For loop expects one of 'upto', 'below', 'downto', 'above'"),
                            .err.range = branch.branch.nodes.data[4].range,
                        };
                    }
                }

                range.to = branch.branch.nodes.data[5];

                ForRange* rp = mem_alloc(sizeof(ForRange), &a);
                *rp = range;
                push_ptr(rp, &loop_fors);
            } else if (eq_symbol(&branch.branch.nodes.data[0], string_to_symbol(mv_string("while")))) {
                RawTree *raw_term = (branch.branch.nodes.len == 2)
                    ? &branch.branch.nodes.data[1]
                    : raw_slice(&branch, 1, &a);
                push_ptr(raw_term, &loop_whiles);
            } else {
                return (MacroResult) {
                    .result_type = Left,
                    .err.message = mv_string("Malformed loop clause: expected on of 'while', 'for'"),
                    .err.range = branch.branch.nodes.data[0].range,
                };
            }
        } else {
            push_rawtree(nodes.data[i], &loop_body_nodes);
        }
    }

    RawTreeArray start_go_to_nodes = mk_rawtree_array(2 + loop_fors.len, &a);
    push_rawtree(atom_symbol("go-to"), &start_go_to_nodes);
    push_rawtree(atom_symbol("loop-continue"), &start_go_to_nodes);

    RawTreeArray continue_go_to_nodes = mk_rawtree_array(2 + loop_fors.len, &a);
    push_rawtree(atom_symbol("go-to"), &continue_go_to_nodes);
    push_rawtree(atom_symbol("loop-continue"), &continue_go_to_nodes);

    RawTree loop_condition;
    if (loop_fors.len + loop_whiles.len == 0) {
        loop_condition = (RawTree) { 
            .type = RawAtom,
            .atom.type = ABool,
            .atom.boolean = true, // TODO should this be false? no condition =
                                  // no loop?
        };
    }

    if (loop_fors.len > 0) {
        loop_condition = mk_condition(*(ForRange*)loop_fors.data[0], &a);
        for (size_t i = 1; i < loop_fors.len; i++) {
            RawTree new_condition = mk_condition(*(ForRange*)loop_fors.data[i], &a);
            RawTreeArray and_fn_nodes = mk_rawtree_array(3, &a);
            push_rawtree(atom_symbol("."), &and_fn_nodes);
            push_rawtree(atom_symbol("and"), &and_fn_nodes);
            push_rawtree(atom_symbol("bool"), &and_fn_nodes);
            RawTree and = (RawTree) {
                .type = RawBranch,
                .branch.hint = HExpression,
                .branch.nodes = and_fn_nodes,
            };

            RawTreeArray and_nodes = mk_rawtree_array(3, &a);

            push_rawtree(and, &and_nodes);
            push_rawtree(loop_condition, &and_nodes);
            push_rawtree(new_condition, &and_nodes);
            loop_condition = (RawTree) {
                .type = RawBranch,
                .branch.hint = HExpression,
                .branch.nodes = and_nodes,
            };
        }
    }

    if (loop_whiles.len > 0) {
        size_t start_idx = 0;
        if (loop_fors.len == 0) {
            loop_condition = *(RawTree*)loop_whiles.data[0];
            start_idx++;
        }
        for (size_t i = start_idx; i < loop_whiles.len; i++) {
            RawTree new_condition = *(RawTree*)loop_whiles.data[i];
            RawTreeArray and_fn_nodes = mk_rawtree_array(3, &a);
            push_rawtree(atom_symbol("."), &and_fn_nodes);
            push_rawtree(atom_symbol("and"), &and_fn_nodes);
            push_rawtree(atom_symbol("bool"), &and_fn_nodes);
            RawTree and = (RawTree) {
                .type = RawBranch,
                .branch.hint = HExpression,
                .branch.nodes = and_fn_nodes,
            };

            RawTreeArray and_nodes = mk_rawtree_array(3, &a);

            push_rawtree(and, &and_nodes);
            push_rawtree(loop_condition, &and_nodes);
            push_rawtree(new_condition, &and_nodes);
            loop_condition = (RawTree) {
                .type = RawBranch,
                .branch.hint = HExpression,
                .branch.nodes = and_nodes,
            };
        }
    }

    // _nodes = mk_rawtree_array(4, &a); // (if condition (go-to loop-body ...) (go-to exit))

    RawTreeArray loop_body_arg_nodes = mk_rawtree_array(loop_fors.len, &a);
    
    for (size_t i = 0; i < loop_fors.len; i++) {
        ForRange* fr = loop_fors.data[i];
        push_rawtree(fr->from, &start_go_to_nodes);

        push_rawtree(fr->name, &loop_body_arg_nodes);

        // Increment or decrement appropriately
        // TODO: replace with +/- (using the num trait) rather than u64.+/u64.-
        RawTreeArray func_nodes = mk_rawtree_array(3, &a);
        push_rawtree(atom_symbol("."), &func_nodes);
        push_rawtree(atom_symbol(((fr->type == UpTo) | (fr->type == Below)) ? "+" : "-"), &func_nodes);
        push_rawtree(atom_symbol("u64"), &func_nodes);
        RawTree func_term = (RawTree) {
            .type = RawBranch,
            .branch.hint = HExpression,
            .branch.nodes = func_nodes,
        };

        RawTreeArray call_nodes = mk_rawtree_array(4, &a);
        push_rawtree(func_term, &call_nodes);
        push_rawtree(fr->name, &call_nodes);

        RawTree one_atom = (RawTree) {
            .type = RawAtom,
            .atom.type = AIntegral,
            .atom.int_64 = 1,
        };
        push_rawtree(one_atom, &call_nodes);

        RawTree next_val = (RawTree) {
            .type = RawBranch,
            .branch.hint = HExpression,
            .branch.nodes = call_nodes,
        };
        push_rawtree(next_val, &continue_go_to_nodes);
    }

    RawTree loop_body_args = (RawTree) {
        .type = RawBranch,
        .branch.hint = HSpecial,
        .branch.nodes = loop_body_arg_nodes,
    };

    RawTreeArray loop_body_label_nodes = mk_rawtree_array(3, &a);
    push_rawtree(atom_symbol("loop-continue"), &loop_body_label_nodes);
    push_rawtree(loop_body_args, &loop_body_label_nodes);

    RawTreeArray unit_nodes = mk_rawtree_array(2, &a);
    push_rawtree(atom_symbol(":"), &unit_nodes);
    push_rawtree(atom_symbol("unit"), &unit_nodes);
    RawTree unit_term = (RawTree) {
        .type = RawBranch,
        .branch.hint = HExpression,
        .branch.nodes = unit_nodes,
    };

    RawTreeArray labels_nodes = mk_rawtree_array(4, &a);
    RawTree labels_atom = (RawTree) {
        .type = RawAtom,
        .atom.type = ASymbol,
        .atom.symbol = string_to_symbol(mv_string("labels")),
    };
    push_rawtree(labels_atom, &labels_nodes);

    RawTree start_go_to = (RawTree) {
        .type = RawBranch,
        .branch.hint = HExpression,
        .branch.nodes = start_go_to_nodes,
    };
    push_rawtree(start_go_to, &labels_nodes);

    // TODO: Add if (condition) (go-to continue ...) (go-to exit ...) 
    RawTreeArray if_nodes = mk_rawtree_array(4, &a);
    push_rawtree(atom_symbol("if"), &if_nodes);
    push_rawtree(loop_condition, &if_nodes);
    push_rawtree(unit_term, &if_nodes);

    RawTreeArray goto_exit_nodes = mk_rawtree_array(2, &a);
    push_rawtree(atom_symbol("go-to"), &goto_exit_nodes);
    push_rawtree(atom_symbol("exit"), &goto_exit_nodes);
    RawTree goto_exit = (RawTree) {
        .type = RawBranch,
        .branch.hint = HExpression,
        .branch.nodes = goto_exit_nodes,
    };
    push_rawtree(goto_exit, &if_nodes);

    RawTree if_expr = (RawTree) {
        .type = RawBranch,
        .branch.hint = HExpression,
        .branch.nodes = if_nodes,
    };
    // This index was reserved earlier
    loop_body_nodes.data[1] = if_expr;

    RawTree goto_continue = (RawTree) {
        .type = RawBranch,
        .branch.hint = HExpression,
        .branch.nodes = continue_go_to_nodes,
    };
    push_rawtree(goto_continue, &loop_body_nodes);

    RawTree loop_body = (RawTree) {
        .type = RawBranch,
        .branch.hint = HExpression,
        .branch.nodes = loop_body_nodes,
    };
    push_rawtree(loop_body, &loop_body_label_nodes);
    RawTree loop_body_label = (RawTree) {
        .type = RawBranch,
        .branch.hint = HSpecial,
        .branch.nodes = loop_body_label_nodes,
    };
    push_rawtree(loop_body_label, &labels_nodes);

    RawTreeArray loop_exit_nodes = mk_rawtree_array(2, &a);

    push_rawtree(atom_symbol("exit"), &loop_exit_nodes);
    push_rawtree(unit_term, &loop_exit_nodes);
    RawTree loop_exit = (RawTree) {
        .type = RawBranch,
        .branch.hint = HSpecial,
        .branch.nodes = loop_exit_nodes,
    };
    push_rawtree(loop_exit, &labels_nodes);

    return (MacroResult) {
        .result_type = Right,
        .term.type = RawBranch,
        .term.branch.hint = HExpression,
        .term.branch.nodes = labels_nodes,
    };
    // Now, construct the loop expression
    // each for-variable must
    // (labels (go-to loop-body initial-val-1 initial-val-2 ...)
    //   [loop-body [var-1 var-2 ...]
    //     (seq loop-expr-1 loop-expr-2 ...
    //       (if (num.bool.and loop-cont (num.bool.and loop-expr-2 ...)))
    //       )
    //    [exit :unit]])
    
}

void build_loop_macro(PiType* type, Assembler* ass, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(a, 1, "nodes", mk_array_ctype(a), mk_macro_result_ctype(a));

    convert_c_fn(loop_macro, &fn_ctype, type, ass, a, point); 
}

void add_extra_module(Assembler* ass, Package* base, Allocator* default_allocator, Allocator* a) {
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(0, a),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, a),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("extra")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, base, NULL, a);
    delete_module_header(header);

    PiType* typep;
    Symbol sym;
    ErrorPoint point;
    if (catch_error(point)) {
        panic(point.error_message);
    }

    Segments null_segments = (Segments) {
        .code = mk_u8_array(0, a),
        .data = mk_u8_array(0, a),
    };

    Allocator arena = mk_arena_allocator(4096, a);
    a = &arena;
    
    // uint64_t dyn_curr_package = mk_dynamic_var(sizeof(void*), &base); 
    std_perm_allocator = mk_dynamic_var(sizeof(Allocator), default_allocator); 
    typep = mk_dynamic_type(a, mk_struct_type(a, 4,
                                             "malloc", mk_prim_type(a, Address),
                                             "realloc", mk_prim_type(a, Address),
                                             "free", mk_prim_type(a, Address),
                                             "ctx", mk_prim_type(a, Address)));
    sym = string_to_symbol(mv_string("perm-allocator"));
    add_def(module, sym, *typep, &std_perm_allocator, null_segments, NULL);

    std_current_allocator = mk_dynamic_var(sizeof(Allocator), default_allocator); 
    sym = string_to_symbol(mv_string("current-allocator"));
    add_def(module, sym, *typep, &std_current_allocator, null_segments, NULL);

    void* nul = NULL;
    std_temp_allocator = mk_dynamic_var(sizeof(void*), &nul); 
    std_current_module = mk_dynamic_var(sizeof(Module*), &nul); 

    typep = mk_dynamic_type(a, mk_prim_type(a, Address));
    sym = string_to_symbol(mv_string("temp-allocator"));
    add_def(module, sym, *typep, &std_temp_allocator, null_segments, NULL);
    clear_assembler(ass);

    // C Wrappers!
    Segments fn_segments = {.data = mk_u8_array(0, a),};
    Segments prepped;

    // exit : Proc [] Unit
    typep = mk_proc_type(a, 0, mk_prim_type(a, Unit));
    build_exit_fn(ass, a, &point);
    sym = string_to_symbol(mv_string("exit"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    // panic : All [A] Proc [String] A
    typep = build_panic_fn_ty(a);
    build_panic_fn(ass, a, &point);
    sym = string_to_symbol(mv_string("panic"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    // malloc : Proc [U64] Address
    typep = mk_proc_type(a, 1, mk_prim_type(a, UInt_64), mk_prim_type(a, Address));
    build_malloc_fn(ass, a, &point);
    sym = string_to_symbol(mv_string("malloc"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    // realloc : Proc (Address U64) Address
    typep = mk_proc_type(a, 2, mk_prim_type(a, Address),
                        mk_prim_type(a, UInt_64),
                        mk_prim_type(a, Address));
    build_realloc_fn(ass, a, &point);
    sym = string_to_symbol(mv_string("realloc"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    // realloc : Proc [String] Unit
    typep = mk_proc_type(a, 1, mk_prim_type(a, Address), mk_prim_type(a, Unit));
    build_free_fn(ass, a, &point);
    sym = string_to_symbol(mv_string("free"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    // print : Proc [String] Unit
    typep = mk_proc_type(a, 1, mk_string_type(a), mk_prim_type(a, Unit));
    build_print_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("print"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    build_println_fn(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("print-ln"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    // load-module : Proc [String] Unit
    typep = mk_proc_type(a, 1, mk_string_type(a), mk_prim_type(a, Unit));
    build_load_module_fun(ass, a, &point);
    sym = string_to_symbol(mv_string("load-module"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    // run-script : Proc [String] Unit
    typep = mk_proc_type(a, 1, mk_string_type(a), mk_enum_type(a, 2, "Ok", 0, "Err", 1, mk_string_type(a)));
    build_run_script_fun(typep, ass, a, &point);
    sym = string_to_symbol(mv_string("run-script"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    PiType* syntax_array = mk_app_type(a, get_array_type(), get_syntax_type());
    PiType* macro_proc = mk_proc_type(a, 1, syntax_array, get_macro_result_type());

    // loop : Macro ≃ Proc [(Array Syntax)] Syntax
    typep = mk_prim_type(a, TMacro);
    build_loop_macro(macro_proc, ass, a, &point);
    sym = string_to_symbol(mv_string("loop"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    add_module(string_to_symbol(mv_string("extra")), module, base);

    sdelete_u8_array(null_segments.code);
    sdelete_u8_array(null_segments.data);
    sdelete_u8_array(fn_segments.data);
    release_arena_allocator(arena);
}
