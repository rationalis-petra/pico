#include <stdio.h>
#include <stdlib.h>

#include "platform/machine_info.h"
#include "platform/signals.h"
#include "platform/signals.h"

#include "pico/data/client/allocator.h"
#include "pico/stdlib/core.h"
#include "pico/stdlib/extra.h"
#include "pico/stdlib/platform/submodules.h"
#include "pico/stdlib/meta/meta.h"
#include "pico/syntax/concrete.h"
#include "pico/analysis/abstraction.h"
#include "pico/codegen/codegen.h"
#include "pico/codegen/backend-direct/internal.h"


static jump_buf* m_buf;
void set_exit_callback(jump_buf* buf) { m_buf = buf; }


void build_panic_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    // Pop return address (we don't need it)
    build_binary_op(Add, reg(RSP, sz_64), imm8(8), ass, a, point);

    // panic : All [A] Proc (String) A
#if ABI == SYSTEM_V_64
    // panic (str.memsize = rdi, str.bytes = rsi)
    // 
    // copy address into RDI
    build_unary_op(Pop, reg(RDI, sz_64), ass, a, point);
    build_unary_op(Pop, reg(RSI, sz_64), ass, a, point);

#elif ABI == WIN_64
    // panic (&str = rcx)
    // copy address into RCX
    build_binary_op(Mov, reg(RCX, sz_64), reg(RSP, sz_64), ass, a, point);
#endif
    generate_c_call(panic, ass, a, point);
}

PiType* build_panic_fn_ty(PiAllocator* pia) {
    PiType* proc_ty = mk_proc_type(pia, 1, mk_string_type(pia), mk_var_type(pia, "A"));

    SymbolPiList types = mk_sym_list(1, pia);
    push_sym(string_to_symbol(mv_string("A")), &types);

    PiType* out_ty = call_alloc(sizeof(PiType), pia);
    *out_ty =  (PiType) {.sort = TAll, .binder.vars = types, .binder.body = proc_ty};
    return out_ty;
}

void exit_callback() {
    long_jump(*m_buf, 1);
}

void build_exit_fn(Assembler* ass, Allocator* a, ErrorPoint* point) {
    generate_c_call(exit_callback, ass, a, point);
}

RawTree atom_symbol(const char *str) {
    return (RawTree) {
        .type = RawAtom,
        .atom.type = ASymbol,
        .atom.symbol = string_to_symbol(mv_string(str)),
    };
}

typedef enum {UpTo, Below, Above, DownTo, Then} RangeType; 

typedef struct {
    RangeType type;
    RawTree name;
    RawTree from;
    RawTree to;
} ForRange;

bool mk_condition(ForRange range, RawTree* out, PiAllocator *pia) {
    const char* comparator = NULL;
    if (range.type != Then) {
        switch (range.type) {
        case UpTo: comparator = "<="; break;
        case Below: comparator = "<"; break;
        case Above: comparator = ">"; break;
        case DownTo: comparator = ">="; break;
        default: panic(mv_string("unrecognised comparator"));
        }
    
        RawTreePiList proj_nodes = mk_rawtree_list(3, pia);
        push_rawtree(atom_symbol("."), &proj_nodes);
        push_rawtree(atom_symbol(comparator), &proj_nodes);
        push_rawtree(atom_symbol("u64"), &proj_nodes);

        RawTree proj = (RawTree) {
            .type = RawBranch,
            .branch.hint = HExpression,
            .branch.nodes = proj_nodes,
        };

        RawTreePiList comp_nodes = mk_rawtree_list(3, pia);
        push_rawtree(proj, &comp_nodes);
        push_rawtree(range.name, &comp_nodes);
        push_rawtree(range.to, &comp_nodes);

        *out = (RawTree) {
            .type = RawBranch,
            .branch.hint = HExpression,
            .branch.nodes = comp_nodes,
        };
        return true;
    }
    return false;
}

MacroResult loop_macro(RawTreePiList nodes) {
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
    PiAllocator pico_allocator = get_std_current_allocator();
    PiAllocator* pia = &pico_allocator;

    RawTreePiList loop_body_nodes = mk_rawtree_list(nodes.len + 1, pia);
    push_rawtree(atom_symbol("seq"), &loop_body_nodes);

    // We push a 'null' rawtree because this will later be assigned to the
    // loop-check 
    push_rawtree((RawTree){}, &loop_body_nodes);
    AddrPiList loop_fors = mk_addr_list(2, pia);
    AddrPiList loop_whiles = mk_addr_list(2, pia);
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

                if (eq_symbol(&branch.branch.nodes.data[2], string_to_symbol(mv_string("=")))) {
                    range.from = branch.branch.nodes.data[3];

                    if (!eq_symbol(&branch.branch.nodes.data[4], string_to_symbol(mv_string("then")))) {
                        return (MacroResult) {
                            .result_type = Left,
                            .err.message = mv_string("for x = loop clause expects 'then' as next token"),
                            .err.range = branch.branch.nodes.data[4].range,
                        };
                    } 
                    range.type = Then;

                    range.to = branch.branch.nodes.data[5];

                    ForRange* rp = call_alloc(sizeof(ForRange), pia);
                    *rp = range;
                    push_addr(rp, &loop_fors);
                } else if (eq_symbol(&branch.branch.nodes.data[2], string_to_symbol(mv_string("from")))) {
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

                    ForRange* rp = call_alloc(sizeof(ForRange), pia);
                    *rp = range;
                    push_addr(rp, &loop_fors);
                } else {
                    return (MacroResult) {
                        .result_type = Left,
                        .err.message = mv_string("For loop expects 'from'"),
                        .err.range = branch.branch.nodes.data[2].range,
                    };
                }
            } else if (eq_symbol(&branch.branch.nodes.data[0], string_to_symbol(mv_string("while")))) {
                RawTree *raw_term = (branch.branch.nodes.len == 2)
                    ? &branch.branch.nodes.data[1]
                    : raw_slice(&branch, 1, pia);
                push_addr(raw_term, &loop_whiles);
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

    RawTreePiList start_go_to_nodes = mk_rawtree_list(2 + loop_fors.len, pia);
    push_rawtree(atom_symbol("go-to"), &start_go_to_nodes);
    push_rawtree(atom_symbol("loop-continue"), &start_go_to_nodes);

    RawTreePiList continue_go_to_nodes = mk_rawtree_list(2 + loop_fors.len, pia);
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

    bool cond_initialized = false;
    if (loop_fors.len > 0) {
        for (size_t i = 0; i < loop_fors.len; i++) {
            RawTree new_condition;
            if (mk_condition(*(ForRange*)loop_fors.data[i], &new_condition, pia)) {
                if (!cond_initialized) {
                    cond_initialized = true;
                    loop_condition = new_condition;
                    continue;
                }
            }
            RawTreePiList and_fn_nodes = mk_rawtree_list(3, pia);
            push_rawtree(atom_symbol("."), &and_fn_nodes);
            push_rawtree(atom_symbol("and"), &and_fn_nodes);
            push_rawtree(atom_symbol("bool"),&and_fn_nodes);
            RawTree and = (RawTree) {
                .type = RawBranch,
                .branch.hint = HExpression,
                .branch.nodes = and_fn_nodes,
            };

            RawTreePiList and_nodes = mk_rawtree_list(3, pia);
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
        if (!cond_initialized) {
            loop_condition = *(RawTree*)loop_whiles.data[0];
            start_idx++;
        }
        for (size_t i = start_idx; i < loop_whiles.len; i++) {
            RawTree new_condition = *(RawTree*)loop_whiles.data[i];
            RawTreePiList and_fn_nodes = mk_rawtree_list(3, pia);
            push_rawtree(atom_symbol("."), &and_fn_nodes);
            push_rawtree(atom_symbol("and"), &and_fn_nodes);
            push_rawtree(atom_symbol("bool"), &and_fn_nodes);
            RawTree and = (RawTree) {
                .type = RawBranch,
                .branch.hint = HExpression,
                .branch.nodes = and_fn_nodes,
            };

            RawTreePiList and_nodes = mk_rawtree_list(3, pia);

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

    RawTreePiList loop_body_arg_nodes = mk_rawtree_list(loop_fors.len, pia);
    
    for (size_t i = 0; i < loop_fors.len; i++) {
        ForRange* fr = loop_fors.data[i];
        push_rawtree(fr->from, &start_go_to_nodes);

        push_rawtree(fr->name, &loop_body_arg_nodes);
        if (fr->type == Then) {
            push_rawtree(fr->to, &continue_go_to_nodes);
        } else {

            // Increment or decrement appropriately
            // TODO: replace with +/- (using the num trait) rather than u64.+/u64.-
            RawTreePiList func_nodes = mk_rawtree_list(3, pia);
            push_rawtree(atom_symbol("."), &func_nodes);
            push_rawtree(atom_symbol(((fr->type == UpTo) | (fr->type == Below)) ? "+" : "-"), &func_nodes);
            push_rawtree(atom_symbol("u64"), &func_nodes);
            RawTree func_term = (RawTree) {
                .type = RawBranch,
                .branch.hint = HExpression,
                .branch.nodes = func_nodes,
            };

            RawTreePiList call_nodes = mk_rawtree_list(4, pia);
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
    }

    RawTree loop_body_args = (RawTree) {
        .type = RawBranch,
        .branch.hint = HSpecial,
        .branch.nodes = loop_body_arg_nodes,
    };

    RawTreePiList loop_body_label_nodes = mk_rawtree_list(3, pia);
    push_rawtree(atom_symbol("loop-continue"), &loop_body_label_nodes);
    push_rawtree(loop_body_args, &loop_body_label_nodes);

    RawTreePiList unit_nodes = mk_rawtree_list(2, pia);
    push_rawtree(atom_symbol(":"), &unit_nodes);
    push_rawtree(atom_symbol("unit"), &unit_nodes);
    RawTree unit_term = (RawTree) {
        .type = RawBranch,
        .branch.hint = HExpression,
        .branch.nodes = unit_nodes,
    };

    RawTreePiList labels_nodes = mk_rawtree_list(4, pia);
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
    RawTreePiList if_nodes = mk_rawtree_list(4, pia);
    push_rawtree(atom_symbol("if"), &if_nodes);
    push_rawtree(loop_condition, &if_nodes);
    push_rawtree(unit_term, &if_nodes);

    RawTreePiList goto_exit_nodes = mk_rawtree_list(2, pia);
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

    RawTreePiList loop_exit_nodes = mk_rawtree_list(2, pia);

    push_rawtree(atom_symbol("exit"), &loop_exit_nodes);
    push_rawtree(unit_term, &loop_exit_nodes);
    RawTree loop_exit = (RawTree) {
        .type = RawBranch,
        .branch.hint = HSpecial,
        .branch.nodes = loop_exit_nodes,
    };
    push_rawtree(loop_exit, &labels_nodes);

    // Now, construct the loop expression
    // each for-variable must
    // (labels (go-to loop-body initial-val-1 initial-val-2 ...)
    //   [loop-body [var-1 var-2 ...]
    //     (seq loop-expr-1 loop-expr-2 ...
    //       (if (num.bool.and loop-cont (num.bool.and loop-expr-2 ...)))
    //       )
    //    [exit :unit]])
    
    return (MacroResult) {
        .result_type = Right,
        .term.type = RawBranch,
        .term.branch.hint = HExpression,
        .term.branch.nodes = labels_nodes,
    };
}

void build_loop_macro(PiType* type, Assembler* ass, PiAllocator* pia, Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "nodes", mk_list_ctype(pia), mk_macro_result_ctype(pia));

    convert_c_fn(loop_macro, &fn_ctype, type, ass, a, point); 
}

MacroResult ann_macro(RawTreePiList nodes) {
    if (nodes.len < 3) {
        return (MacroResult) {
            .result_type = Left,
            .err.message = mv_string("Malformed annotation (ann): expected at least three terms"),
            .err.range = nodes.data[0].range,
        };
    }

    PiAllocator pia = get_std_current_allocator();
    RawTreePiList decl_nodes = mk_rawtree_list(3, &pia);
    push_rawtree((RawTree) {
            .type = RawAtom,
            .atom.type = ASymbol,
            .atom.symbol = string_to_symbol(mv_string("declare")),
        }, &decl_nodes);

    push_rawtree(nodes.data[1], &decl_nodes);

    RawTreePiList field_nodes = mk_rawtree_list(2, &pia);
    RawTreePiList proj_nodes = mk_rawtree_list(2, &pia);

    // push '.type'
    push_rawtree((RawTree) {
            .type = RawAtom,
            .atom.type = ASymbol,
            .atom.symbol = string_to_symbol(mv_string(".")),
        }, &proj_nodes);
    push_rawtree((RawTree) {
            .type = RawAtom,
            .atom.type = ASymbol,
            .atom.symbol = string_to_symbol(mv_string("type")),
        }, &proj_nodes);

    // push '.type'
    push_rawtree((RawTree) {
            .type = RawBranch,
            .branch.hint = HExpression,
            .branch.nodes = proj_nodes,
        }, &field_nodes);

    // push <type>
    RawTree *tree;
    if (nodes.len == 3) {
        tree = &nodes.data[2];
    } else {
        const size_t drop = 2;
        tree = call_alloc(sizeof(RawTree), &pia);
        *tree = (RawTree) {
            .type = RawBranch,
            .range.start = nodes.data[drop].range.start,
            .range.end = nodes.data[nodes.len - 1].range.end,
            .branch.hint = HExpression,
            .branch.nodes.len = nodes.len - drop,
            .branch.nodes.size = nodes.size - drop,
            .branch.nodes.data = nodes.data + drop,
            .branch.nodes.gpa = nodes.gpa,
        };
    }
    push_rawtree(*tree, &field_nodes);

    // push [.type <type>]
    push_rawtree((RawTree) {
            .type = RawBranch,
            .branch.hint = HSpecial,
            .branch.nodes = field_nodes,
        }, &decl_nodes);

    return (MacroResult) {
        .result_type = Right,
        .term.type = RawBranch,
        .term.branch.hint = HExpression,
        .term.branch.nodes = decl_nodes,
    };
}

void build_ann_macro(PiType* type, Assembler* ass, PiAllocator* pia,  Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "nodes", mk_list_ctype(pia), mk_macro_result_ctype(pia));

    convert_c_fn(ann_macro, &fn_ctype, type, ass, a, point); 
}

MacroResult when_macro(RawTreePiList nodes) {
    if (nodes.len < 3) {
        return (MacroResult) {
            .result_type = Left,
            .err.message = mv_string("Malformed when: expected at least two terms: the condition and body."),
            .err.range = nodes.data[0].range,
        };
    }

    PiAllocator pia = get_std_current_allocator();
    RawTreePiList if_nodes = mk_rawtree_list(3, &pia);
    push_rawtree((RawTree) {
            .type = RawAtom,
            .atom.type = ASymbol,
            .atom.symbol = string_to_symbol(mv_string("if")),
        }, &if_nodes);

    push_rawtree(nodes.data[1], &if_nodes);

    RawTreePiList unit_nodes = mk_rawtree_list(2, &pia);

    // push <type>
    RawTree body;
    if (nodes.len == 3) {
        body = nodes.data[2];
    } else {
        const size_t drop = 2;
        body = (RawTree) {
            .type = RawBranch,
            .range.start = nodes.data[drop].range.start,
            .range.end = nodes.data[nodes.len - 1].range.end,
            .branch.hint = HExpression,
            .branch.nodes.len = nodes.len - drop,
            .branch.nodes.size = nodes.size - drop,
            .branch.nodes.data = nodes.data + drop,
            .branch.nodes.gpa = nodes.gpa,
        };
    }
    push_rawtree(body, &if_nodes);

    // push ':unit'
    push_rawtree((RawTree) {
            .type = RawAtom,
            .atom.type = ASymbol,
            .atom.symbol = string_to_symbol(mv_string(":")),
        }, &unit_nodes);
    push_rawtree((RawTree) {
            .type = RawAtom,
            .atom.type = ASymbol,
            .atom.symbol = string_to_symbol(mv_string("unit")),
        }, &unit_nodes);
    push_rawtree((RawTree) {
            .type = RawBranch,
            .branch.hint = HExpression,
            .branch.nodes = unit_nodes,
        }, &if_nodes);

    return (MacroResult) {
        .result_type = Right,
        .term.type = RawBranch,
        .term.branch.hint = HExpression,
        .term.branch.nodes = if_nodes,
    };
}

void build_when_macro(PiType* type, Assembler* ass, PiAllocator* pia,  Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "nodes", mk_list_ctype(pia), mk_macro_result_ctype(pia));

    convert_c_fn(when_macro, &fn_ctype, type, ass, a, point); 
}

MacroResult thread_macro(RawTreePiList nodes) {
    if (nodes.len < 2) {
        return (MacroResult) {
            .result_type = Left,
            .err.message = mv_string("Malformed threading macro (->): expected at least one term!"),
            .err.range = nodes.data[0].range,
        };
    }

    PiAllocator pia = get_std_current_allocator();
    RawTree current_node = nodes.data[1];
    for (size_t i = 2; i < nodes.len; i++) {
        RawTree current = nodes.data[i];
        if (current.type != RawBranch) {
            return (MacroResult) {
                .result_type = Left,
                .err.message = mv_string("All terms in a thread after the first should be a composite term."),
                .err.range = nodes.data[0].range,
            };
        }
        if (current.branch.nodes.len < 1) {
            return (MacroResult) {
                .result_type = Left,
                .err.message = mv_string("All terms in a thread after the first should be a composite term with at least one component."),
                .err.range = nodes.data[0].range,
            };
        }
        RawTreePiList new_nodes = mk_rawtree_list(current.branch.nodes.len + 1, &pia);
        push_rawtree(current.branch.nodes.data[0], &new_nodes);
        push_rawtree(current_node, &new_nodes);
        for (size_t j = 1; j < current.branch.nodes.len; j++) {
            push_rawtree(current.branch.nodes.data[j], &new_nodes);
        }
        current_node = (RawTree) {
            .type = RawBranch,
            .range.start = current.range.start,
            .range.end = current.range.end,
            .branch.hint = current.branch.hint,
            .branch.nodes = new_nodes,
        };
    }

    return (MacroResult) {
        .result_type = Right,
        .term = current_node,
    };
}

void build_thread_macro(PiType* type, Assembler* ass, PiAllocator* pia,  Allocator* a, ErrorPoint* point) {
    CType fn_ctype = mk_fn_ctype(pia, 1, "nodes", mk_list_ctype(pia), mk_macro_result_ctype(pia));

    convert_c_fn(thread_macro, &fn_ctype, type, ass, a, point); 
}

void add_extra_module(Assembler* ass, Package* base, RegionAllocator* region) {
    Allocator ra = ra_to_gpa(region);
    Imports imports = (Imports) {
        .clauses = mk_import_clause_array(0, &ra),
    };
    Exports exports = (Exports) {
        .export_all = true,
        .clauses = mk_export_clause_array(0, &ra),
    };
    ModuleHeader header = (ModuleHeader) {
        .name = string_to_symbol(mv_string("extra")),
        .imports = imports,
        .exports = exports,
    };
    Module* module = mk_module(header, base, NULL);

    PiType* typep;
    Symbol sym;
    ErrorPoint point;
    if (catch_error(point)) {
        panic(point.error_message);
    }

    PiAllocator pico_region = convert_to_pallocator(&ra);
    PiAllocator* pia = &pico_region;
    
    // C Wrappers!
    Segments fn_segments = {.data = mk_u8_array(0, &ra),};
    Segments prepped;

    // exit : Proc [] Unit
    typep = mk_proc_type(pia, 0, mk_prim_type(pia, Unit));
    build_exit_fn(ass, &ra, &point);
    sym = string_to_symbol(mv_string("exit"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    // panic : All [A] Proc [String] A
    typep = build_panic_fn_ty(pia);
    build_panic_fn(ass, &ra, &point);
    sym = string_to_symbol(mv_string("panic"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    PiType* syntax_array = mk_app_type(pia, get_list_type(), get_syntax_type());
    PiType* macro_proc = mk_proc_type(pia, 1, syntax_array, get_macro_result_type());

    // loop : Macro ≃ Proc [(Array Syntax)] Syntax
    typep = mk_prim_type(pia, TMacro);
    build_loop_macro(macro_proc, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("loop"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_prim_type(pia, TMacro);
    build_ann_macro(macro_proc, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("ann"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_prim_type(pia, TMacro);
    build_when_macro(macro_proc, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("when"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    typep = mk_prim_type(pia, TMacro);
    build_thread_macro(macro_proc, ass, pia, &ra, &point);
    sym = string_to_symbol(mv_string("->"));
    fn_segments.code = get_instructions(ass);
    prepped = prep_target(module, fn_segments, ass, NULL);
    add_def(module, sym, *typep, &prepped.code.data, prepped, NULL);
    clear_assembler(ass);

    Result r =add_module(string_to_symbol(mv_string("extra")), module, base);
    if (r.type == Err) panic(r.error_message);
}
