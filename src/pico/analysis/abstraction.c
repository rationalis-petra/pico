#include "pico/analysis/abstraction.h"

#include "data/result.h"
#include "data/string.h"
#include "pico/values/values.h"
#include "pico/syntax/concrete.h"
#include "pico/syntax/syntax.h"
#include "pico/binding/shadow_env.h"

// Internal functions 
AbsExprResult abstract_expr_i(RawTree raw, ShadowEnv* env, Allocator* a);

bool is_symbol(RawTree* raw) {
    return (raw->type == RawAtom && raw->atom.type == ASymbol);
}

bool get_fieldname(RawTree* raw, Symbol* fieldname) {
    if (raw->type == RawList && raw->nodes.len == 2) {
        raw = raw->nodes.data[1];
        if (is_symbol(raw)) {
            *fieldname = raw->atom.symbol;
            return true;
        } else {
            return false;
        }
    }
    else {
        return false;
    }
}

bool get_symbol_list(SymbolArray* arr, RawTree nodes) {
    if (nodes.type == RawAtom) { return false; }

    for (size_t i = 0; i < nodes.nodes.len; i++) {
        RawTree* node = nodes.nodes.data[i];
        if (node->type != RawAtom) { return false; }
        if (node->atom.type != ASymbol) { return false;  }
        push_u64(node->atom.symbol, arr);
    }
    return true;
}

AbsResult to_toplevel(AbsExprResult res) {
    AbsResult out;
    switch (res.type) {
    case Ok: {
        out.type = Ok;
        out.out.type = TLExpr;
        out.out.expr = res.out;
        break;
    }
    case Err:
        out.type = Err;
        out.error_message = res.error_message;
    }
    return out;
}

AbsExprResult mk_application(RawTree raw, ShadowEnv* env, Allocator* a) {
    AbsExprResult fn_res = abstract_expr_i(*(RawTree*)(aref_ptr(0, raw.nodes)), env, a);
    if (fn_res.type == Err) {
        return fn_res;
    }

    AbsExprResult res;
    if (fn_res.out.type == SConstructor) {
        res = (AbsExprResult) {
            .type = Ok,
            .out.type = SVariant,
            .out.variant.enum_type = fn_res.out.constructor.enum_type,
            .out.variant.tagname = fn_res.out.constructor.tagname,
            .out.variant.args = mk_ptr_array(raw.nodes.len - 1, a),
        };

        for (size_t i = 1; i < raw.nodes.len; i++) {
            AbsExprResult arg_res = abstract_expr_i(*(RawTree*)(aref_ptr(i, raw.nodes)), env, a);
            if (arg_res.type == Ok) {
                Syntax* arg = mem_alloc(sizeof(Syntax), a);
                *arg = arg_res.out;
                push_ptr(arg, &res.out.variant.args);
            }
            else {
                delete_syntax(res.out, a);
                res = arg_res;
                break;
            }
        }
    } else {
        res = (AbsExprResult) {
            .type = Ok,
            .out.type = SApplication,
            .out.application.function = mem_alloc(sizeof(Syntax), a),
            .out.application.args = mk_ptr_array(raw.nodes.len - 1, a),
        };
        *res.out.application.function = fn_res.out;

        for (size_t i = 1; i < raw.nodes.len; i++) {
            AbsExprResult arg_res = abstract_expr_i(*(RawTree*)(aref_ptr(i, raw.nodes)), env, a);
            if (arg_res.type == Ok) {
                Syntax* arg = mem_alloc(sizeof(Syntax), a);
                *arg = arg_res.out;
                push_ptr(arg, &res.out.application.args);
            }
            else {
                delete_syntax(res.out, a);
                res = arg_res;
                break;
            }
        }
    }

    return res;
}

AbsExprResult mk_term(TermFormer former, RawTree raw, ShadowEnv* env, Allocator* a) {
    AbsExprResult res;
    switch (former) {
    case FProcedure: {
        if (raw.nodes.len < 3) {
            res.type = Err;
            res.error_message = mk_string("Procedure term former requires at least 2 arguments!", a);
            return res;
        }

        SymbolArray arguments = mk_u64_array(2, a);
        if (!get_symbol_list(&arguments, *(RawTree*)raw.nodes.data[1])) {
            res.type = Err;
            res.error_message = mk_string("Procedure term former requires first arguments to be a symbol-list!", a);
            return res;
        }

        shadow_vars(arguments, env);

        RawTree* raw_term;
        if (raw.nodes.len == 3) {
            raw_term = (RawTree*)aref_ptr(2, raw.nodes);
        } else {
            raw_term = mem_alloc (sizeof(RawTree), a);

            raw_term->nodes.len = raw.nodes.len - 2;
            raw_term->nodes.size = raw.nodes.size - 2;
            raw_term->nodes.data = raw.nodes.data + 2;
            raw_term->type = RawList;
        }
        AbsExprResult rbody = abstract_expr_i(*raw_term, env, a);


        if (rbody.type == Err) {
            return rbody;
        }
        else 
            res.type = Ok;
        res.out.type = SProcedure;
        res.out.procedure.args = arguments;
        res.out.procedure.body = mem_alloc(sizeof(Syntax), a);
        *res.out.procedure.body = rbody.out;
        break;
    }
    case FApplication: {
        res = mk_application(raw, env, a);
        break;
    }
    case FVariant: {
        if (raw.nodes.len == 2) {
            // Check that we are indeed getting a result
            // Get the tag name of the variant
            RawTree* msym = (RawTree*)raw.nodes.data[1];
            if (msym->type != RawAtom && msym->atom.type != ASymbol) {
                res = (AbsExprResult) {
                    .type = Err,
                    .error_message = mv_string("Argument to variant term former should be symbol"),
                };
                return res;
            }
            Symbol lit = msym->atom.symbol;

            if (lit == string_to_symbol(mv_string("true"))) {
                return (AbsExprResult) {
                    .type = Ok,
                    .out.type = SLitBool,
                    .out.lit_i64 = true,
                };
            }
            else if (lit == string_to_symbol(mv_string("false"))) {
                return (AbsExprResult) {
                    .type = Ok,
                    .out.type = SLitBool,
                    .out.lit_i64 = false,
                };
            }

            return (AbsExprResult) {
                .type = Err,
                .error_message = mv_string("Variant term former needs two arguments!"),
            };
        }
        // : sym Type

        else if (raw.nodes.len != 3) {
            return (AbsExprResult) {
                .type = Err,
                .error_message = mv_string("Variant term former needs two arguments!"),
            };
        }

        // Get the Type portion of the projector 
        res = abstract_expr_i(*(RawTree*)raw.nodes.data[2], env, a);
        if (res.type == Err) return res;

        Syntax* var_type = mem_alloc(sizeof(Syntax), a);
        *var_type = res.out;
        
        // Check that we are indeed getting a result
        // Get the tag gname of the variant
        RawTree* msym = (RawTree*)raw.nodes.data[1];
        if (msym->type != RawAtom && msym->atom.type != ASymbol) {
            res = (AbsExprResult) {
                .type = Err,
                .error_message = mv_string("Second argument to projection term former should be symbol"),
            };
            return res;
        }

        res.type = Ok;
        res.out = (Syntax) {
            .type = SConstructor,
            .constructor.enum_type = var_type,
            .constructor.tagname = msym->atom.symbol,
        };
        break;
    }
    case FMatch: {
        res = abstract_expr_i(*(RawTree*)raw.nodes.data[1], env, a);
        if (res.type == Err) return res;
        Syntax* sval = mem_alloc(sizeof(Syntax), a);
        *sval = res.out;

        ClauseArray clauses = mk_ptr_array(raw.nodes.len - 2, a);
        
        for (size_t i = 2; i < raw.nodes.len; i++) {
            // For each clause, we need three things:
            // 1. The tag, corresponding to the enum
            // 2. The variable(s) destructuring the enum
            // 3. The body
            // Get the clause
            RawTree* raw_clause = (RawTree*)raw.nodes.data[i];
            if (raw_clause->type != RawList || raw_clause->nodes.len < 2) {
                return (AbsExprResult) {
                    .type = Err,
                    .error_message = mv_string("Match Clause has incorrect number of elements!"),
                };
            }

            // Get the pattern
            RawTree* raw_pattern = (RawTree*)raw_clause->nodes.data[0];
            if (raw_pattern->type != RawList) {
                return (AbsExprResult) {
                    .type = Err,
                    .error_message = mv_string("Match Pattern should be a list!"),
                };
            }

            // The pattern has two parts: the variables & the tag
            // The tag should be in a constructor (i.e. list)
            Symbol clause_tagname; 
            if (!get_fieldname(raw_pattern->nodes.data[0], &clause_tagname)) {
                return (AbsExprResult) {
                    .type = Err,
                    .error_message = mv_string("Unable to get tagname in pattern."),
                };
            }

            SymbolArray clause_binds = mk_u64_array(raw_pattern->nodes.len - 1, a);
            for (size_t s = 1; s < raw_pattern->nodes.len; s++) {
                RawTree* raw_name = raw_pattern->nodes.data[s];
                if (!is_symbol(raw_name)) {
                    return (AbsExprResult) {
                        .type = Err,
                        .error_message = mv_string("Pattern binding was not a symbol!"),
                    };
                }
                push_u64(raw_name->atom.symbol, &clause_binds); 
            }

            // Get the term
            RawTree* raw_term;
            if (raw_clause->nodes.len == 2) {
                raw_term = (RawTree*)aref_ptr(2, raw.nodes);
            } else {
                raw_term = mem_alloc (sizeof(RawTree), a);

                raw_term->nodes.len = raw.nodes.len - 2;
                raw_term->nodes.size = raw.nodes.size - 2;
                raw_term->nodes.data = raw.nodes.data + 2;
                raw_term->type = RawList;
            }

            AbsExprResult clause_body_res = abstract_expr_i(*(RawTree*)raw_clause->nodes.data[1], env, a);
            if (clause_body_res.type == Err) return clause_body_res;
            Syntax* clause_body = mem_alloc(sizeof(Syntax), a);
            *clause_body = clause_body_res.out;

            SynClause* clause = mem_alloc(sizeof(SynClause), a);
            *clause = (SynClause) {
                .tagname = clause_tagname,
                .vars = clause_binds,
                .body = clause_body,
            };
            push_ptr(clause, &clauses);
        }

        res.type = Ok;
        res.out = (Syntax) {
            .type = SMatch,
            .match.val = sval,
            .match.clauses = clauses,
        };
        break;
    }
    case FStructure: {
        if (raw.nodes.len < 2) {
            res.type = Err;
            res.error_message = mk_string("Structure term former needs a structure type argument", a);
            return res;
        }

        // Get the type of the structure
        res = abstract_expr_i(*(RawTree*)raw.nodes.data[1], env, a);
        if (res.type == Err) return res;

        Syntax* stype = mem_alloc(sizeof(Syntax), a);
        *stype = res.out;

        // Construct a structure
        SymPtrAMap fields = mk_sym_ptr_amap(raw.nodes.len, a);
        for (size_t i = 2; i < raw.nodes.len; i++) {
            RawTree* fdesc = raw.nodes.data[i];
            if (fdesc->type != RawList) {
                res.type = Err;
                res.error_message = mv_string("Structure expects all field descriptors to be lists.");
                return res;
            }
            
            if (fdesc->nodes.len != 2) {
                res.type = Err;
                res.error_message = mv_string("Structure expects all field descriptors to have 2 elements.");
                return res;
            }

            Symbol field;
            if (!get_fieldname(fdesc->nodes.data[0], &field)) {
                res.type = Err;
                res.error_message = mv_string("Structure has malformed field name.");
                return res;
            }

            res = abstract_expr_i(*(RawTree*) fdesc->nodes.data[1], env, a);
            if (res.type == Err) return res;
            Syntax* syn = mem_alloc(sizeof(Syntax), a);
            *syn = res.out;

            sym_ptr_insert(field, syn, &fields);
        }

        res.type = Ok;
        res.out = (Syntax) {
            .type = SStructure,
            .structure.ptype = stype,
            .structure.fields = fields,
        };
        break;
    }
    case FProjector: {
        if (raw.nodes.len != 3) {
            res.type = Err;
            res.error_message = mk_string("Projection term former needs two arguments!", a);
            return res;
        }
        // Get the structure portion of the proector 
        res = abstract_expr_i(*(RawTree*)raw.nodes.data[2], env, a);
        if (res.type == Err) return res;

        Syntax* structure = mem_alloc(sizeof(Syntax), a);
        *structure = res.out;
        
        // Get the symbol portion of the projector
        RawTree* msym = (RawTree*)raw.nodes.data[1];
        if (msym->type != RawAtom && msym->atom.type != ASymbol) {
            res = (AbsExprResult) {
                .type = Err,
                .error_message = mv_string("Second argument to projection term former should be symbol"),
            };
            return res;
        }

        res.type = Ok;
        res.out = (Syntax) {
            .type = SProjector,
            .projector.field = msym->atom.symbol,
            .projector.val = structure,
        };
        break;
    }
    case FIf: {
        if (raw.nodes.len != 4) {
            res.type = Err;
            res.error_message = mk_string("If term former expects precisely 3 arguments!", a);
            return res;
        }
        SynArray terms = mk_ptr_array(3, a);
        for (size_t i = 1; i < raw.nodes.len; i++) {
            RawTree* rptr = aref_ptr(i, raw.nodes);
            AbsExprResult rterm = abstract_expr_i(*rptr, env, a);
            if (rterm.type == Err) {
                for (size_t i =0; i < terms.len; i++)
                    delete_syntax_pointer(terms.data[i], a);
                sdelete_ptr_array(terms);
                return rterm;
            }
            else {
                Syntax* term = mem_alloc(sizeof(Syntax), a);
                *term = rterm.out;
                push_ptr(term, &terms);
            }
        }

        res.type = Ok;
        res.out = (Syntax) {
            .type = SIf,
            .if_expr.condition = aref_ptr(0, terms),
            .if_expr.true_branch = aref_ptr(1, terms),
            .if_expr.false_branch = aref_ptr(2, terms),
        };
        sdelete_ptr_array(terms);
        break;
    }
    case FLet: {
        res.type = Err;
        res.error_message = mv_string("Let term former not implemented!");
        break;
    }
    case FProcType: {
        if (raw.nodes.len != 3) {
            return (AbsExprResult) {
                .type = Err,
                .error_message = mv_string("Wrong number of terms to proc type former."),
            };
        }

        RawTree* raw_args = raw.nodes.data[1];
        if (raw_args->type != RawList) {
                return (AbsExprResult) {
                    .type = Err,
                    .error_message = mv_string("Procedure argument list should be list."),
                };
        }
        
        PtrArray arg_types = mk_ptr_array(raw_args->nodes.len, a);

        for (size_t i = 0; i < raw_args->nodes.len; i++) {
            RawTree* raw_arg = raw_args->nodes.data[i];
            res = abstract_expr_i(*raw_arg, env, a);
            if (res.type == Err) return res;

            Syntax* arg_ty = mem_alloc(sizeof(Syntax), a);
            *arg_ty = res.out;
            push_ptr(arg_ty, &arg_types);
        }

        RawTree* raw_return = raw.nodes.data[2]; 
        res = abstract_expr_i(*raw_return, env, a);
        if (res.type == Err) return res;

        Syntax* return_type = mem_alloc(sizeof(RawTree), a);
        *return_type = res.out;

        res.type = Ok;
        res.out = (Syntax) {
            .type = SProcType,
            .proc_type.args = arg_types,
            .proc_type.return_type = return_type,
        };
        break;
    }
    case FStructType: {
        SymSynAMap field_types = mk_sym_ptr_amap(raw.nodes.len, a);

        for (size_t i = 1; i < raw.nodes.len; i++) {
            RawTree* fdesc = raw.nodes.data[i];
            if (fdesc->type != RawList) {
                res.type = Err;
                res.error_message = mv_string("Structure type expects all field descriptors to be lists.");
                return res;
            };
            
            if (fdesc->nodes.len != 2) {
                res.type = Err;
                res.error_message = mv_string("Structure type expects all field descriptors to have 2 elements.");
                return res;
            };

            Symbol field;
            if (!get_fieldname(fdesc->nodes.data[0], &field)) {
                res.type = Err;
                res.error_message = mv_string("Structure type has malformed field name.");
                return res;
            };

            res = abstract_expr_i(*(RawTree*) fdesc->nodes.data[1], env, a);
            if (res.type == Err) {
                return res;
            }

            Syntax* field_ty = mem_alloc(sizeof(Syntax), a);
            *field_ty = res.out;

            sym_ptr_insert(field, field_ty, &field_types);
        }

        res.type = Ok;
        res.out = (Syntax) {
            .type = SStructType,
            .struct_type.fields = field_types,
        };
        break;
    }
    case FEnumType: {
        SymPtrAMap enum_variants = mk_sym_ptr_amap(raw.nodes.len, a);

        for (size_t i = 1; i < raw.nodes.len; i++) {
            RawTree* edesc = raw.nodes.data[i];

            if (edesc->type != RawList) {
                res.type = Err;
                res.error_message = mv_string("Enumeration type expects all enum descriptors to be lists.");
                return res;
            };
            
            if (edesc->nodes.len < 1) {
                res.type = Err;
                res.error_message = mv_string("Enumeration type expects all enum descriptors to have at least 1 elements.");
                return res;
            };

            Symbol tagname;
            PtrArray* types = mem_alloc(sizeof(PtrArray), a);
            *types = mk_ptr_array(edesc->nodes.len - 1, a);

            if (!get_fieldname(edesc->nodes.data[0], &tagname)) {
                res.type = Err;
                res.error_message = mv_string("Enum type has malformed field name.");
                return res;
            };

            for (size_t i = 1; i < edesc->nodes.len; i++) {
                res = abstract_expr_i(*(RawTree*) edesc->nodes.data[i], env, a);
                if (res.type == Err) {
                    return res;
                }
                Syntax* field_ty = mem_alloc(sizeof(Syntax), a);
                *field_ty = res.out;
                push_ptr(field_ty, types);
            }

            sym_ptr_insert(tagname, types, &enum_variants);
        }

        res.type = Ok;
        res.out = (Syntax) {
            .type = SEnumType,
            .enum_type.variants = enum_variants,
        };
        break;
    }
    default:
        res.type = Err;
        res.error_message = mv_string("Internal Error: Invalid term former!");
    }
    return res;
}

AbsExprResult abstract_expr_i(RawTree raw, ShadowEnv* env, Allocator* a) {
    AbsExprResult res;
    // Resolution does not perform type analysis, so we set the type pointer to NULL
    // This is then resolved in the typechecking stage;
    res.out.ptype = NULL;
    switch (raw.type) {
    case RawAtom: {
        if (raw.atom.type == ASymbol) {
            res = (AbsExprResult) {
                .type = Ok,
                .out.type = SVariable,
                .out.variable = raw.atom.symbol,
            };
        }
        else if (raw.atom.type == AI64) {
            res = (AbsExprResult) {
                .type = Ok,
                .out.type = SLitI64,
                .out.lit_i64 = raw.atom.int_64,
            };
        }
        else if (raw.atom.type == ABool) {
            res = (AbsExprResult) {
                .type = Ok,
                .out.type = SLitBool,
                .out.lit_i64 = raw.atom.int_64,
            };
        } else  {
            res = (AbsExprResult) {
                .type = Err,
                .error_message = mk_string("Currently, can only make literal values out of type i64." , a),
            };
        }
        break;
    }
    case RawList: {
        // Currently, can only have function calls, so all Raw lists compile down to an application
        if (raw.nodes.len < 1) {
            return (AbsExprResult) {
                .type = Err,
                .error_message = mk_string("Raw Syntax must have at least one element!", a),
            };
        }
        if (is_symbol(aref_ptr(0, raw.nodes))) {
            Symbol sym = ((RawTree*)aref_ptr(0, raw.nodes))->atom.symbol;
            ShadowEntry entry = shadow_env_lookup(sym, env);
            switch (entry.type) {
            case SErr:
                res.type = Err;
                String str1 = mk_string("Can't find symbol! ", a);
                String* str2 = symbol_to_string(sym);
                res.error_message = string_cat(str1, *str2, a);
                delete_string(str1, a);
                break;
            case SShadowed: 
                return mk_application(raw, env, a);
                break;
            case SLocal: 
                return (AbsExprResult) {
                    .type = Err,
                    .error_message = mk_string("Higher kinded types not currently supported!", a),
                };
                break;
            case SGlobal:
                if (entry.vtype->sort == TPrim && entry.vtype->prim == TFormer) {
                    return mk_term(*((TermFormer*)entry.value), raw, env, a);
                } else {
                    return mk_application(raw, env, a);
                }
            }
        }
        else {
            return mk_application(raw, env, a);
        }
        break;
    }
    default: {
        res.type = Err;
        res.error_message = mk_string("Name Resolution Received an invalid Raw Syntax Tree", a);
    }
    }
    return res;
}

AbsResult mk_toplevel(TermFormer former, RawTree raw, ShadowEnv* env, Allocator* a) {
    AbsResult res;
    switch (former) {
    case FDefine: {
        if (raw.nodes.len < 3) {
            res.type = Err;
            res.error_message = mk_string("Definitions expect at least 2 terms", a);
            return res;
        }

        if (!is_symbol(raw.nodes.data[1])) {
            res.type = Err;
            res.error_message = mk_string("First argument to definitions should be a symbol", a);
            return res;
        }

        Symbol sym = ((RawTree*)aref_ptr(1, raw.nodes))->atom.symbol;
        
        RawTree* raw_term;
        if (raw.nodes.len == 3) {
            raw_term = (RawTree*)aref_ptr(2, raw.nodes);
        } else {
            raw_term = mem_alloc (sizeof(RawTree), a);

            raw_term->nodes.len = raw.nodes.len - 2;
            raw_term->nodes.size = raw.nodes.size - 2;
            raw_term->nodes.data = raw.nodes.data + 2;
            raw_term->type = RawList;
        }

        shadow_var(sym, env);
        AbsExprResult inter = abstract_expr_i(*raw_term, env, a);
        shadow_pop(env, 1);

        if (inter.type == Err) {
            res.type = Err;
            res.error_message = inter.error_message;
            return res;
        }

        Syntax* term = (Syntax*) mem_alloc(sizeof(Syntax), a);
        *term = inter.out;
        // res.out;

        res.type = Ok;
        res.out.type = TLDef;
        res.out.def.bind = sym;
        res.out.def.value = term;

        break;
    }
    default:
        // fallback: make term. 
        res = to_toplevel(mk_term(former, raw, env, a));
        break;
    }
    return res;
}

AbsResult abstract_i(RawTree raw, ShadowEnv* env, Allocator* a) {
    // first: try a unique toplevel-form
    bool unique_toplevel = false;

    if (raw.type == RawList && raw.nodes.len > 1) {
        if (is_symbol(aref_ptr(0, raw.nodes))) {
            Symbol sym = ((RawTree*)aref_ptr(0, raw.nodes))->atom.symbol;
            ShadowEntry entry = shadow_env_lookup(sym, env);
            switch (entry.type) {
            case SGlobal: {
                if (entry.vtype->sort == TPrim && entry.vtype->prim == TFormer) {
                    unique_toplevel = true;
                    return mk_toplevel(*((TermFormer*)entry.value), raw, env, a);
                }
                break;
            }
            default:
                break;
            }
        }
    } 

    if (!unique_toplevel) {
        return to_toplevel(abstract_expr_i(raw, env, a));
    }

    return (AbsResult) { 
        .type = Err,
        .error_message = mk_string("Logic error in abstract_i: reached unreachable area.", a),
    };
}

AbsExprResult abstract_expr(RawTree raw, Environment* env, Allocator* a) {
    ShadowEnv* s_env = mk_shadow_env(a, env);
    AbsExprResult out = abstract_expr_i(raw, s_env, a);
    delete_shadow_env(s_env, a);
    return out; 
}

AbsResult abstract(RawTree raw, Environment* env, Allocator* a) {
    ShadowEnv* s_env = mk_shadow_env(a, env);
    AbsResult out = abstract_i(raw, s_env, a);
    delete_shadow_env(s_env, a);
    return out;
}
