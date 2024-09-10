#include "pico/analysis/abstraction.h"

#include "data/result.h"
#include "data/string.h"
#include "pico/values/values.h"
#include "pico/syntax/concrete.h"
#include "pico/syntax/syntax.h"
#include "pico/binding/shadow_env.h"

// Declarations for mutual recursion
// ---
// "Internal" implementation of resolve_dynamic 

AbsExprResult abstract_expr_i(RawTree raw, ShadowEnv* env, Allocator* a);

bool is_symbol(RawTree* raw) {
    return (raw->type == RawAtom && raw->data.atom.type == ASymbol);
}

bool get_fieldname(RawTree* raw, Symbol* fieldname) {
    if (raw->type == RawList && raw->data.nodes.len == 2) {
        raw = raw->data.nodes.data[1];
        if (is_symbol(raw)) {
            *fieldname = raw->data.atom.symbol;
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

    for (size_t i = 0; i < nodes.data.nodes.len; i++) {
        RawTree* node = aref_ptr(i, nodes.data.nodes);
        if (node->type != RawAtom) { return false; }
        if (node->data.atom.type != ASymbol) { return false;  }
        push_u64(node->data.atom.symbol, arr);
    }
    return true;
}

PiType* get_raw_type(RawTree raw, ShadowEnv* env) {
    PiType* out = NULL;
    if (raw.type != RawAtom || raw.data.atom.type != ASymbol) {
        return out;
    }

    shadow_entry entry = shadow_env_lookup(raw.data.atom.symbol, env);
    if (entry.type == SGlobal
        && entry.vtype->sort == TPrim
        && entry.vtype->prim == TType) {
            out = entry.value;
    }
    return out;
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
    AbsExprResult fn_res = abstract_expr_i(*(RawTree*)(aref_ptr(0, raw.data.nodes)), env, a);
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
            .out.variant.args = mk_ptr_array(raw.data.nodes.len - 1, a),
        };

        for (size_t i = 1; i < raw.data.nodes.len; i++) {
            AbsExprResult arg_res = abstract_expr_i(*(RawTree*)(aref_ptr(i, raw.data.nodes)), env, a);
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
            .out.application.args = mk_ptr_array(raw.data.nodes.len - 1, a),
        };
        *res.out.application.function = fn_res.out;

        for (size_t i = 1; i < raw.data.nodes.len; i++) {
            AbsExprResult arg_res = abstract_expr_i(*(RawTree*)(aref_ptr(i, raw.data.nodes)), env, a);
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
        if (raw.data.nodes.len < 3) {
            res.type = Err;
            res.error_message = mk_string("Procedure term former requires at least 2 arguments!", a);
            return res;
        }

        SymbolArray arguments = mk_u64_array(2, a);
        if (!get_symbol_list(&arguments, *(RawTree*)aref_ptr(1, raw.data.nodes))) {
            res.type = Err;
            res.error_message = mk_string("Procedure term former requires first arguments to be a symbol-list!", a);
            return res;
        }

        shadow_vars(arguments, env);

        RawTree* raw_term;
        if (raw.data.nodes.len == 3) {
            raw_term = (RawTree*)aref_ptr(2, raw.data.nodes);
        } else {
            raw_term = mem_alloc (sizeof(RawTree), a);

            raw_term->data.nodes.len = raw.data.nodes.len - 2;
            raw_term->data.nodes.size = raw.data.nodes.size - 2;
            raw_term->data.nodes.data = raw.data.nodes.data + 2;
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
        if (raw.data.nodes.len == 2) {
            // Check that we are indeed getting a result
            // Get the tag name of the variant
            RawTree* msym = (RawTree*)raw.data.nodes.data[1];
            if (msym->type != RawAtom && msym->data.atom.type != ASymbol) {
                res = (AbsExprResult) {
                    .type = Err,
                    .error_message = mv_string("Argument to variant term former should be symbol"),
                };
                return res;
            }
            Symbol lit = msym->data.atom.symbol;

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

        else if (raw.data.nodes.len != 3) {
            return (AbsExprResult) {
                .type = Err,
                .error_message = mv_string("Variant term former needs two arguments!"),
            };
        }

        // Get the Type portion of the projector 
        res = abstract_expr_i(*(RawTree*)raw.data.nodes.data[2], env, a);
        if (res.type == Err) return res;

        Syntax* var_type = mem_alloc(sizeof(Syntax), a);
        *var_type = res.out;
        
        // Check that we are indeed getting a result
        // Get the tag gname of the variant
        RawTree* msym = (RawTree*)raw.data.nodes.data[1];
        if (msym->type != RawAtom && msym->data.atom.type != ASymbol) {
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
            .constructor.tagname = msym->data.atom.symbol,
        };
        break;
    }
    case FMatch: {
        res = abstract_expr_i(*(RawTree*)raw.data.nodes.data[1], env, a);
        if (res.type == Err) return res;
        Syntax* sval = mem_alloc(sizeof(Syntax), a);
        *sval = res.out;

        ClauseArray clauses = mk_ptr_array(raw.data.nodes.len - 2, a);
        
        for (size_t i = 2; i < raw.data.nodes.len; i++) {
            // For each clause, we need three things:
            // 1. The tag, corresponding to the enum
            // 2. The variable(s) destructuring the enum
            // 3. The body
            // Get the clause
            RawTree* raw_clause = (RawTree*)raw.data.nodes.data[i];
            if (raw_clause->type != RawList || raw_clause->data.nodes.len < 2) {
                return (AbsExprResult) {
                    .type = Err,
                    .error_message = mv_string("Match Clause has incorrect number of elements!"),
                };
            }

            // Get the pattern
            RawTree* raw_pattern = (RawTree*)raw_clause->data.nodes.data[0];
            if (raw_pattern->type != RawList) {
                return (AbsExprResult) {
                    .type = Err,
                    .error_message = mv_string("Match Pattern should be a list!"),
                };
            }

            // The pattern has two parts: the variables & the tag
            // The tag should be in a constructor (i.e. list)
            Symbol clause_tagname; 
            if (!get_fieldname(raw_pattern->data.nodes.data[0], &clause_tagname)) {
                return (AbsExprResult) {
                    .type = Err,
                    .error_message = mv_string("Unable to get tagname in pattern."),
                };
            }

            SymbolArray clause_binds = mk_u64_array(raw_pattern->data.nodes.len - 1, a);
            for (size_t s = 1; s < raw_pattern->data.nodes.len; s++) {
                RawTree* raw_name = raw_pattern->data.nodes.data[s];
                if (!is_symbol(raw_name)) {
                    return (AbsExprResult) {
                        .type = Err,
                        .error_message = mv_string("Pattern binding was not a symbol!"),
                    };
                }
                push_u64(raw_name->data.atom.symbol, &clause_binds); 
            }

            // Get the term
            RawTree* raw_term;
            if (raw_clause->data.nodes.len == 2) {
                raw_term = (RawTree*)aref_ptr(2, raw.data.nodes);
            } else {
                raw_term = mem_alloc (sizeof(RawTree), a);

                raw_term->data.nodes.len = raw.data.nodes.len - 2;
                raw_term->data.nodes.size = raw.data.nodes.size - 2;
                raw_term->data.nodes.data = raw.data.nodes.data + 2;
                raw_term->type = RawList;
            }

            AbsExprResult clause_body_res = abstract_expr_i(*(RawTree*)raw_clause->data.nodes.data[1], env, a);
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
        if (raw.data.nodes.len < 2) {
            res.type = Err;
            res.error_message = mk_string("Structure term former needs a structure type argument", a);
            return res;
        }

        // Get the type of the structure
        res = abstract_expr_i(*(RawTree*)raw.data.nodes.data[1], env, a);
        if (res.type == Err) return res;

        Syntax* stype = mem_alloc(sizeof(Syntax), a);
        *stype = res.out;

        // Construct a structure
        SymPtrAMap fields = mk_sym_ptr_amap(raw.data.nodes.len, a);
        for (size_t i = 2; i < raw.data.nodes.len; i++) {
            RawTree* fdesc = raw.data.nodes.data[i];
            if (fdesc->type != RawList) {
                res.type = Err;
                res.error_message = mv_string("Structure expects all field descriptors to be lists.");
                return res;
            }
            
            if (fdesc->data.nodes.len != 2) {
                res.type = Err;
                res.error_message = mv_string("Structure expects all field descriptors to have 2 elements.");
                return res;
            }

            Symbol field;
            if (!get_fieldname(fdesc->data.nodes.data[0], &field)) {
                res.type = Err;
                res.error_message = mv_string("Structure has malformed field name.");
                return res;
            }

            res = abstract_expr_i(*(RawTree*) fdesc->data.nodes.data[1], env, a);
            if (res.type == Err) return res;
            Syntax* syn = mem_alloc(sizeof(Syntax), a);
            *syn = res.out;

            sym_ptr_insert(field, syn, &fields);
        }

        res.type = Ok;
        res.out.type = SStructure;
        res.out.structure.ptype = stype;
        res.out.structure.fields = fields;
        break;
    }
    case FProjector: {
        if (raw.data.nodes.len != 3) {
            res.type = Err;
            res.error_message = mk_string("Projection term former needs two arguments!", a);
            return res;
        }
        // Get the structure portion of the proector 
        res = abstract_expr_i(*(RawTree*)raw.data.nodes.data[2], env, a);
        if (res.type == Err) return res;

        Syntax* structure = mem_alloc(sizeof(Syntax), a);
        *structure = res.out;
        
        // Get the symbol portion of the projector
        RawTree* msym = (RawTree*)raw.data.nodes.data[1];
        if (msym->type != RawAtom && msym->data.atom.type != ASymbol) {
            res = (AbsExprResult) {
                .type = Err,
                .error_message = mv_string("Second argument to projection term former should be symbol"),
            };
            return res;
        }

        res.type = Ok;
        res.out = (Syntax) {
            .type = SProjector,
            .projector.field = msym->data.atom.symbol,
            .projector.val = structure,
        };
        break;
    }
    case FIf: {
        if (raw.data.nodes.len != 4) {
            res.type = Err;
            res.error_message = mk_string("If term former expects precisely 3 arguments!", a);
            return res;
        }
        SynArray terms = mk_ptr_array(3, a);
        for (size_t i = 1; i < raw.data.nodes.len; i++) {
            RawTree* rptr = aref_ptr(i, raw.data.nodes);
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
        res.out.type = SIf;
        res.out.if_expr.condition = aref_ptr(0, terms);
        res.out.if_expr.true_branch = aref_ptr(1, terms);
        res.out.if_expr.false_branch = aref_ptr(2, terms);
        sdelete_ptr_array(terms);
        break;
    }
    case FLet: {
        res.type = Err;
        res.error_message = mv_string("Let term former not implemented!");
        break;
    }
    case FProcType: {
        res.type = Err;
        res.error_message = mv_string("Proc type former not implemented!");
        break;
    }
    case FStructType: {
        // Construct a structure type

        PiType* out_ty = mem_alloc(sizeof(PiType), a);
        SymPtrAMap field_types = mk_sym_ptr_amap(raw.data.nodes.len, a);

        for (size_t i = 1; i < raw.data.nodes.len; i++) {
            RawTree* fdesc = raw.data.nodes.data[i];
            if (fdesc->type != RawList) {
                res.type = Err;
                res.error_message = mv_string("Structure type expects all field descriptors to be lists.");
                return res;
            };
            
            if (fdesc->data.nodes.len != 2) {
                res.type = Err;
                res.error_message = mv_string("Structure type expects all field descriptors to have 2 elements.");
                return res;
            };

            Symbol field;
            if (!get_fieldname(fdesc->data.nodes.data[0], &field)) {
                res.type = Err;
                res.error_message = mv_string("Structure type has malformed field name.");
                return res;
            };

            res = abstract_expr_i(*(RawTree*) fdesc->data.nodes.data[1], env, a);
            if (res.type == Err) {
                return res;
            }
            if (res.out.type != SType) {
                res.type = Err;
                res.error_message = mv_string("Structure type fields must have their own types.");
                return res;
            }

            PiType* field_ty = res.out.type_val;

            sym_ptr_insert(field, field_ty, &field_types);
        }

        out_ty->sort = TStruct;
        out_ty->structure.fields = field_types;

        res.type = Ok;
        res.out.type = SType;
        res.out.type_val = out_ty;
        break;
    }
    case FEnumType: {
        // Construct an enum type

        PiType* out_ty = mem_alloc(sizeof(PiType), a);
        SymPtrAMap enum_variants = mk_sym_ptr_amap(raw.data.nodes.len, a);

        for (size_t i = 1; i < raw.data.nodes.len; i++) {
            RawTree* edesc = raw.data.nodes.data[i];

            if (edesc->type != RawList) {
                res.type = Err;
                res.error_message = mv_string("Enumeration type expects all enum descriptors to be lists.");
                return res;
            };
            
            if (edesc->data.nodes.len < 1) {
                res.type = Err;
                res.error_message = mv_string("Enumeration type expects all enum descriptors to have at least 1 elements.");
                return res;
            };

            Symbol tagname;
            PtrArray* types = mem_alloc(sizeof(PtrArray), a);
            *types = mk_ptr_array(edesc->data.nodes.len - 1, a);

            if (!get_fieldname(edesc->data.nodes.data[0], &tagname)) {
                res.type = Err;
                res.error_message = mv_string("Enum type has malformed field name.");
                return res;
            };

            for (size_t i = 1; i < edesc->data.nodes.len; i++) {
                res = abstract_expr_i(*(RawTree*) edesc->data.nodes.data[i], env, a);
                if (res.type == Err) {
                    return res;
                }
                if (res.out.type != SType) {
                    res.type = Err;
                    res.error_message = mv_string("Structure type fields must have their own types.");
                    return res;
                }
                PiType* field_ty = res.out.type_val;
                push_ptr(field_ty, types);
            }

            sym_ptr_insert(tagname, types, &enum_variants);
        }

        out_ty->sort = TEnum;
        out_ty->enumeration.variants = enum_variants;

        res.type = Ok;
        res.out.type = SType;
        res.out.type_val = out_ty;
        break;
    }
    default:
        res.type = Err;
        res.error_message = mv_string("Internal Error: Invalid term former!");
    }
    return res;
}

// Convert to AST at runtime
AbsExprResult abstract_expr_i(RawTree raw, ShadowEnv* env, Allocator* a) {
    AbsExprResult res;
    // Resolution does not perform type analysis, so we set the type pointer to NULL
    // This is then resolved in the typechecking stage;
    res.out.ptype = NULL;
    switch (raw.type) {
    case RawAtom: {
        if (raw.data.atom.type == ASymbol) {
            PiType* ty = get_raw_type(raw, env);
            if (ty) {
                res.type = Ok;
                res.out.type = SType;
                res.out.type_val = ty;
            } else {
                res.type = Ok;
                res.out.type = SVariable;
                res.out.variable = raw.data.atom.symbol;
            }

        }
        else if (raw.data.atom.type == AI64) {
            res.type = Ok;
            res.out.type = SLitI64;
            res.out.lit_i64 = raw.data.atom.int_64;
        }
        else if (raw.data.atom.type == ABool) {
            res.type = Ok;
            res.out.type = SLitBool;
            res.out.lit_i64 = raw.data.atom.int_64;
        } else  {
            res.type = Err;
            res.error_message = mk_string("Currently, can only make literal values out of type i64." , a);
        }
        break;
    }
    case RawList: {
        // Currently, can only have function calls, so all Rawaists compile down to an application
        if (raw.data.nodes.len < 1) {
            res.type = Err;
            res.error_message = mk_string("Raw Syntax must have at least one element!", a);
            return res;
        }
        if (is_symbol(aref_ptr(0, raw.data.nodes))) {
            Symbol sym = ((RawTree*)aref_ptr(0, raw.data.nodes))->data.atom.symbol;
            shadow_entry entry = shadow_env_lookup(sym, env);
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
        if (raw.data.nodes.len < 3) {
            res.type = Err;
            res.error_message = mk_string("Definitions expect at least 2 terms", a);
            return res;
        }

        if (!is_symbol(raw.data.nodes.data[1])) {
            res.type = Err;
            res.error_message = mk_string("First argument to definitions should be a symbol", a);
            return res;
        }

        Symbol sym = ((RawTree*)aref_ptr(1, raw.data.nodes))->data.atom.symbol;
        
        RawTree* raw_term;
        if (raw.data.nodes.len == 3) {
            raw_term = (RawTree*)aref_ptr(2, raw.data.nodes);
        } else {
            raw_term = mem_alloc (sizeof(RawTree), a);

            raw_term->data.nodes.len = raw.data.nodes.len - 2;
            raw_term->data.nodes.size = raw.data.nodes.size - 2;
            raw_term->data.nodes.data = raw.data.nodes.data + 2;
            raw_term->type = RawList;
        }

        shadow_var(sym, env);
        AbsExprResult inter = abstract_expr_i(*raw_term, env, a);
        pop_shadow(env, 1);

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

    if (raw.type == RawList && raw.data.nodes.len > 1) {
        if (is_symbol(aref_ptr(0, raw.data.nodes))) {
            Symbol sym = ((RawTree*)aref_ptr(0, raw.data.nodes))->data.atom.symbol;
            shadow_entry entry = shadow_env_lookup(sym, env);
            switch (entry.type) {
            case SGlobal:
                if (entry.vtype->sort == TPrim && entry.vtype->prim == TFormer) {
                    unique_toplevel = true;
                    return mk_toplevel(*((TermFormer*)entry.value), raw, env, a);
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
