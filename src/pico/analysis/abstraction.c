#include "pico/analysis/abstraction.h"

#include "data/result.h"
#include "data/string.h"
#include "pico/values/values.h"
#include "pico/syntax/concrete.h"
#include "pico/syntax/syntax.h"
#include "pico/binding/shadow_env.h"

// Internal functions 
Syntax* abstract_expr_i(RawTree raw, ShadowEnv* env, Allocator* a, ErrorPoint* point);

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
        if (node->type != RawAtom || node->atom.type != ASymbol) { return false; }
        push_u64(node->atom.symbol, arr);
    }
    return true;
}

Result get_annotated_symbol_list(SymPtrAssoc *args, RawTree list, ShadowEnv* env, Allocator* a, ErrorPoint* point) {
    Result error_result = {.type = Err, .error_message = mv_string("Malformed proc argument list.")};
    if (list.type != RawList) { return error_result; }

    for (size_t i = 0; i < list.nodes.len; i++) {
        RawTree* annotation = list.nodes.data[i];
        if (annotation->type == RawAtom) {
            sym_ptr_bind(annotation->atom.symbol, NULL, args);
        } else if (annotation->type == RawList || annotation->nodes.len == 2) {
            RawTree* arg = annotation->nodes.data[0];
            RawTree* raw_type = annotation->nodes.data[1];
            if (arg->type != RawAtom || arg->atom.type != ASymbol) { return error_result; }
            Syntax* type = abstract_expr_i(*raw_type, env, a, point); 

            sym_ptr_bind(arg->atom.symbol, type, args);
        } else { return error_result; }
    }
    return (Result) {.type = Ok};
}

Syntax* mk_application(RawTree raw, ShadowEnv* env, Allocator* a, ErrorPoint* point) {
    Syntax* fn_syn = abstract_expr_i(*(RawTree*)(raw.nodes.data[0]), env, a, point);

    Syntax* res = mem_alloc(sizeof(Syntax), a);
    if (fn_syn->type == SConstructor) {
        *res = (Syntax) {
            .type = SVariant,
            .variant.enum_type = fn_syn->constructor.enum_type,
            .variant.tagname = fn_syn->constructor.tagname,
            .variant.args = mk_ptr_array(raw.nodes.len - 1, a),
        };

        for (size_t i = 1; i < raw.nodes.len; i++) {
            Syntax* arg = abstract_expr_i(*(RawTree*)(raw.nodes.data[i]), env, a, point);
            push_ptr(arg, &res->variant.args);
        }
    } else {
        *res = (Syntax) {
            .type = SApplication,
            .application.function = mem_alloc(sizeof(Syntax), a),
            .application.args = mk_ptr_array(raw.nodes.len - 1, a),
        };
        res->application.function = fn_syn;

        for (size_t i = 1; i < raw.nodes.len; i++) {
            Syntax* arg = abstract_expr_i(*(RawTree*)(raw.nodes.data[i]), env, a, point);
            push_ptr(arg, &res->application.args);
        }
    }

    return res;
}

Syntax* mk_term(TermFormer former, RawTree raw, ShadowEnv* env, Allocator* a, ErrorPoint* point) {
    Syntax* res = mem_alloc(sizeof(Syntax), a);
    switch (former) {
    case FProcedure: {
        if (raw.nodes.len < 3) {
            throw_error(point, mk_string("Procedure term former requires at least 2 arguments!", a));
        }

        SymPtrAssoc arguments = mk_sym_ptr_assoc(8, a);
        SymbolArray to_shadow = mk_u64_array(8, a);
        Result args_out = get_annotated_symbol_list(&arguments, *(RawTree*)raw.nodes.data[1], env, a, point);
        if (args_out.type == Err) throw_error(point, args_out.error_message);

        shadow_vars(to_shadow, env);

        RawTree* raw_term;
        if (raw.nodes.len == 3) {
            raw_term = (RawTree*)raw.nodes.data[2];
        } else {
            raw_term = mem_alloc (sizeof(RawTree), a);

            raw_term->nodes.len = raw.nodes.len - 2;
            raw_term->nodes.size = raw.nodes.size - 2;
            raw_term->nodes.data = raw.nodes.data + 2;
            raw_term->type = RawList;
        }
        Syntax* body = abstract_expr_i(*raw_term, env, a, point);
        shadow_pop(arguments.len, env);

        *res = (Syntax) {
            .type = SProcedure,
            .procedure.args = arguments,
            .procedure.body = body
        };
        break;
    }
    case FAll: {
        if (raw.nodes.len < 3) {
            throw_error(point, mk_string("all term former requires at least 2 arguments!", a));
        }

        SymbolArray arguments = mk_u64_array(2, a);
        if (!get_symbol_list(&arguments, *(RawTree*)raw.nodes.data[1])) {
            throw_error(point, mk_string("all term former requires first arguments to be a symbol-list!", a));
        }

        shadow_vars(arguments, env);

        RawTree* raw_term;
        if (raw.nodes.len == 3) {
            raw_term = raw.nodes.data[2];
        } else {
            raw_term = mem_alloc (sizeof(RawTree), a);

            raw_term->nodes.len = raw.nodes.len - 2;
            raw_term->nodes.size = raw.nodes.size - 2;
            raw_term->nodes.data = raw.nodes.data + 2;
            raw_term->type = RawList;
        }
        Syntax* body = abstract_expr_i(*raw_term, env, a, point);

        *res = (Syntax) {
            .type = SAll,
            .all.args = arguments,
            .all.body = body,
        };
        break;
    }
    case FApplication: {
        res = mk_application(raw, env, a, point);
        break;
    }
    case FVariant: {
        if (raw.nodes.len == 2) {
            // Check that we are indeed getting a result
            // Get the tag name of the variant
            RawTree* msym = (RawTree*)raw.nodes.data[1];
            if (msym->type != RawAtom && msym->atom.type != ASymbol) {
                throw_error(point, mv_string("Argument to variant term former should be symbol"));
            }
            Symbol lit = msym->atom.symbol;

            if (lit == string_to_symbol(mv_string("true"))) {
                *res = (Syntax) {.type = SLitBool, .boolean = true,};
            }
            else if (lit == string_to_symbol(mv_string("false"))) {
                *res = (Syntax) {.type = SLitBool, .boolean = false,};
            }

            throw_error(point, mv_string("Variant term former needs two arguments!"));
        }
        // : sym Type

        else if (raw.nodes.len != 3) {
            throw_error(point, mv_string("Variant term former needs two arguments!"));
        }

        // Get the Type portion of the projector 
        Syntax* var_type = abstract_expr_i(*(RawTree*)raw.nodes.data[2], env, a, point);
        
        // Check that we are indeed getting a result
        // Get the tag gname of the variant
        RawTree* msym = (RawTree*)raw.nodes.data[1];
        if (msym->type != RawAtom && msym->atom.type != ASymbol) {
            throw_error(point, mv_string("Second argument to projection term former should be symbol"));
        };

        //res.type = Ok;
        *res = (Syntax) {
            .type = SConstructor,
            .constructor.enum_type = var_type,
            .constructor.tagname = msym->atom.symbol,
        };
        break;
    }
    case FMatch: {
        Syntax* sval = abstract_expr_i(*(RawTree*)raw.nodes.data[1], env, a, point);

        ClauseArray clauses = mk_ptr_array(raw.nodes.len - 2, a);
        
        for (size_t i = 2; i < raw.nodes.len; i++) {
            // For each clause, we need three things:
            // 1. The tag, corresponding to the enum
            // 2. The variable(s) destructuring the enum
            // 3. The body
            // Get the clause
            RawTree* raw_clause = (RawTree*)raw.nodes.data[i];
            if (raw_clause->type != RawList || raw_clause->nodes.len < 2) {
                throw_error(point, mv_string("Match Clause has incorrect number of elements!"));
            }

            // Get the pattern
            RawTree* raw_pattern = (RawTree*)raw_clause->nodes.data[0];
            if (raw_pattern->type != RawList) {
                throw_error(point, mv_string("Match Pattern should be a list!"));
            }

            // The pattern has two parts: the variables & the tag
            // The tag should be in a constructor (i.e. list)
            Symbol clause_tagname; 
            if (!get_fieldname(raw_pattern->nodes.data[0], &clause_tagname)) {
                throw_error(point, mv_string("Unable to get tagname in pattern."));
            }

            SymbolArray clause_binds = mk_u64_array(raw_pattern->nodes.len - 1, a);
            for (size_t s = 1; s < raw_pattern->nodes.len; s++) {
                RawTree* raw_name = raw_pattern->nodes.data[s];
                if (!is_symbol(raw_name)) {
                    throw_error(point, mv_string("Pattern binding was not a symbol!"));
                }
                push_u64(raw_name->atom.symbol, &clause_binds); 
            }

            // Get the term
            RawTree* raw_term;
            if (raw_clause->nodes.len == 2) {
                raw_term = (RawTree*)raw.nodes.data[2];
            } else {
                raw_term = mem_alloc (sizeof(RawTree), a);

                raw_term->nodes.len = raw.nodes.len - 2;
                raw_term->nodes.size = raw.nodes.size - 2;
                raw_term->nodes.data = raw.nodes.data + 2;
                raw_term->type = RawList;
            }

            Syntax* clause_body = abstract_expr_i(*(RawTree*)raw_clause->nodes.data[1], env, a, point);

            SynClause* clause = mem_alloc(sizeof(SynClause), a);
            *clause = (SynClause) {
                .tagname = clause_tagname,
                .vars = clause_binds,
                .body = clause_body,
            };
            push_ptr(clause, &clauses);
        }

        *res = (Syntax) {
            .type = SMatch,
            .match.val = sval,
            .match.clauses = clauses,
        };
        break;
    }
    case FStructure: {
        if (raw.nodes.len < 2) {
            throw_error(point, mk_string("Structure term former needs a structure type argument", a));
        }

        // Get the type of the structure
        Syntax* stype = abstract_expr_i(*(RawTree*)raw.nodes.data[1], env, a, point);

        // Construct a structure
        SymPtrAMap fields = mk_sym_ptr_amap(raw.nodes.len, a);
        for (size_t i = 2; i < raw.nodes.len; i++) {
            RawTree* fdesc = raw.nodes.data[i];
            if (fdesc->type != RawList) {
                throw_error(point, mv_string("Structure expects all field descriptors to be lists."));
            }
            
            if (fdesc->nodes.len != 2) {
                throw_error(point, mv_string("Structure expects all field descriptors to have 2 elements."));
            }

            Symbol field;
            if (!get_fieldname(fdesc->nodes.data[0], &field)) {
                throw_error(point, mv_string("Structure has malformed field name."));
            }

            Syntax* syn = abstract_expr_i(*(RawTree*) fdesc->nodes.data[1], env, a, point);

            sym_ptr_insert(field, syn, &fields);
        }

        *res = (Syntax) {
            .type = SStructure,
            .structure.ptype = stype,
            .structure.fields = fields,
        };
        break;
    }
    case FProjector: {
        if (raw.nodes.len != 3) {
            throw_error(point, mk_string("Projection term former needs two arguments!", a));
        }
        // Get the structure portion of the proector 
        Syntax* structure = abstract_expr_i(*(RawTree*)raw.nodes.data[2], env, a, point);
        
        // Get the symbol portion of the projector
        RawTree* msym = (RawTree*)raw.nodes.data[1];
        if (msym->type != RawAtom && msym->atom.type != ASymbol) {
            throw_error(point, mv_string("Second argument to projection term former should be symbol"));
        }

        *res = (Syntax) {
            .type = SProjector,
            .projector.field = msym->atom.symbol,
            .projector.val = structure,
        };
        break;
    }
    case FIf: {
        if (raw.nodes.len != 4) {
            throw_error(point, mv_string("Term former 'if' expects precisely 3 arguments!"));
        }
        SynArray terms = mk_ptr_array(3, a);
        for (size_t i = 1; i < raw.nodes.len; i++) {
            RawTree* rptr = raw.nodes.data[i];
            Syntax* term = abstract_expr_i(*rptr, env, a, point);
            push_ptr(term, &terms);
        }

        *res = (Syntax) {
            .type = SIf,
            .if_expr.condition = terms.data[0],
            .if_expr.true_branch = terms.data[1],
            .if_expr.false_branch = terms.data[2],
        };
        break;
    }
    case FIs: {
        if (raw.nodes.len != 3) {
            throw_error(point, mv_string("Term former 'is' expects precisely 2 arguments!"));
        }

        Syntax* term = abstract_expr_i(*(RawTree*)raw.nodes.data[1], env, a, point);
        Syntax* type = abstract_expr_i(*(RawTree*)raw.nodes.data[2], env, a, point);
        
        *res = (Syntax) {
            .type = SIs,
            .is = {.val = term, .type = type},
        };
        break;
    }
    case FLet: {
        throw_error(point, mv_string("Let term former not implemented!"));
        break;
    }
    case FProcType: {
        if (raw.nodes.len != 3) {
            throw_error(point, mv_string("Wrong number of terms to proc type former."));
        }

        RawTree* raw_args = raw.nodes.data[1];
        if (raw_args->type != RawList) {
            throw_error(point, mv_string("Procedure argument list should be list."));
        }
        
        PtrArray arg_types = mk_ptr_array(raw_args->nodes.len, a);

        for (size_t i = 0; i < raw_args->nodes.len; i++) {
            RawTree* raw_arg = raw_args->nodes.data[i];
            Syntax* arg_ty = abstract_expr_i(*raw_arg, env, a, point);

            push_ptr(arg_ty, &arg_types);
        }

        RawTree* raw_return = raw.nodes.data[2]; 
        Syntax* return_type = abstract_expr_i(*raw_return, env, a, point);

        *res = (Syntax) {
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
                throw_error(point, mv_string("Structure type expects all field descriptors to be lists."));
            };
            
            if (fdesc->nodes.len != 2) {
                throw_error(point, mv_string("Structure type expects all field descriptors to have 2 elements."));
            };

            Symbol field;
            if (!get_fieldname(fdesc->nodes.data[0], &field)) {
                throw_error(point, mv_string("Structure type has malformed field name."));
            };

            Syntax* field_ty = abstract_expr_i(*(RawTree*) fdesc->nodes.data[1], env, a, point);

            sym_ptr_insert(field, field_ty, &field_types);
        }

        *res = (Syntax) {
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
                throw_error(point, mv_string("Enumeration type expects all enum descriptors to be lists."));
            };
            
            if (edesc->nodes.len < 1) {
                throw_error(point, mv_string("Enumeration type expects all enum descriptors to have at least 1 elements."));
            };

            Symbol tagname;
            PtrArray* types = mem_alloc(sizeof(PtrArray), a);
            *types = mk_ptr_array(edesc->nodes.len - 1, a);

            if (!get_fieldname(edesc->nodes.data[0], &tagname)) {
                throw_error(point, mv_string("Enum type has malformed field name."));
                return res;
            };

            for (size_t i = 1; i < edesc->nodes.len; i++) {
                Syntax* field_ty = abstract_expr_i(*(RawTree*) edesc->nodes.data[i], env, a, point);
                push_ptr(field_ty, types);
            }

            sym_ptr_insert(tagname, types, &enum_variants);
        }

        *res = (Syntax) {
            .type = SEnumType,
            .enum_type.variants = enum_variants,
        };
        break;
    }
    default:
        throw_error(point, mv_string("Internal Error: Invalid term former!"));
    }
    return res;
}

Syntax* abstract_expr_i(RawTree raw, ShadowEnv* env, Allocator* a, ErrorPoint* point) {
    Syntax* res = mem_alloc(sizeof(Syntax), a);
    // Resolution does not perform type analysis, so we set the type pointer to NULL
    // This is then resolved in the typechecking stage;
    res->ptype = NULL;
    switch (raw.type) {
    case RawAtom: {
        if (raw.atom.type == ASymbol) {
            *res = (Syntax) {
                .type = SVariable,
                .variable = raw.atom.symbol,
            };
        }
        else if (raw.atom.type == AIntegral) {
            *res = (Syntax) {
                .type = SLitUntypedIntegral,
                .integral.value = raw.atom.int_64,
            };
        }
        else if (raw.atom.type == ABool) {
            *res = (Syntax) {
                .type = SLitBool,
                .boolean = (bool) raw.atom.int_64,
            };
        } else  {
            throw_error(point, mk_string("Currently, can only make literal values out of type i64." , a));
        }
        break;
    }
    case RawList: {
        // Currently, can only have function calls, so all Raw lists compile down to an application
        if (raw.nodes.len < 1) {
            throw_error(point, mk_string("Raw Syntax must have at least one element!", a));
        }
        if (is_symbol(raw.nodes.data[0])) {
            Symbol sym = ((RawTree*)raw.nodes.data[0])->atom.symbol;
            ShadowEntry entry = shadow_env_lookup(sym, env);
            switch (entry.type) {
            case SErr:
                throw_error(point, mk_string("Can't find symbol! ", a));
                break;
            case SShadowed: 
                return mk_application(raw, env, a, point);
                break;
            case SLocal: 
                throw_error(point, mk_string("Higher kinded types not currently supported!", a));
                break;
            case SGlobal:
                if (entry.vtype->sort == TPrim && entry.vtype->prim == TFormer) {
                    return mk_term(*((TermFormer*)entry.value), raw, env, a, point);
                } else {
                    return mk_application(raw, env, a, point);
                }
            }
        }
        else {
            return mk_application(raw, env, a, point);
        }
        break;
    }
    default: {
        throw_error(point, mk_string("Name Resolution Received an invalid Raw Syntax Tree", a));
    }
    }
    return res;
}

TopLevel mk_toplevel(TermFormer former, RawTree raw, ShadowEnv* env, Allocator* a, ErrorPoint* point) {
    TopLevel res;
    switch (former) {
    case FDefine: {
        if (raw.nodes.len < 3) {
            throw_error(point, mk_string("Definitions expect at least 2 terms", a));
        }

        if (!is_symbol(raw.nodes.data[1])) {
            throw_error(point, mk_string("First argument to definitions should be a symbol", a));
        }

        Symbol sym = ((RawTree*)raw.nodes.data[1])->atom.symbol;
        
        RawTree* raw_term;
        if (raw.nodes.len == 3) {
            raw_term = raw.nodes.data[2];
        } else {
            raw_term = mem_alloc (sizeof(RawTree), a);

            raw_term->nodes.len = raw.nodes.len - 2;
            raw_term->nodes.size = raw.nodes.size - 2;
            raw_term->nodes.data = raw.nodes.data + 2;
            raw_term->type = RawList;
        }

        shadow_var(sym, env);
        Syntax* term = abstract_expr_i(*raw_term, env, a, point);
        shadow_pop(1, env);

        res = (TopLevel) {
            .type = TLDef,
            .def.bind = sym,
            .def.value = term,
        };

        break;
    }
    default:
        res = (TopLevel) {
            .type = TLExpr,
            .expr = mk_term(former, raw, env, a, point),
        };
        break;
    }
    return res;
}

TopLevel abstract_i(RawTree raw, ShadowEnv* env, Allocator* a, ErrorPoint* point) {
    // first: try a unique toplevel-form
    bool unique_toplevel = false;

    if (raw.type == RawList && raw.nodes.len > 1) {
        if (is_symbol(raw.nodes.data[0])) {
            Symbol sym = ((RawTree*)raw.nodes.data[0])->atom.symbol;
            ShadowEntry entry = shadow_env_lookup(sym, env);
            switch (entry.type) {
            case SGlobal: {
                if (entry.vtype->sort == TPrim && entry.vtype->prim == TFormer) {
                    unique_toplevel = true;
                    return mk_toplevel(*((TermFormer*)entry.value), raw, env, a, point);
                }
                break;
            }
            default:
                break;
            }
        }
    } 

    if (!unique_toplevel) {
        return (TopLevel) {
            .type = TLExpr,
            .expr = abstract_expr_i(raw, env, a, point),
        };
    }

    throw_error(point, mk_string("Logic error in abstract_i: reached unreachable area.", a));
}

Syntax* abstract_expr(RawTree raw, Environment* env, Allocator* a, ErrorPoint* point) {
    ShadowEnv* s_env = mk_shadow_env(a, env);
    Syntax* out = abstract_expr_i(raw, s_env, a, point);
    return out; 
}

TopLevel abstract(RawTree raw, Environment* env, Allocator* a, ErrorPoint* point) {
    ShadowEnv* s_env = mk_shadow_env(a, env);
    TopLevel out = abstract_i(raw, s_env, a, point);
    return out;
}
