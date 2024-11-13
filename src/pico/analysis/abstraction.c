#include "pico/analysis/abstraction.h"

#include "platform/signals.h"
#include "data/result.h"
#include "data/string.h"
#include "pico/values/values.h"
#include "pico/syntax/concrete.h"
#include "pico/syntax/syntax.h"
#include "pico/binding/shadow_env.h"

// Internal functions declarations needed for interface implementation
Syntax* abstract_expr_i(RawTree raw, ShadowEnv* env, Allocator* a, ErrorPoint* point);
TopLevel abstract_i(RawTree raw, ShadowEnv* env, Allocator* a, ErrorPoint* point);
bool eq_symbol(RawTree* raw, Symbol s);
bool is_symbol(RawTree* raw);
Imports abstract_imports(RawTree* raw, Allocator* a, ErrorPoint* point);
Exports abstract_exports(RawTree* raw, Allocator* a, ErrorPoint* point);
ImportClause abstract_import_clause(RawTree* raw, Allocator* a, ErrorPoint* point);
ExportClause abstract_export_clause(RawTree* raw, Allocator* a, ErrorPoint* point);

//------------------------------------------------------------------------------
// Interface Implementation
//------------------------------------------------------------------------------

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

ModuleHeader* abstract_header(RawTree raw, Allocator* a, ErrorPoint* point) {
    if (raw.type != RawList)
        throw_error(point, mv_string("Expected module header to be list."));

    // Expected format:
    // (module <modulename>
    //   (import import-clause*)?
    //   (export import-clause*)?)
    size_t idx = 0;
    if (raw.nodes.len <= idx) 
        throw_error(point, mv_string("Expecting keyword 'module' in module header. Got nothing!"));
    if (raw.nodes.len > 4)
        throw_error(point, mv_string("Too many parameters in module header."));

    if (!eq_symbol(raw.nodes.data[0], string_to_symbol(mv_string("module"))))
        throw_error(point, mv_string("Expecting keyword 'module' in module header."));

    idx++;
    if (raw.nodes.len <= idx) 
        throw_error(point, mv_string("Expecting parameter 'modulename' in module header. Got nothing!"));

    if (!is_symbol(raw.nodes.data[1]))
        throw_error(point, mv_string("Expecting parameter 'modulename' in module header."));
    Symbol module_name = ((RawTree*)raw.nodes.data[1])->atom.symbol;

    // Now, imports/exports
    Imports imports;
    Exports exports;
    exports.export_all = false;

    idx++;
    if (raw.nodes.len <= idx) {
        imports.clauses = mk_import_clause_array(0, a);
        exports.clauses = mk_export_clause_array(0, a);
    } else if (raw.nodes.len <= idx+1){
        RawTree* clauses_1 = raw.nodes.data[2];
        if (clauses_1->type != RawList)
            throw_error(point, mv_string("Expecting import/export list."));
        if (clauses_1->nodes.len < 1)
            throw_error(point, mv_string("Not enough elements in import/export list"));

        if (eq_symbol(clauses_1->nodes.data[0], string_to_symbol(mv_string("import")))) {
            imports.clauses = mk_import_clause_array(clauses_1->nodes.len - 1, a);
            exports.clauses = mk_export_clause_array(0, a);

            for (size_t i = 1; i < clauses_1->nodes.len; i++) {
                ImportClause clause = abstract_import_clause(clauses_1->nodes.data[i], a, point);
                push_import_clause(clause, &imports.clauses);
            }
        } else if (eq_symbol(clauses_1->nodes.data[0], string_to_symbol(mv_string("export")))) {
            imports.clauses = mk_import_clause_array(0, a);
            exports.clauses = mk_export_clause_array(clauses_1->nodes.len - 1, a);

            for (size_t i = 1; i < clauses_1->nodes.len; i++) {
                ExportClause clause = abstract_export_clause(clauses_1->nodes.data[i], a, point);
                push_export_clause(clause, &exports.clauses);
            }
        } else {
            throw_error(point, mv_string("Expecting import/export list header."));
        }
    } else {
        RawTree* clauses_1 = raw.nodes.data[2];
        if (clauses_1->type != RawList)
            throw_error(point, mv_string("Expecting import list."));
        if (clauses_1->nodes.len < 1)
            throw_error(point, mv_string("Not enough elements in import list"));
        if (!eq_symbol(clauses_1->nodes.data[0], string_to_symbol(mv_string("import"))))
            throw_error(point, mv_string("Expecting 'import' at head of import list"));

        imports.clauses = mk_import_clause_array(clauses_1->nodes.len - 1, a);

        for (size_t i = 1; i < clauses_1->nodes.len; i++) {
        }

        RawTree* clauses_2 = raw.nodes.data[3];
        if (clauses_2->type != RawList)
            throw_error(point, mv_string("Expecting export list."));
        if (clauses_2->nodes.len < 1)
            throw_error(point, mv_string("Not enough elements in export list"));
        if (!eq_symbol(clauses_2->nodes.data[0], string_to_symbol(mv_string("export"))))
            throw_error(point, mv_string("Expecting 'export' at head of export list"));

        for (size_t i = 1; i < clauses_2->nodes.len; i++) {
        }

        exports.clauses = mk_export_clause_array(clauses_2->nodes.len - 1, a);
    }

    ModuleHeader* out = mem_alloc(sizeof(ModuleHeader), a);
    *out = (ModuleHeader) {
        .name = module_name,
        .imports = imports,
        .exports = exports,
    };
    return out;
}

//------------------------------------------------------------------------------
// Internal Implementation
//------------------------------------------------------------------------------

bool eq_symbol(RawTree* raw, Symbol s) {
    return (raw->type == RawAtom
            && raw->atom.type == ASymbol
            && raw->atom.symbol == s);
}

bool is_symbol(RawTree* raw) {
    return (raw->type == RawAtom && raw->atom.type == ASymbol);
}

//  Helper function for various struct-related, helpers, when handling 
//  [. fieldname] clauses
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

// Helper function for labels, when we are expecting [label expr] clauses
// returns true on success, false on failure
bool get_label(RawTree* raw, Symbol* fieldname) {
    if (raw->type == RawList && raw->nodes.len == 2) {
        raw = raw->nodes.data[0];
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

// Helper function for retrieving a symbol list
// returns true on success, false on failure
bool get_symbol_list(SymbolArray* arr, RawTree nodes) {
    if (nodes.type != RawList) { return false; }

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
    } else if (raw.nodes.len > 1
               && ((RawTree*)raw.nodes.data[1])->type == RawList
               && ((RawTree*)raw.nodes.data[1])->hint == HImplicit) {
        RawTree typelist = *(RawTree*)raw.nodes.data[1];

        *res = (Syntax) {
            .type = SAllApplication,
            .all_application.function = mem_alloc(sizeof(Syntax), a),
            .all_application.types = mk_ptr_array(typelist.nodes.len, a),
            .all_application.args = mk_ptr_array(raw.nodes.len - 2, a),
        };
        res->all_application.function = fn_syn;

        for (size_t i = 0; i < typelist.nodes.len; i++) {
            Syntax* type = abstract_expr_i(*(RawTree*)(typelist.nodes.data[i]), env, a, point);
            push_ptr(type, &res->all_application.types);
        }

        for (size_t i = 2; i < raw.nodes.len; i++) {
            Syntax* arg = abstract_expr_i(*(RawTree*)(raw.nodes.data[i]), env, a, point);
            push_ptr(arg, &res->all_application.args);
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
            } else {
                throw_error(point, mv_string("Variant term former needs two arguments!"));
            }
        }
        // : sym Type

        else if (raw.nodes.len != 3) {
            throw_error(point, mv_string("Variant term former needs two arguments!"));
        } else {

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
        }
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
    case FLet: {
        throw_error(point, mv_string("Term former 'let' not implemented!"));
        break;
    }
    case FIf: {
        if (raw.nodes.len != 4) {
            throw_error(point, mv_string("Term former 'if' expects precisely 3 arguments!"));
        }
        SynArray terms = mk_ptr_array(3, a);
        for (size_t i = 1; i < raw.nodes.len; i++) {
            Syntax* term = abstract_expr_i(*(RawTree*)raw.nodes.data[i], env, a, point);
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
    case FLabels: {
        if (raw.nodes.len < 2) {
            throw_error(point, mv_string("Term former 'labels' expects at least 1 argument!"));
        }

        Syntax* entry = abstract_expr_i(*(RawTree*)raw.nodes.data[1], env, a, point);

        SymPtrAssoc terms = mk_sym_ptr_assoc(raw.nodes.len - 2, a);
        for (size_t i = 2; i < raw.nodes.len; i++) {
            Symbol label;
            RawTree* label_expr = raw.nodes.data[i];
            if (!get_label(label_expr, &label)) {
                throw_error(point, mv_string("Each label must be of the form [label expr]"));
            }
            Syntax* res = abstract_expr_i(*(RawTree*)label_expr->nodes.data[1], env, a, point);
            sym_ptr_bind(label, res, &terms);
        }

        *res = (Syntax) {
            .type = SLabels,
            .labels.entry = entry,
            .labels.terms = terms,
        };
        break;
    }
    case FGoTo: {
        if (raw.nodes.len != 2) {
            throw_error(point, mv_string("Term former 'go-to' expects one argument!"));
        }
        RawTree* label = raw.nodes.data[1]; 

        if (label->type != RawAtom && label->atom.type != ASymbol) {
            throw_error(point, mv_string("Term former 'go-to' expects one argument!"));
        }

        *res = (Syntax) {
            .type = SGoTo,
            .go_to.label = label->atom.symbol,
        };
        break;
    }
    case FWithReset: {
        // with-reset [lbl] e [l1 l2] e
        if (raw.nodes.len != 5) {
          throw_error(point, mv_string("Term former 'with-reset' expects exactly 5 arguments!"));
        }
        SymbolArray reset_binds = mk_u64_array(1, a);
        SymbolArray handle_binds = mk_u64_array(2, a);

        if (!get_symbol_list(&reset_binds, *(RawTree*) raw.nodes.data[1]) || reset_binds.len != 1) {
          throw_error(point, mv_string("Term former 'with-reset' 1st argument list malformed."));
        }

        Symbol reset_point_sym = reset_binds.data[0];
        if (reset_binds.len != 1) {
            throw_error(point, mv_string("Term former 'with-reset' expects exactly 5 arguments!"));
        }

        Syntax* expr = abstract_expr_i(*(RawTree*)raw.nodes.data[2], env, a, point);

        if (!get_symbol_list(&handle_binds, *(RawTree*) raw.nodes.data[3]) || handle_binds.len != 3) {
            throw_error(point, mv_string("Handler list malformed!"));
        }

        Symbol in_sym = handle_binds.data[1];
        Symbol cont_sym = handle_binds.data[2];

        Syntax* handler = abstract_expr_i(*(RawTree*)raw.nodes.data[4], env, a, point);

        *res = (Syntax) {
            .type = SWithReset,
            .with_reset.point_sym = reset_point_sym,
            .with_reset.expr = expr,
            .with_reset.in_sym = in_sym,
            .with_reset.cont_sym = cont_sym,
            .with_reset.handler = handler,
        };
        break;
    }
    case FResetTo: {
        if (raw.nodes.len != 3) {
            throw_error(point, mv_string("Term former 'reset-to' expects two arguments!"));
        }

        Syntax* rpoint = abstract_expr_i(*(RawTree*)raw.nodes.data[1], env, a, point);
        Syntax* rarg = abstract_expr_i(*(RawTree*)raw.nodes.data[2], env, a, point);
        *res = (Syntax) {
            .type = SResetTo,
            .reset_to.point = rpoint,
            .reset_to.arg = rarg,
        };
        break;
    }
    case FSequence: {
        SynArray terms = mk_ptr_array(raw.nodes.len - 1, a);
        for (size_t i = 1; i < raw.nodes.len; i++) {
            push_ptr(abstract_expr_i(*(RawTree*)raw.nodes.data[i], env, a, point), &terms);
        }
        
        *res = (Syntax) {
            .type = SSequence,
            .sequence.terms = terms,
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
    case FResetType: {
        if (raw.nodes.len != 3) {
            throw_error(point, mv_string("Reset type former expects exactly 2 arguments!"));
        }
        Syntax* in_ty = abstract_expr_i(*(RawTree*) raw.nodes.data[1], env, a, point);
        Syntax* out_ty = abstract_expr_i(*(RawTree*) raw.nodes.data[2], env, a, point);

        *res = (Syntax) {
            .type = SResetType,
            .reset_type.in = in_ty,
            .reset_type.out = out_ty,
        };
        break;
    }
    case FAllType: {
        throw_error(point, mv_string("Abstract for 'All' not implemented!"));
        break;
    }
    default:
        panic(mv_string("Internal Error: Invalid term former!"));
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
        switch(raw.atom.type) {
        case ASymbol: {
            *res = (Syntax) {
                .type = SVariable,
                .variable = raw.atom.symbol,
            };
            break;
        }
        case AIntegral: {
            *res = (Syntax) {
                .type = SLitUntypedIntegral,
                .integral.value = raw.atom.int_64,
            };
            break;
        }
        case ABool: {
            *res = (Syntax) {
                .type = SLitBool,
                .boolean = (bool) raw.atom.int_64,
            };
            break;
        }
        case AString: {
            *res = (Syntax) {
                .type = SLitString,
                .string = raw.atom.string,
            };
            break;
        }
        default:
            panic(mv_string("Don't know how to make a literal from this atom!."));
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
                throw_error(point, string_cat(mv_string("Can't find symbol: "), *symbol_to_string(sym) , a));
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
        panic(mv_string("Internal Error: Name Resolution Received an invalid Raw Syntax Tree"));
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

ImportClause abstract_import_clause(RawTree* raw, Allocator* a, ErrorPoint* point) {
    if (is_symbol(raw)) {
        return (ImportClause) {
            .type = ImportName,
            .name = raw->atom.symbol,
        };
    } else if (raw->type == RawList) {
        // Possibilities:
        // (name1 :all)
        // (name1 :as name2)
        // (. name2 name1)
        // (. (list-of-names) name1)
        if (raw->nodes.len == 2) {
            if (!is_symbol(raw->nodes.data[0]))
                throw_error(point, mv_string("Invalid import clause: first element should be symbol"));
            Symbol name = ((RawTree*)raw->nodes.data[0])->atom.symbol;

            
            Symbol middle;
            if(!get_fieldname(raw->nodes.data[1], &middle))
                throw_error(point, mv_string("Invalid import clause - expected :all"));
            if (middle != string_to_symbol(mv_string("all")))
                throw_error(point, mv_string("Invalid import clause - expected :all"));

            return (ImportClause) {
                .type = ImportPathAll,
                .name = name,
            };
        } else if (raw->nodes.len == 3) {

            if (!is_symbol(raw->nodes.data[0]) || !is_symbol(raw->nodes.data[2]))
                throw_error(point, mv_string("Invalid import clause"));

            // Check for '.'
            if (eq_symbol(raw->nodes.data[0], string_to_symbol(mv_string(".")))) {
                Symbol src;
                if(!get_fieldname(raw->nodes.data[2], &src))
                    throw_error(point, mv_string("Invalid import-. source"));

                RawTree* raw_members = raw->nodes.data[1];
                if (is_symbol(raw_members)) {
                    return (ImportClause) {
                        .type = ImportPath,
                        .name = src,
                        .member = raw_members->atom.symbol,
                    };
                } else if (raw_members->type == RawList) {
                    SymbolArray members = mk_u64_array(raw_members->nodes.len, a);
                    if (!get_symbol_list(&members, *raw_members))
                        throw_error(point, mv_string("Invalid import-. members"));
                    return (ImportClause) {
                        .type = ImportPathMany,
                        .name = src,
                        .members = members,
                    };
                } else {
                    throw_error(point, mv_string("Invalid import-. member(s)"));
                }

            } else {
                Symbol middle;
                if(!get_fieldname(raw->nodes.data[1], &middle))
                    throw_error(point, mv_string("Invalid import clause"));
                if (middle != string_to_symbol(mv_string("as")))
                    throw_error(point, mv_string("Invalid import clause"));

                Symbol name;
                Symbol rename;
                if(!get_fieldname(raw->nodes.data[0], &name))
                    throw_error(point, mv_string("Invalid import-as name"));
                if(!get_fieldname(raw->nodes.data[0], &rename))
                    throw_error(point, mv_string("Invalid import-as new name"));
                return (ImportClause) {
                    .type = ImportNameAs,
                    .name = name,
                    .rename = rename,
                };
            }
        } else {
            throw_error(point, mv_string("Invalid import clause - incorrect number of itesm"));
        }
    } else {
        throw_error(point, mv_string("Invalid import clause - is atom!"));
    }
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
ExportClause abstract_export_clause(RawTree* raw, Allocator* a, ErrorPoint* point) {
    if (is_symbol(raw)) {
        return (ExportClause) {
            .type = ExportName,
            .name = raw->atom.symbol,
        };
    } else if (raw->type == RawList) {
        // Export as
        // looks like (<symbol/name> (<symbol/:> <symbol/as>) <symbol/name>)

        if (raw->nodes.len != 3)
            throw_error(point, mv_string("Invalid export clause"));

        Symbol middle;
        if(!get_fieldname(raw->nodes.data[1], &middle))
            throw_error(point, mv_string("Invalid export clause"));
        if (middle != string_to_symbol(mv_string("as")))
            throw_error(point, mv_string("Invalid export clause"));

        Symbol name;
        Symbol rename;
        if(!get_fieldname(raw->nodes.data[0], &name))
            throw_error(point, mv_string("Invalid export-as name"));
        if(!get_fieldname(raw->nodes.data[0], &rename))
            throw_error(point, mv_string("Invalid export-as new name"));

        return (ExportClause) {
            .type = ExportNameAs,
            .name = name,
            .rename = rename,
        };
    } else {
        throw_error(point, mv_string("Invalid export clause"));
    }
}
#pragma GCC diagnostic pop
