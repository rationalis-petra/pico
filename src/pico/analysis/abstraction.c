#include "platform/machine_info.h"

#include "platform/signals.h"
#include "data/result.h"
#include "data/string.h"
#include "pretty/string_printer.h"

#include "pico/values/values.h"
#include "pico/stdlib/extra.h"
#include "pico/syntax/concrete.h"
#include "pico/syntax/syntax.h"
#include "pico/binding/shadow_env.h"
#include "pico/analysis/abstraction.h"

// Internal functions declarations needed for interface implementation
Syntax* abstract_expr_i(RawTree raw, ShadowEnv* env, Allocator* a, ErrorPoint* point);
TopLevel abstract_i(RawTree raw, ShadowEnv* env, Allocator* a, ErrorPoint* point);

Syntax* mk_term(TermFormer former, RawTree raw, ShadowEnv* env, Allocator* a, ErrorPoint* point);
bool eq_symbol(RawTree* raw, Symbol s);
bool is_symbol(RawTree* raw);
RawTree* raw_slice(RawTree* raw, size_t drop, Allocator* a);
Module* try_get_module(Syntax* syn, ShadowEnv* env);

Imports abstract_imports(RawTree* raw, Allocator* a, ErrorPoint* point);
Exports abstract_exports(RawTree* raw, Allocator* a, ErrorPoint* point);
ImportClause abstract_import_clause(RawTree* raw, Allocator* a, ErrorPoint* point);
ExportClause abstract_export_clause(RawTree* raw, ErrorPoint* point);

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
    if (raw.type != RawBranch)
        throw_error(point, mv_string("Expected module header to be list."));

    // Expected format:
    // (module <modulename>
    //   (import import-clause*)?
    //   (export import-clause*)?)
    size_t idx = 0;
    if (raw.branch.nodes.len <= idx) 
        throw_error(point, mv_string("Expecting keyword 'module' in module header. Got nothing!"));
    if (raw.branch.nodes.len > 4)
        throw_error(point, mv_string("Too many parameters in module header."));

    if (!eq_symbol(raw.branch.nodes.data[0], string_to_symbol(mv_string("module"))))
        throw_error(point, mv_string("Expecting keyword 'module' in module header."));

    idx++;
    if (raw.branch.nodes.len <= idx) 
        throw_error(point, mv_string("Expecting parameter 'modulename' in module header. Got nothing!"));

    if (!is_symbol(raw.branch.nodes.data[1]))
        throw_error(point, mv_string("Expecting parameter 'modulename' in module header."));
    Symbol module_name = ((RawTree*)raw.branch.nodes.data[1])->atom.symbol;

    // Now, imports/exports
    Imports imports;
    Exports exports;
    exports.export_all = false;

    idx++;
    if (raw.branch.nodes.len <= idx) {
        imports.clauses = mk_import_clause_array(0, a);
        exports.clauses = mk_export_clause_array(0, a);
    } else if (raw.branch.nodes.len <= idx+1){
        RawTree* clauses_1 = raw.branch.nodes.data[2];
        if (clauses_1->type != RawBranch)
            throw_error(point, mv_string("Expecting import/export list."));
        if (clauses_1->branch.nodes.len < 1)
            throw_error(point, mv_string("Not enough elements in import/export list"));

        if (eq_symbol(clauses_1->branch.nodes.data[0], string_to_symbol(mv_string("import")))) {
            imports.clauses = mk_import_clause_array(clauses_1->branch.nodes.len - 1, a);
            exports.clauses = mk_export_clause_array(0, a);

            for (size_t i = 1; i < clauses_1->branch.nodes.len; i++) {
                ImportClause clause = abstract_import_clause(clauses_1->branch.nodes.data[i], a, point);
                push_import_clause(clause, &imports.clauses);
            }
        } else if (eq_symbol(clauses_1->branch.nodes.data[0], string_to_symbol(mv_string("export")))) {
            imports.clauses = mk_import_clause_array(0, a);
            exports.clauses = mk_export_clause_array(clauses_1->branch.nodes.len - 1, a);

            for (size_t i = 1; i < clauses_1->branch.nodes.len; i++) {
                ExportClause clause = abstract_export_clause(clauses_1->branch.nodes.data[i], point);
                push_export_clause(clause, &exports.clauses);
            }
        } else {
            throw_error(point, mv_string("Expecting import/export list header."));
        }
    } else {
        RawTree* clauses_1 = raw.branch.nodes.data[2];
        if (clauses_1->type != RawBranch)
            throw_error(point, mv_string("Expecting import list."));
        if (clauses_1->branch.nodes.len < 1)
            throw_error(point, mv_string("Not enough elements in import list"));
        if (!eq_symbol(clauses_1->branch.nodes.data[0], string_to_symbol(mv_string("import"))))
            throw_error(point, mv_string("Expecting 'import' at head of import list"));

        imports.clauses = mk_import_clause_array(clauses_1->branch.nodes.len - 1, a);

        for (size_t i = 1; i < clauses_1->branch.nodes.len; i++) {
        }

        RawTree* clauses_2 = raw.branch.nodes.data[3];
        if (clauses_2->type != RawBranch)
            throw_error(point, mv_string("Expecting export list."));
        if (clauses_2->branch.nodes.len < 1)
            throw_error(point, mv_string("Not enough elements in export list"));
        if (!eq_symbol(clauses_2->branch.nodes.data[0], string_to_symbol(mv_string("export"))))
            throw_error(point, mv_string("Expecting 'export' at head of export list"));

        for (size_t i = 1; i < clauses_2->branch.nodes.len; i++) {
        }

        exports.clauses = mk_export_clause_array(clauses_2->branch.nodes.len - 1, a);
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

RawTree* raw_slice(RawTree* raw, size_t drop, Allocator* a) {
    RawTree* out = mem_alloc(sizeof(RawTree), a);
    *out = (RawTree) {
        .type = RawBranch,
        .branch.hint = raw->branch.hint,
        .branch.nodes.len = raw->branch.nodes.len - drop,
        .branch.nodes.size = raw->branch.nodes.size - drop,
        .branch.nodes.data = raw->branch.nodes.data + drop,
        .branch.nodes.gpa = raw->branch.nodes.gpa,
    };
    return out;
}
                                                            

//  Helper function for various struct-related, helpers, when handling 
//  [. fieldname] clauses
bool get_fieldname(RawTree* raw, Symbol* fieldname) {
    if (raw->type == RawBranch && raw->branch.nodes.len == 2) {
        raw = raw->branch.nodes.data[1];
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
    if (raw->type == RawBranch && raw->branch.nodes.len == 2) {
        raw = raw->branch.nodes.data[0];
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
    if (nodes.type != RawBranch) { return false; }

    for (size_t i = 0; i < nodes.branch.nodes.len; i++) {
        RawTree* node = nodes.branch.nodes.data[i];
        if (node->type != RawAtom || node->atom.type != ASymbol) { return false; }
        push_u64(node->atom.symbol, arr);
    }
    return true;
}

Result get_annotated_symbol_list(SymPtrAssoc *args, RawTree list, ShadowEnv* env, Allocator* a, ErrorPoint* point) {
    Result error_result = {.type = Err, .error_message = mv_string("Malformed proc argument list.")};
    if (list.type != RawBranch) { return error_result; }

    for (size_t i = 0; i < list.branch.nodes.len; i++) {
        RawTree* annotation = list.branch.nodes.data[i];
        if (annotation->type == RawAtom) {
            sym_ptr_bind(annotation->atom.symbol, NULL, args);
        } else if (annotation->type == RawBranch || annotation->branch.nodes.len == 2) {
            RawTree* arg = annotation->branch.nodes.data[0];
            RawTree* raw_type = annotation->branch.nodes.data[1];
            if (arg->type != RawAtom || arg->atom.type != ASymbol) { return error_result; }
            Syntax* type = abstract_expr_i(*raw_type, env, a, point); 

            sym_ptr_bind(arg->atom.symbol, type, args);
        } else { return error_result; }
    }
    return (Result) {.type = Ok};
}

Syntax* mk_application(RawTree raw, ShadowEnv* env, Allocator* a, ErrorPoint* point) {
    Syntax* fn_syn = abstract_expr_i(*(RawTree*)(raw.branch.nodes.data[0]), env, a, point);

    // TODO (IMPROVEMENT): make sure that ptype not null
    if (fn_syn->type == SAbsVariable && fn_syn->ptype->sort == TPrim&& fn_syn->ptype->prim == TFormer) {
        return mk_term(*((TermFormer*)fn_syn->abvar.value), raw, env, a, point);
    }

    Syntax* res = mem_alloc(sizeof(Syntax), a);
    if (fn_syn->type == SConstructor) {
        *res = (Syntax) {
            .type = SVariant,
            .ptype = NULL,
            .variant.enum_type = fn_syn->constructor.enum_type,
            .variant.tagname = fn_syn->constructor.tagname,
            .variant.args = mk_ptr_array(raw.branch.nodes.len - 1, a),
        };

        for (size_t i = 1; i < raw.branch.nodes.len; i++) {
            Syntax* arg = abstract_expr_i(*(RawTree*)(raw.branch.nodes.data[i]), env, a, point);
            push_ptr(arg, &res->variant.args);
        }
    } else if (raw.branch.nodes.len > 1
               && ((RawTree*)raw.branch.nodes.data[1])->type == RawBranch
               && ((RawTree*)raw.branch.nodes.data[1])->branch.hint == HImplicit) {
        RawTree typelist = *(RawTree*)raw.branch.nodes.data[1];

        *res = (Syntax) {
            .type = SAllApplication,
            .ptype = NULL,
            .all_application.function = mem_alloc(sizeof(Syntax), a),
            .all_application.types = mk_ptr_array(typelist.branch.nodes.len, a),
            .all_application.implicits = mk_ptr_array(0, a),
            .all_application.args = mk_ptr_array(raw.branch.nodes.len - 2, a),
        };
        res->all_application.function = fn_syn;

        for (size_t i = 0; i < typelist.branch.nodes.len; i++) {
            Syntax* type = abstract_expr_i(*(RawTree*)(typelist.branch.nodes.data[i]), env, a, point);
            push_ptr(type, &res->all_application.types);
        }

        for (size_t i = 2; i < raw.branch.nodes.len; i++) {
            Syntax* arg = abstract_expr_i(*(RawTree*)(raw.branch.nodes.data[i]), env, a, point);
            push_ptr(arg, &res->all_application.args);
        }
    } else {
        *res = (Syntax) {
            .type = SApplication,
            .ptype = NULL,
            .application.function = mem_alloc(sizeof(Syntax), a),
            .application.implicits = mk_ptr_array(0, a),
            .application.args = mk_ptr_array(raw.branch.nodes.len - 1, a),
        };
        res->application.function = fn_syn;

        for (size_t i = 1; i < raw.branch.nodes.len; i++) {
            Syntax* arg = abstract_expr_i(*(RawTree*)(raw.branch.nodes.data[i]), env, a, point);
            push_ptr(arg, &res->application.args);
        }
    }

    return res;
}

Syntax* mk_term(TermFormer former, RawTree raw, ShadowEnv* env, Allocator* a, ErrorPoint* point) {
    switch (former) {
    case FDefine:
        throw_error(point, mk_string("'Define' not supported as inner-expression term former.", a));
    case FDeclare:
        throw_error(point, mk_string("'Declare' not supported as inner-expression term former.", a));
    case FProcedure: {
        if (raw.branch.nodes.len < 3) {
            throw_error(point, mk_string("Procedure term former requires at least 2 arguments!", a));
        }

        SymPtrAssoc implicits = mk_sym_ptr_assoc(0, a);
        SymPtrAssoc arguments = mk_sym_ptr_assoc(8, a);

        size_t args_index = 1;
        if (((RawTree*)raw.branch.nodes.data[args_index])->type == RawBranch
            && ((RawTree*)raw.branch.nodes.data[args_index])->branch.hint == HImplicit) {
            Result args_out = get_annotated_symbol_list(&implicits, *(RawTree*)raw.branch.nodes.data[args_index], env, a, point);
            if (args_out.type == Err) throw_error(point, args_out.error_message);
            args_index++;
        }

        if (((RawTree*)raw.branch.nodes.data[args_index])->type == RawBranch
            && ((RawTree*)raw.branch.nodes.data[args_index])->branch.hint == HSpecial) {
            Result args_out = get_annotated_symbol_list(&arguments, *(RawTree*)raw.branch.nodes.data[args_index], env, a, point);
            if (args_out.type == Err) throw_error(point, args_out.error_message);
            args_index++;
        }

        // TODO (BUG): shadow the arguments!
        SymbolArray to_shadow = mk_u64_array(8, a);
        shadow_vars(to_shadow, env);

        RawTree* raw_term;
        if (raw.branch.nodes.len == args_index + 1) {
            raw_term = raw.branch.nodes.data[args_index];
        } else {
            raw_term = raw_slice(&raw, args_index, a);
        }
        Syntax* body = abstract_expr_i(*raw_term, env, a, point);
        shadow_pop(arguments.len, env);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SProcedure,
            .ptype = NULL,
            .procedure.args = arguments,
            .procedure.implicits = implicits, 
            .procedure.body = body
        };
        return res;
    }
    case FAll: {
        if (raw.branch.nodes.len < 3) {
            throw_error(point, mk_string("all term former requires at least 2 arguments!", a));
        }

        SymbolArray arguments = mk_u64_array(2, a);
        if (!get_symbol_list(&arguments, *(RawTree*)raw.branch.nodes.data[1])) {
            throw_error(point, mk_string("all term former requires first arguments to be a symbol-list!", a));
        }

        shadow_vars(arguments, env);

        RawTree* raw_term;
        if (raw.branch.nodes.len == 3) {
            raw_term = raw.branch.nodes.data[2];
        } else {
            raw_term= raw_slice(&raw, 2, a);
        }
        Syntax* body = abstract_expr_i(*raw_term, env, a, point);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SAll,
            .ptype = NULL,
            .all.args = arguments,
            .all.body = body,
        };
        return res;
    }
    case FMacro: {
        if (raw.branch.nodes.len < 2) {
            throw_error(point, mv_string("Malformed macro expression: expects at least 1 arg."));
        }
        RawTree* body = raw.branch.nodes.len == 2 ? raw.branch.nodes.data[1] : raw_slice(&raw, 1, a);
        Syntax* transformer = abstract_expr_i(*body, env, a, point);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SMacro,
            .ptype = NULL,
            .transformer = transformer,
        };
        return res;
    }
    case FApplication: {
        return mk_application(raw, env, a, point);
    }
    case FVariant: {
        if (raw.branch.nodes.len == 2) {
            // Check that we are indeed getting a result
            // Get the tag name of the variant
            RawTree* msym = (RawTree*)raw.branch.nodes.data[1];
            if (msym->type != RawAtom && msym->atom.type != ASymbol) {
                throw_error(point, mv_string("Argument to variant term former should be symbol"));
            }
            Symbol lit = msym->atom.symbol;

            if (lit == string_to_symbol(mv_string("true"))) {
                Syntax* res = mem_alloc(sizeof(Syntax), a);
                *res = (Syntax) {.type = SLitBool, .ptype = NULL, .boolean = true,};
                return res;
            }
            else if (lit == string_to_symbol(mv_string("false"))) {
                Syntax* res = mem_alloc(sizeof(Syntax), a);
                *res = (Syntax) {.type = SLitBool, .ptype = NULL, .boolean = false,};
                return res;
            } else {
                throw_error(point, mv_string("Variant term former needs two arguments!"));
            }
        }
        else if (raw.branch.nodes.len != 3) {
            throw_error(point, mv_string("Variant term former needs two arguments!"));
        } else {
            // Get the Type portion of the projector 
            Syntax* var_type = abstract_expr_i(*(RawTree*)raw.branch.nodes.data[2], env, a, point);
        
            // Check that we are indeed getting a result
            // Get the tag gname of the variant
            RawTree* msym = (RawTree*)raw.branch.nodes.data[1];
            if (msym->type != RawAtom && msym->atom.type != ASymbol) {
                throw_error(point, mv_string("First argument to projection term former should be symbol"));
            };

            Syntax* res = mem_alloc(sizeof(Syntax), a);
            *res = (Syntax) {
                .type = SConstructor,
                .ptype = NULL,
                .constructor.enum_type = var_type,
                .constructor.tagname = msym->atom.symbol,
            };
            return res;
        }
        break;
    }
    case FMatch: {
        Syntax* sval = abstract_expr_i(*(RawTree*)raw.branch.nodes.data[1], env, a, point);

        ClauseArray clauses = mk_ptr_array(raw.branch.nodes.len - 2, a);
        
        for (size_t i = 2; i < raw.branch.nodes.len; i++) {
            // For each clause, we need three things:
            // 1. The tag, corresponding to the enum
            // 2. The variable(s) destructuring the enum
            // 3. The body
            // Get the clause
            RawTree* raw_clause = (RawTree*)raw.branch.nodes.data[i];
            if (raw_clause->type != RawBranch || raw_clause->branch.nodes.len < 2) {
                throw_error(point, mv_string("Match Clause has incorrect number of elements!"));
            }

            // Get the pattern
            RawTree* raw_pattern = (RawTree*)raw_clause->branch.nodes.data[0];
            if (raw_pattern->type != RawBranch) {
                throw_error(point, mv_string("Match Pattern should be a list!"));
            }

            // The pattern has two parts: the variables & the tag
            // The tag should be in a constructor (i.e. list)
            Symbol clause_tagname; 
            if (!get_fieldname(raw_pattern->branch.nodes.data[0], &clause_tagname)) {
                PtrArray nodes = mk_ptr_array(2, a);
                push_ptr(mv_str_doc(mv_string("Unable to get tagname in pattern:"), a), &nodes);
                push_ptr(pretty_rawtree(*(RawTree*)raw_pattern->branch.nodes.data[0], a), &nodes);
                Document* doc = mv_sep_doc(nodes, a);
                throw_error(point, doc_to_str(doc, a));
            }

            SymbolArray clause_binds = mk_u64_array(raw_pattern->branch.nodes.len - 1, a);
            for (size_t s = 1; s < raw_pattern->branch.nodes.len; s++) {
                RawTree* raw_name = raw_pattern->branch.nodes.data[s];
                if (!is_symbol(raw_name)) {
                    throw_error(point, mv_string("Pattern binding was not a symbol!"));
                }
                push_u64(raw_name->atom.symbol, &clause_binds); 
            }

            // Get the term
            RawTree *raw_term = (raw_clause->branch.nodes.len == 2)
                ? (RawTree *)raw_clause->branch.nodes.data[1]
                : raw_slice(raw_clause, 2, a);
            Syntax* clause_body = abstract_expr_i(*raw_term, env, a, point);

            SynClause* clause = mem_alloc(sizeof(SynClause), a);
            *clause = (SynClause) {
                .tagname = clause_tagname,
                .vars = clause_binds,
                .body = clause_body,
            };
            push_ptr(clause, &clauses);
        }

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SMatch,
            .ptype = NULL,
            .match.val = sval,
            .match.clauses = clauses,
        };
        return res;
    }
    case FStructure: {
        if (raw.branch.nodes.len < 2) {
            throw_error(point, mk_string("Structure term former needs a structure type argument", a));
        }

        // Get the type of the structure
        Syntax* stype = abstract_expr_i(*(RawTree*)raw.branch.nodes.data[1], env, a, point);

        // Construct a structure
        SymPtrAMap fields = mk_sym_ptr_amap(raw.branch.nodes.len, a);
        for (size_t i = 2; i < raw.branch.nodes.len; i++) {
            RawTree* fdesc = raw.branch.nodes.data[i];
            if (fdesc->type != RawBranch) {
                throw_error(point, mv_string("Structure expects all field descriptors to be lists."));
            }
            
            if (fdesc->branch.nodes.len < 2) {
                throw_error(point, mv_string("Structure expects all field descriptors to have at least 2 elements."));
            }

            Symbol field;
            if (!get_fieldname(fdesc->branch.nodes.data[0], &field)) {
                throw_error(point, mv_string("Structure has malformed field name."));
            }

            RawTree* val_desc = fdesc->branch.nodes.len == 2 ? fdesc->branch.nodes.data[1] : raw_slice(fdesc, 1, a); 
            Syntax* syn = abstract_expr_i(*val_desc, env, a, point);

            sym_ptr_insert(field, syn, &fields);
        }

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SStructure,
            .ptype = NULL,
            .structure.ptype = stype,
            .structure.fields = fields,
        };
        return res;
    }
    case FProjector: {
        if (raw.branch.nodes.len != 3) {
            throw_error(point, mk_string("Projection term former needs two arguments!", a));
        }

        // Get the source portion of the proector 
        Syntax* source = abstract_expr_i(*(RawTree*)raw.branch.nodes.data[2], env, a, point);

        // Get the symbol portion of the projector
        RawTree* msym = (RawTree*)raw.branch.nodes.data[1];
        if (msym->type != RawAtom && msym->atom.type != ASymbol) {
            throw_error(point, mv_string("Second argument to projection term former should be symbol"));
        }

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        Module* m = try_get_module(source, env);
        if (m) {
            // TODO (INVESTIGATION): do we have a better way to manage lookup
            // Note: seems like having to check for the special case of
            // Kind/Constraint is causing issues. 
            ModuleEntry* e = get_def(msym->atom.symbol, m);
            if (e) {
                *res = (Syntax) {
                    .type = SAbsVariable,
                    .ptype = &e->type,
                    .abvar.index = 0,
                    .abvar.value = (e->type.sort == TKind || e->type.sort == TConstraint) ? &e->value : e->value,
                };
                return res;
            } else {
                throw_error(point, mv_string("Field not found in module!"));
            }
        } else {
            *res = (Syntax) {
                .type = SProjector,
                .ptype = NULL,
                .projector.field = msym->atom.symbol,
                .projector.val = source,
            };
            return res;
        }
    }
    case FInstance: {
        SymbolArray params = mk_u64_array(0, a);
        SymPtrAssoc implicits = mk_sym_ptr_assoc(0, a);
        Syntax* constraint;

        size_t start_idx = 1;
        RawTree* current = raw.branch.nodes.data[start_idx];

        switch (current->type == RawBranch ? current->branch.hint : HNone) {
        case HNone: goto parse_constraint;
        case HExpression: goto parse_constraint;
        case HImplicit: goto parse_implicits;
        case HSpecial: goto parse_params;
        default: panic(mv_string("invalid hint!"));
        }
        // There are 2 optional nodes, we may skip them
        parse_params:

        if (!get_symbol_list(&params, *current)) {
          throw_error(point, mv_string("Instance parameter list malformed."));
        }

        current = raw.branch.nodes.data[++start_idx];
        switch (current->type == RawBranch ? current->branch.hint : HNone) {
        case HNone: goto parse_constraint;
        case HExpression: goto parse_constraint;
        case HImplicit: goto parse_implicits;
        case HSpecial: throw_error(point, mv_string("Invalid instance"));
        default: panic(mv_string("invalid hint!"));
        }

        parse_implicits:

        get_annotated_symbol_list(&implicits, *current, env, a, point);

        current = raw.branch.nodes.data[++start_idx];

        parse_constraint:

        constraint = abstract_expr_i(*current, env, a, point);
        start_idx++;

        SymPtrAMap fields = mk_sym_ptr_amap(raw.branch.nodes.len - start_idx, a);
        for (size_t i = start_idx; i < raw.branch.nodes.len; i++) {
            RawTree* fdesc = raw.branch.nodes.data[i];
            if (fdesc->type != RawBranch) {
                throw_error(point, mv_string("Instance expects all field descriptors to be lists."));
            }
            
            if (fdesc->branch.nodes.len < 2) {
                throw_error(point, mv_string("Instance expects all field descriptors to have at least 2 elements."));
            }

            Symbol field;
            if (!get_fieldname(fdesc->branch.nodes.data[0], &field)) {
                throw_error(point, mv_string("Instance has malformed field name."));
            }

            RawTree* val_desc = fdesc->branch.nodes.len == 2 ? fdesc->branch.nodes.data[1] : raw_slice(fdesc, 1, a); 
            Syntax* syn = abstract_expr_i(*val_desc, env, a, point);

            sym_ptr_insert(field, syn, &fields);
        }

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SInstance,
            .ptype = NULL,
            .instance.params = params,
            .instance.implicits = implicits,
            .instance.constraint = constraint,
            .instance.fields = fields,
        };
        return res;
    }
    case FDynamic: {
        if (raw.branch.nodes.len < 2) {
            throw_error(point, mv_string("Malformed dynamic expression: expects at least 1 arg."));
        }
        RawTree* body = raw.branch.nodes.len == 2 ? raw.branch.nodes.data[1] : raw_slice(&raw, 1, a);
        Syntax* dynamic = abstract_expr_i(*body, env, a, point);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SDynamic,
            .ptype = NULL,
            .dynamic = dynamic,
        };
        return res;
    }
    case FDynamicUse: {
        if (raw.branch.nodes.len != 2) {
            throw_error(point, mv_string("Malformed use expression."));
        }
        Syntax* use = abstract_expr_i(*(RawTree*)raw.branch.nodes.data[1], env, a, point);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SDynamicUse,
            .ptype = NULL,
            .use = use,
        };
        return res;
    }
    case FLet: {
        SymSynAMap bindings = mk_sym_ptr_amap(raw.branch.nodes.len - 1, a);
        size_t index = 1;

        bool is_special = true;
        while (is_special) {
            RawTree* bind = raw.branch.nodes.data[index];
            // let [x₁ e₁]
            //     [x₂ e₂]
            //  body
            is_special = bind->type == RawBranch && bind->branch.hint == HSpecial;
            if (is_special) {
                index++;
                Symbol sym;
                if (bind->type != RawBranch || bind->branch.nodes.len != 2) {
                    throw_error(point, mv_string("Malformed symbol binding in let-expression"));
                }
                if (!get_label(bind, &sym)) {
                    throw_error(point, mv_string("Expected symbol binding in let-expression"));
                }

                shadow_var(sym, env);
                Syntax* bind_body = abstract_expr_i(*(RawTree*)bind->branch.nodes.data[1], env, a, point);
                sym_ptr_insert(sym, bind_body, &bindings);
            }
        }

        if (index > raw.branch.nodes.len - 1) {
            throw_error(point, mv_string("Let expression has no body!"));
        }

        if (index < raw.branch.nodes.len - 1) {
            throw_error(point, mv_string("Let expression multiple bodies!"));
        }

        Syntax* body = abstract_expr_i(*(RawTree*)raw.branch.nodes.data[index], env, a, point);
        shadow_pop(bindings.len, env);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SLet,
            .ptype = NULL,
            .let_expr.bindings = bindings,
            .let_expr.body = body,
        };
        return res;
    }
    case FDynamicLet: {
        PtrArray bindings = mk_ptr_array(raw.branch.nodes.len - 1, a);
        size_t index = 1;

        bool is_special = true;
        while (is_special && index < raw.branch.nodes.len) {
            RawTree* bind = raw.branch.nodes.data[index];
            // bind [x₁ e₁]
            //      [x₂ e₂]
            //  body
            is_special = bind->type == RawBranch && bind->branch.hint == HSpecial;
            if (is_special) {
                index++;
                DynBinding* dbind = mem_alloc(sizeof(DynBinding), a);
                if (bind->type != RawBranch || bind->branch.nodes.len != 2) {
                    throw_error(point, mv_string("Malformed symbol binding in bind-expression"));
                }

                dbind->var = abstract_expr_i(*(RawTree*)bind->branch.nodes.data[0], env, a, point);
                dbind->expr = abstract_expr_i(*(RawTree*)bind->branch.nodes.data[1], env, a, point);
                push_ptr(dbind, &bindings);
            }
        }

        if (index > raw.branch.nodes.len - 1) {
            throw_error(point, mv_string("Bind expression has no body!"));
        }

        if (index < raw.branch.nodes.len - 1) {
            throw_error(point, mv_string("Bind expression multiple bodies!"));
        }

        Syntax* body = abstract_expr_i(*(RawTree*)raw.branch.nodes.data[index], env, a, point);
        shadow_pop(bindings.len, env);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SDynamicLet,
            .ptype = NULL,
            .dyn_let_expr.bindings = bindings,
            .dyn_let_expr.body = body,
        };
        return res;
    }
    case FIf: {
        if (raw.branch.nodes.len != 4) {
            throw_error(point, mv_string("Term former 'if' expects precisely 3 arguments!"));
        }
        SynArray terms = mk_ptr_array(3, a);
        for (size_t i = 1; i < raw.branch.nodes.len; i++) {
            Syntax* term = abstract_expr_i(*(RawTree*)raw.branch.nodes.data[i], env, a, point);
            push_ptr(term, &terms);
        }

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SIf,
            .ptype = NULL,
            .if_expr.condition = terms.data[0],
            .if_expr.true_branch = terms.data[1],
            .if_expr.false_branch = terms.data[2],
        };
        return res;
    }
    case FLabels: {
        if (raw.branch.nodes.len < 2) {
            throw_error(point, mv_string("Term former 'labels' expects at least 1 argument!"));
        }

        Syntax* entry = abstract_expr_i(*(RawTree*)raw.branch.nodes.data[1], env, a, point);

        SymPtrAssoc terms = mk_sym_ptr_assoc(raw.branch.nodes.len - 2, a);
        for (size_t i = 2; i < raw.branch.nodes.len; i++) {
            Symbol label;
            RawTree* label_expr = raw.branch.nodes.data[i];
            if (!get_label(label_expr, &label)) {
                throw_error(point, mv_string("Each label must be of the form [label expr]"));
            }
            Syntax* res = abstract_expr_i(*(RawTree*)label_expr->branch.nodes.data[1], env, a, point);
            sym_ptr_bind(label, res, &terms);
        }

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SLabels,
            .ptype = NULL,
            .labels.entry = entry,
            .labels.terms = terms,
        };
        return res;
    }
    case FGoTo: {
        if (raw.branch.nodes.len != 2) {
            throw_error(point, mv_string("Term former 'go-to' expects one argument!"));
        }
        RawTree* label = raw.branch.nodes.data[1]; 

        if (label->type != RawAtom && label->atom.type != ASymbol) {
            throw_error(point, mv_string("Term former 'go-to' expects one argument!"));
        }

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SGoTo,
            .ptype = NULL,
            .go_to.label = label->atom.symbol,
        };
        return res;
    }
    case FWithReset: {
        // with-reset [lbl] e [l1 l2] e
        if (raw.branch.nodes.len != 5) {
          throw_error(point, mv_string("Term former 'with-reset' expects exactly 5 arguments!"));
        }
        SymbolArray reset_binds = mk_u64_array(1, a);
        SymbolArray handle_binds = mk_u64_array(2, a);

        if (!get_symbol_list(&reset_binds, *(RawTree*) raw.branch.nodes.data[1]) || reset_binds.len != 1) {
          throw_error(point, mv_string("Term former 'with-reset' 1st argument list malformed."));
        }

        Symbol reset_point_sym = reset_binds.data[0];
        if (reset_binds.len != 1) {
            throw_error(point, mv_string("Term former 'with-reset' expects exactly 5 arguments!"));
        }

        Syntax* expr = abstract_expr_i(*(RawTree*)raw.branch.nodes.data[2], env, a, point);

        if (!get_symbol_list(&handle_binds, *(RawTree*) raw.branch.nodes.data[3]) || handle_binds.len != 3) {
            throw_error(point, mv_string("Handler list malformed!"));
        }

        Symbol in_sym = handle_binds.data[1];
        Symbol cont_sym = handle_binds.data[2];

        Syntax* handler = abstract_expr_i(*(RawTree*)raw.branch.nodes.data[4], env, a, point);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SWithReset,
            .ptype = NULL,
            .with_reset.point_sym = reset_point_sym,
            .with_reset.expr = expr,
            .with_reset.in_sym = in_sym,
            .with_reset.cont_sym = cont_sym,
            .with_reset.handler = handler,
        };
        return res;
    }
    case FResetTo: {
        if (raw.branch.nodes.len != 3) {
            throw_error(point, mv_string("Term former 'reset-to' expects two arguments!"));
        }

        Syntax* rpoint = abstract_expr_i(*(RawTree*)raw.branch.nodes.data[1], env, a, point);
        Syntax* rarg = abstract_expr_i(*(RawTree*)raw.branch.nodes.data[2], env, a, point);
        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SResetTo,
            .ptype = NULL,
            .reset_to.point = rpoint,
            .reset_to.arg = rarg,
        };
        return res;
    }
    case FSequence: {
        SynArray elements = mk_ptr_array(raw.branch.nodes.len - 1, a);
        for (size_t i = 1; i < raw.branch.nodes.len; i++) {
            RawTree* tree = raw.branch.nodes.data[i];
            SeqElt* elt = mem_alloc(sizeof(SeqElt), a);
            if (tree->type == RawBranch && tree->branch.hint == HSpecial) {
                if (tree->type != RawBranch
                    || tree->branch.nodes.len != 3
                    || !eq_symbol(tree->branch.nodes.data[0], string_to_symbol(mv_string("let!")))) {
                    throw_error(point, mv_string("Invalid let! binding in seq"));
                }
                RawTree* rsym = tree->branch.nodes.data[1];
                if (rsym->type != RawAtom || rsym->atom.type != ASymbol) {
                    throw_error(point, mv_string("Invalid let! binding in seq"));
                }
                
                *elt = (SeqElt) {
                    .is_binding = true,
                    .symbol = rsym->atom.symbol,
                    .expr = abstract_expr_i(*(RawTree*)tree->branch.nodes.data[2], env, a, point),
                };
            } else {
                *elt = (SeqElt) {
                    .is_binding = false,
                    .expr = abstract_expr_i(*tree, env, a, point),
                };
            }
            push_ptr(elt, &elements);
        }
        
        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SSequence,
            .ptype = NULL,
            .sequence.elements = elements,
        };
        return res;
    }
    case FModule:
        throw_error(point, mk_string("'Module' not supported as inner-expression term former.", a));
    case FIs: {
        if (raw.branch.nodes.len != 3) {
            throw_error(point, mv_string("Term former 'is' expects precisely 2 arguments!"));
        }

        Syntax* term = abstract_expr_i(*(RawTree*)raw.branch.nodes.data[1], env, a, point);
        Syntax* type = abstract_expr_i(*(RawTree*)raw.branch.nodes.data[2], env, a, point);
        
        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SIs,
            .ptype = NULL,
            .is = {.val = term, .type = type},
        };
        return res;
    }
    case FInTo: {
        if (raw.branch.nodes.len != 3) {
            throw_error(point, mv_string("Term former 'into' expects precisely 2 arguments!"));
        }

        Syntax* type = abstract_expr_i(*(RawTree*)raw.branch.nodes.data[1], env, a, point);
        Syntax* term = abstract_expr_i(*(RawTree*)raw.branch.nodes.data[2], env, a, point);
        
        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SInTo,
            .ptype = NULL,
            .into = {.val = term, .type = type},
        };
        return res;
    }
    case FOutOf: {
        if (raw.branch.nodes.len != 3) {
            throw_error(point, mv_string("Term former 'out-of' expects precisely 2 arguments!"));
        }

        Syntax* type = abstract_expr_i(*(RawTree*)raw.branch.nodes.data[1], env, a, point);
        Syntax* term = abstract_expr_i(*(RawTree*)raw.branch.nodes.data[2], env, a, point);
        
        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SOutOf,
            .ptype = NULL,
            .into = {.val = term, .type = type},
        };
        return res;
    }
    case FDynAlloc: {
        if (raw.branch.nodes.len != 2) {
            throw_error(point, mv_string("Term former 'dynamic-alloc' expects precisely 1 arguments!"));
        }

        Syntax* term = abstract_expr_i(*(RawTree*)raw.branch.nodes.data[1], env, a, point);
        
        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SDynAlloc,
            .ptype = NULL,
            .size = term,
        };
        return res;
    }
    case FSizeOf: {
        if (raw.branch.nodes.len != 2) {
            throw_error(point, mv_string("Term former 'size-of' expects precisely 1 argument!"));
        }

        Syntax* term = abstract_expr_i(*(RawTree*)raw.branch.nodes.data[1], env, a, point);
        
        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SSizeOf,
            .ptype = NULL,
            .size = term,
        };
        return res;
    }
    case FAlignOf: {
        if (raw.branch.nodes.len != 2) {
            throw_error(point, mv_string("Term former 'align-of' expects precisely 1 argument!"));
        }

        Syntax* term = abstract_expr_i(*(RawTree*)raw.branch.nodes.data[1], env, a, point);
        
        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SAlignOf,
            .ptype = NULL,
            .size = term,
        };
        return res;
    }
    case FProcType: {
        if (raw.branch.nodes.len != 3) {
            throw_error(point, mv_string("Wrong number of terms to proc type former."));
        }

        RawTree* raw_args = raw.branch.nodes.data[1];
        if (raw_args->type != RawBranch) {
            throw_error(point, mv_string("Procedure argument list should be list."));
        }
        
        PtrArray arg_types = mk_ptr_array(raw_args->branch.nodes.len, a);

        for (size_t i = 0; i < raw_args->branch.nodes.len; i++) {
            RawTree* raw_arg = raw_args->branch.nodes.data[i];
            Syntax* arg_ty = abstract_expr_i(*raw_arg, env, a, point);

            push_ptr(arg_ty, &arg_types);
        }

        RawTree* raw_return = raw.branch.nodes.data[2]; 
        Syntax* return_type = abstract_expr_i(*raw_return, env, a, point);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SProcType,
            .ptype = NULL,
            .proc_type.args = arg_types,
            .proc_type.return_type = return_type,
        };
        return res;
    }
    case FStructType: {
        SymSynAMap field_types = mk_sym_ptr_amap(raw.branch.nodes.len, a);

        for (size_t i = 1; i < raw.branch.nodes.len; i++) {
            RawTree* fdesc = raw.branch.nodes.data[i];
            if (fdesc->type != RawBranch) {
                throw_error(point, mv_string("Structure type expects all field descriptors to be lists."));
            };
            
            if (fdesc->branch.nodes.len < 2) {
                throw_error(point, mv_string("Structure type expects all field descriptors to have a type."));
            };

            Symbol field;
            if (!get_fieldname(fdesc->branch.nodes.data[0], &field)) {
                throw_error(point, mv_string("Structure type has malformed field name."));
            };

            RawTree* raw_ty = (fdesc->branch.nodes.len == 2) ? fdesc->branch.nodes.data[1] : raw_slice(fdesc, 1, a);
            Syntax* field_ty = abstract_expr_i(*raw_ty, env, a, point);

            sym_ptr_insert(field, field_ty, &field_types);
        }

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SStructType,
            .ptype = NULL,
            .struct_type.fields = field_types,
        };
        return res;
    }
    case FEnumType: {
        SymPtrAMap enum_variants = mk_sym_ptr_amap(raw.branch.nodes.len, a);

        for (size_t i = 1; i < raw.branch.nodes.len; i++) {
            RawTree* edesc = raw.branch.nodes.data[i];

            if (edesc->type != RawBranch) {
                throw_error(point, mv_string("Enumeration type expects all enum descriptors to be lists."));
            };
            
            if (edesc->branch.nodes.len < 1) {
                throw_error(point, mv_string("Enumeration type expects all enum descriptors to have at least 1 elements."));
            };

            Symbol tagname;
            PtrArray* types = mem_alloc(sizeof(PtrArray), a);
            *types = mk_ptr_array(edesc->branch.nodes.len - 1, a);

            if (!get_fieldname(edesc->branch.nodes.data[0], &tagname)) {
                throw_error(point, mv_string("Enum type has malformed field name."));
            };

            for (size_t i = 1; i < edesc->branch.nodes.len; i++) {
                Syntax* field_ty = abstract_expr_i(*(RawTree*) edesc->branch.nodes.data[i], env, a, point);
                push_ptr(field_ty, types);
            }

            sym_ptr_insert(tagname, types, &enum_variants);
        }

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SEnumType,
            .ptype = NULL,
            .enum_type.variants = enum_variants,
        };
        return res;
    }
    case FResetType: {
        if (raw.branch.nodes.len != 3) {
            throw_error(point, mv_string("Reset type former expects exactly 2 arguments!"));
        }
        Syntax* in_ty = abstract_expr_i(*(RawTree*) raw.branch.nodes.data[1], env, a, point);
        Syntax* out_ty = abstract_expr_i(*(RawTree*) raw.branch.nodes.data[2], env, a, point);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SResetType,
            .ptype = NULL,
            .reset_type.in = in_ty,
            .reset_type.out = out_ty,
        };
        return res;
    }
    case FDynamicType: {
        if (raw.branch.nodes.len <= 1) {
            throw_error(point, mv_string("Malformed Dynamic Type expression: expects at least 1 arg."));
        }
        RawTree* body = raw.branch.nodes.len == 2 ? raw.branch.nodes.data[1] : raw_slice(&raw, 1, a);
        Syntax* dyn_ty = abstract_expr_i(*body, env, a, point);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SDynamicType,
            .ptype = NULL,
            .dynamic_type = dyn_ty,
        };
        return res;
    }
    case FNamedType: {
        if (raw.branch.nodes.len <= 2) {
            throw_error(point, mv_string("Malformed Named Type expression: expects at least 2 args."));
        }

        RawTree* rname = raw.branch.nodes.data[1];
        if (!is_symbol(rname)) {
            throw_error(point, mv_string("Malformed Named Type expression: 1st arg to be name."));
        }
        Symbol name = rname->atom.symbol;

        RawTree* body = raw.branch.nodes.len == 3 ? raw.branch.nodes.data[2] : raw_slice(&raw, 2, a);
        Syntax* rec_body = abstract_expr_i(*body, env, a, point);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SNamedType,
            .ptype = NULL,
            .named_type.name = name,
            .named_type.body = rec_body,
        };
        return res;
    }
    case FDistinctType: {
        if (raw.branch.nodes.len <= 1) {
            throw_error(point, mv_string("Malformed Distinct Type expression: expects at least 1 arg."));
        }
        RawTree* body = raw.branch.nodes.len == 2 ? raw.branch.nodes.data[1] : raw_slice(&raw, 1, a);
        Syntax* distinct = abstract_expr_i(*body, env, a, point);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SDistinctType,
            .ptype = NULL,
            .distinct_type = distinct,
        };
        return res;
    }
    case FOpaqueType: {
        if (raw.branch.nodes.len <= 1) {
            throw_error(point, mv_string("Malformed Opaque Type expression: expects at least 1 arg."));
        }
        RawTree* body = raw.branch.nodes.len == 2 ? raw.branch.nodes.data[1] : raw_slice(&raw, 1, a);
        Syntax* opaque = abstract_expr_i(*body, env, a, point);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SOpaqueType,
            .ptype = NULL,
            .opaque_type = opaque,
        };
        return res;
    }
    case FTraitType: {
        if (raw.branch.nodes.len < 2) {
            throw_error(point, mv_string("Wrong number of terms to trait type former."));
        }

        RawTree* raw_vars = (RawTree*)raw.branch.nodes.data[1];
        SymbolArray vars = mk_u64_array(raw_vars->branch.nodes.len, a);

        if (!get_symbol_list(&vars, *raw_vars)) {
            throw_error(point, mv_string("Malformed Trait parameter list."));
        }

        SymPtrAMap fields = mk_sym_ptr_amap(raw.branch.nodes.len - 2, a);
        for (size_t i = 2; i < raw.branch.nodes.len; i++) {
            RawTree* fdesc = raw.branch.nodes.data[i];
            if (fdesc->type != RawBranch) {
                throw_error(point, mv_string("Trait expects all field descriptors to be lists."));
            }
            
            if (fdesc->branch.nodes.len < 2) {
                throw_error(point, mv_string("Trait expects all field descriptors to have at least 2 elements."));
            }

            Symbol field;
            if (!get_fieldname(fdesc->branch.nodes.data[0], &field)) {
                throw_error(point, mv_string("Trait has malformed field name."));
            }
            Syntax* syn;
            if (fdesc->branch.nodes.len == 2) {
                syn = abstract_expr_i(*(RawTree*) fdesc->branch.nodes.data[1], env, a, point);
            } else {
                RawTree* raw_term = raw_slice(fdesc, 1, a);
                syn = abstract_expr_i(*raw_term, env, a, point);
            }

            sym_ptr_insert(field, syn, &fields);
        }

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = STraitType,
            .ptype = NULL,
            .trait.vars = vars,
            .trait.fields = fields,
        };
        return res;
    }
    case FAllType: {
        if (raw.branch.nodes.len < 3) {
            throw_error(point, mk_string("All term former requires at least 2 arguments!", a));
        }

        SymbolArray vars = mk_u64_array(8, a);
        if (!get_symbol_list(&vars, *(RawTree*)raw.branch.nodes.data[1])) {
            throw_error(point, mk_string("All argument list malformed", a));
        }

        shadow_vars(vars, env);

        RawTree* raw_term;
        if (raw.branch.nodes.len == 3) {
            raw_term = (RawTree*)raw.branch.nodes.data[2];
        } else {
            raw_term = raw_slice(&raw, 2, a);
        }
        Syntax* body = abstract_expr_i(*raw_term, env, a, point);
        shadow_pop(vars.len, env);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SAllType,
            .ptype = NULL,
            .bind_type.bindings = vars,
            .bind_type.body = body,
        };
        return res;
    }
    case FFamily: {
        if (raw.branch.nodes.len < 3) {
            throw_error(point, mk_string("Family term former requires at least 2 arguments!", a));
        }

        SymbolArray vars = mk_u64_array(8, a);
        if (!get_symbol_list(&vars, *(RawTree*)raw.branch.nodes.data[1])) {
            throw_error(point, mk_string("All argument list malformed", a));
        }

        shadow_vars(vars, env);

        RawTree* raw_term = (raw.branch.nodes.len == 3)
            ? raw.branch.nodes.data[2]
            : raw_slice(&raw, 2, a);
        
        Syntax* body = abstract_expr_i(*raw_term, env, a, point);
        shadow_pop(vars.len, env);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = STypeFamily,
            .ptype = NULL,
            .bind_type.bindings = vars,
            .bind_type.body = body,
        };
        return res;
    }
    case FCType: {
        RawTree* raw_term = (raw.branch.nodes.len == 2)
            ? raw.branch.nodes.data[1]
            : raw_slice(&raw, 1, a);
        Syntax* c_type = abstract_expr_i(*raw_term, env, a, point);
        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SCType,
            .ptype = NULL,
            .c_type = c_type,
        };
        return res;
    }
    case FReinterpretNative:
    case FReinterpretRelic: {
        if (raw.branch.nodes.len < 3) {
            throw_error(point, mk_string("Reinterpret term former requires at least 2 arguments!", a));
        }

        Syntax* type = abstract_expr_i(*(RawTree*)raw.branch.nodes.data[1], env, a, point);

        RawTree* raw_term = (raw.branch.nodes.len == 3)
            ? raw.branch.nodes.data[2]
            : raw_slice(&raw, 2, a);

        Syntax* body = abstract_expr_i(*raw_term, env, a, point);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SReinterpret,
            .ptype = NULL,
            .reinterpret.from_native = former == FReinterpretNative,
            .reinterpret.type = type, 
            .reinterpret.body = body,
        };
        return res;
    }
    case FConvertNative:
    case FConvertRelic: {
        if (raw.branch.nodes.len < 3) {
            throw_error(point, mk_string("Convert term former requires at least 2 arguments!", a));
        }

        Syntax* type = abstract_expr_i(*(RawTree*)raw.branch.nodes.data[1], env, a, point);

        RawTree* raw_term = (raw.branch.nodes.len == 3)
            ? raw.branch.nodes.data[2]
            : raw_slice(&raw, 2, a);

        Syntax* body = abstract_expr_i(*raw_term, env, a, point);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SConvert,
            .ptype = NULL,
            .convert.from_native = former == FConvertNative,
            .convert.type = type, 
            .convert.body = body,
        };
        return res;
    }
    }
    panic(mv_string("Invalid termformer provided to mk_term."));
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
                .ptype = NULL,
                .variable = raw.atom.symbol,
            };
            break;
        }
        case AIntegral: {
            *res = (Syntax) {
                .type = SLitUntypedIntegral,
                .ptype = NULL,
                .integral.value = raw.atom.int_64,
            };
            break;
        }
        case AFloating: {
            *res = (Syntax) {
                .type = SLitUntypedFloating,
                .ptype = NULL,
                .floating.value = raw.atom.float_64,
            };
            break;
        }
        case ABool: {
            *res = (Syntax) {
                .type = SLitBool,
                .ptype = NULL,
                .boolean = (bool) raw.atom.int_64,
            };
            break;
        }
        case AString: {
            *res = (Syntax) {
                .type = SLitString,
                .ptype = NULL,
                .string = raw.atom.string,
            };
            break;
        }
        default:
            panic(mv_string("Don't know how to make a literal from this atom!."));
        }
        break;
    }
    case RawBranch: {
        // Currently, can only have function calls, so all Raw lists compile down to an application
        if (raw.branch.nodes.len < 1) {
            throw_error(point, mk_string("Raw Syntax must have at least one element!", a));
        }

        if (is_symbol(raw.branch.nodes.data[0])) {
            Symbol sym = ((RawTree*)raw.branch.nodes.data[0])->atom.symbol;
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
                } else if (entry.vtype->sort == TPrim && entry.vtype->prim == TMacro) {
                    // Call the function (uses Pico ABI)
                    RawTree output;
                    PtrArray input = raw.branch.nodes;
                    void* dvars = get_dynamic_memory();
                    void* dynamic_memory_space = mem_alloc(4096, a);

                    Allocator* old_tmp_alloc = set_std_tmp_allocator(a);

                    int64_t out;
                    __asm__ __volatile__(
                                         "push %%rbp       \n"
                                         "push %%r15       \n"
                                         "push %%r14       \n"
                                         // Push output ptr & sizeof (Syntax), resp
                                         "push %5          \n"
                                         "push %6          \n"

                                         "mov %3, %%r14    \n"
                                         "mov %2, %%r15    \n"
                                         //"sub $0x8, %%rbp  \n" // Do this to align RSP & RBP?

                                         // Push arg (array) onto stack
                                         "push 0x18(%4)       \n"
                                         "push 0x10(%4)       \n"
                                         "push 0x8(%4)        \n"
                                         "push (%4)         \n"

                                         "mov %%rsp, %%rbp \n"

                                         // Call function, this should consume 'Array' from the stack and push
                                         // 'Raw Syntax' onto the stack
                                         "call *%1         \n"

                                         // After calling the function, we
                                         // expect a Syntax to be atop the stack:
#if ABI == SYSTEM_V_64
                                         // memcpy (dest = rdi, src = rsi, size = rdx)
                                         // retval = rax
                                         "mov 0x30(%%rsp), %%rdx   \n"
                                         "mov 0x38(%%rsp), %%rdi   \n"
                                         "mov %%rsp, %%rsi         \n"
                                         "call memcpy              \n"

#elif ABI == WIN_64
                                         // memcpy (dest = rcx, src = rdx, size = r8)
                                         // retval = rax
                                         "mov 0x30(%%rsp), %%r8   \n"
                                         "mov 0x38(%%rsp), %%rcx   \n"
                                         "mov %%rsp, %%rdx         \n"
                                         "sub $0x20, %%rsp          \n"
                                         "call memcpy            \n"
                                         "add $0x20, %%rsp          \n"
#else
#error "Unknown calling convention"
#endif
                                         // pop value from stack 
                                         "mov 0x30(%%rsp), %%rax   \n"
                                         "add %%rax, %%rsp         \n"
                                         // pop stashed size & dest from stack
                                         "add $0x10, %%rsp          \n"

                                         "pop %%r14        \n"
                                         "pop %%r15        \n"
                                         "pop %%rbp        \n"
                                         : "=r" (out)

                                         : "r" (*(uint64_t*)entry.value)
                                           , "r" (dvars)
                                           , "r" (dynamic_memory_space)
                                           , "r" (&input)
                                           , "r" (&output)
                                           , "c" (sizeof(RawTree))) ;

                    set_std_tmp_allocator(old_tmp_alloc);
                    mem_free(dynamic_memory_space, a);
                    return abstract_expr_i(output, env, a, point);
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
        if (raw.branch.nodes.len < 3) {
            throw_error(point, mk_string("Definitions expect at least 2 terms", a));
        }

        if (!is_symbol(raw.branch.nodes.data[1])) {
            throw_error(point, mk_string("First argument to definitions should be a symbol", a));
        }

        Symbol sym = ((RawTree*)raw.branch.nodes.data[1])->atom.symbol;
        
        RawTree* raw_term = (raw.branch.nodes.len == 3) ? raw.branch.nodes.data[2] : raw_slice(&raw, 2, a);

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

    if (raw.type == RawBranch && raw.branch.nodes.len > 1) {
        if (is_symbol(raw.branch.nodes.data[0])) {
            Symbol sym = ((RawTree*)raw.branch.nodes.data[0])->atom.symbol;
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
            .type = Import,
            .name = raw->atom.symbol,
        };
    } else if (raw->type == RawBranch) {
        // Possibilities:
        // (name1 :all)
        // (name1 :as name2)
        // (. name2 name1)
        // (. (list-of-names) name1)
        if (raw->branch.nodes.len == 2) {
            if (!is_symbol(raw->branch.nodes.data[0]))
                throw_error(point, mv_string("Invalid import clause: first element should be symbol"));
            Symbol name = ((RawTree*)raw->branch.nodes.data[0])->atom.symbol;

            
            Symbol middle;
            if(!get_fieldname(raw->branch.nodes.data[1], &middle))
                throw_error(point, mv_string("Invalid import clause - expected :all"));
            if (middle != string_to_symbol(mv_string("all")))
                throw_error(point, mv_string("Invalid import clause - expected :all"));

            return (ImportClause) {
                .type = ImportAll,
                .name = name,
            };
        } else if (raw->branch.nodes.len == 3) {

            if (!is_symbol(raw->branch.nodes.data[0]) || !is_symbol(raw->branch.nodes.data[2]))
                throw_error(point, mv_string("Invalid import clause"));

            // Check for '.'
            if (eq_symbol(raw->branch.nodes.data[0], string_to_symbol(mv_string(".")))) {
                Symbol src;
                if(!get_fieldname(raw->branch.nodes.data[2], &src))
                    throw_error(point, mv_string("Invalid import-. source"));

                RawTree* raw_members = raw->branch.nodes.data[1];
                if (is_symbol(raw_members)) {
                    return (ImportClause) {
                        .type = Import,
                        .name = src,
                        .member = raw_members->atom.symbol,
                    };
                } else if (raw_members->type == RawBranch) {
                    SymbolArray members = mk_u64_array(raw_members->branch.nodes.len, a);
                    if (!get_symbol_list(&members, *raw_members))
                        throw_error(point, mv_string("Invalid import-. members"));
                    return (ImportClause) {
                        .type = ImportMany,
                        .name = src,
                        .members = members,
                    };
                } else {
                    throw_error(point, mv_string("Invalid import-. member(s)"));
                }

            } else {
                Symbol middle;
                if(!get_fieldname(raw->branch.nodes.data[1], &middle))
                    throw_error(point, mv_string("Invalid import clause"));
                if (middle != string_to_symbol(mv_string("as")))
                    throw_error(point, mv_string("Invalid import clause"));

                Symbol name;
                Symbol rename;
                if(!get_fieldname(raw->branch.nodes.data[0], &name))
                    throw_error(point, mv_string("Invalid import-as name"));
                if(!get_fieldname(raw->branch.nodes.data[0], &rename))
                    throw_error(point, mv_string("Invalid import-as new name"));
                return (ImportClause) {
                    .type = ImportAs,
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

ExportClause abstract_export_clause(RawTree* raw, ErrorPoint* point) {
    if (is_symbol(raw)) {
        return (ExportClause) {
            .type = ExportName,
            .name = raw->atom.symbol,
        };
    } else if (raw->type == RawBranch) {
        // Export as
        // looks like (<symbol/name> (<symbol/:> <symbol/as>) <symbol/name>)

        if (raw->branch.nodes.len != 3)
            throw_error(point, mv_string("Invalid export clause"));

        Symbol middle;
        if(!get_fieldname(raw->branch.nodes.data[1], &middle))
            throw_error(point, mv_string("Invalid export clause"));
        if (middle != string_to_symbol(mv_string("as")))
            throw_error(point, mv_string("Invalid export clause"));

        Symbol name;
        Symbol rename;
        if(!get_fieldname(raw->branch.nodes.data[0], &name))
            throw_error(point, mv_string("Invalid export-as name"));
        if(!get_fieldname(raw->branch.nodes.data[0], &rename))
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

Module* try_get_module(Syntax* syn, ShadowEnv* env) {
    switch (syn->type) {
    case SVariable: {
        ShadowEntry se = shadow_env_lookup(syn->variable, env);
        if ((se.type == SGlobal || se.type == SLocal) && se.is_module) {
            return se.module;
        }
        break;
    }
    case SProjector: {
        panic(mv_string("TODO: implement try_get_module for sprojector"));
    }
    default:
        return NULL;
    }
    return NULL;
}
