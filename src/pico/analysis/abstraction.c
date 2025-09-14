#include "platform/machine_info.h"

#include "platform/signals.h"
#include "data/result.h"
#include "data/string.h"

#include "pico/data/error.h"
#include "pico/values/values.h"
#include "pico/stdlib/extra.h"
#include "pico/syntax/concrete.h"
#include "pico/syntax/syntax.h"
#include "pico/binding/shadow_env.h"
#include "pico/analysis/abstraction.h"

// Internal types
typedef enum {
    HeadSyntax, HeadFormer, HeadMacro
} HeadType;

typedef struct {
    HeadType type;
    union {
        Syntax* syn;
        TermFormer former;
        uint64_t macro_addr;
    };
} ComptimeHead;

// Internal functions declarations needed for interface implementation
Syntax* abstract_expr_i(RawTree raw, ShadowEnv* env, Allocator* a, PiErrorPoint* point);
TopLevel abstract_i(RawTree raw, ShadowEnv* env, Allocator* a, PiErrorPoint* point);

Syntax* mk_application_body(Syntax* fn_syn, RawTree raw, ShadowEnv* env, Allocator* a, PiErrorPoint* point);
Syntax* mk_term(TermFormer former, RawTree raw, ShadowEnv* env, Allocator* a, PiErrorPoint* point);
Syntax* mk_array_literal(RawTree raw, ShadowEnv* env, Allocator* a, PiErrorPoint* point);
ComptimeHead comptime_head(RawTree raw, ShadowEnv* env, Allocator* a, PiErrorPoint* point);
Module* try_get_module(Syntax* syn, ShadowEnv* env);
Syntax* resolve_module_projector(Range range, Syntax* source, RawTree* msym, ShadowEnv* env, Allocator* a, PiErrorPoint* point);
SymbolArray* try_get_path(Syntax* syn, Allocator* a);

Imports abstract_imports(RawTree* raw, Allocator* a, PiErrorPoint* point);
Exports abstract_exports(RawTree* raw, Allocator* a, PiErrorPoint* point);
ImportClause abstract_import_clause(RawTree* raw, Allocator* a, PiErrorPoint* point);
ExportClause abstract_export_clause(RawTree* raw, PiErrorPoint* point, Allocator* a);

//------------------------------------------------------------------------------
// Interface Implementation
//------------------------------------------------------------------------------

Syntax* abstract_expr(RawTree raw, Environment* env, Allocator* a, PiErrorPoint* point) {
    ShadowEnv* s_env = mk_shadow_env(a, env);
    Syntax* out = abstract_expr_i(raw, s_env, a, point);
    return out; 
}

TopLevel abstract(RawTree raw, Environment* env, Allocator* a, PiErrorPoint* point) {
    ShadowEnv* s_env = mk_shadow_env(a, env);
    TopLevel out = abstract_i(raw, s_env, a, point);
    return out;
}

ModuleHeader* abstract_header(RawTree raw, Allocator* a, PiErrorPoint* point) {
    // Expected format:
    // Note: postfix ?  = expression is optional
    //       postfix *  = zero or more required
    //       infix |    = choice
    //       between <> = class
    //       between ⦇⦈ = grouped, the ⦇⦈ are not literal
    // 
    // ( module <modulename>
    //   ⦇ ( import <import-clause>* ) | ( export ⦇:all | <export-clause>*⦈ ) ⦈* )
    // 
    // Where
    //  <modulename> is a symbol
    // 
    // <import-clause> has the format
    //  ( <path> ⦇:all | :except <symbol-list> | :only <symbol-list>⦈? ) 
    // 
    // <export-clause> has the format
    //  <symbol>
    //

    // Prep an error that can then be filled out and thrown.
    PicoError err;
    if (raw.type != RawBranch) {
        err.range = raw.range;
        err.message = mv_cstr_doc("Expected module header to be list.", a);
        throw_pi_error(point, err);
    }

    if (raw.branch.nodes.len <= 2) {
        err.range = raw.range;
        err.message = mv_cstr_doc("Expecting keyword module header to have at least two elements - (module <modulename>). Got nothing!", a);
        throw_pi_error(point, err);
    }
    if (raw.branch.nodes.len > 4) {
        err.range = raw.range;
        err.message = mv_cstr_doc("Too many parameters in module header.", a);
        throw_pi_error(point, err);
    }

    if (!eq_symbol(&raw.branch.nodes.data[0], string_to_symbol(mv_string("module")))) {
        err.range = raw.branch.nodes.data[0].range;
        err.message = mv_cstr_doc("Expecting keyword 'module' in module header.", a);
        throw_pi_error(point, err);
    }

    if (!is_symbol(raw.branch.nodes.data[1])) {
        err.range = raw.branch.nodes.data[1].range;
        err.message = mv_cstr_doc("Expecting parameter 'modulename' in module header.", a);
        throw_pi_error(point, err);
    }
    Symbol module_name = raw.branch.nodes.data[1].atom.symbol;

    // Now, imports/exports
    Imports imports;
    Exports exports;
    exports.export_all = false;

    imports.clauses = mk_import_clause_array(8, a);
    exports.clauses = mk_export_clause_array(8, a);
    for (size_t i = 2; i < raw.branch.nodes.len; i++) {
        RawTree clauses = raw.branch.nodes.data[i];
        if (clauses.type != RawBranch) {
            err.range = clauses.range;
            err.message = mv_cstr_doc("Expecting import/export list.", a);
            throw_pi_error(point, err);
        }
        if (clauses.branch.nodes.len < 1) {
            err.range = clauses.range;
            err.message = mv_cstr_doc("Not enough elements in import/export list", a);
            throw_pi_error(point, err);
        }

        if (eq_symbol(&clauses.branch.nodes.data[0], string_to_symbol(mv_string("import")))) {
            for (size_t i = 1; i < clauses.branch.nodes.len; i++) {
                ImportClause clause = abstract_import_clause(&clauses.branch.nodes.data[i], a, point);
                push_import_clause(clause, &imports.clauses);
            }
        } else if (eq_symbol(&clauses.branch.nodes.data[0], string_to_symbol(mv_string("export")))) {
            for (size_t i = 1; i < clauses.branch.nodes.len; i++) {
                ExportClause clause = abstract_export_clause(&clauses.branch.nodes.data[i], point, a);
                push_export_clause(clause, &exports.clauses);
            }
        } else {
            err.range = clauses.range;
            err.message = mv_cstr_doc("Expecting import/export list header.", a);
            throw_pi_error(point, err);
        }
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
  return (raw->type == RawAtom &&
          raw->atom.type == ASymbol &&
          raw->atom.symbol.name == s.name && 
          raw->atom.symbol.did == s.did);
}

bool is_symbol(RawTree raw) {
    return (raw.type == RawAtom && raw.atom.type == ASymbol);
}

RawTree* raw_slice(RawTree* raw, size_t drop, Allocator* a) {
#ifdef DEBUG
  if (drop > raw->branch.nodes.len) {
      panic(mv_string("Dropping more nodes than there are!"));
  }
#endif
    RawTree* out = mem_alloc(sizeof(RawTree), a);
    *out = (RawTree) {
        .type = RawBranch,
        .range.start = raw->branch.nodes.data[drop].range.start,
        .range.end = raw->branch.nodes.data[raw->branch.nodes.len - 1].range.end,
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
        raw = &raw->branch.nodes.data[1];
        if (is_symbol(*raw)) {
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
    if (raw->type == RawBranch && raw->branch.nodes.len >= 1) {
        raw = &raw->branch.nodes.data[0];
        if (is_symbol(*raw)) {
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
        RawTree node = nodes.branch.nodes.data[i];
        if (node.type != RawAtom || node.atom.type != ASymbol) { return false; }
        push_symbol(node.atom.symbol, arr);
    }
    return true;
}

Result get_annotated_symbol_list(SymPtrAssoc *args, RawTree list, ShadowEnv* env, Allocator* a, PiErrorPoint* point) {
    Result error_result = {.type = Err, .error_message = mv_string("Malformed proc argument list.")};
    if (list.type != RawBranch) { return error_result; }

    for (size_t i = 0; i < list.branch.nodes.len; i++) {
        RawTree annotation = list.branch.nodes.data[i];
        if (annotation.type == RawAtom) {
            sym_ptr_bind(annotation.atom.symbol, NULL, args);
        } else if (annotation.type == RawBranch || annotation.branch.nodes.len == 2) {
            RawTree arg = annotation.branch.nodes.data[0];
            RawTree raw_type = annotation.branch.nodes.data[1];
            if (arg.type != RawAtom || arg.atom.type != ASymbol) { return error_result; }
            Syntax* type = abstract_expr_i(raw_type, env, a, point); 

            sym_ptr_bind(arg.atom.symbol, type, args);
        } else { return error_result; }
    }
    return (Result) {.type = Ok};
}

Syntax* mk_application_body(Syntax* fn_syn, RawTree raw, ShadowEnv* env, Allocator* a, PiErrorPoint* point) {
    Syntax* res = mem_alloc(sizeof(Syntax), a);
    if (fn_syn->type == SConstructor) {
        *res = (Syntax) {
            .type = SVariant,
            .ptype = NULL,
            .range = raw.range,
            .variant.enum_type = fn_syn->constructor.enum_type,
            .variant.tagname = fn_syn->constructor.tagname,
            .variant.args = mk_ptr_array(raw.branch.nodes.len - 1, a),
        };

        for (size_t i = 1; i < raw.branch.nodes.len; i++) {
            Syntax* arg = abstract_expr_i(raw.branch.nodes.data[i], env, a, point);
            push_ptr(arg, &res->variant.args);
        }
    } else if (raw.branch.nodes.len > 1
               && raw.branch.nodes.data[1].type == RawBranch
               && raw.branch.nodes.data[1].branch.hint == HImplicit) {
        RawTree typelist = raw.branch.nodes.data[1];

        *res = (Syntax) {
            .type = SAllApplication,
            .ptype = NULL,
            .range = raw.range,
            .all_application.function = mem_alloc(sizeof(Syntax), a),
            .all_application.types = mk_ptr_array(typelist.branch.nodes.len, a),
            .all_application.implicits = mk_ptr_array(0, a),
            .all_application.args = mk_ptr_array(raw.branch.nodes.len - 2, a),
        };
        res->all_application.function = fn_syn;

        for (size_t i = 0; i < typelist.branch.nodes.len; i++) {
            Syntax* type = abstract_expr_i(typelist.branch.nodes.data[i], env, a, point);
            push_ptr(type, &res->all_application.types);
        }

        for (size_t i = 2; i < raw.branch.nodes.len; i++) {
            Syntax* arg = abstract_expr_i(raw.branch.nodes.data[i], env, a, point);
            push_ptr(arg, &res->all_application.args);
        }
    } else {
        *res = (Syntax) {
            .type = SApplication,
            .ptype = NULL,
            .range = raw.range,
            .application.function = mem_alloc(sizeof(Syntax), a),
            .application.implicits = mk_ptr_array(0, a),
            .application.args = mk_ptr_array(raw.branch.nodes.len - 1, a),
        };
        res->application.function = fn_syn;

        for (size_t i = 1; i < raw.branch.nodes.len; i++) {
            Syntax* arg = abstract_expr_i(raw.branch.nodes.data[i], env, a, point);
            push_ptr(arg, &res->application.args);
        }
    }

    return res;
}

Syntax* mk_application(RawTree raw, ShadowEnv* env, Allocator* a, PiErrorPoint* point) {
    Syntax* fn_syn = abstract_expr_i((raw.branch.nodes.data[0]), env, a, point);
    return mk_application_body(fn_syn, raw, env, a, point);
}

Syntax* mk_term(TermFormer former, RawTree raw, ShadowEnv* env, Allocator* a, PiErrorPoint* point) {
    PicoError err;
    switch (former) {
    case FDefine:
        err.range = raw.branch.nodes.data[0].range;
        err.message = mv_cstr_doc("'Define' (def) not supported as inner-expression term former.", a);
        throw_pi_error(point, err);
    case FDeclare:
        err.range = raw.branch.nodes.data[0].range;
        err.message = mv_cstr_doc("'Declare' not supported as inner-expression term former.", a);
        throw_pi_error(point, err);
    case FImport:
        err.range = raw.branch.nodes.data[0].range;
        err.message = mv_cstr_doc("'Import' (open) not supported as inner-expression term former.", a);
        throw_pi_error(point, err);
    case FProcedure: {
        if (raw.branch.nodes.len < 3) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Procedure term former requires at least 2 arguments!", a);
            throw_pi_error(point, err);
        }

        SymPtrAssoc implicits = mk_sym_ptr_assoc(0, a);
        SymPtrAssoc arguments = mk_sym_ptr_assoc(8, a);

        size_t args_index = 1;
        if (raw.branch.nodes.data[args_index].type == RawBranch
            && raw.branch.nodes.data[args_index].branch.hint == HImplicit) {
            Result args_out = get_annotated_symbol_list(&implicits, raw.branch.nodes.data[args_index], env, a, point);
            err.range = raw.branch.nodes.data[args_index].range;
            err.message = mv_str_doc(args_out.error_message, a);
            if (args_out.type == Err) throw_pi_error(point, err);
            args_index++;
        }
             
        if (raw.branch.nodes.data[args_index].type == RawBranch
            && raw.branch.nodes.data[args_index].branch.hint == HSpecial) {
            Result args_out = get_annotated_symbol_list(&arguments, raw.branch.nodes.data[args_index], env, a, point);
            err.range = raw.branch.nodes.data[args_index].range;
            err.message = mv_str_doc(args_out.error_message, a);
            if (args_out.type == Err) throw_pi_error(point, err);
            args_index++;
        }

        // TODO (BUG): shadow the arguments!
        SymbolArray to_shadow = mk_symbol_array(arguments.len, a);
        for (size_t i = 0; i < arguments.len; i++) {
            push_symbol(arguments.data[i].key, &to_shadow);
        }
        shadow_vars(to_shadow, env);

        RawTree* raw_term;
        if (raw.branch.nodes.len == args_index + 1) {
            raw_term = &raw.branch.nodes.data[args_index];
        } else {
            raw_term = raw_slice(&raw, args_index, a);
        }
        Syntax* body = abstract_expr_i(*raw_term, env, a, point);
        shadow_pop(arguments.len, env);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SProcedure,
            .ptype = NULL,
            .range = raw.range,
            .procedure.args = arguments,
            .procedure.implicits = implicits, 
            .procedure.body = body
        };
        return res;
    }
    case FAll: {
        if (raw.branch.nodes.len < 3) {
            err.range = raw.range;
            err.message = mv_cstr_doc("all term former requires at least 2 arguments!", a);
            throw_pi_error(point, err);
        }

        SymbolArray arguments = mk_symbol_array(2, a);
        if (!get_symbol_list(&arguments, raw.branch.nodes.data[1])) {
            err.range = raw.branch.nodes.data[1].range;
            err.message = mv_cstr_doc("all term former requires first arguments to be a symbol-list!", a);
            throw_pi_error(point, err);
        }

        shadow_vars(arguments, env);

        RawTree* raw_term;
        if (raw.branch.nodes.len == 3) {
            raw_term = &raw.branch.nodes.data[2];
        } else {
            raw_term= raw_slice(&raw, 2, a);
        }
        Syntax* body = abstract_expr_i(*raw_term, env, a, point);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SAll,
            .ptype = NULL,
            .range = raw.range,
            .all.args = arguments,
            .all.body = body,
        };
        return res;
    }
    case FMacro: {
        if (raw.branch.nodes.len < 2) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Malformed macro expression: expects at least 1 arg.", a);
            throw_pi_error(point, err);
        }
        RawTree* body = raw.branch.nodes.len == 2 ? &raw.branch.nodes.data[1] : raw_slice(&raw, 1, a);
        Syntax* transformer = abstract_expr_i(*body, env, a, point);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SMacro,
            .ptype = NULL,
            .range = raw.range,
            .transformer = transformer,
        };
        return res;
    }
    case FApplication: {
        return mk_application(raw, env, a, point);
    }
    case FExists: {
        if (raw.branch.nodes.len < 3) {
            err.message = mv_cstr_doc("Not enough terms provided to exists", a);
            throw_pi_error(point, err);
        }
        Syntax* type = abstract_expr_i(raw.branch.nodes.data[1], env, a, point);

        SynArray types = mk_ptr_array(8, a);
        {
            RawTree raw_types = raw.branch.nodes.data[2];
            if (raw_types.type != RawBranch && raw_types.branch.hint != HSpecial) {
                err.range = raw_types.range;
                err.message = mv_cstr_doc("Exists expects second argument to be a set of types", a);
                throw_pi_error(point, err);
            }

            for (size_t i = 0; i < raw_types.branch.nodes.len; i++) {
                Syntax* syn = abstract_expr_i(raw_types.branch.nodes.data[i], env, a, point);
                push_ptr(syn, &types);
            }
        }

        size_t body_idx = 3;
        SynArray implicits = mk_ptr_array(8, a);
        {
            RawTree raw_implicits = raw.branch.nodes.data[3];
            if (raw_implicits.type == RawBranch && raw_implicits.branch.hint == HImplicit) {
                body_idx++;
                for (size_t i = 0; i < raw_implicits.branch.nodes.len; i++) {
                    Syntax* syn = abstract_expr_i(raw_implicits.branch.nodes.data[i], env, a, point);
                    push_ptr(syn, &implicits);
                }
            }
        }

        if (body_idx == raw.branch.nodes.len) {
            err.message = mv_cstr_doc("Exists lacking a body", a);
            throw_pi_error(point, err);
        }

        RawTree *raw_term = (raw.branch.nodes.len == body_idx+1)
            ? &raw.branch.nodes.data[body_idx]
            : raw_slice(&raw, body_idx, a);
        Syntax* body = abstract_expr_i(*raw_term, env, a, point);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SExists,
            .ptype = NULL,
            .range = raw.range,
            .exists.type = type,
            .exists.types = types,
            .exists.implicits = implicits,
            .exists.body = body,
        };
        return res;
    }
    case FUnpack: {
        if (raw.branch.nodes.len < 3) {
            err.message = mv_cstr_doc("Not enough terms provided to unpack", a);
            throw_pi_error(point, err);
        }
        Syntax* packed = abstract_expr_i(raw.branch.nodes.data[1], env, a, point);

        SymbolArray types = mk_symbol_array(8, a);
        {
            RawTree raw_types = raw.branch.nodes.data[2];
            if (!get_symbol_list(&types, raw_types) || raw_types.branch.hint != HSpecial) {
                err.range = raw_types.range;
                err.message = mv_cstr_doc("Not enought terms provided to unpack", a);
                throw_pi_error(point, err);
            }
        }

        size_t body_idx = 3;
        SymbolArray implicits = mk_symbol_array(8, a);
        {
            RawTree raw_implicits = raw.branch.nodes.data[3];
            if (raw_implicits.type == RawBranch && raw_implicits.branch.hint == HImplicit) {
                body_idx++;
                if (!get_symbol_list(&types, raw_implicits)) {
                    err.range = raw_implicits.range;
                    err.message = mv_cstr_doc("Malformed implicit list in unpack", a);
                    throw_pi_error(point, err);
                }
            }
        }

        if (body_idx == raw.branch.nodes.len) {
            err.message = mv_cstr_doc("Unpack lacking a body", a);
            throw_pi_error(point, err);
        }

        RawTree *raw_term = (raw.branch.nodes.len == body_idx+1)
            ? &raw.branch.nodes.data[body_idx]
            : raw_slice(&raw, body_idx, a);
        Syntax* body = abstract_expr_i(*raw_term, env, a, point);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SUnpack,
            .ptype = NULL,
            .range = raw.range,
            .unpack.packed = packed,
            .unpack.types = types,
            .unpack.implicits = implicits,
            .unpack.body = body,
        };
        return res;
    }
    case FVariant: {
        if (raw.branch.nodes.len == 2) {
            // Check that we are indeed getting a result
            // Get the tag name of the variant
            RawTree msym = raw.branch.nodes.data[1];
            if (msym.type != RawAtom && msym.atom.type != ASymbol) {
                err.range = msym.range;
                err.message = mv_cstr_doc("Argument to variant term former should be symbol", a);
                throw_pi_error(point, err);
            }
            Symbol lit = msym.atom.symbol;

            // TODO (FEAT): caese special handling of unit, true, false syntaxes
            //              - make 'normal' types
            if (symbol_eq(lit, string_to_symbol(mv_string("unit")))) {
                Syntax* res = mem_alloc(sizeof(Syntax), a);
                *res = (Syntax) {.type = SLitUnit, .ptype = NULL, .range = raw.range, .boolean = true,};
                return res;
            }
            if (symbol_eq(lit, string_to_symbol(mv_string("true")))) {
                Syntax* res = mem_alloc(sizeof(Syntax), a);
                *res = (Syntax) {.type = SLitBool, .ptype = NULL, .range = raw.range, .boolean = true,};
                return res;
            }
            else if (symbol_eq(lit, string_to_symbol(mv_string("false")))) {
                Syntax* res = mem_alloc(sizeof(Syntax), a);
                *res = (Syntax) {.type = SLitBool, .ptype = NULL,  .range = raw.range, .boolean = false,};
                return res;
            } else {
                // Check that we are indeed getting a result
                // Get the tag gname of the variant
                RawTree msym = raw.branch.nodes.data[1];
                if (msym.type != RawAtom && msym.atom.type != ASymbol) {
                    err.range = msym.range;
                    err.message = mv_cstr_doc("First argument to variant term former should be symbol", a);
                    throw_pi_error(point, err);
                };

                Syntax* res = mem_alloc(sizeof(Syntax), a);
                *res = (Syntax) {
                    .type = SConstructor,
                    .ptype = NULL,
                    .range = raw.range,
                    .constructor.enum_type = NULL,
                    .constructor.tagname = msym.atom.symbol,
                };
                return res;
            }
        }
        else if (raw.branch.nodes.len != 3) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Variant term former expects at most two arguments!", a);
            throw_pi_error(point, err);
        } else {
            // Get the Type portion of the projector 
            Syntax* var_type = abstract_expr_i(raw.branch.nodes.data[2], env, a, point);
        
            // Check that we are indeed getting a result
            // Get the tag gname of the variant
            RawTree msym = raw.branch.nodes.data[1];
            if (msym.type != RawAtom && msym.atom.type != ASymbol) {
                err.range = msym.range;
                err.message = mv_cstr_doc("First argument to variant term former should be symbol", a);
                throw_pi_error(point, err);
            };

            Syntax* res = mem_alloc(sizeof(Syntax), a);
            *res = (Syntax) {
                .type = SConstructor,
                .ptype = NULL,
                .range = raw.range,
                .constructor.enum_type = var_type,
                .constructor.tagname = msym.atom.symbol,
            };
            return res;
        }
        break;
    }
    case FMatch: {
        if (raw.branch.nodes.len < 2) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Match expects at least 1 argument", a);
            throw_pi_error(point, err);
        }
        if (!is_expr(raw.branch.nodes.data[1])) {
            err.range = raw.branch.nodes.data[1].range;
            err.message = mv_cstr_doc("Match expects first argument to be expression (use parentheses '(' and ')')", a);
            throw_pi_error(point, err);
        }
        Syntax* sval = abstract_expr_i(raw.branch.nodes.data[1], env, a, point);

        ClauseArray clauses = mk_ptr_array(raw.branch.nodes.len - 2, a);
        
        for (size_t i = 2; i < raw.branch.nodes.len; i++) {
            // For each clause, we need three things:
            // 1. The tag, corresponding to the enum
            // 2. The variable(s) destructuring the enum
            // 3. The body
            // Get the clause
            RawTree raw_clause = raw.branch.nodes.data[i];
            if (raw_clause.type != RawBranch || raw_clause.branch.nodes.len < 2) {
                err.range = raw_clause.range;
                err.message = mv_cstr_doc("Match Clause has incorrect number of elements!", a);
                throw_pi_error(point, err);
            }

            // Get the pattern
            RawTree raw_pattern = raw_clause.branch.nodes.data[0];
            if (raw_pattern.type != RawBranch) {
                err.range = raw_pattern.range;
                err.message = mv_cstr_doc("Match Pattern should be a list!", a);
                throw_pi_error(point, err);
            }

            // The pattern has two parts: the variables & the tag
            // The tag should be in a constructor (i.e. list)

            Symbol clause_tagname; 
            SymbolArray clause_binds;
            RawTree mcol = raw_pattern.branch.nodes.data[0];
            if (raw_pattern.branch.nodes.len == 2 && is_symbol(mcol) && symbol_eq(mcol.atom.symbol, string_to_symbol(mv_string(":")))) {
                RawTree mname = raw_pattern.branch.nodes.data[1];
                if (!is_symbol(mname)) {
                    err.range = mname.range;
                    err.message = mv_cstr_doc("Bad pattern in match clause", a);
                    throw_pi_error(point, err);
                }

                clause_tagname = mname.atom.symbol;
                clause_binds = mk_symbol_array(0, a);
            } else {
                if (!get_fieldname(&raw_pattern.branch.nodes.data[0], &clause_tagname)) {
                    PtrArray nodes = mk_ptr_array(2, a);
                    push_ptr(mv_str_doc(mv_string("Unable to get tagname in pattern:"), a), &nodes);
                    push_ptr(pretty_rawtree(raw_pattern.branch.nodes.data[0], a), &nodes);
                    Document* doc = mv_sep_doc(nodes, a);
                    err.range = raw_clause.range;
                    err.message = doc;
                    throw_pi_error(point, err);
                }

                clause_binds = mk_symbol_array(raw_pattern.branch.nodes.len - 1, a);
                for (size_t s = 1; s < raw_pattern.branch.nodes.len; s++) {
                    RawTree raw_name = raw_pattern.branch.nodes.data[s];
                    if (!is_symbol(raw_name)) {
                        err.range = raw_clause.range;
                        err.message = mv_cstr_doc("Pattern binding was not a symbol!", a);
                        throw_pi_error(point, err);
                    }
                    push_symbol(raw_name.atom.symbol, &clause_binds); 
                }
            }

            // Get the term
            RawTree *raw_term = (raw_clause.branch.nodes.len == 2)
                ? &raw_clause.branch.nodes.data[1]
                : raw_slice(&raw_clause, 1, a);
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
            .range = raw.range,
            .match.val = sval,
            .match.clauses = clauses,
        };
        return res;
    }
    case FStructure: {
        if (raw.branch.nodes.len < 2) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Structure term former needs a structure type argument", a);
            throw_pi_error(point, err);
        }

        Syntax* sbase;
        size_t start_idx = 1;
        // Get the type of the structure
        if (raw.branch.nodes.data[1].type != RawBranch || raw.branch.nodes.data[1].branch.hint != HSpecial) {
            start_idx = 2;
            sbase = abstract_expr_i(raw.branch.nodes.data[1], env, a, point);
        } else {
            sbase = NULL;
        }

        // Construct a structure
        SymPtrAMap fields = mk_sym_ptr_amap(raw.branch.nodes.len, a);
        for (size_t i = start_idx; i < raw.branch.nodes.len; i++) {
            RawTree fdesc = raw.branch.nodes.data[i];
            if (fdesc.type != RawBranch) {
                err.range = fdesc.range;
                err.message = mv_cstr_doc("Structure expects all field descriptors to be lists.", a);
                throw_pi_error(point, err);
            }
            
            if (fdesc.branch.nodes.len < 2) {
                err.range = fdesc.range;
                err.message = mv_cstr_doc("Structure expects all field descriptors to have at least 2 elements.", a);
                throw_pi_error(point, err);
            }

            Symbol field;
            if (!get_fieldname(&fdesc.branch.nodes.data[0], &field)) {
                err.range = fdesc.branch.nodes.data[0].range;
                err.message = mv_cstr_doc("Structure has malformed field name.", a);
                throw_pi_error(point, err);
            }

            RawTree* val_desc = fdesc.branch.nodes.len == 2 ? &fdesc.branch.nodes.data[1] : raw_slice(&fdesc, 1, a); 
            Syntax* syn = abstract_expr_i(*val_desc, env, a, point);

            sym_ptr_insert(field, syn, &fields);
        }

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SStructure,
            .ptype = NULL,
            .range = raw.range,
            .structure.base = sbase,
            .structure.fields = fields,
        };
        return res;
    }
    case FProjector: {
        if (raw.branch.nodes.len != 3) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Projection term former needs two arguments!", a);
            throw_pi_error(point, err);
        }

        // Get the source portion of the proector 
        Syntax* source = abstract_expr_i(raw.branch.nodes.data[2], env, a, point);

        // Get the symbol portion of the projector
        RawTree msym = raw.branch.nodes.data[1];
        if (msym.type != RawAtom && msym.atom.type != ASymbol) {
            err.range = msym.range;
            err.message = mv_cstr_doc("Second argument to projection term former should be symbol", a);
            throw_pi_error(point, err);
        }

        return resolve_module_projector(raw.range, source, &msym, env, a, point);
    }
    case FInstance: {
        SymbolArray params = mk_symbol_array(0, a);
        SymPtrAssoc implicits = mk_sym_ptr_assoc(0, a);
        Syntax* constraint;

        size_t start_idx = 1;
        RawTree current = raw.branch.nodes.data[start_idx];

        switch (current.type == RawBranch ? current.branch.hint : HExpression) {
        case HExpression: goto parse_constraint;
        case HImplicit: goto parse_implicits;
        case HSpecial: goto parse_params;
        default: panic(mv_string("invalid hint!"));
        }
        // There are 2 optional nodes, we may skip them
        parse_params:

        if (!get_symbol_list(&params, current)) {
            err.range = current.range;
            err.message = mv_cstr_doc("Instance parameter list malformed.", a);
            throw_pi_error(point, err);
        }

        current = raw.branch.nodes.data[++start_idx];
        switch (current.type == RawBranch ? current.branch.hint : HExpression) {
        case HExpression: goto parse_constraint;
        case HImplicit: goto parse_implicits;
        case HSpecial:
            err.range = current.range;
            err.message = mv_cstr_doc("Invalid instance", a);
            throw_pi_error(point, err);
        default: panic(mv_string("invalid hint!"));
        }

        parse_implicits:

        get_annotated_symbol_list(&implicits, current, env, a, point);

        current = raw.branch.nodes.data[++start_idx];

        parse_constraint:

        constraint = abstract_expr_i(current, env, a, point);
        start_idx++;

        SymPtrAMap fields = mk_sym_ptr_amap(raw.branch.nodes.len - start_idx, a);
        for (size_t i = start_idx; i < raw.branch.nodes.len; i++) {
            RawTree fdesc = raw.branch.nodes.data[i];
            if (fdesc.type != RawBranch) {
                err.range = fdesc.range;
                err.message = mv_cstr_doc("Instance expects all field descriptors to be lists.", a);
                throw_pi_error(point, err);
            }
            
            if (fdesc.branch.nodes.len < 2) {
                err.range = fdesc.range;
                err.message = mv_cstr_doc("Instance expects all field descriptors to have at least 2 elements.", a);
                throw_pi_error(point, err);
            }

            Symbol field;
            if (!get_fieldname(&fdesc.branch.nodes.data[0], &field)) {
                err.range = fdesc.branch.nodes.data[0].range;
                err.message = mv_cstr_doc("Instance has malformed field name.", a);
                throw_pi_error(point, err);
            }

            RawTree* val_desc = fdesc.branch.nodes.len == 2 ? &fdesc.branch.nodes.data[1] : raw_slice(&fdesc, 1, a); 
            Syntax* syn = abstract_expr_i(*val_desc, env, a, point);

            sym_ptr_insert(field, syn, &fields);
        }

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SInstance,
            .ptype = NULL,
            .range = raw.range,
            .instance.params = params,
            .instance.implicits = implicits,
            .instance.constraint = constraint,
            .instance.fields = fields,
        };
        return res;
    }
    case FGenArray:
        panic(mv_string("abstract for gen-array not implemented"));
    case FWith:
        panic(mv_string("abstract for with not implemented"));
    case FDynamic: {
        if (raw.branch.nodes.len < 2) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Malformed dynamic expression: expects at least 1 arg.", a);
            throw_pi_error(point, err);
        }
        RawTree* body = raw.branch.nodes.len == 2 ? &raw.branch.nodes.data[1] : raw_slice(&raw, 1, a);
        Syntax* dynamic = abstract_expr_i(*body, env, a, point);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SDynamic,
            .ptype = NULL,
            .range = raw.range,
            .dynamic = dynamic,
        };
        return res;
    }
    case FDynamicUse: {
        if (raw.branch.nodes.len != 2) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Malformed use expression: expects at least 1 arg.", a);
            throw_pi_error(point, err);
        }
        Syntax* use = abstract_expr_i(raw.branch.nodes.data[1], env, a, point);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SDynamicUse,
            .ptype = NULL,
            .range = raw.range,
            .use = use,
        };
        return res;
    }
    case FDynamicSet: {
        if (raw.branch.nodes.len < 3) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Malformed set expression: expects 2 args.", a);
            throw_pi_error(point, err);
        }
        Syntax* dynamic = abstract_expr_i(raw.branch.nodes.data[1], env, a, point);
        Syntax* val = abstract_expr_i(*(raw.branch.nodes.len == 3 ? &raw.branch.nodes.data[2] : raw_slice(&raw, 2, a)), env, a, point);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SDynamicSet,
            .ptype = NULL,
            .range = raw.range,
            .dynamic_set.dynamic = dynamic,
            .dynamic_set.new_val = val,
        };
        return res;
    }
    case FDynamicLet: {
        PtrArray bindings = mk_ptr_array(raw.branch.nodes.len - 1, a);
        size_t index = 1;

        bool is_special = true;
        while (is_special && index < raw.branch.nodes.len) {
            RawTree bind = raw.branch.nodes.data[index];
            // bind [x₁ e₁]
            //      [x₂ e₂]
            //  body
            is_special = bind.type == RawBranch && bind.branch.hint == HSpecial;
            if (is_special) {
                index++;
                DynBinding* dbind = mem_alloc(sizeof(DynBinding), a);
                if (bind.type != RawBranch || bind.branch.nodes.len != 2) {
                    err.range = bind.range;
                    err.message = mv_cstr_doc("Malformed symbol binding in bind-expression", a);
                    throw_pi_error(point, err);
                }

                dbind->var = abstract_expr_i(bind.branch.nodes.data[0], env, a, point);
                dbind->expr = abstract_expr_i(bind.branch.nodes.data[1], env, a, point);
                push_ptr(dbind, &bindings);
            }
        }

        if (index > raw.branch.nodes.len - 1) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Bind expression has no body!", a);
            throw_pi_error(point, err);
        }

        if (index < raw.branch.nodes.len - 1) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Bind expression multiple bodies!", a);
            throw_pi_error(point, err);
        }

        Syntax* body = abstract_expr_i(raw.branch.nodes.data[index], env, a, point);
        shadow_pop(bindings.len, env);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SDynamicLet,
            .ptype = NULL,
            .range = raw.range,
            .dyn_let_expr.bindings = bindings,
            .dyn_let_expr.body = body,
        };
        return res;
    }
    case FLet: {
        SymSynAMap bindings = mk_sym_ptr_amap(raw.branch.nodes.len - 1, a);
        size_t index = 1;

        bool is_special = true;
        while (is_special) {
            RawTree bind = raw.branch.nodes.data[index];
            // let [x₁ e₁]
            //     [x₂ e₂]
            //  body
            is_special = bind.type == RawBranch && bind.branch.hint == HSpecial;
            if (is_special) {
                index++;
                Symbol sym;
                if (bind.type != RawBranch || bind.branch.nodes.len < 2) {
                    err.range = bind.range;
                    err.message = mv_cstr_doc("Malformed symbol binding in let-expression, not enough terms!", a);
                    throw_pi_error(point, err);
                }
                if (!get_label(&bind, &sym)) {
                    err.range = bind.range;
                    err.message = mv_cstr_doc("Expected symbol binding in let-expression", a);
                    throw_pi_error(point, err);
                }

                shadow_var(sym, env);
                Syntax *bind_body = bind.branch.nodes.len == 2
                    ? abstract_expr_i(bind.branch.nodes.data[1], env, a, point)
                    : abstract_expr_i(*raw_slice(&bind, 1, a), env, a, point);

                sym_ptr_insert(sym, bind_body, &bindings);
            }
        }

        if (index > raw.branch.nodes.len - 1) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Let expression has no body!", a);
            throw_pi_error(point, err);
        }

        if (index < raw.branch.nodes.len - 1) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Let expression multiple bodies!", a);
            throw_pi_error(point, err);
        }

        Syntax* body = abstract_expr_i(raw.branch.nodes.data[index], env, a, point);
        shadow_pop(bindings.len, env);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SLet,
            .ptype = NULL,
            .range = raw.range,
            .let_expr.bindings = bindings,
            .let_expr.body = body,
        };
        return res;
    }
    case FIf: {
        if (raw.branch.nodes.len != 4) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Term former 'if' expects precisely 3 arguments!", a);
            throw_pi_error(point, err);
        }
        SynArray terms = mk_ptr_array(3, a);
        for (size_t i = 1; i < raw.branch.nodes.len; i++) {
            Syntax* term = abstract_expr_i(raw.branch.nodes.data[i], env, a, point);
            push_ptr(term, &terms);
        }

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SIf,
            .ptype = NULL,
            .range = raw.range,
            .if_expr.condition = terms.data[0],
            .if_expr.true_branch = terms.data[1],
            .if_expr.false_branch = terms.data[2],
        };
        return res;
    }
    case FLabels: {
        if (raw.branch.nodes.len < 2) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Term former 'labels' expects at least 1 argument!", a);
            throw_pi_error(point, err);
        }

        Syntax* entry = abstract_expr_i(raw.branch.nodes.data[1], env, a, point);

        SymPtrAssoc terms = mk_sym_ptr_assoc(raw.branch.nodes.len - 2, a);
        for (size_t i = 2; i < raw.branch.nodes.len; i++) {
            Symbol label;
            RawTree* label_expr = &raw.branch.nodes.data[i];
            if (!get_label(label_expr, &label)) {
                err.range = label_expr->range;
                err.message = mv_cstr_doc("Each label must be of the form [label [args]? expr]. However, label is incorrect", a);
                throw_pi_error(point, err);
            }

            if (label_expr->branch.nodes.len < 2) {
                err.range = label_expr->range;
                err.message = mv_cstr_doc("Each label branch is expected to have at least 2 terms. However, this only has 1.", a);
                throw_pi_error(point, err);
            }
            
            size_t index = 1;
            SymPtrAssoc arguments = mk_sym_ptr_assoc(8, a);
            if (label_expr->branch.nodes.data[index].type == RawBranch
                && label_expr->branch.nodes.data[index].branch.hint == HSpecial) {
                Result args_out = get_annotated_symbol_list(&arguments, label_expr->branch.nodes.data[index++], env, a, point);
                if (args_out.type == Err) {
                    err.range = label_expr->branch.nodes.data[index].range;
                    err.message = mv_str_doc(args_out.error_message, a);
                    throw_pi_error(point, err);
                }
            }

            if (label_expr->branch.nodes.len < 1 + index) {
                err.range = label_expr->range;
                err.message = mv_cstr_doc("Label branch missing body!", a);
                throw_pi_error(point, err);
            }

            RawTree* raw_term = (label_expr->branch.nodes.len == index + 1)
                ? &label_expr->branch.nodes.data[index]
                : raw_slice(label_expr, index, a);
            Syntax* res = abstract_expr_i(*raw_term, env, a, point);
            SynLabelBranch* branch = mem_alloc(sizeof(SynLabelBranch), a);
            *branch = (SynLabelBranch) {
                .args = arguments,
                .body = res,
            };
            sym_ptr_bind(label, branch, &terms);
        }

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SLabels,
            .ptype = NULL,
            .range = raw.range,
            .labels.entry = entry,
            .labels.terms = terms,
        };
        return res;
    }
    case FGoTo: {
        if (raw.branch.nodes.len < 2) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Term former 'go-to' expects at least one argument!", a);
            throw_pi_error(point, err);
        }
        RawTree* label = &raw.branch.nodes.data[1]; 

        if (label->type != RawAtom && label->atom.type != ASymbol) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Term former 'go-to' expects first argument to be a symbol!", a);
            throw_pi_error(point, err);
        }

        PtrArray args = mk_ptr_array(raw.branch.nodes.len - 2, a);
        for (size_t i = 2; i < raw.branch.nodes.len; i++) {
            push_ptr(abstract_expr_i(raw.branch.nodes.data[i], env, a, point), &args);
        }

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SGoTo,
            .ptype = NULL,
            .range = raw.range,
            .go_to.label = label->atom.symbol,
            .go_to.args = args,
        };
        return res;
    }
    case FWithReset: {
        // with-reset [lbl] e [l1 l2] e
        if (raw.branch.nodes.len != 5) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Term former 'with-reset' expects exactly 5 arguments!", a);
            throw_pi_error(point, err);
        }
        SymbolArray reset_binds = mk_symbol_array(1, a);
        SymbolArray handle_binds = mk_symbol_array(2, a);

        if (!get_symbol_list(&reset_binds, raw.branch.nodes.data[1]) || reset_binds.len != 1) {
            err.range = raw.branch.nodes.data[1].range;
            err.message = mv_cstr_doc("Term former 'with-reset' 1st argument list malformed.", a);
            throw_pi_error(point, err);
        }

        Symbol reset_point_sym = reset_binds.data[0];
        if (reset_binds.len != 1) {
            err.range = raw.branch.nodes.data[0].range;
            err.message = mv_cstr_doc("Term former 'with-reset' expects exactly 5 arguments!", a);
            throw_pi_error(point, err);
        }

        Syntax* expr = abstract_expr_i(raw.branch.nodes.data[2], env, a, point);

        if (!get_symbol_list(&handle_binds, raw.branch.nodes.data[3]) || handle_binds.len != 3) {
            err.range = raw.branch.nodes.data[3].range;
            err.message = mv_cstr_doc("Handler list malformed!", a);
            throw_pi_error(point, err);
        }

        Symbol in_sym = handle_binds.data[1];
        Symbol cont_sym = handle_binds.data[2];

        Syntax* handler = abstract_expr_i(raw.branch.nodes.data[4], env, a, point);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SWithReset,
            .ptype = NULL,
            .range = raw.range,
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
            err.range = raw.range;
            err.message = mv_cstr_doc("Term former 'reset-to' expects two arguments!", a);
            throw_pi_error(point, err);
        }

        Syntax* rpoint = abstract_expr_i(raw.branch.nodes.data[1], env, a, point);
        Syntax* rarg = abstract_expr_i(raw.branch.nodes.data[2], env, a, point);
        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SResetTo,
            .ptype = NULL,
            .range = raw.range,
            .reset_to.point = rpoint,
            .reset_to.arg = rarg,
        };
        return res;
    }
    case FSequence: {
        SynArray elements = mk_ptr_array(raw.branch.nodes.len - 1, a);
        size_t num_binds = 0;
        for (size_t i = 1; i < raw.branch.nodes.len; i++) {
            RawTree tree = raw.branch.nodes.data[i];
            if (tree.type == RawBranch && tree.branch.hint == HSpecial) {
                if (tree.type != RawBranch
                    || !eq_symbol(&tree.branch.nodes.data[0], string_to_symbol(mv_string("let!")))) {
                    err.range = tree.branch.nodes.data[0].range;
                    err.message = mv_cstr_doc("Special nodes in a 'seq' are expected to have a head 'let!'", a);
                    throw_pi_error(point, err);
                }
                if (tree.branch.nodes.len < 3) {
                    err.range = tree.range;
                    err.message = mv_cstr_doc("let! requires at least 2 terms.", a);
                    throw_pi_error(point, err);
                }
                RawTree rsym = tree.branch.nodes.data[1];
                if (rsym.type != RawAtom || rsym.atom.type != ASymbol) {
                    err.range = tree.branch.nodes.data[1].range;
                    err.message = mv_cstr_doc("'let!' expected to bind a symbol", a);
                    throw_pi_error(point, err);
                }

                RawTree* body = tree.branch.nodes.len > 3 ? raw_slice(&tree, 2, a) : &tree.branch.nodes.data[2];
                SeqElt* elt = mem_alloc(sizeof(SeqElt), a);
                *elt = (SeqElt) {
                    .is_binding = true,
                    .symbol = rsym.atom.symbol,
                    .expr = abstract_expr_i(*body, env, a, point),
                };
                push_ptr(elt, &elements);
            } else {
                SeqElt* elt = mem_alloc(sizeof(SeqElt), a);
                *elt = (SeqElt) {
                    .is_binding = false,
                    .expr = abstract_expr_i(tree, env, a, point),
                };
                push_ptr(elt, &elements);
            }
        }
        shadow_pop(num_binds, env);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SSequence,
            .ptype = NULL,
            .range = raw.range,
            .sequence.elements = elements,
        };
        return res;
    }
    case FModule:
        err.range = raw.range;
        err.message = mk_cstr_doc("'Module' not supported as inner-expression term former.", a);
        throw_pi_error(point, err);
    case FIs: {
        if (raw.branch.nodes.len != 3) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Term former 'is' expects precisely 2 arguments!", a);
            throw_pi_error(point, err);
        }

        Syntax* term = abstract_expr_i(raw.branch.nodes.data[1], env, a, point);
        Syntax* type = abstract_expr_i(raw.branch.nodes.data[2], env, a, point);
        
        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SIs,
            .ptype = NULL,
            .range = raw.range,
            .is = {.val = term, .type = type},
        };
        return res;
    }
    case FInTo: {
        if (raw.branch.nodes.len != 3) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Term former 'into' expects precisely 2 arguments!", a);
            throw_pi_error(point, err);
        }

        Syntax* type = abstract_expr_i(raw.branch.nodes.data[1], env, a, point);
        Syntax* term = abstract_expr_i(raw.branch.nodes.data[2], env, a, point);
        
        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SInTo,
            .ptype = NULL,
            .range = raw.range,
            .into = {.val = term, .type = type},
        };
        return res;
    }
    case FOutOf: {
        if (raw.branch.nodes.len != 3) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Term former 'out-of' expects precisely 2 arguments!", a);
            throw_pi_error(point, err);
        }

        Syntax* type = abstract_expr_i(raw.branch.nodes.data[1], env, a, point);
        Syntax* term = abstract_expr_i(raw.branch.nodes.data[2], env, a, point);
        
        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SOutOf,
            .ptype = NULL,
            .range = raw.range,
            .into = {.val = term, .type = type},
        };
        return res;
    }
    case FName: {
        if (raw.branch.nodes.len != 3) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Term former 'name' expects precisely 2 arguments!", a);
            throw_pi_error(point, err);
        }

        Syntax* type = abstract_expr_i(raw.branch.nodes.data[1], env, a, point);
        Syntax* term = abstract_expr_i(raw.branch.nodes.data[2], env, a, point);
        
        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SName,
            .ptype = NULL,
            .range = raw.range,
            .name = {.val = term, .type = type},
        };
        return res;
    }
    case FUnName: {
        if (raw.branch.nodes.len != 2) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Term former 'unname' expects precisely 2 arguments!", a);
            throw_pi_error(point, err);
        }

        Syntax* term = abstract_expr_i(raw.branch.nodes.data[1], env, a, point);
        
        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SUnName,
            .ptype = NULL,
            .range = raw.range,
            .unname = term,
        };
        return res;
    }
    case FWiden: {
        if (raw.branch.nodes.len != 3) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Term former 'widen' expects precisely 2 arguments!", a);
            throw_pi_error(point, err);
        }

        Syntax* type = abstract_expr_i(raw.branch.nodes.data[2], env, a, point);
        Syntax* term = abstract_expr_i(raw.branch.nodes.data[1], env, a, point);
        
        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SWiden,
            .ptype = NULL,
            .range = raw.range,
            .name = {.val = term, .type = type},
        };
        return res;
    }
    case FNarrow: {
        if (raw.branch.nodes.len != 3) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Term former 'narrow' expects precisely 2 arguments!", a);
            throw_pi_error(point, err);
        }

        Syntax* type = abstract_expr_i(raw.branch.nodes.data[2], env, a, point);
        Syntax* term = abstract_expr_i(raw.branch.nodes.data[1], env, a, point);
        
        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SNarrow,
            .ptype = NULL,
            .range = raw.range,
            .name = {.val = term, .type = type},
        };
        return res;
    }
    case FDynAlloc: {
        if (raw.branch.nodes.len != 2) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Term former 'dynamic-alloc' expects precisely 2 arguments!", a);
            throw_pi_error(point, err);
        }

        Syntax* term = abstract_expr_i(raw.branch.nodes.data[1], env, a, point);
        
        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SDynAlloc,
            .ptype = NULL,
            .range = raw.range,
            .size = term,
        };
        return res;
    }
    case FSizeOf: {
        if (raw.branch.nodes.len != 2) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Term former 'size-of' expects precisely 1 argument!", a);
            throw_pi_error(point, err);
        }

        Syntax* term = abstract_expr_i(raw.branch.nodes.data[1], env, a, point);
        
        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SSizeOf,
            .ptype = NULL,
            .range = raw.range,
            .size = term,
        };
        return res;
    }
    case FAlignOf: {
        if (raw.branch.nodes.len != 2) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Term former 'align-of' expects precisely 1 argument!", a);
            throw_pi_error(point, err);
        }

        Syntax* term = abstract_expr_i(raw.branch.nodes.data[1], env, a, point);
        
        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SAlignOf,
            .ptype = NULL,
            .range = raw.range,
            .size = term,
        };
        return res;
    }
    case FOffsetOf: {
        if (raw.branch.nodes.len != 3) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Term former 'offset-of' expects precisely 2 arguments!", a);
            throw_pi_error(point, err);
        }

        if (!is_symbol(raw.branch.nodes.data[1])) {
            err.range = raw.branch.nodes.data[1].range;
            err.message = mv_cstr_doc("offset-of requires first term to be a symbol (fieldname) of a structure", a);
            throw_pi_error(point, err);
        }
        Symbol field = raw.branch.nodes.data[1].atom.symbol;
        Syntax* term = abstract_expr_i(raw.branch.nodes.data[2], env, a, point);
        
        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SOffsetOf,
            .ptype = NULL,
            .range = raw.range,
            .offset_of.field = field,
            .offset_of.body = term,
        };
        return res;
    }
    case FArrayType: {
        panic(mv_string("Array type former not implemented."));
    }
    case FProcType: {
        if (raw.branch.nodes.len != 3) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Wrong number of terms to proc type former.", a);
            throw_pi_error(point, err);
        }

        RawTree raw_args = raw.branch.nodes.data[1];
        if (raw_args.type != RawBranch) {
            err.range = raw.branch.nodes.data[1].range;
            err.message = mv_cstr_doc("Procedure argument list should be list.", a);
            throw_pi_error(point, err);
        }
        
        PtrArray arg_types = mk_ptr_array(raw_args.branch.nodes.len, a);

        for (size_t i = 0; i < raw_args.branch.nodes.len; i++) {
            Syntax* arg_ty = abstract_expr_i(raw_args.branch.nodes.data[i], env, a, point);
            push_ptr(arg_ty, &arg_types);
        }

        Syntax* return_type = abstract_expr_i(raw.branch.nodes.data[2], env, a, point);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SProcType,
            .ptype = NULL,
            .range = raw.range,
            .proc_type.args = arg_types,
            .proc_type.return_type = return_type,
        };
        return res;
    }
    case FStructType: {
        SymSynAMap field_types = mk_sym_ptr_amap(raw.branch.nodes.len, a);

        for (size_t i = 1; i < raw.branch.nodes.len; i++) {
            RawTree fdesc = raw.branch.nodes.data[i];
            if (fdesc.type != RawBranch) {
                err.range = fdesc.range;
                err.message = mv_cstr_doc("Structure type expects all field descriptors to be lists.", a);
                throw_pi_error(point, err);
            };
            
            if (fdesc.branch.nodes.len < 2) {
                err.range = fdesc.range;
                err.message = mv_cstr_doc("Structure type expects all field descriptors to have a type.", a);
                throw_pi_error(point, err);
            };

            Symbol field;
            if (!get_fieldname(&fdesc.branch.nodes.data[0], &field)) {
                err.range = fdesc.branch.nodes.data[0].range;
                err.message = mv_cstr_doc("Structure type has malformed field name.", a);
                throw_pi_error(point, err);
            };

            RawTree* raw_ty = (fdesc.branch.nodes.len == 2) ? &fdesc.branch.nodes.data[1] : raw_slice(&fdesc, 1, a);
            Syntax* field_ty = abstract_expr_i(*raw_ty, env, a, point);

            sym_ptr_insert(field, field_ty, &field_types);
        }

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SStructType,
            .ptype = NULL,
            .range = raw.range,
            .struct_type.fields = field_types,
        };
        return res;
    }
    case FEnumType: {
        SymPtrAMap enum_variants = mk_sym_ptr_amap(raw.branch.nodes.len, a);

        for (size_t i = 1; i < raw.branch.nodes.len; i++) {
            RawTree edesc = raw.branch.nodes.data[i];

            if (edesc.type != RawBranch) {
                err.range = edesc.branch.nodes.data[0].range;
                err.message = mv_cstr_doc("Enumeration type expects all variant descriptors to be lists.", a);
                throw_pi_error(point, err);
            };
            
            if (edesc.branch.nodes.len < 1) {
                err.range = edesc.branch.nodes.data[0].range;
                err.message = mv_cstr_doc("Enumeration type expects all variant descriptors to have at least 1 elements.", a);
                throw_pi_error(point, err);
            };

            // First, see if 'edesc' has ':' followed by 1 symbol
            RawTree mcol = edesc.branch.nodes.data[0];
            // TODO replace ":" with 'a symbol that resolves to the constructor/variant
            // term former!
            if (edesc.branch.nodes.len == 2 && is_symbol(mcol) && symbol_eq(mcol.atom.symbol, string_to_symbol(mv_string(":")))) {
                RawTree mname = edesc.branch.nodes.data[1];
                if (!is_symbol(mname)) {
                    err.range = mname.range;
                    err.message = mv_cstr_doc("Enumeration type expects variant descriptors to have a symbol name.", a);
                    throw_pi_error(point, err);
                } 

                PtrArray* types = mem_alloc(sizeof(PtrArray), a);
                *types = mk_ptr_array(0, a);
                sym_ptr_insert(mname.atom.symbol, types, &enum_variants);
            } else {
                Symbol tagname;
                PtrArray* types = mem_alloc(sizeof(PtrArray), a);
                *types = mk_ptr_array(edesc.branch.nodes.len - 1, a);

                if (!get_fieldname(&edesc.branch.nodes.data[0], &tagname)) {
                    err.range = edesc.branch.nodes.data[0].range;
                    err.message = mv_cstr_doc("Enum type has malformed field name.", a);
                    throw_pi_error(point, err);
                };

                for (size_t i = 1; i < edesc.branch.nodes.len; i++) {
                    Syntax* field_ty = abstract_expr_i(edesc.branch.nodes.data[i], env, a, point);
                    push_ptr(field_ty, types);
                }

                sym_ptr_insert(tagname, types, &enum_variants);
            }
        }

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SEnumType,
            .ptype = NULL,
            .range = raw.range,
            .enum_type.variants = enum_variants,
        };
        return res;
    }
    case FResetType: {
        if (raw.branch.nodes.len != 3) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Reset type former expects exactly 2 arguments!", a);
            throw_pi_error(point, err);
        }
        Syntax* in_ty = abstract_expr_i(raw.branch.nodes.data[1], env, a, point);
        Syntax* out_ty = abstract_expr_i(raw.branch.nodes.data[2], env, a, point);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SResetType,
            .ptype = NULL,
            .range = raw.range,
            .reset_type.in = in_ty,
            .reset_type.out = out_ty,
        };
        return res;
    }
    case FDynamicType: {
        if (raw.branch.nodes.len <= 1) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Malformed Dynamic Type expression: expects at least 1 arg.", a);
            throw_pi_error(point, err);
        }
        RawTree* body = raw.branch.nodes.len == 2 ? &raw.branch.nodes.data[1] : raw_slice(&raw, 1, a);
        Syntax* dyn_ty = abstract_expr_i(*body, env, a, point);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SDynamicType,
            .ptype = NULL,
            .range = raw.range,
            .dynamic_type = dyn_ty,
        };
        return res;
    }
    case FNamedType: {
        if (raw.branch.nodes.len <= 2) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Malformed Named Type expression: expects at least 2 args.", a);
            throw_pi_error(point, err);
        }

        RawTree* rname = &raw.branch.nodes.data[1];
        if (!is_symbol(*rname)) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Malformed Named Type expression: 1st arg to be name.", a);
            throw_pi_error(point, err);
        }
        Symbol name = rname->atom.symbol;

        RawTree* body = raw.branch.nodes.len == 3 ? &raw.branch.nodes.data[2] : raw_slice(&raw, 2, a);
        Syntax* rec_body = abstract_expr_i(*body, env, a, point);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SNamedType,
            .ptype = NULL,
            .range = raw.range,
            .named_type.name = name,
            .named_type.body = rec_body,
        };
        return res;
    }
    case FDistinctType: {
        if (raw.branch.nodes.len <= 1) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Malformed Distinct Type expression: expects at least 1 arg.", a);
            throw_pi_error(point, err);
        }
        RawTree* body = raw.branch.nodes.len == 2 ? &raw.branch.nodes.data[1] : raw_slice(&raw, 1, a);
        Syntax* distinct = abstract_expr_i(*body, env, a, point);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SDistinctType,
            .ptype = NULL,
            .range = raw.range,
            .distinct_type = distinct,
        };
        return res;
    }
    case FOpaqueType: {
        if (raw.branch.nodes.len <= 1) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Malformed Opaque Type expression: expects at least 1 arg.", a);
            throw_pi_error(point, err);
        }
        RawTree* body = raw.branch.nodes.len == 2 ? &raw.branch.nodes.data[1] : raw_slice(&raw, 1, a);
        Syntax* opaque = abstract_expr_i(*body, env, a, point);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SOpaqueType,
            .ptype = NULL,
            .range = raw.range,
            .opaque_type = opaque,
        };
        return res;
    }
    case FTraitType: {
        if (raw.branch.nodes.len < 2) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Wrong number of terms to trait type former.", a);
            throw_pi_error(point, err);
        }

        RawTree raw_vars = raw.branch.nodes.data[1];
        SymbolArray vars = mk_symbol_array(raw_vars.branch.nodes.len, a);

        if (!get_symbol_list(&vars, raw_vars)) {
            err.range = raw_vars.range;
            err.message = mv_cstr_doc("Malformed Trait parameter list.", a);
            throw_pi_error(point, err);
        }

        SymPtrAMap fields = mk_sym_ptr_amap(raw.branch.nodes.len - 2, a);
        for (size_t i = 2; i < raw.branch.nodes.len; i++) {
            RawTree fdesc = raw.branch.nodes.data[i];
            if (fdesc.type != RawBranch) {
                err.range = fdesc.range;
                err.message = mv_cstr_doc("Trait expects all field descriptors to be lists.", a);
                throw_pi_error(point, err);
            }
            
            if (fdesc.branch.nodes.len < 2) {
                err.range = fdesc.range;
                err.message = mv_cstr_doc("Trait expects all field descriptors to have at least 2 elements.", a);
                throw_pi_error(point, err);
            }

            Symbol field;
            if (!get_fieldname(&fdesc.branch.nodes.data[0], &field)) {
                err.range = fdesc.branch.nodes.data[0].range;
                err.message = mv_cstr_doc("Trait has malformed field name.", a);
                throw_pi_error(point, err);
            }
            Syntax* syn;
            if (fdesc.branch.nodes.len == 2) {
                syn = abstract_expr_i(fdesc.branch.nodes.data[1], env, a, point);
            } else {
                RawTree* raw_term = raw_slice(&fdesc, 1, a);
                syn = abstract_expr_i(*raw_term, env, a, point);
            }

            sym_ptr_insert(field, syn, &fields);
        }

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = STraitType,
            .ptype = NULL,
            .range = raw.range,
            .trait.vars = vars,
            .trait.fields = fields,
        };
        return res;
    }
    case FAllType: {
        if (raw.branch.nodes.len < 3) {
            err.range = raw.range;
            err.message = mk_cstr_doc("All term former requires at least 2 arguments!", a);
            throw_pi_error(point, err);
        }

        SymbolArray vars = mk_symbol_array(8, a);
        if (!get_symbol_list(&vars, raw.branch.nodes.data[1])) {
            err.range = raw.branch.nodes.data[1].range;
            err.message = mk_cstr_doc("All argument list malformed", a);
            throw_pi_error(point, err);
        }

        shadow_vars(vars, env);

        RawTree* raw_term;
        if (raw.branch.nodes.len == 3) {
            raw_term = &raw.branch.nodes.data[2];
        } else {
            raw_term = raw_slice(&raw, 2, a);
        }
        Syntax* body = abstract_expr_i(*raw_term, env, a, point);
        shadow_pop(vars.len, env);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SAllType,
            .ptype = NULL,
            .range = raw.range,
            .bind_type.bindings = vars,
            .bind_type.body = body,
        };
        return res;
    }
    case FExistsType: {
        if (raw.branch.nodes.len < 3) {
            err.range = raw.range;
            err.message = mk_cstr_doc("Exists type former requires at least 2 arguments!", a);
            throw_pi_error(point, err);
        }

        SymbolArray vars = mk_symbol_array(8, a);
        if (!get_symbol_list(&vars, raw.branch.nodes.data[1])) {
            err.range = raw.branch.nodes.data[1].range;
            err.message = mk_cstr_doc("Exists argument list malformed", a);
            throw_pi_error(point, err);
        }
        shadow_vars(vars, env);

        size_t body_idx = 2;
        SynArray implicits = mk_ptr_array(8, a);
        {
            RawTree raw_implicits = raw.branch.nodes.data[body_idx];
            if (raw_implicits.type == RawBranch && raw_implicits.branch.hint == HImplicit) {
                body_idx++;
                for (size_t i = 0; i < raw_implicits.branch.nodes.len; i++) {
                    Syntax* syn = abstract_expr_i(raw_implicits.branch.nodes.data[i], env, a, point);
                    push_ptr(syn, &implicits);
                }
            }
        }

        RawTree* raw_term;
        if (raw.branch.nodes.len == body_idx + 1) {
            raw_term = &raw.branch.nodes.data[body_idx];
        } else {
            raw_term = raw_slice(&raw, body_idx, a);
        }
        Syntax* body = abstract_expr_i(*raw_term, env, a, point);
        shadow_pop(vars.len, env);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SExistsType,
            .ptype = NULL,
            .range = raw.range,
            .exists_type.vars = vars,
            .exists_type.implicits = implicits,
            .exists_type.body = body,
        };
        return res;
    }
    case FFamily: {
        if (raw.branch.nodes.len < 3) {
            err.range = raw.range;
            err.message = mk_cstr_doc("Family term former requires at least 2 arguments!", a);
            throw_pi_error(point, err);
        }

        SymbolArray vars = mk_symbol_array(8, a);
        if (!get_symbol_list(&vars, raw.branch.nodes.data[1])) {
            err.range = raw.range;
            err.message = mk_cstr_doc("All argument list malformed", a);
            throw_pi_error(point, err);
        }

        shadow_vars(vars, env);

        RawTree* raw_term = (raw.branch.nodes.len == 3)
            ? &raw.branch.nodes.data[2]
            : raw_slice(&raw, 2, a);
        
        Syntax* body = abstract_expr_i(*raw_term, env, a, point);
        shadow_pop(vars.len, env);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = STypeFamily,
            .ptype = NULL,
            .range = raw.range,
            .bind_type.bindings = vars,
            .bind_type.body = body,
        };
        return res;
    }
    case FLiftCType: {
        RawTree* raw_term = (raw.branch.nodes.len == 2)
            ? &raw.branch.nodes.data[1]
            : raw_slice(&raw, 1, a);
        Syntax* c_type = abstract_expr_i(*raw_term, env, a, point);
        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SLiftCType,
            .ptype = NULL,
            .range = raw.range,
            .c_type = c_type,
        };
        return res;
    }
    case FReinterpretNative:
    case FReinterpretRelic: {
        if (raw.branch.nodes.len < 3) {
            err.range = raw.range;
            err.message = mk_cstr_doc("Reinterpret term former requires at least 2 arguments!", a);
            throw_pi_error(point, err);
        }

        Syntax* type = abstract_expr_i(raw.branch.nodes.data[1], env, a, point);

        RawTree* raw_term = (raw.branch.nodes.len == 3)
            ? &raw.branch.nodes.data[2]
            : raw_slice(&raw, 2, a);

        Syntax* body = abstract_expr_i(*raw_term, env, a, point);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SReinterpret,
            .ptype = NULL,
            .range = raw.range,
            .reinterpret.from_native = former == FReinterpretNative,
            .reinterpret.type = type, 
            .reinterpret.body = body,
        };
        return res;
    }
    case FConvertNative:
    case FConvertRelic: {
        if (raw.branch.nodes.len < 3) {
            err.range = raw.range;
            err.message = mk_cstr_doc("convert term former requires at least 2 arguments!", a);
            throw_pi_error(point, err);
        }

        Syntax* type = abstract_expr_i(raw.branch.nodes.data[1], env, a, point);

        RawTree* raw_term = (raw.branch.nodes.len == 3)
            ? &raw.branch.nodes.data[2]
            : raw_slice(&raw, 2, a);

        Syntax* body = abstract_expr_i(*raw_term, env, a, point);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SConvert,
            .ptype = NULL,
            .range = raw.range,
            .convert.from_native = former == FConvertNative,
            .convert.type = type, 
            .convert.body = body,
        };
        return res;
    }
    case FTypeOf: {
        if (raw.branch.nodes.len < 2) {
            err.range = raw.range;
            err.message = mk_cstr_doc("type-of term former requires 1 argument!", a);
            throw_pi_error(point, err);
        }
        RawTree* raw_term = (raw.branch.nodes.len == 2)
            ? &raw.branch.nodes.data[1]
            : raw_slice(&raw, 1, a);

        Syntax* body = abstract_expr_i(*raw_term, env, a, point);

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = STypeOf,
            .ptype = NULL,
            .range = raw.range,
            .type_of = body,
        };
        return res;
    }
    case FDescribe: {
        if (raw.branch.nodes.len != 2) {
            err.range = raw.range;
            err.message = mk_cstr_doc("describe term former requires 1 argument!", a);
            throw_pi_error(point, err);
        }

        Syntax* abs = abstract_expr_i(raw.branch.nodes.data[1], env, a, point);
        SymbolArray* path = try_get_path(abs, a);

        if (path) {
            Syntax* res = mem_alloc(sizeof(Syntax), a);
            *res = (Syntax) {
                .type = SDescribe,
                .ptype = NULL,
                .range = raw.range,
                .to_describe = *path,
            };
            return res;
        } else {
            err.range = raw.branch.nodes.data[1].range;
            err.message = mk_cstr_doc("describe expects argument to be a path!", a);
            throw_pi_error(point, err);
        }
        break;
    }
    case FQuote: {
        if (raw.branch.nodes.len != 2) {
            err.range = raw.range;
            err.message = mk_cstr_doc("quote term former expects 1 argument!", a);
            throw_pi_error(point, err);
        }

        RawTree body = raw.branch.nodes.data[1];

        Syntax* res = mem_alloc(sizeof(Syntax), a);
        *res = (Syntax) {
            .type = SQuote,
            .ptype = NULL,
            .range = raw.range,
            .quoted = body,
        };
        return res;
    }
    case FCapture: {
        if (raw.branch.nodes.len != 2) {
            err.range = raw.range;
            err.message = mk_cstr_doc("capture term former expects 1 argument!", a);
            throw_pi_error(point, err);
        }
        
        void* value = NULL;
        PiType* type = NULL;

        RawTree capture = raw.branch.nodes.data[1];
        if (is_symbol(capture)) {
            ShadowEntry entry = shadow_env_lookup(capture.atom.symbol, env);
            if (entry.type == SGlobal) {
                value = entry.value;
                type = entry.vtype;
            }
            
            Syntax* res = mem_alloc(sizeof(Syntax), a);
            *res = (Syntax) {
                .type = SCapture,
                .ptype = NULL,
                .range = capture.range,
                .capture.value = value,
                .capture.type = type,
            };
            return res;
        }

        Syntax* cap = abstract_expr_i(capture, env, a, point);
        if (cap->type != SAbsVariable) {
            err.range = capture.range;
            err.message = mk_cstr_doc("Capture term former must capture a symbol or path", a);
            throw_pi_error(point, err);
        } else {
            Syntax* res = mem_alloc(sizeof(Syntax), a);
            *res = (Syntax) {
                .type = SCapture,
                .ptype = NULL,
                .range = capture.range,
                .capture.value = cap->abvar.value,
                .capture.type = cap->ptype,
            };
            return res;
        }
    }
    }
    panic(mv_string("Invalid term-former provided to mk_term."));
}

void get_array_shape(RawTree raw, U64Array* shape_out, Allocator *a, PiErrorPoint *point) {
    if (raw.type == RawBranch && raw.branch.hint == HData) {
        if (raw.branch.nodes.len == 0) {
            PicoError err = (PicoError) {
                .range = raw.range,
                .message = mk_cstr_doc("Array literal must have > 1 terms", a),
            };
            throw_pi_error(point, err);
        }
        push_u64(raw.branch.nodes.len, shape_out);
        get_array_shape(raw.branch.nodes.data[0], shape_out, a, point);
    }
}

void mk_array_inner(PtrArray* terms, uint64_t depth, U64Array expected_shape, RawTree raw, ShadowEnv *env, Allocator *a, PiErrorPoint *point) {
    if (raw.type == RawBranch && raw.branch.hint == HData) {
        if (expected_shape.len <= depth) {
            PicoError err = (PicoError) {
                .range = raw.range,
                .message = mk_cstr_doc("Non-rectangular array: this part does not match the expected shape - too deep.", a),
            };
            throw_pi_error(point, err);
        }
        if (raw.branch.nodes.len != expected_shape.data[depth]) {
            PicoError err = (PicoError) {
                .range = raw.range,
                .message = mk_cstr_doc("Non-rectangular array: this part does not match the expected shape.", a),
            };
            throw_pi_error(point, err);
        }
        for (size_t i = 0; i < raw.branch.nodes.len; i++) {
            mk_array_inner(terms, depth + 1, expected_shape, raw.branch.nodes.data[i], env, a, point);
        }
    } else {
        // just abstract and push
        Syntax* term = abstract_expr_i(raw, env, a, point);
        push_ptr(term, terms);
    }
}

Syntax *mk_array_literal(RawTree raw, ShadowEnv *env, Allocator *a, PiErrorPoint *point) {
    PtrArray terms = mk_ptr_array(8, a);
    U64Array shape = mk_u64_array(8, a);
    get_array_shape(raw, &shape, a, point);
    mk_array_inner(&terms, 0, shape, raw, env, a, point);

    Syntax* res = mem_alloc(sizeof(Syntax), a);
    *res = (Syntax) {
        .type = SLitArray,
        .ptype = NULL,
        .range = raw.range,
        .array_lit.shape = shape,
        .array_lit.subterms = terms,
    };
    return res;
}

MacroResult eval_macro(ComptimeHead head, RawTree raw, Allocator* a) {
    // Call the function (uses Pico ABI)
    MacroResult output;
    RawTreeArray input = raw.branch.nodes;
    void* dvars = get_dynamic_memory();
    void* dynamic_memory_space = mem_alloc(4096, a);
    void* dynamic_memory_ptr = dynamic_memory_space + 4095; // ??

    Allocator old_temp_alloc = set_std_temp_allocator(*a);
    Allocator old_current_alloc = set_std_current_allocator(*a);

    // TODO: swap so this is backend independent (use foreign_adapters to call) 
    int64_t out;
    __asm__ __volatile__(
                         // save nonvolatile registers
                         "push %%rbp       \n" // Nonvolatile on System V + Win64
                         "push %%rbx       \n" // Nonvolatile on System V + Win64
                         "push %%rdi       \n" // Nonvolatile on Win 64
                         "push %%rsi       \n" // Nonvolatile on Win 64
                         "push %%r15       \n" // for dynamic vars
                         "push %%r14       \n" // for dynamic memory space
                         "push %%r13       \n" // for control/indexing memory space
                         "push %%r12       \n" // Nonvolatile on System V + Win64

                         "mov %3, %%r13    \n"
                         "mov %2, %%r15    \n"
                         //"sub $0x8, %%rbp  \n" // Do this to align RSP & RBP?

                         // Push output ptr & sizeof (MacroResult), resp
                         "push %5          \n" // output ptr
                         "push %6          \n" // sizeof (MacroResult)

                         // Push arg (array) onto stack
                         "push 0x30(%4)       \n"
                         "push 0x28(%4)       \n"
                         "push 0x20(%4)       \n"
                         "push 0x18(%4)       \n"
                         "push 0x10(%4)       \n"
                         "push 0x8(%4)        \n"
                         "push (%4)         \n"

                         "mov %%rsp, %%rbp \n"

                         // Call function, this should consume 'Array' from the stack and push
                         // 'Raw Syntax' onto the stack
                         "call *%1         \n"

                         // After calling the function, we
                         // expect a RawTree to be atop the stack:
#if ABI == SYSTEM_V_64
                         // memcpy (dest = rdi, src = rsi, size = rdx)
                         // retval = rax 
                         // Note: 0x60 = sizeof(MacroResult)
                         "mov 0x60(%%rsp), %%rdx   \n"
                         "mov 0x68(%%rsp), %%rdi   \n"
                         "mov %%rsp, %%rsi         \n"
                         "call memcpy              \n"

#elif ABI == WIN_64
                         // memcpy (dest = rcx, src = rdx, size = r8)
                         // retval = rax
                         "mov 0x60(%%rsp), %%r8    \n"
                         "mov 0x68(%%rsp), %%rcx   \n"
                         "mov %%rsp, %%rdx         \n"
                         "sub $0x20, %%rsp         \n"
                         "call memcpy              \n"
                         "add $0x20, %%rsp         \n"
#else
#error "Unknown calling convention"
#endif
                         // pop value from stack 
                         // Note: 0x60 = sizeof(MacroResult)
                         "mov 0x60(%%rsp), %%rax   \n"
                         "add %%rax, %%rsp         \n"
                         // pop stashed size & dest from stack
                         "add $0x10, %%rsp          \n"

                         "pop %%r12        \n"
                         "pop %%r13        \n"
                         "pop %%r14        \n"
                         "pop %%r15        \n"
                         "pop %%rsi        \n" 
                         "pop %%rdi        \n" 
                         "pop %%rbx        \n"
                         "pop %%rbp        \n"
                         : "=r" (out)

                         : "r" (head.macro_addr)
                           , "r" (dynamic_memory_ptr)
                           , "r" (dvars)
                           , "r" (&input)
                           , "r" (&output)
                           , "c" (sizeof(MacroResult)) 
                         : "r13", "r14", "r15");

    set_std_temp_allocator(old_temp_alloc);
    set_std_current_allocator(old_current_alloc);
    mem_free(dynamic_memory_space, a);
    return output;
}

Syntax* abstract_expr_i(RawTree raw, ShadowEnv* env, Allocator* a, PiErrorPoint* point) {
    PicoError err;
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
                .range = raw.range,
                .variable = raw.atom.symbol,
            };
            break;
        }
        case AIntegral: {
            *res = (Syntax) {
                .type = SLitUntypedIntegral,
                .ptype = NULL,
                .range = raw.range,
                .integral.value = raw.atom.int_64,
            };
            break;
        }
        case AFloating: {
            *res = (Syntax) {
                .type = SLitUntypedFloating,
                .ptype = NULL,
                .range = raw.range,
                .floating.value = raw.atom.float_64,
            };
            break;
        }
        case ABool: {
            *res = (Syntax) {
                .type = SLitBool,
                .ptype = NULL,
                .range = raw.range,
                .boolean = (bool) raw.atom.int_64,
            };
            break;
        }
        case AString: {
            *res = (Syntax) {
                .type = SLitString,
                .ptype = NULL,
                .range = raw.range,
                .string = raw.atom.string,
            };
            break;
        }
        case ACapture: {
            *res = (Syntax) {
                .type = SAbsVariable,
                .ptype = raw.atom.capture.type,
                .range = raw.range,
                .abvar.value = raw.atom.capture.value,
                .abvar.index = 0,
                // TODO: symbol?
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
        if (raw.branch.hint == HData) {
            return mk_array_literal(raw, env, a, point);
        }

        if (raw.branch.nodes.len < 1) {
            err.range = raw.range;
            err.message = mk_cstr_doc("Raw Syntax must have at least one element!", a);
            throw_pi_error(point, err);
        }

        if (!is_expr(raw.branch.nodes.data[0])) {
            err.range = raw.branch.nodes.data[0].range;
            err.message = mk_cstr_doc("Unprocessed non-expression: syntax hints '[', ']' or implicits '{' '}'\n" 
                                      "should only be used for term specific syntax or implicit arguments, respectively.", a);
            throw_pi_error(point, err);
        }
        
        ComptimeHead head = comptime_head(raw.branch.nodes.data[0], env, a, point);
        switch (head.type) {
        case HeadSyntax:
            return mk_application_body(head.syn, raw, env, a, point);
        case HeadFormer:
            return mk_term(head.former, raw, env, a, point);
        case HeadMacro: {
            // Call the function (uses Pico ABI)
            MacroResult output = eval_macro(head, raw, a);
            if (output.result_type == 1) {
                return abstract_expr_i(output.term, env, a, point);
            } else {
                PicoError err = (PicoError) {
                    .message = mk_str_doc(output.err.message, a),
                    .range = output.err.range,
                };
                throw_pi_error(point, err);
            }
        }
        default:
            panic(mv_string("Bad comptime head given!"));
        }
        break;
    }
    default: {
        panic(mv_string("Internal Error: Name Resolution Received an invalid Raw Syntax Tree"));
    }
    }
    return res;
}


TopLevel mk_toplevel(TermFormer former, RawTree raw, ShadowEnv* env, Allocator* a, PiErrorPoint* point) {
    PicoError err;
    TopLevel res;
    switch (former) {
    case FDefine: {
        if (raw.branch.nodes.len < 3) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Definitions expect at least 2 terms", a);
            throw_pi_error(point, err);
        }

        if (!is_symbol(raw.branch.nodes.data[1])) {
            err.range = raw.branch.nodes.data[1].range;
            err.message = mv_cstr_doc("First argument to definitions should be a symbol", a);
            throw_pi_error(point, err);
        }

        Symbol sym = raw.branch.nodes.data[1].atom.symbol;
        
        RawTree* raw_term = (raw.branch.nodes.len == 3) ? &raw.branch.nodes.data[2] : raw_slice(&raw, 2, a);

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
    case FDeclare: {
        if (raw.branch.nodes.len < 3) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Declarations expect at least 2 terms", a);
            throw_pi_error(point, err);
        }

        if (!is_symbol(raw.branch.nodes.data[1])) {
            err.range = raw.branch.nodes.data[1].range;
            err.message = mv_cstr_doc("First argument to declaration should be a symbol", a);
            throw_pi_error(point, err);
        }

        Symbol sym = raw.branch.nodes.data[1].atom.symbol;
        
        // Declaration bodies are a set of field, value pairs, e.g.
        // [.type <Type>]
        // [.optimise <level>]
        // [.inline-hint <hint>]

        shadow_var(sym, env);
        SymPtrAMap properties = mk_sym_ptr_amap(raw.branch.nodes.len, a);
        for (size_t i = 2; i < raw.branch.nodes.len; i++) {
            RawTree fdesc = raw.branch.nodes.data[i];
            if (fdesc.type != RawBranch) {
                err.range = fdesc.range;
                err.message = mv_cstr_doc("Declaration expects all property descriptors to be compound terms.", a);
                throw_pi_error(point, err);
            }
            
            if (fdesc.branch.nodes.len < 2) {
                err.range = fdesc.range;
                err.message = mv_cstr_doc("Declaration expects all property descriptors to have at least 2 elements.", a);
                throw_pi_error(point, err);
            }

            Symbol field;
            if (!get_fieldname(&fdesc.branch.nodes.data[0], &field)) {
                err.range = fdesc.branch.nodes.data[0].range;
                err.message = mv_cstr_doc("Declaration has malformed property name.", a);
                throw_pi_error(point, err);
            }

            RawTree* val_desc = fdesc.branch.nodes.len == 2 ? &fdesc.branch.nodes.data[1] : raw_slice(&fdesc, 1, a); 
            Syntax* syn = abstract_expr_i(*val_desc, env, a, point);

            sym_ptr_insert(field, syn, &properties);
        }
        shadow_pop(1, env);

        res = (TopLevel) {
            .type = TLDecl,
            .decl.bind = sym,
            .decl.properties = properties,
        };

        break;
    }
    case FImport: {
        if (raw.branch.nodes.len < 2) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Import expect at least 2 terms", a);
            throw_pi_error(point, err);
        }

        ImportClauseArray clauses = mk_import_clause_array(raw.branch.nodes.len - 1, a);
        for (size_t i = 1; i < raw.branch.nodes.len; i++) {
            ImportClause clause = abstract_import_clause(&raw.branch.nodes.data[i], a, point);
            push_import_clause(clause, &clauses);
        }

        res = (TopLevel) {
            .type = TLImport,
            .import.range = raw.range,
            .import.clauses = clauses,
        };
        break;
    }
    default: {
        res = (TopLevel) {
            .type = TLExpr,
            .expr = mk_term(former, raw, env, a, point),
        };
        break;
    }
    }
    return res;
}

TopLevel abstract_i(RawTree raw, ShadowEnv* env, Allocator* a, PiErrorPoint* point) {
    // first: try a unique toplevel-form
    bool unique_toplevel = false;

    if (raw.type == RawBranch && raw.branch.nodes.len > 1) {
        if (raw.branch.hint == HData) {
            return (TopLevel) {
                .type = TLExpr,
                .expr = mk_array_literal(raw, env, a, point),
            };
        }

        ComptimeHead head = comptime_head(raw.branch.nodes.data[0], env, a, point);
        switch (head.type) {
        case HeadSyntax:
            return (TopLevel) {
                .type = TLExpr,
                .expr = mk_application_body(head.syn, raw, env, a, point),
            };
        case HeadFormer:
            return mk_toplevel(head.former, raw, env, a, point);
        case HeadMacro: {
            MacroResult output = eval_macro(head, raw, a);
            if (output.result_type == 1) {
                return abstract_i(output.term, env, a, point);
            } else {
                PicoError err = (PicoError) {
                    .message = mk_str_doc(output.err.message, a),
                    .range = output.err.range,
                };
                throw_pi_error(point, err);
            }
        }
        }
    } 

    if (!unique_toplevel) {
        return (TopLevel) {
            .type = TLExpr,
            .expr = abstract_expr_i(raw, env, a, point),
        };
    }

    panic(mv_string("Logic error in abstract_i: reached unreachable area."));
}

SymbolArray get_module_path(RawTree raw, Allocator* a, PiErrorPoint* point) {
    PicoError err = (PicoError) {.range = raw.range};
    SymbolArray syms = mk_symbol_array(2, a); 
    while (raw.type == RawBranch) {
        if (raw.branch.nodes.len != 3) {
            err.message = mv_cstr_doc("Invalid path component - expected 3 elements", a);
            throw_pi_error(point, err);
        }
        if (!eq_symbol(&raw.branch.nodes.data[0], string_to_symbol(mv_string(".")))) {
            err.message = mv_cstr_doc("Invalid path component - projector not '.'", a);
            throw_pi_error(point, err);
        }

        if(!is_symbol(raw.branch.nodes.data[1])) {
            err.range = raw.branch.nodes.data[1].range;
            err.message = mv_cstr_doc("Invalid path component - field not symbol", a);
            throw_pi_error(point, err);
        }
        push_symbol(raw.branch.nodes.data[1].atom.symbol, &syms);
        raw = raw.branch.nodes.data[2];
    }

    if (!is_symbol(raw)) {
        err.message = mv_cstr_doc("Invalid path component", a);
        throw_pi_error(point, err);
    }
    push_symbol(raw.atom.symbol, &syms);

    // Finally, reverse the path, as
    // foo.bar.baz => (. baz (. bar foo)) => [baz bar foo] => [foo bar baz]
    for (size_t i = 0; i < syms.len / 2; i++) {
        Symbol tmp = syms.data[i];
        syms.data[i] = syms.data[syms.len - (i + 1)];
        syms.data[syms.len - (i + 1)] = tmp;
    }
    return syms;
}

ImportClause abstract_import_clause(RawTree* raw, Allocator* a, PiErrorPoint* point) {
    PicoError err;
    if (is_symbol(*raw)) {
        SymbolArray path = mk_symbol_array(1,a);
        push_symbol(raw->atom.symbol, &path);
        return (ImportClause) {
            .type = Import,
            .path = path,
        };
    } else if (raw->type == RawBranch) {
        // Possibilities:
        // (name1 )
        // (name1 :all)
        // (name1 :as name2)
        // (. name2 name1)
        // (. (list-of-names) name1)
        if (raw->branch.nodes.len == 1) {
            if (!is_symbol(raw->branch.nodes.data[0])) {
                err.range = raw->range;
                err.message = mv_cstr_doc("Invalid import clause - expected symbol", a);
                throw_pi_error(point, err);
            }

            SymbolArray path = mk_symbol_array(1, a);
            push_symbol(raw->atom.symbol, &path);
            return (ImportClause) {
                .type = Import,
                .path = path,
            };
        } else if (raw->branch.nodes.len == 2) {
            SymbolArray path = get_module_path(raw->branch.nodes.data[0], a, point);
            
            Symbol middle;
            if(!get_fieldname(&raw->branch.nodes.data[1], &middle)) {
                err.range = raw->branch.nodes.data[1].range;
                err.message = mv_cstr_doc("Invalid import clause - expected :all", a);
                throw_pi_error(point, err);
            }
            if (!symbol_eq(middle, string_to_symbol(mv_string("all")))) {
                err.range = raw->branch.nodes.data[1].range;
                err.message = mv_cstr_doc("Invalid import clause - expected :all", a);
                throw_pi_error(point, err);
            }

            return (ImportClause) {
                .type = ImportAll,
                .path = path,
            };
        } else if (raw->branch.nodes.len == 3) {
            if (!is_symbol(raw->branch.nodes.data[0]) || !is_symbol(raw->branch.nodes.data[2])) {
                err.range = raw->range;
                err.message = mv_cstr_doc("Invalid import clause", a);
                throw_pi_error(point, err);
            }

            // Check for '.'
            if (eq_symbol(&raw->branch.nodes.data[0], string_to_symbol(mv_string(".")))) {
                Symbol src;
                if(!get_fieldname(&raw->branch.nodes.data[2], &src)) {
                    err.range = raw->branch.nodes.data[2].range;
                    err.message = mv_cstr_doc("Invalid import-. source", a);
                    throw_pi_error(point, err);
                }

                RawTree raw_members = raw->branch.nodes.data[1];
                if (is_symbol(raw_members)) {
                    SymbolArray path = mk_symbol_array(1,a);
                    push_symbol(src, &path);
                    return (ImportClause) {
                        .type = Import,
                        .path = path,
                        .member = raw_members.atom.symbol,
                    };
                } else if (raw_members.type == RawBranch) {
                    SymbolArray members = mk_symbol_array(raw_members.branch.nodes.len, a);
                    if (!get_symbol_list(&members, raw_members)) {
                        err.range = raw->branch.nodes.data[2].range;
                        err.message = mv_cstr_doc("Invalid import-. members", a);
                        throw_pi_error(point, err);
                    }
                    SymbolArray path = mk_symbol_array(1,a);
                    push_symbol(src, &path);
                    return (ImportClause) {
                        .type = ImportMany,
                        .path = path,
                        .members = members,
                    };
                } else {
                    err.range = raw->branch.nodes.data[1].range;
                    err.message = mv_cstr_doc("Invalid import-. member(s)", a);
                    throw_pi_error(point, err);
                }

            } else {
                Symbol middle;
                if(!get_fieldname(&raw->branch.nodes.data[1], &middle)) {
                    err.range = raw->branch.nodes.data[1].range;
                    err.message = mv_cstr_doc("Invalid import clause", a);
                    throw_pi_error(point, err);
                }
                if (!symbol_eq(middle , string_to_symbol(mv_string("as")))) {
                    err.range = raw->branch.nodes.data[1].range;
                    err.message = mv_cstr_doc("Invalid import clause", a);
                    throw_pi_error(point, err);
                }

                Symbol name;
                Symbol rename;
                if(!get_fieldname(&raw->branch.nodes.data[0], &name)) {
                    err.range = raw->branch.nodes.data[0].range;
                    err.message = mv_cstr_doc("Invalid import-as name", a);
                    throw_pi_error(point, err);
                }
                if(!get_fieldname(&raw->branch.nodes.data[0], &rename)) {
                    err.range = raw->branch.nodes.data[0].range;
                    err.message = mv_cstr_doc("Invalid import-as new name", a);
                    throw_pi_error(point, err);
                }
                SymbolArray path = mk_symbol_array(1,a);
                push_symbol(name, &path);
                return (ImportClause) {
                    .type = ImportAs,
                    .path = path,
                    .rename = rename,
                };
            }
        } else {
            err.range = raw->range;
            err.message = mv_cstr_doc("Invalid import clause - incorrect number of items", a);
            throw_pi_error(point, err);
        }
    } else {
        err.range = raw->range;
        err.message = mv_cstr_doc("Invalid import clause - is atom!", a);
        throw_pi_error(point, err);
    }
}

ExportClause abstract_export_clause(RawTree* raw, PiErrorPoint* point, Allocator* a) {
    PicoError err;
    if (is_symbol(*raw)) {
        return (ExportClause) {
            .type = ExportName,
            .name = raw->atom.symbol,
        };
    } else if (raw->type == RawBranch) {
        // If branch, there are two options
        // (var1 :as var2)
        // (:all)
        // looks like (<symbol/name> (<symbol/:> <symbol/as>) <symbol/name>)

        if (raw->branch.nodes.len == 2) {
            Symbol all_sym;
            if(!get_fieldname(raw, &all_sym)) {
                err.range = raw->range;
                err.message = mv_cstr_doc("Invalid export clause - couldn't get fieldname from expected 'all' ", a);
                throw_pi_error(point, err);
            }
            if (!symbol_eq(all_sym, string_to_symbol(mv_string("all")))) {
                err.range = raw->range;
                err.message = mv_cstr_doc("Invalid export - expected 'all', but got something else", a);
                throw_pi_error(point, err);
            }

            return (ExportClause) {
                .type = ExportAll,
            };
        } else if (raw->branch.nodes.len == 3) {
            Symbol middle;
            if(!get_fieldname(&raw->branch.nodes.data[1], &middle)) {
                err.range = raw->branch.nodes.data[1].range;
                err.message = mv_cstr_doc("Invalid export clause", a);
                throw_pi_error(point, err);
            }
            if (!symbol_eq(middle, string_to_symbol(mv_string("as")))) {
                err.range = raw->branch.nodes.data[1].range;
                err.message = mv_cstr_doc("Invalid export clause", a);
                throw_pi_error(point, err);
            }

            Symbol name;
            Symbol rename;
            if(!get_fieldname(&raw->branch.nodes.data[0], &name)) {
                err.range = raw->branch.nodes.data[0].range;
                err.message = mv_cstr_doc("Invalid export-as name", a);
                throw_pi_error(point, err);
            }
            if(!get_fieldname(&raw->branch.nodes.data[0], &rename)) {
                err.range = raw->branch.nodes.data[0].range;
                err.message = mv_cstr_doc("Invalid export-as new name", a);
                throw_pi_error(point, err);
            }

            return (ExportClause) {
                .type = ExportNameAs,
                .name = name,
                .rename = rename,
            };
        } else {
            err.range = raw->range;
            err.message = mv_cstr_doc("Invalid export clause - for branches, expect either 2 argument (: all) or 3 (sym1 :as sym2)", a);
            throw_pi_error(point, err);
        }

    } else {
        err.range = raw->range;
        err.message = mv_cstr_doc("Invalid export clause", a);
        throw_pi_error(point, err);
    }
}

// Consider the expression (data.list.list 1 2 3). In this case 'data.list.list'
// is a macro, and so we need to be able to resolve the module lookup at abstraction
// time. Comptime head gets the 'head' of an application like this, so we know
// if it is a macro/term former & can abstract accordingly
ComptimeHead comptime_head(RawTree raw, ShadowEnv* env, Allocator* a, PiErrorPoint* point) {
    if (is_symbol(raw)) {
        Symbol sym = raw.atom.symbol;
        ShadowEntry entry = shadow_env_lookup(sym, env);
        switch (entry.type) {
        case SGlobal:
            if (!entry.is_module && entry.vtype->sort == TPrim && entry.vtype->prim == TFormer) {
                return (ComptimeHead){.type = HeadFormer, .former = *((TermFormer*)entry.value)};
            } else if (!entry.is_module && entry.vtype->sort == TPrim && entry.vtype->prim == TMacro) {
                return (ComptimeHead){.type = HeadMacro, .macro_addr = *((uint64_t*)entry.value)};
            }
            break;
        default:
            break;
        }
    }
    Syntax* syn = abstract_expr_i(raw, env, a, point);
    if (syn->type == SAbsVariable) {
        if (syn->ptype->sort == TPrim && syn->ptype->prim == TFormer) {
            return (ComptimeHead){.type = HeadFormer, .former = *((TermFormer*)syn->abvar.value)};
        } else if (syn->ptype->sort == TPrim && syn->ptype->prim == TMacro) {
            return (ComptimeHead){.type = HeadMacro, .macro_addr = *((uint64_t*)syn->abvar.value)};
        }
    }
    return (ComptimeHead){.type = HeadSyntax, .syn= syn};
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
        Module *module = try_get_module(syn->projector.val, env);
        if (module) {
            ModuleEntry* entry = get_def(syn->projector.field, module);
            if (entry->is_module) { return entry->value; }
            else { return NULL; }
        } else {
            return NULL;
        }
    }
    default:
        return NULL;
    }
    return NULL;
}

SymbolArray* try_get_path(Syntax* syn, Allocator* a) {
    SymbolArray arr = mk_symbol_array(8, a);
    bool running = true;
    while (running) {
        switch (syn->type) {
        case SVariable: {
            push_symbol(syn->variable, &arr);
            running = false;
            break;
        }
        case SProjector: {
            push_symbol(syn->projector.field, &arr);
            syn = syn->projector.val;
            break;
        }
        default:
            return NULL;
        }
    }
    reverse_symbol_array(arr);
    SymbolArray* out = mem_alloc(sizeof(SymbolArray), a);
    *out = arr;
    return out;
}

Syntax* resolve_module_projector(Range range, Syntax* source, RawTree* msym, ShadowEnv* env, Allocator* a, PiErrorPoint* point) {
    Syntax* res = mem_alloc(sizeof(Syntax), a);
    Module* m = try_get_module(source, env);
    if (m) {
        // TODO (INVESTIGATION): do we have a better way to manage lookup
        // Note: seems like having to check for the special case of
        // Kind/Constraint is causing issues. 
        ModuleEntry* e = get_def(msym->atom.symbol, m);
        if (e) {
            if (e->is_module) {
                *res = (Syntax) {
                    .type = SProjector,
                    .ptype = NULL,
                    .range = range,
                    .projector.field = msym->atom.symbol,
                    .projector.val = source,
                };
            } else {
                *res = (Syntax) {
                    .type = SAbsVariable,
                    .ptype = &e->type,
                    .range = msym->range,
                    .abvar.index = 0,
                    .abvar.value = (e->type.sort == TKind || e->type.sort == TConstraint) ? &e->value : e->value,
                    .abvar.symbol = msym->atom.symbol,
                };
            }
            return res;
        } else {
            PicoError err = (PicoError) {
                .range = msym->range,
                .message = mv_str_doc(string_cat(mv_string("Field not found in module: "),
                                                 symbol_to_string(msym->atom.symbol, a), a), a)
            };
            throw_pi_error(point, err);
        }
    } else {
        *res = (Syntax) {
            .type = SProjector,
            .ptype = NULL,
            .range = range,
            .projector.field = msym->atom.symbol,
            .projector.val = source,
        };
        return res;
    }
}
