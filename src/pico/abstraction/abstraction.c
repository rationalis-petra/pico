#include <string.h>
#include "platform/machine_info.h"

#include "platform/signals.h"
#include "data/result.h"
#include "data/string.h"

#include "pico/data/error.h"
#include "pico/values/values.h"
#include "pico/stdlib/platform/submodules.h"
#include "pico/syntax/concrete.h"
#include "pico/syntax/syntax.h"
#include "pico/binding/shadow_env.h"
#include "pico/abstraction/abstraction.h"
#include "pico/abstraction/abstraction_errors.h"

// Internal types
typedef enum {
    HeadSyntax, HeadFormer, HeadMacro
} HeadType;

typedef struct {
    HeadType type;
    union {
        SynRef syn;
        TermFormer former;
        uint64_t macro_addr;
    };
} ComptimeHead;


// Internal functions declarations needed forthe  interface implementation 
TopLevel abstract_i(RawTree raw, AbstractionICtx ctx);
SynRef abstract_expr_i(RawTree raw, AbstractionICtx ctx);

SynRef mk_application_body(SynRef fn_syn, RawTree raw, AbstractionICtx ctx);
SynRef mk_term(TermFormer former, RawTree raw, AbstractionICtx ctx);
ComptimeHead comptime_head(RawTree raw, AbstractionICtx ctx);
Module* try_get_module(SynRef syn, AbstractionICtx ctx);
SynRef resolve_module_projector(Range range, SynRef source, RawTree* msym, AbstractionICtx ctx);
SymbolArray* try_get_path(SynRef syn, AbstractionICtx ctx);
DevFlag check_dev_flags(RawTree curr, AbstractionICtx ctx);
bool is_special(RawTree curr);

// Array-specific helpers
void deduce_dimension(U64Array* dims, RawTree nodes, AbstractionICtx ctx);
RawTreePiList next_node_arr(U64Array* index, U64Array dims, RawTree nodes, AbstractionICtx ctx);

// Module header helpers
Imports abstract_imports(RawTree* raw, Allocator* a, PiErrorPoint* point);
Exports abstract_exports(RawTree* raw, Allocator* a, PiErrorPoint* point);
ImportClause abstract_import_clause(RawTree* raw, Allocator* a, PiErrorPoint* point);
ExportClause abstract_export_clause(RawTree* raw, PiErrorPoint* point, Allocator* a);

//------------------------------------------------------------------------------
// Interface Implementation
//------------------------------------------------------------------------------

SynRef abstract_expr(RawTree raw, AbstractionCtx ctx) {
    ShadowEnv* s_env = mk_shadow_env(ctx.a, ctx.env);
    PiAllocator pi_alloc = convert_to_pallocator(ctx.a);

    PiAllocator tmp_alloc = convert_to_pallocator(ctx.a);
    PiAllocator old_temp_alloc = set_std_temp_allocator(tmp_alloc);
    PiAllocator old_current_alloc = set_std_current_allocator(tmp_alloc);

    void* vstack_memory_space = mem_alloc(4096, ctx.a);
    void* dynamic_memory_space = mem_alloc(4096, ctx.a);

    AbstractionICtx ictx = {
        .gpa = ctx.a,
        .tape = ctx.tape,
        .pia = &pi_alloc,
        .env = s_env,
        .point = ctx.point,
        .vstack_memory_ptr = vstack_memory_space + 4095,
        .dynamic_memory_ptr = dynamic_memory_space,
    };
    SynRef out = abstract_expr_i(raw, ictx);

    mem_free(dynamic_memory_space, ctx.a);
    mem_free(vstack_memory_space, ctx.a);

    set_std_temp_allocator(old_temp_alloc);
    set_std_current_allocator(old_current_alloc);
    return out;
}

TopLevel abstract(RawTree raw, AbstractionCtx ctx) {
    ShadowEnv* s_env = mk_shadow_env(ctx.a, ctx.env);
    void* vstack_memory_space = mem_alloc(4096, ctx.a);
    void* dynamic_memory_space = mem_alloc(4096, ctx.a);
    PiAllocator pi_alloc = convert_to_pallocator(ctx.a);
    PiAllocator tmp_alloc = convert_to_pallocator(ctx.a);
    PiAllocator old_temp_alloc = set_std_temp_allocator(tmp_alloc);
    PiAllocator old_current_alloc = set_std_current_allocator(tmp_alloc);

    AbstractionICtx ictx = {
        .gpa = ctx.a,
        .tape = ctx.tape,
        .pia = &pi_alloc,
        .env = s_env,
        .point = ctx.point,
        .vstack_memory_ptr = vstack_memory_space + 4095,
        .dynamic_memory_ptr = dynamic_memory_space,
    };

    TopLevel out = abstract_i(raw, ictx);

    mem_free(dynamic_memory_space, ctx.a);
    mem_free(vstack_memory_space, ctx.a);
    set_std_temp_allocator(old_temp_alloc);
    set_std_current_allocator(old_current_alloc);
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
        .range = raw.range,
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

RawTree* raw_slice(RawTree* raw, size_t drop, PiAllocator* pia) {
#ifdef DEBUG
  if (drop > raw->branch.nodes.len) {
      panic(mv_string("Dropping more nodes than there are!"));
  }
#endif
    RawTree* out = call_alloc(sizeof(RawTree), pia);
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

Result get_annotated_symbol_list(SymPtrAMap *args, RawTree list, AbstractionICtx ctx) {
    Result error_result = {.type = Err, .error_message = mv_string("Malformed proc argument list.")};
    if (list.type != RawBranch) { return error_result; }

    for (size_t i = 0; i < list.branch.nodes.len; i++) {
        RawTree annotation = list.branch.nodes.data[i];
        if (annotation.type == RawAtom) {
            // TODO (bug): check for duplicaes
            sym_ptr_insert(annotation.atom.symbol, NULL, args);
        } else if (annotation.type == RawBranch || annotation.branch.nodes.len > 1) {
            // TODO (bug): check for duplicaes
            RawTree arg = annotation.branch.nodes.data[0];
            if (arg.type != RawAtom || arg.atom.type != ASymbol) { return error_result; }
            RawTree* raw_type;
            if (annotation.branch.nodes.len == 2) {
              raw_type = &annotation.branch.nodes.data[1];
            } else {
              raw_type = raw_slice(&annotation, 1, ctx.pia);
            }
            SynRef* ref = mem_alloc(sizeof(SynRef), ctx.gpa);
            *ref = abstract_expr_i(*raw_type, ctx); 

            sym_ptr_insert(arg.atom.symbol, ref, args);
        } else {
          // TODO (Improvement): produce index, range?
          return error_result;
        }
    }
    return (Result) {.type = Ok};
}

SynRef mk_application_body(SynRef fn_syn, RawTree raw, AbstractionICtx ctx) {
    Allocator* a = ctx.gpa;
    SynRef res = new_syntax(ctx.tape);
    set_range(res, (SynRange){.term = raw.range}, ctx.tape);

    //     .range = raw.range,
    Syntax fn = get_syntax(fn_syn, ctx.tape);
    if (fn.type == SConstructor) {
        Syntax syn = {
            .type = SVariant,
            .variant.has_enum_type = fn.constructor.has_enum_type,
            .variant.enum_type = fn.constructor.enum_type,
            .variant.tagname = fn.constructor.tagname,
            .variant.args = mk_syn_array(raw.branch.nodes.len - 1, a),
        };

        for (size_t i = 1; i < raw.branch.nodes.len; i++) {
            SynRef arg = abstract_expr_i(raw.branch.nodes.data[i], ctx);
            push_syn(arg, &syn.variant.args);
        }
        set_syntax(res, syn, ctx.tape);
    } else if (raw.branch.nodes.len > 1
               && raw.branch.nodes.data[1].type == RawBranch
               && raw.branch.nodes.data[1].branch.hint == HImplicit) {
        RawTree typelist = raw.branch.nodes.data[1];

        Syntax syn = {
            .type = SAllApplication,
            .all_application.function = fn_syn,
            .all_application.types = mk_syn_array(typelist.branch.nodes.len, a),
            .all_application.implicits = mk_syn_array(0, a),
            .all_application.args = mk_syn_array(raw.branch.nodes.len - 2, a),
        };

        for (size_t i = 0; i < typelist.branch.nodes.len; i++) {
            SynRef type = abstract_expr_i(typelist.branch.nodes.data[i], ctx);
            push_syn(type, &syn.all_application.types);
        }

        for (size_t i = 2; i < raw.branch.nodes.len; i++) {
            SynRef arg = abstract_expr_i(raw.branch.nodes.data[i], ctx);
            push_syn(arg, &syn.all_application.args);
        }
        set_syntax(res, syn, ctx.tape);
    } else {
        Syntax syn = (Syntax) {
            .type = SApplication,
            .application.function = fn_syn,
            .application.implicits = mk_syn_array(0, a),
            .application.args = mk_syn_array(raw.branch.nodes.len - 1, a),
        };

        for (size_t i = 1; i < raw.branch.nodes.len; i++) {
            SynRef arg = abstract_expr_i(raw.branch.nodes.data[i], ctx);
            push_syn(arg, &syn.application.args);
        }
        set_syntax(res, syn, ctx.tape);
    }

    return res;
}

SynRef mk_application(RawTree raw, AbstractionICtx ctx) {
    SynRef fn_syn = abstract_expr_i((raw.branch.nodes.data[0]), ctx);
    return mk_application_body(fn_syn, raw, ctx);
}


SynRef mk_term(TermFormer former, RawTree raw, AbstractionICtx ctx) {
    Allocator* a = ctx.gpa;
    PicoError err;
    switch (former) {
    case FDefine:
        err.range = raw.branch.nodes.data[0].range;
        err.message = mv_cstr_doc("'Define' (def) not supported as inner-expression term former.", a);
        throw_pi_error(ctx.point, err);
    case FDeclare:
        err.range = raw.branch.nodes.data[0].range;
        err.message = mv_cstr_doc("'Declare' not supported as inner-expression term former.", a);
        throw_pi_error(ctx.point, err);
    case FImport:
        err.range = raw.branch.nodes.data[0].range;
        err.message = mv_cstr_doc("'Import' (open) not supported as inner-expression term former.", a);
        throw_pi_error(ctx.point, err);
    case FProcedure: {
        if (raw.branch.nodes.len < 3) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Procedure term former requires at least 2 arguments!", a);
            throw_pi_error(ctx.point, err);
        }

        SymPtrAMap implicits = mk_sym_ptr_amap(0, a);
        SymPtrAMap arguments = mk_sym_ptr_amap(8, a);

        size_t args_index = 1;
        if (raw.branch.nodes.data[args_index].type == RawBranch
            && raw.branch.nodes.data[args_index].branch.hint == HImplicit) {
            Result args_out = get_annotated_symbol_list(&implicits, raw.branch.nodes.data[args_index], ctx);
            err.range = raw.branch.nodes.data[args_index].range;
            err.message = mv_str_doc(args_out.error_message, a);
            if (args_out.type == Err) throw_pi_error(ctx.point, err);
            args_index++;
        }
             
        if (is_special(raw.branch.nodes.data[args_index])) {
            Result args_out = get_annotated_symbol_list(&arguments, raw.branch.nodes.data[args_index], ctx);
            err.range = raw.branch.nodes.data[args_index].range;
            err.message = mv_str_doc(args_out.error_message, a);
            if (args_out.type == Err) throw_pi_error(ctx.point, err);
            args_index++;
        }

        bool preserve_dyn_memory = false;
        if (is_special(raw.branch.nodes.data[args_index])) {
            RawTree props = raw.branch.nodes.data[args_index];
            for (size_t i = 0; i < props.branch.nodes.len; i++) {
                RawTree prop = props.branch.nodes.data[i];
                if (eq_symbol(&prop, string_to_symbol(mv_string("preserve-dyn-memory")))) {
                    preserve_dyn_memory = true;
                } else {
                    err.range = raw.branch.nodes.data[args_index].range;
                    err.message = mv_cstr_doc("Unrecognized proc property: must be one of [preserve-dyn-memory]", a);
                    throw_pi_error(ctx.point, err);
                }
            }

            args_index++;
        }

        SymbolArray to_shadow = mk_symbol_array(arguments.len, a);
        for (size_t i = 0; i < arguments.len; i++) {
            push_symbol(arguments.data[i].key, &to_shadow);
        }
        shadow_vars(to_shadow, ctx.env);

        RawTree* raw_term;
        if (raw.branch.nodes.len == args_index + 1) {
            raw_term = &raw.branch.nodes.data[args_index];
        } else {
            raw_term = raw_slice(&raw, args_index, ctx.pia);
        }
        SynRef body = abstract_expr_i(*raw_term, ctx);
        shadow_pop(arguments.len, ctx.env);

        Syntax syn = {
            .type = SProcedure,
            .procedure.args = arguments,
            .procedure.implicits = implicits, 
            .procedure.body = body,
            .procedure.preserve_dyn_memory = preserve_dyn_memory,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn, ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FAll: {
        if (raw.branch.nodes.len < 3) {
            err.range = raw.range;
            err.message = mv_cstr_doc("all term former requires at least 2 arguments!", a);
            throw_pi_error(ctx.point, err);
        }

        SymbolArray arguments = mk_symbol_array(2, a);
        if (!get_symbol_list(&arguments, raw.branch.nodes.data[1])) {
            err.range = raw.branch.nodes.data[1].range;
            err.message = mv_cstr_doc("all term former requires first arguments to be a symbol-list!", a);
            throw_pi_error(ctx.point, err);
        }

        shadow_vars(arguments, ctx.env);

        RawTree* raw_term;
        if (raw.branch.nodes.len == 3) {
            raw_term = &raw.branch.nodes.data[2];
        } else {
            raw_term = raw_slice(&raw, 2, ctx.pia);
        }
        SynRef body = abstract_expr_i(*raw_term, ctx);

        Syntax syn = {
            .type = SAll,
            .all.args = arguments,
            .all.body = body,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FMacro: {
        if (raw.branch.nodes.len < 2) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Malformed macro expression: expects at least 1 arg.", a);
            throw_pi_error(ctx.point, err);
        }
        RawTree* body = raw.branch.nodes.len == 2 ? &raw.branch.nodes.data[1] : raw_slice(&raw, 1, ctx.pia);
        SynRef transformer = abstract_expr_i(*body, ctx);

        Syntax syn = {
            .type = SMacro,
            .transformer = transformer,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FApplication: {
        return mk_application(raw, ctx);
    }
    case FSeal: {
        if (raw.branch.nodes.len < 3) {
            err.message = mv_cstr_doc("Not enough terms provided to seal", a);
            throw_pi_error(ctx.point, err);
        }
        SynRef type = abstract_expr_i(raw.branch.nodes.data[1], ctx);

        SynArray types = mk_syn_array(8, a);
        {
            RawTree raw_types = raw.branch.nodes.data[2];
            if (!is_special(raw_types)) {
                err.range = raw_types.range;
                err.message = mv_cstr_doc("Seal expects second argument to be a set of types", a);
                throw_pi_error(ctx.point, err);
            }

            for (size_t i = 0; i < raw_types.branch.nodes.len; i++) {
                SynRef syn = abstract_expr_i(raw_types.branch.nodes.data[i], ctx);
                push_syn(syn, &types);
            }
        }

        size_t body_idx = 3;
        SynArray implicits = mk_syn_array(8, a);
        {
            RawTree raw_implicits = raw.branch.nodes.data[3];
            if (raw_implicits.type == RawBranch && raw_implicits.branch.hint == HImplicit) {
                body_idx++;
                for (size_t i = 0; i < raw_implicits.branch.nodes.len; i++) {
                    SynRef syn = abstract_expr_i(raw_implicits.branch.nodes.data[i], ctx);
                    push_syn(syn, &implicits);
                }
            }
        }

        if (body_idx == raw.branch.nodes.len) {
            err.message = mv_cstr_doc("Seal lacking a body", a);
            throw_pi_error(ctx.point, err);
        }

        RawTree *raw_term = (raw.branch.nodes.len == body_idx+1)
            ? &raw.branch.nodes.data[body_idx]
            : raw_slice(&raw, body_idx, ctx.pia);
        SynRef body = abstract_expr_i(*raw_term, ctx);

        Syntax syn = {
            .type = SSeal,
            .seal.type = type,
            .seal.types = types,
            .seal.implicits = implicits,
            .seal.body = body,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FUnseal: {
        if (raw.branch.nodes.len < 3) {
            err.message = mv_cstr_doc("Not enough terms provided to unseal", a);
            throw_pi_error(ctx.point, err);
        }
        SynRef sealed; 
        Symbol binder;
        {

            RawTree raw_binder = raw.branch.nodes.data[1];
            if (!is_special(raw_binder)
                || raw_binder.branch.nodes.len != 2
                || !is_symbol(raw_binder.branch.nodes.data[0])) {

                err.range = raw_binder.range;
                err.message = mv_cstr_doc("Unseal binding form expected here, i.e. (unseal [var sealed-val] ...)", a);
                throw_pi_error(ctx.point, err);
            }
            
            binder = raw_binder.branch.nodes.data[0].atom.symbol;
            sealed = abstract_expr_i(raw_binder.branch.nodes.data[1], ctx);
        }

        SymbolArray types = mk_symbol_array(8, a);
        {
            RawTree raw_types = raw.branch.nodes.data[2];
            if (!is_special(raw_types) || !get_symbol_list(&types, raw_types)) {
                err.range = raw_types.range;
                err.message = mv_cstr_doc("Invalid type binding provided to unseal", a);
                throw_pi_error(ctx.point, err);
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
                    err.message = mv_cstr_doc("Malformed implicit list in unseal", a);
                    throw_pi_error(ctx.point, err);
                }
            }
        }

        if (body_idx == raw.branch.nodes.len) {
            err.message = mv_cstr_doc("Unseal lacking a body", a);
            throw_pi_error(ctx.point, err);
        }

        RawTree *raw_term = (raw.branch.nodes.len == body_idx+1)
            ? &raw.branch.nodes.data[body_idx]
            : raw_slice(&raw, body_idx, ctx.pia);
        SynRef body = abstract_expr_i(*raw_term, ctx);

        Syntax syn = {
            .type = SUnseal,
            .unseal.sealed = sealed,
            .unseal.binder = binder,
            .unseal.types = types,
            .unseal.implicits = implicits,
            .unseal.body = body,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FArray: {
        U64Array dims = mk_u64_array(2, a);
        SynArray elements = mk_syn_array(16, a);
        // Special Case: Empty Array
        if (raw.branch.nodes.len == 1) {
            Syntax syn = {
                .type = SArray,
                .array.dimensions = dims,
                .array.elements = elements,
            };
            SynRef res = new_syntax(ctx.tape);
            set_syntax(res, syn , ctx.tape);
            set_range(res, (SynRange){.term = raw.range}, ctx.tape);
            return res;
        }

        // Are array dimensions provided?  
        size_t idx = 1;
        if (raw.branch.nodes.data[idx].type == RawBranch &&
            raw.branch.nodes.data[idx].branch.hint == HImplicit) {
            RawTreePiList nodes = raw.branch.nodes.data[idx].branch.nodes;
            for (size_t i = 0; i < nodes.len; i++) {
                RawTree dim = nodes.data[i];
                if (dim.type != RawAtom || dim.atom.type != AIntegral)
                    array_incorrect_dimtype(dim, ctx);
                push_u64((uint64_t)dim.atom.int_64, &dims);
            }
            idx++;
        }

        if (idx + 1 != raw.branch.nodes.len) {
            array_incorrect_numterms(raw, idx + 1, ctx);
        }
        RawTree nodes = raw.branch.nodes.data[idx];
        if (idx == 1) {
            deduce_dimension(&dims, nodes, ctx);
        }
        // Actual Values
        U64Array index = mk_zero_index(dims.len, a);
        while (index_less(index, dims)) {
            RawTreePiList local_nodes = next_node_arr(&index, dims, nodes, ctx);
            size_t last = dims.data[dims.len - 1];
            for (size_t i = 0; i < last; i++) {
                SynRef syn = abstract_expr_i(local_nodes.data[i], ctx);
                push_syn(syn, &elements);
            }
        }
        
        Syntax syn = {
            .type = SArray,
            .array.dimensions = dims,
            .array.elements = elements,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FArrayElt: {
        if (raw.branch.nodes.len != 3) {
            array_elt_incorrect_numterms(raw, ctx);
        }
        SynArray index;
        RawTree raw_index = raw.branch.nodes.data[1];
        if (raw_index.type == RawBranch && raw_index.branch.hint == HSpecial) {
            index = mk_syn_array(raw_index.branch.nodes.len, a);
            for (size_t i = 0; i < raw_index.branch.nodes.len; i++) {
                push_syn(abstract_expr_i(raw_index.branch.nodes.data[i], ctx), &index);
            }
        } else {
            index = mk_syn_array(1, a);
            push_syn(abstract_expr_i(raw_index, ctx), &index);
        }
        
        Syntax syn = {
            .type = SArrayElt,
            .array_elt.index = index,
            .array_elt.array = abstract_expr_i(raw.branch.nodes.data[2], ctx),
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FStructure: {
        Option_t has_base = None;
        SynRef sbase;
        size_t start_idx = 1;

        // Get the type of the structure
        if (raw.branch.nodes.len > 1) {
            if (!is_special(raw.branch.nodes.data[1])) {
                start_idx = 2;
                has_base = Some;
                sbase = abstract_expr_i(raw.branch.nodes.data[1], ctx);
            } else {
                has_base = None;
                sbase = (SynRef){};
            }
        }

        // Construct a structure
        SymSynAMap fields = mk_sym_syn_amap(raw.branch.nodes.len, a);
        for (size_t i = start_idx; i < raw.branch.nodes.len; i++) {
            RawTree fdesc = raw.branch.nodes.data[i];
            if (fdesc.type != RawBranch) {
                struct_bad_fdesc_type(fdesc, ctx);
            }
            
            if (fdesc.branch.nodes.len < 2) {
                struct_bad_fdesc_len(fdesc, ctx);
            }

            Symbol field;
            if (!get_fieldname(&fdesc.branch.nodes.data[0], &field)) {
              struct_bad_fdesc_fieldname(fdesc, ctx);
            }

            RawTree* val_desc = fdesc.branch.nodes.len == 2 ? &fdesc.branch.nodes.data[1] : raw_slice(&fdesc, 1, ctx.pia); 
            SynRef syn = abstract_expr_i(*val_desc, ctx);

            // Check that there are no duplicates, then insert
            size_t found_idx;
            if (sym_syn_find(&found_idx, field, fields)) {
                struct_duplicate_fieldname(fdesc, field, ctx);
            }
            
            sym_syn_insert(field, syn, &fields);
        }

        Syntax syn = {
            .type = SStructure,
            .structure.has_base = has_base,
            .structure.base = sbase,
            .structure.fields = fields,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FProjector: {
        if (raw.branch.nodes.len != 3) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Projection term former needs two arguments!", a);
            throw_pi_error(ctx.point, err);
        }

        // Get the source portion of the proector 
        SynRef source = abstract_expr_i(raw.branch.nodes.data[2], ctx);

        // Get the symbol portion of the projector
        RawTree msym = raw.branch.nodes.data[1];
        if (msym.type != RawAtom && msym.atom.type != ASymbol) {
            err.range = msym.range;
            err.message = mv_cstr_doc("Second argument to projection term former should be symbol", a);
            throw_pi_error(ctx.point, err);
        }

        return resolve_module_projector(raw.range, source, &msym, ctx);
    }
    case FVariant: {
        if (raw.branch.nodes.len == 2) {
            // Check that we are indeed getting a result
            // Get the tag name of the variant
            RawTree msym = raw.branch.nodes.data[1];
            if (msym.type != RawAtom && msym.atom.type != ASymbol) {
                err.range = msym.range;
                err.message = mv_cstr_doc("Argument to variant term former should be symbol", a);
                throw_pi_error(ctx.point, err);
            }
            Symbol lit = msym.atom.symbol;

            // TODO (FEAT): caese special handling of unit, true, false syntaxes
            //              - make 'normal' types
            if (symbol_eq(lit, string_to_symbol(mv_string("unit")))) {
                Syntax syn = {.type = SLitUnit};
                SynRef res = new_syntax(ctx.tape);
                set_syntax(res, syn , ctx.tape);
                set_range(res, (SynRange){.term = raw.range}, ctx.tape);
                return res;
            }
            if (symbol_eq(lit, string_to_symbol(mv_string("true")))) {
                Syntax syn = {.type = SLitBool, .boolean = true,};
                SynRef res = new_syntax(ctx.tape);
                set_syntax(res, syn , ctx.tape);
                set_range(res, (SynRange){.term = raw.range}, ctx.tape);
                return res;
            }
            else if (symbol_eq(lit, string_to_symbol(mv_string("false")))) {
                Syntax syn = {.type = SLitBool, .boolean = false,};
                SynRef res = new_syntax(ctx.tape);
                set_syntax(res, syn , ctx.tape);
                set_range(res, (SynRange){.term = raw.range}, ctx.tape);
                return res;
            } else {
                // Check that we are indeed getting a result
                // Get the tag gname of the variant
                RawTree msym = raw.branch.nodes.data[1];
                if (msym.type != RawAtom && msym.atom.type != ASymbol) {
                    err.range = msym.range;
                    err.message = mv_cstr_doc("First argument to variant term former should be symbol", a);
                    throw_pi_error(ctx.point, err);
                };

                Syntax syn = {
                    .type = SConstructor,
                    .constructor.has_enum_type = None,
                    .constructor.tagname = msym.atom.symbol,
                };
                SynRef res = new_syntax(ctx.tape);
                set_syntax(res, syn , ctx.tape);
                set_range(res, (SynRange){.term = raw.range}, ctx.tape);
                return res;
            }
        }
        else if (raw.branch.nodes.len != 3) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Variant term former expects at most two arguments!", a);
            throw_pi_error(ctx.point, err);
        } else {
            // Get the Type portion of the projector 
            SynRef var_type = abstract_expr_i(raw.branch.nodes.data[2], ctx);
        
            // Check that we are indeed getting a result
            // Get the tag gname of the variant
            RawTree msym = raw.branch.nodes.data[1];
            if (msym.type != RawAtom && msym.atom.type != ASymbol) {
                err.range = msym.range;
                err.message = mv_cstr_doc("First argument to variant term former should be symbol", a);
                throw_pi_error(ctx.point, err);
            };

            Syntax syn = {
                .type = SConstructor,
                .constructor.has_enum_type = Some,
                .constructor.enum_type = var_type,
                .constructor.tagname = msym.atom.symbol,
            };
            SynRef res = new_syntax(ctx.tape);
            set_syntax(res, syn , ctx.tape);
            set_range(res, (SynRange){.term = raw.range}, ctx.tape);
            return res;
        }
    }
    case FMatch: {
        if (raw.branch.nodes.len < 2) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Match expects at least 1 argument", a);
            throw_pi_error(ctx.point, err);
        }
        if (!is_expr(raw.branch.nodes.data[1])) {
            err.range = raw.branch.nodes.data[1].range;
            err.message = mv_cstr_doc("Match expects first argument to be expression (use parentheses '(' and ')')", a);
            throw_pi_error(ctx.point, err);
        }
        SynRef sval = abstract_expr_i(raw.branch.nodes.data[1], ctx);

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
                throw_pi_error(ctx.point, err);
            }

            // Get the pattern

            Symbol clause_tagname = {}; 
            SymbolArray clause_binds = {};
            bool is_wildcard = false;
            RawTree raw_pattern = raw_clause.branch.nodes.data[0];
            if (raw_pattern.type != RawBranch) {
                if (eq_symbol(&raw_pattern, string_to_symbol(mv_string("_")))) {
                    if (i + 1 != raw.branch.nodes.len) {
                        err.range = raw_clause.range;
                        err.message = mv_cstr_doc("A wildcard pattern must be the last pattern in a match clause.", a);
                        throw_pi_error(ctx.point, err);
                    }

                    is_wildcard = true;
                } else {
                    err.range = raw_clause.range;
                    err.message = mv_cstr_doc("Expecting pattern but got a symbol instead. This is not yet supported.", a);
                    throw_pi_error(ctx.point, err);
                }
            } else {
                // The pattern has two parts: the variables & the tag
                // The tag should be in a constructor (i.e. list)

                RawTree mcol = raw_pattern.branch.nodes.data[0];
                if (raw_pattern.branch.nodes.len == 2 && is_symbol(mcol) && symbol_eq(mcol.atom.symbol, string_to_symbol(mv_string(":")))) {
                    RawTree mname = raw_pattern.branch.nodes.data[1];
                    if (!is_symbol(mname)) {
                        err.range = mname.range;
                        err.message = mv_cstr_doc("Bad pattern in match clause", a);
                        throw_pi_error(ctx.point, err);
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
                        throw_pi_error(ctx.point, err);
                    }

                    clause_binds = mk_symbol_array(raw_pattern.branch.nodes.len - 1, a);
                    for (size_t s = 1; s < raw_pattern.branch.nodes.len; s++) {
                        RawTree raw_name = raw_pattern.branch.nodes.data[s];
                        if (!is_symbol(raw_name)) {
                            err.range = raw_clause.range;
                            err.message = mv_cstr_doc("Pattern binding was not a symbol!", a);
                            throw_pi_error(ctx.point, err);
                        }
                        push_symbol(raw_name.atom.symbol, &clause_binds); 
                    }
                }
            }

            // Get the term
            RawTree *raw_term = (raw_clause.branch.nodes.len == 2)
                ? &raw_clause.branch.nodes.data[1]
                : raw_slice(&raw_clause, 1, ctx.pia);
            SynRef clause_body = abstract_expr_i(*raw_term, ctx);

            SynClause* clause = mem_alloc(sizeof(SynClause), a);
            *clause = (SynClause) {
                .tagname = clause_tagname,
                .vars = clause_binds,
                .body = clause_body,
                .is_wildcard = is_wildcard,
            };
            push_ptr(clause, &clauses);
        }

        Syntax syn = {
            .type = SMatch,
            .match.val = sval,
            .match.clauses = clauses,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FInstance: {
        SymbolArray params = mk_symbol_array(0, a);
        SymPtrAMap implicits = mk_sym_ptr_amap(0, a);
        SynRef constraint;

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
            throw_pi_error(ctx.point, err);
        }

        current = raw.branch.nodes.data[++start_idx];
        switch (current.type == RawBranch ? current.branch.hint : HExpression) {
        case HExpression: goto parse_constraint;
        case HImplicit: goto parse_implicits;
        case HSpecial:
            err.range = current.range;
            err.message = mv_cstr_doc("Invalid instance", a);
            throw_pi_error(ctx.point, err);
        default: panic(mv_string("invalid hint!"));
        }

        parse_implicits:

        get_annotated_symbol_list(&implicits, current, ctx);

        current = raw.branch.nodes.data[++start_idx];

        parse_constraint:

        constraint = abstract_expr_i(current, ctx);
        start_idx++;

        SymSynAMap fields = mk_sym_syn_amap(raw.branch.nodes.len - start_idx, a);
        for (size_t i = start_idx; i < raw.branch.nodes.len; i++) {
            RawTree fdesc = raw.branch.nodes.data[i];
            if (fdesc.type != RawBranch) {
                err.range = fdesc.range;
                err.message = mv_cstr_doc("Instance expects all field descriptors to be lists.", a);
                throw_pi_error(ctx.point, err);
            }
            
            if (fdesc.branch.nodes.len < 2) {
                err.range = fdesc.range;
                err.message = mv_cstr_doc("Instance expects all field descriptors to have at least 2 elements.", a);
                throw_pi_error(ctx.point, err);
            }

            Symbol field;
            if (!get_fieldname(&fdesc.branch.nodes.data[0], &field)) {
                err.range = fdesc.branch.nodes.data[0].range;
                err.message = mv_cstr_doc("Instance has malformed field name.", a);
                throw_pi_error(ctx.point, err);
            }

            RawTree* val_desc = fdesc.branch.nodes.len == 2 ? &fdesc.branch.nodes.data[1] : raw_slice(&fdesc, 1, ctx.pia); 
            SynRef syn = abstract_expr_i(*val_desc, ctx);

            sym_syn_insert(field, syn, &fields);
        }

        Syntax syn = (Syntax) {
            .type = SInstance,
            .instance.params = params,
            .instance.implicits = implicits,
            .instance.constraint = constraint,
            .instance.fields = fields,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FDynamic: {
        if (raw.branch.nodes.len < 2) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Malformed dynamic expression: expects at least 1 arg.", a);
            throw_pi_error(ctx.point, err);
        }
        RawTree* body = raw.branch.nodes.len == 2 ? &raw.branch.nodes.data[1] : raw_slice(&raw, 1, ctx.pia);
        SynRef dynamic = abstract_expr_i(*body, ctx);

        Syntax syn = {
            .type = SDynamic,
            .dynamic = dynamic,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FDynamicUse: {
        if (raw.branch.nodes.len != 2) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Malformed use expression: expects at least 1 arg.", a);
            throw_pi_error(ctx.point, err);
        }
        SynRef use = abstract_expr_i(raw.branch.nodes.data[1], ctx);

        Syntax syn = {
            .type = SDynamicUse,
            .use = use,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FDynamicSet: {
        if (raw.branch.nodes.len < 3) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Malformed set expression: expects 2 args.", a);
            throw_pi_error(ctx.point, err);
        }
        SynRef dynamic = abstract_expr_i(raw.branch.nodes.data[1], ctx);
        SynRef val = abstract_expr_i(*(raw.branch.nodes.len == 3 ? &raw.branch.nodes.data[2] : raw_slice(&raw, 2, ctx.pia)), ctx);

        Syntax syn = {
            .type = SDynamicSet,
            .dynamic_set.dynamic = dynamic,
            .dynamic_set.new_val = val,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FDynamicLet: {
        PtrArray bindings = mk_ptr_array(raw.branch.nodes.len - 1, a);
        size_t index = 1;

        bool special = true;
        while (special && index < raw.branch.nodes.len) {
            RawTree bind = raw.branch.nodes.data[index];
            // bind [x₁ e₁]
            //      [x₂ e₂]
            //  body
            special = is_special(bind);
            if (special) {
                index++;
                DynBinding* dbind = mem_alloc(sizeof(DynBinding), a);
                if (bind.type != RawBranch || bind.branch.nodes.len != 2) {
                    err.range = bind.range;
                    err.message = mv_cstr_doc("Malformed symbol binding in bind-expression", a);
                    throw_pi_error(ctx.point, err);
                }

                dbind->var = abstract_expr_i(bind.branch.nodes.data[0], ctx);
                dbind->expr = abstract_expr_i(bind.branch.nodes.data[1], ctx);
                push_ptr(dbind, &bindings);
            }
        }

        if (index > raw.branch.nodes.len - 1) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Bind expression has no body!", a);
            throw_pi_error(ctx.point, err);
        }

        RawTree* raw_body = (raw.branch.nodes.len == index + 1)
            ? &raw.branch.nodes.data[index]
            : raw_slice(&raw, index, ctx.pia);

        SynRef body = abstract_expr_i(*raw_body, ctx);

        Syntax syn = {
            .type = SDynamicLet,
            .dyn_let_expr.bindings = bindings,
            .dyn_let_expr.body = body,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FLet: {
        SymSynAMap bindings = mk_sym_syn_amap(raw.branch.nodes.len - 1, a);
        size_t index = 1;

        bool special = true;
        while (special) {
            RawTree bind = raw.branch.nodes.data[index];
            // let [x₁ e₁]
            //     [x₂ e₂]
            //  body
            special = is_special(bind);
            if (special) {
                index++;
                Symbol sym;
                if (bind.type != RawBranch || bind.branch.nodes.len < 2) {
                    err.range = bind.range;
                    err.message = mv_cstr_doc("Malformed symbol binding in let-expression, not enough terms!", a);
                    throw_pi_error(ctx.point, err);
                }
                if (!get_label(&bind, &sym)) {
                    err.range = bind.range;
                    err.message = mv_cstr_doc("Expected symbol binding in let-expression", a);
                    throw_pi_error(ctx.point, err);
                }

                shadow_var(sym, ctx.env);
                SynRef bind_body = bind.branch.nodes.len == 2
                    ? abstract_expr_i(bind.branch.nodes.data[1], ctx)
                    : abstract_expr_i(*raw_slice(&bind, 1, ctx.pia), ctx);

                sym_syn_insert(sym, bind_body, &bindings);
            }
        }

        if (index > raw.branch.nodes.len - 1) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Let expression has no body!", a);
            throw_pi_error(ctx.point, err);
        }

        if (index < raw.branch.nodes.len - 1) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Let expression multiple bodies!", a);
            throw_pi_error(ctx.point, err);
        }

        SynRef body = abstract_expr_i(raw.branch.nodes.data[index], ctx);
        shadow_pop(bindings.len, ctx.env);

        Syntax syn = {
            .type = SLet,
            .let_expr.bindings = bindings,
            .let_expr.body = body,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FIf: {
        if (raw.branch.nodes.len != 4) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Term former 'if' expects precisely 3 arguments!", a);
            throw_pi_error(ctx.point, err);
        }
        SynArray terms = mk_syn_array(3, a);
        for (size_t i = 1; i < raw.branch.nodes.len; i++) {
            SynRef term = abstract_expr_i(raw.branch.nodes.data[i], ctx);
            push_syn(term, &terms);
        }

        Syntax syn = {
            .type = SIf,
            .if_expr.condition = terms.data[0],
            .if_expr.true_branch = terms.data[1],
            .if_expr.false_branch = terms.data[2],
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FCond: {
        if (raw.branch.nodes.len < 2) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Term former 'cond' expects at least one term!", a);
            throw_pi_error(ctx.point, err);
        }
        PtrArray clauses = mk_ptr_array(raw.branch.nodes.len - 2, a);
        for (size_t i = 1; i < raw.branch.nodes.len - 1; i++) {
            RawTree raw_clause = raw.branch.nodes.data[i];
            if (raw_clause.type != RawBranch) {
                err.range = raw_clause.range;
                err.message = mv_cstr_doc("Expected composite special term here. Got an atom instead.", a);
                throw_pi_error(ctx.point, err);
            }
            if (raw_clause.branch.hint != HSpecial) {
                err.range = raw_clause.range;
                err.message = mv_cstr_doc("Expected a composite special term here. Hint: use square brackets '[' and ']'\n"
                                          " to produce a special term.", a);
                throw_pi_error(ctx.point, err);
            }
            if (raw_clause.branch.nodes.len < 2) {
                err.range = raw_clause.range;
                err.message = mv_cstr_doc("Clause in a cond must have at least two terms: the condition and the body.", a);
                throw_pi_error(ctx.point, err);
            }
            
            SynRef condition = abstract_expr_i(raw_clause.branch.nodes.data[0], ctx);
            
            RawTree* branch_term = (raw_clause.branch.nodes.len == 2)
                ? &raw_clause.branch.nodes.data[1]
                : raw_slice(&raw_clause, 1, ctx.pia);
            SynRef branch = abstract_expr_i(*branch_term, ctx);

            CondClause* clause = mem_alloc(sizeof(CondClause), a);
            *clause = (CondClause) {.condition = condition, .branch = branch};

            push_ptr(clause, &clauses);
        }

        RawTree else_clause = raw.branch.nodes.data[raw.branch.nodes.len - 1];
        SynRef otherwise;
        {
            if (else_clause.type != RawBranch) {
                err.range = else_clause.range;
                err.message = mv_cstr_doc("Expected composite special term here. Got an atom instead.", a);
                throw_pi_error(ctx.point, err);
            }
            if (else_clause.branch.hint != HSpecial) {
                err.range = else_clause.range;
                err.message = mv_cstr_doc("Expected a composite special term here. Hint: use square brackets '[' and ']'\n"
                                          " to produce a special term.", a);
                throw_pi_error(ctx.point, err);
            }
            
            RawTree else_cond = else_clause.branch.nodes.data[0];
            SynRef cond = abstract_expr_i(else_cond, ctx);
            Syntax cond_syn = get_syntax(cond, ctx.tape);
            if (!(cond_syn.type == SLitBool && cond_syn.boolean == true)) {
                err.range = else_clause.range;
                err.message = mv_cstr_doc("The final clause in a 'cond' must always have a condition that is\n"
                                          " the literal :true.", a);
                throw_pi_error(ctx.point, err);
            }

            RawTree* branch_term = (else_clause.branch.nodes.len == 2)
                ? &else_clause.branch.nodes.data[1]
                : raw_slice(&else_clause, 1, ctx.pia);
            otherwise = abstract_expr_i(*branch_term, ctx);
        }

        Syntax syn = {
            .type = SCond,
            .cond.clauses = clauses,
            .cond.otherwise = otherwise,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FLabels: {
        if (raw.branch.nodes.len < 2) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Term former 'labels' expects at least 1 argument!", a);
            throw_pi_error(ctx.point, err);
        }

        SynRef entry = abstract_expr_i(raw.branch.nodes.data[1], ctx);

        SymPtrAssoc terms = mk_sym_ptr_assoc(raw.branch.nodes.len - 2, a);
        for (size_t i = 2; i < raw.branch.nodes.len; i++) {
            Symbol label;
            RawTree* label_expr = &raw.branch.nodes.data[i];
            if (!get_label(label_expr, &label)) {
                err.range = label_expr->range;
                err.message = mv_cstr_doc("Each label must be of the form [label [args]? expr]. However, label is incorrect", a);
                throw_pi_error(ctx.point, err);
            }

            if (label_expr->branch.nodes.len < 2) {
                err.range = label_expr->range;
                err.message = mv_cstr_doc("Each label branch is expected to have at least 2 terms. However, this only has 1.", a);
                throw_pi_error(ctx.point, err);
            }
            
            size_t index = 1;
            SymPtrAMap arguments = mk_sym_ptr_amap(8, a);
            if (is_special(label_expr->branch.nodes.data[index])) {
                Result args_out = get_annotated_symbol_list(&arguments, label_expr->branch.nodes.data[index++], ctx);
                if (args_out.type == Err) {
                    err.range = label_expr->branch.nodes.data[index].range;
                    err.message = mv_str_doc(args_out.error_message, a);
                    throw_pi_error(ctx.point, err);
                }
            }

            if (label_expr->branch.nodes.len < 1 + index) {
                err.range = label_expr->range;
                err.message = mv_cstr_doc("Label branch missing body!", a);
                throw_pi_error(ctx.point, err);
            }

            RawTree* raw_term = (label_expr->branch.nodes.len == index + 1)
                ? &label_expr->branch.nodes.data[index]
                : raw_slice(label_expr, index, ctx.pia);
            SynRef res = abstract_expr_i(*raw_term, ctx);
            SynLabelBranch* branch = mem_alloc(sizeof(SynLabelBranch), a);
            *branch = (SynLabelBranch) {
                .args = arguments,
                .body = res,
            };
            sym_ptr_bind(label, branch, &terms);
        }

        Syntax syn = {
            .type = SLabels,
            .labels.entry = entry,
            .labels.terms = terms,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FGoTo: {
        if (raw.branch.nodes.len < 2) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Term former 'go-to' expects at least one argument!", a);
            throw_pi_error(ctx.point, err);
        }
        RawTree* label = &raw.branch.nodes.data[1]; 

        if (label->type != RawAtom && label->atom.type != ASymbol) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Term former 'go-to' expects first argument to be a symbol!", a);
            throw_pi_error(ctx.point, err);
        }

        SynArray args = mk_syn_array(raw.branch.nodes.len - 2, a);
        for (size_t i = 2; i < raw.branch.nodes.len; i++) {
            push_syn(abstract_expr_i(raw.branch.nodes.data[i], ctx), &args);
        }

        Syntax syn = {
            .type = SGoTo,
            .go_to.label = label->atom.symbol,
            .go_to.args = args,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FWithReset: {
        // with-reset [lbl] e [l1 l2] e
        if (raw.branch.nodes.len != 5) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Term former 'with-reset' expects exactly 5 arguments!", a);
            throw_pi_error(ctx.point, err);
        }
        SymbolArray reset_binds = mk_symbol_array(1, a);
        SymbolArray handle_binds = mk_symbol_array(2, a);

        if (!get_symbol_list(&reset_binds, raw.branch.nodes.data[1]) || reset_binds.len != 1) {
            err.range = raw.branch.nodes.data[1].range;
            err.message = mv_cstr_doc("Term former 'with-reset' 1st argument list malformed.", a);
            throw_pi_error(ctx.point, err);
        }

        Symbol reset_point_sym = reset_binds.data[0];
        if (reset_binds.len != 1) {
            err.range = raw.branch.nodes.data[0].range;
            err.message = mv_cstr_doc("Term former 'with-reset' expects exactly 5 arguments!", a);
            throw_pi_error(ctx.point, err);
        }

        SynRef expr = abstract_expr_i(raw.branch.nodes.data[2], ctx);

        if (!get_symbol_list(&handle_binds, raw.branch.nodes.data[3]) || handle_binds.len != 3) {
            err.range = raw.branch.nodes.data[3].range;
            err.message = mv_cstr_doc("Handler list malformed!", a);
            throw_pi_error(ctx.point, err);
        }

        Symbol in_sym = handle_binds.data[1];
        Symbol cont_sym = handle_binds.data[2];

        SynRef handler = abstract_expr_i(raw.branch.nodes.data[4], ctx);

        Syntax syn = {
            .type = SWithReset,
            .with_reset.point_sym = reset_point_sym,
            .with_reset.expr = expr,
            .with_reset.in_sym = in_sym,
            .with_reset.cont_sym = cont_sym,
            .with_reset.handler = handler,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FResetTo: {
        if (raw.branch.nodes.len != 3) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Term former 'reset-to' expects two arguments!", a);
            throw_pi_error(ctx.point, err);
        }

        SynRef rpoint = abstract_expr_i(raw.branch.nodes.data[1], ctx);
        SynRef rarg = abstract_expr_i(raw.branch.nodes.data[2], ctx);
        Syntax syn = {
            .type = SResetTo,
            .reset_to.point = rpoint,
            .reset_to.arg = rarg,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FSequence: {
        PtrArray elements = mk_ptr_array(raw.branch.nodes.len - 1, a);
        size_t num_binds = 0;
        for (size_t i = 1; i < raw.branch.nodes.len; i++) {
            RawTree tree = raw.branch.nodes.data[i];
            if (is_special(tree)) {
                if (tree.type != RawBranch
                    || !eq_symbol(&tree.branch.nodes.data[0], string_to_symbol(mv_string("let!")))) {
                    err.range = tree.branch.nodes.data[0].range;
                    err.message = mv_cstr_doc("Special nodes in a 'seq' are expected to have a head 'let!'", a);
                    throw_pi_error(ctx.point, err);
                }
                if (tree.branch.nodes.len < 3) {
                    err.range = tree.range;
                    err.message = mv_cstr_doc("let! requires at least 2 terms.", a);
                    throw_pi_error(ctx.point, err);
                }
                RawTree rsym = tree.branch.nodes.data[1];
                if (rsym.type != RawAtom || rsym.atom.type != ASymbol) {
                    err.range = tree.branch.nodes.data[1].range;
                    err.message = mv_cstr_doc("'let!' expected to bind a symbol", a);
                    throw_pi_error(ctx.point, err);
                }

                RawTree* body = tree.branch.nodes.len > 3 ? raw_slice(&tree, 2, ctx.pia) : &tree.branch.nodes.data[2];
                SeqElt* elt = mem_alloc(sizeof(SeqElt), a);
                *elt = (SeqElt) {
                    .is_binding = true,
                    .symbol = rsym.atom.symbol,
                    .expr = abstract_expr_i(*body, ctx),
                };
                push_ptr(elt, &elements);

                shadow_var(rsym.atom.symbol, ctx.env);
                num_binds++;
            } else {
                SeqElt* elt = mem_alloc(sizeof(SeqElt), a);
                *elt = (SeqElt) {
                    .is_binding = false,
                    .expr = abstract_expr_i(tree, ctx),
                };
                push_ptr(elt, &elements);
            }
        }
        shadow_pop(num_binds, ctx.env);

        Syntax syn = {
            .type = SSequence,
            .sequence.elements = elements,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FModule:
        err.range = raw.range;
        err.message = mk_cstr_doc("'Module' not supported as inner-expression term former.", a);
        throw_pi_error(ctx.point, err);
    case FIs: {
        if (raw.branch.nodes.len != 3) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Term former 'is' expects precisely 2 arguments!", a);
            throw_pi_error(ctx.point, err);
        }

        SynRef term = abstract_expr_i(raw.branch.nodes.data[1], ctx);
        SynRef type = abstract_expr_i(raw.branch.nodes.data[2], ctx);
        
        Syntax syn = {
            .type = SIs,
            .is = {.val = term, .type = type},
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FInTo: {
        if (raw.branch.nodes.len != 3) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Term former 'into' expects precisely 2 arguments!", a);
            throw_pi_error(ctx.point, err);
        }

        SynRef type = abstract_expr_i(raw.branch.nodes.data[1], ctx);
        SynRef term = abstract_expr_i(raw.branch.nodes.data[2], ctx);
        
        Syntax syn = {
            .type = SInTo,
            .into = {.val = term, .type = type},
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FOutOf: {
        if (raw.branch.nodes.len != 3) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Term former 'out-of' expects precisely 2 arguments!", a);
            throw_pi_error(ctx.point, err);
        }

        SynRef type = abstract_expr_i(raw.branch.nodes.data[1], ctx);
        SynRef term = abstract_expr_i(raw.branch.nodes.data[2], ctx);
        
        Syntax syn = {
            .type = SOutOf,
            .into = {.val = term, .type = type},
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FName: {
        if (raw.branch.nodes.len != 3) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Term former 'name' expects precisely 2 arguments!", a);
            throw_pi_error(ctx.point, err);
        }

        RawTree namer = raw.branch.nodes.data[1];
        Symbol name = {};
        SynArray args = {};
        if (namer.type == RawBranch) {
            RawTreePiList name_list = namer.branch.nodes;
            if (name_list.len < 1) {
                err.range = namer.range;
                err.message = mv_cstr_doc("Term former 'name' expects name list to have at least one member!", a);
                throw_pi_error(ctx.point, err);
            }
            if (!is_symbol(name_list.data[0])) {
                err.range = name_list.data[1].range;
                err.message = mv_cstr_doc("Term former 'name' expects the first argument of name list to be a symbol!", a);
                throw_pi_error(ctx.point, err);
            }
            name = name_list.data[0].atom.symbol;

            args = mk_syn_array(name_list.len - 1, a);
            for (size_t i = 1; i < name_list.len; i++) {
                push_syn(abstract_expr_i(name_list.data[i], ctx), &args);
            }
        }
        else if (!is_symbol(raw.branch.nodes.data[1])) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Term former 'name' expects the first argument to be a symbol or symbol + type list!", a);
            throw_pi_error(ctx.point, err);
        }
        else {
            name = raw.branch.nodes.data[1].atom.symbol;
        }
        SynRef body = abstract_expr_i(raw.branch.nodes.data[2], ctx);
        
        Syntax syn = {
            .type = SName,
            .name = {.name = name, .body = body, .args = args},
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FUnName: {
        if (raw.branch.nodes.len != 2) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Term former 'unname' expects precisely 2 arguments!", a);
            throw_pi_error(ctx.point, err);
        }

        SynRef term = abstract_expr_i(raw.branch.nodes.data[1], ctx);
        
        Syntax syn = {
            .type = SUnName,
            .unname = term,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FWiden: {
        if (raw.branch.nodes.len != 3) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Term former 'widen' expects precisely 2 arguments!", a);
            throw_pi_error(ctx.point, err);
        }

        SynRef type = abstract_expr_i(raw.branch.nodes.data[2], ctx);
        SynRef term = abstract_expr_i(raw.branch.nodes.data[1], ctx);
        
        Syntax syn = {
            .type = SWiden,
            .widen = {.val = term, .type = type},
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FNarrow: {
        if (raw.branch.nodes.len != 3) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Term former 'narrow' expects precisely 2 arguments!", a);
            throw_pi_error(ctx.point, err);
        }

        SynRef type = abstract_expr_i(raw.branch.nodes.data[2], ctx);
        SynRef term = abstract_expr_i(raw.branch.nodes.data[1], ctx);
        
        Syntax syn = {
            .type = SNarrow,
            .narrow = {.val = term, .type = type},
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FSizeOf: {
        if (raw.branch.nodes.len != 2) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Term former 'size-of' expects precisely 1 argument!", a);
            throw_pi_error(ctx.point, err);
        }

        SynRef term = abstract_expr_i(raw.branch.nodes.data[1], ctx);
        
        Syntax syn = {
            .type = SSizeOf,
            .size = term,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FAlignOf: {
        if (raw.branch.nodes.len != 2) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Term former 'align-of' expects precisely 1 argument!", a);
            throw_pi_error(ctx.point, err);
        }

        SynRef term = abstract_expr_i(raw.branch.nodes.data[1], ctx);
        
        Syntax syn = {
            .type = SAlignOf,
            .size = term,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FOffsetOf: {
        if (raw.branch.nodes.len != 3) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Term former 'offset-of' expects precisely 2 arguments!", a);
            throw_pi_error(ctx.point, err);
        }

        if (!is_symbol(raw.branch.nodes.data[1])) {
            err.range = raw.branch.nodes.data[1].range;
            err.message = mv_cstr_doc("offset-of requires first term to be a symbol (fieldname) of a structure", a);
            throw_pi_error(ctx.point, err);
        }
        Symbol field = raw.branch.nodes.data[1].atom.symbol;
        SynRef term = abstract_expr_i(raw.branch.nodes.data[2], ctx);
        
        Syntax syn = {
            .type = SOffsetOf,
            .offset_of.field = field,
            .offset_of.body = term,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FDynAlloc: {
        if (raw.branch.nodes.len != 2) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Term former 'dyn-alloc' expects precisely 1 argument!", a);
            throw_pi_error(ctx.point, err);
        }
        SynRef term = abstract_expr_i(raw.branch.nodes.data[1], ctx);

        Syntax syn = {
            .type = SDynAlloc,
            .size = term,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FProcType: {
        if (raw.branch.nodes.len != 3) {
            proc_tyformer_incorrect_numterms(raw, ctx);
        }

        RawTree raw_args = raw.branch.nodes.data[1];
        if (raw_args.type != RawBranch) {
            err.range = raw.branch.nodes.data[1].range;
            err.message = mv_cstr_doc("Procedure argument list should be list.", a);
            throw_pi_error(ctx.point, err);
        }
        
        SynArray arg_types = mk_syn_array(raw_args.branch.nodes.len, a);

        for (size_t i = 0; i < raw_args.branch.nodes.len; i++) {
            SynRef arg_ty = abstract_expr_i(raw_args.branch.nodes.data[i], ctx);
            push_syn(arg_ty, &arg_types);
        }

        SynRef return_type = abstract_expr_i(raw.branch.nodes.data[2], ctx);

        Syntax syn = {
            .type = SProcType,
            .proc_type.args = arg_types,
            .proc_type.return_type = return_type,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FArrayType: {
        // (Array [n m ...] T)
        if (raw.branch.nodes.len != 3) {
            array_tyformer_incorrect_numterms(raw, ctx);
        }

        RawTree raw_dimensions = raw.branch.nodes.data[1];
        RawTree raw_type = raw.branch.nodes.data[2];


        if (raw_dimensions.type != RawBranch || raw_dimensions.branch.hint != HSpecial) {
            array_tyformer_incorrect_dimformat(raw, ctx);
        }

        RawTreePiList rdims = raw_dimensions.branch.nodes;
        U64Array dimensions = mk_u64_array(rdims.len, a);

        for (size_t i = 0; i < rdims.len; i++) {
            RawTree rdim = rdims.data[i];
            if (rdim.type != RawAtom || rdim.atom.type != AIntegral) {
                array_tyformer_dim_not_number(rdim, ctx);
            }
            push_u64(rdim.atom.int_64, &dimensions);
        }

        Syntax syn = {
            .type = SArrayType,
            .array_type.dimensions = dimensions,
            .array_type.element = abstract_expr_i(raw_type, ctx),
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FStructType: {
        SymSynAMap field_types = mk_sym_syn_amap(raw.branch.nodes.len, a);

        size_t start_idx = 1;
        bool packed = false;
        RawTree ispacked = raw.branch.nodes.data[1];
        if (ispacked.type == RawAtom && ispacked.atom.type == ASymbol) {
            start_idx++;
            if (symbol_eq(ispacked.atom.symbol, string_to_symbol(mv_string("packed")))) {
                packed = true;
            } else {
                err.range = ispacked.range;
                err.message = mv_cstr_doc("Expecting either the symbol 'packed' or a field descriptor.", a);
                throw_pi_error(ctx.point, err);
            }
        }

        for (size_t i = start_idx; i < raw.branch.nodes.len; i++) {
            RawTree fdesc = raw.branch.nodes.data[i];
            if (fdesc.type != RawBranch) {
                err.range = fdesc.range;
                err.message = mv_cstr_doc("Structure type expects all field descriptors to be lists.", a);
                throw_pi_error(ctx.point, err);
            };
            
            if (fdesc.branch.nodes.len < 2) {
                err.range = fdesc.range;
                err.message = mv_cstr_doc("Structure type expects all field descriptors to have a type.", a);
                throw_pi_error(ctx.point, err);
            };

            Symbol field;
            if (!get_fieldname(&fdesc.branch.nodes.data[0], &field)) {
                err.range = fdesc.branch.nodes.data[0].range;
                err.message = mv_cstr_doc("Structure type has malformed field name.", a);
                throw_pi_error(ctx.point, err);
            };

            RawTree* raw_ty = (fdesc.branch.nodes.len == 2) ? &fdesc.branch.nodes.data[1] : raw_slice(&fdesc, 1, ctx.pia);
            SynRef field_ty = abstract_expr_i(*raw_ty, ctx);

            sym_syn_insert(field, field_ty, &field_types);
        }

        Syntax syn = {
            .type = SStructType,
            .struct_type.fields = field_types,
            .struct_type.packed = packed,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FEnumType: {
        SymPtrAMap enum_variants = mk_sym_ptr_amap(raw.branch.nodes.len, a);

        uint8_t tag_size = 64;
        size_t start_idx = 1;
        RawTree esz = raw.branch.nodes.data[1];
        if (esz.type == RawAtom && esz.atom.type == AIntegral) {
            start_idx++;
            int64_t ilit = esz.atom.int_64;
            if (ilit == 8) {
                tag_size = 8;
            } else if (ilit == 16) {
                tag_size = 16;
            } else if (ilit == 32) {
                tag_size = 32;
            } else if (ilit == 64) {
                tag_size = 64;
            } else {
                err.range = esz.range;
                err.message = mv_cstr_doc("The enumeration tagsize (in bits) must be one of 8, 16, 32 or 64.", a);
                throw_pi_error(ctx.point, err);
            }
        }

        for (size_t i = start_idx; i < raw.branch.nodes.len; i++) {
            RawTree edesc = raw.branch.nodes.data[i];

            if (edesc.type != RawBranch) {
                err.range = edesc.range;
                err.message = mv_cstr_doc("Enumeration type expects all variant descriptors to be lists.", a);
                throw_pi_error(ctx.point, err);
            };
            
            if (edesc.branch.nodes.len < 1) {
                err.range = edesc.branch.nodes.data[0].range;
                err.message = mv_cstr_doc("Enumeration type expects all variant descriptors to have at least 1 elements.", a);
                throw_pi_error(ctx.point, err);
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
                    throw_pi_error(ctx.point, err);
                } 

                PtrArray* types = mem_alloc(sizeof(PtrArray), a);
                *types = mk_ptr_array(0, a);
                sym_ptr_insert(mname.atom.symbol, types, &enum_variants);
            } else {
                Symbol tagname;
                SynArray* types = mem_alloc(sizeof(PtrArray), a);
                *types = mk_syn_array(edesc.branch.nodes.len - 1, a);

                if (!get_fieldname(&edesc.branch.nodes.data[0], &tagname)) {
                    err.range = edesc.branch.nodes.data[0].range;
                    err.message = mv_cstr_doc("Enum type has malformed field name.", a);
                    throw_pi_error(ctx.point, err);
                };

                for (size_t i = 1; i < edesc.branch.nodes.len; i++) {
                    SynRef field_ty = abstract_expr_i(edesc.branch.nodes.data[i], ctx);
                    push_syn(field_ty, types);
                }

                sym_ptr_insert(tagname, types, &enum_variants);
            }
        }

        Syntax syn = {
            .type = SEnumType,
            .enum_type.tag_size = tag_size,
            .enum_type.variants = enum_variants,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FResetType: {
        if (raw.branch.nodes.len != 3) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Reset type former expects exactly 2 arguments!", a);
            throw_pi_error(ctx.point, err);
        }
        SynRef in_ty = abstract_expr_i(raw.branch.nodes.data[1], ctx);
        SynRef out_ty = abstract_expr_i(raw.branch.nodes.data[2], ctx);

        Syntax syn = {
            .type = SResetType,
            .reset_type.in = in_ty,
            .reset_type.out = out_ty,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FDynamicType: {
        if (raw.branch.nodes.len <= 1) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Malformed Dynamic Type expression: expects at least 1 arg.", a);
            throw_pi_error(ctx.point, err);
        }
        RawTree* body = raw.branch.nodes.len == 2 ? &raw.branch.nodes.data[1] : raw_slice(&raw, 1, ctx.pia);
        SynRef dyn_ty = abstract_expr_i(*body, ctx);

        Syntax syn = {
            .type = SDynamicType,
            .dynamic_type = dyn_ty,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FNamedType: {
        if (raw.branch.nodes.len <= 2) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Malformed Named Type expression: expects at least 2 args.", a);
            throw_pi_error(ctx.point, err);
        }

        RawTree* rname = &raw.branch.nodes.data[1];
        if (!is_symbol(*rname)) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Malformed Named Type expression: 1st arg to be name.", a);
            throw_pi_error(ctx.point, err);
        }
        Symbol name = rname->atom.symbol;

        RawTree* body = raw.branch.nodes.len == 3 ? &raw.branch.nodes.data[2] : raw_slice(&raw, 2, ctx.pia);
        SynRef rec_body = abstract_expr_i(*body, ctx);

        Syntax syn = {
            .type = SNamedType,
            .named_type.name = name,
            .named_type.body = rec_body,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FDistinctType: {
        if (raw.branch.nodes.len <= 2) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Malformed Distinct Type expression: expects at least 2 args.", a);
            throw_pi_error(ctx.point, err);
        }


        RawTree* rname = &raw.branch.nodes.data[1];
        if (!is_symbol(*rname)) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Malformed Distinct Type expression: 1st arg to be name.", a);
            throw_pi_error(ctx.point, err);
        }
        Symbol name = rname->atom.symbol;
        RawTree* body = raw.branch.nodes.len == 3 ? &raw.branch.nodes.data[2] : raw_slice(&raw, 2, ctx.pia);
        SynRef distinct = abstract_expr_i(*body, ctx);

        Syntax syn = {
            .type = SDistinctType,
            .distinct_type.name = name,
            .distinct_type.body = distinct,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FOpaqueType: {
        if (raw.branch.nodes.len <= 2) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Malformed Opaque Type expression: expects at least 2 args.", a);
            throw_pi_error(ctx.point, err);
        }
       
        RawTree* rname = &raw.branch.nodes.data[1];
        if (!is_symbol(*rname)) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Malformed Opaque Type expression: 1st arg to be name.", a);
            throw_pi_error(ctx.point, err);
        }
        Symbol name = rname->atom.symbol;
        RawTree* body = raw.branch.nodes.len == 3 ? &raw.branch.nodes.data[2] : raw_slice(&raw, 2, ctx.pia);
        SynRef opaque = abstract_expr_i(*body, ctx);

        Syntax syn = {
            .type = SOpaqueType,
            .opaque_type.name = name,
            .opaque_type.body = opaque,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FTraitType: {
        if (raw.branch.nodes.len < 3) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Wrong number of terms to trait type former.", a);
            throw_pi_error(ctx.point, err);
        }

        RawTree* rname = &raw.branch.nodes.data[1];
        if (!is_symbol(*rname)) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Malformed Trait Type expression: 1st arg to be name.", a);
            throw_pi_error(ctx.point, err);
        }
        Symbol name = rname->atom.symbol;
        
        RawTree raw_vars = raw.branch.nodes.data[2];
        SymbolArray vars = mk_symbol_array(raw_vars.branch.nodes.len, a);

        if (!get_symbol_list(&vars, raw_vars)) {
            err.range = raw_vars.range;
            err.message = mv_cstr_doc("Malformed Trait parameter list.", a);
            throw_pi_error(ctx.point, err);
        }

        SymSynAMap fields = mk_sym_syn_amap(raw.branch.nodes.len - 3, a);
        for (size_t i = 3; i < raw.branch.nodes.len; i++) {
            RawTree fdesc = raw.branch.nodes.data[i];
            if (fdesc.type != RawBranch) {
                err.range = fdesc.range;
                err.message = mv_cstr_doc("Trait expects all field descriptors to be lists.", a);
                throw_pi_error(ctx.point, err);
            }
            
            if (fdesc.branch.nodes.len < 2) {
                err.range = fdesc.range;
                err.message = mv_cstr_doc("Trait expects all field descriptors to have at least 2 elements.", a);
                throw_pi_error(ctx.point, err);
            }

            Symbol field;
            if (!get_fieldname(&fdesc.branch.nodes.data[0], &field)) {
                err.range = fdesc.branch.nodes.data[0].range;
                err.message = mv_cstr_doc("Trait has malformed field name.", a);
                throw_pi_error(ctx.point, err);
            }
            SynRef syn;
            if (fdesc.branch.nodes.len == 2) {
                syn = abstract_expr_i(fdesc.branch.nodes.data[1], ctx);
            } else {
                RawTree* raw_term = raw_slice(&fdesc, 1, ctx.pia);
                syn = abstract_expr_i(*raw_term, ctx);
            }

            sym_syn_insert(field, syn, &fields);
        }

        Syntax syn = {
            .type = STraitType,
            .trait.name = name,
            .trait.vars = vars,
            .trait.fields = fields,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FAllType: {
        if (raw.branch.nodes.len < 3) {
            err.range = raw.range;
            err.message = mk_cstr_doc("All term former requires at least 2 arguments!", a);
            throw_pi_error(ctx.point, err);
        }

        SymbolArray vars = mk_symbol_array(8, a);
        if (!get_symbol_list(&vars, raw.branch.nodes.data[1])) {
            err.range = raw.branch.nodes.data[1].range;
            err.message = mk_cstr_doc("All argument list malformed", a);
            throw_pi_error(ctx.point, err);
        }

        shadow_vars(vars, ctx.env);

        RawTree* raw_term;
        if (raw.branch.nodes.len == 3) {
            raw_term = &raw.branch.nodes.data[2];
        } else {
            raw_term = raw_slice(&raw, 2, ctx.pia);
        }
        SynRef body = abstract_expr_i(*raw_term, ctx);
        shadow_pop(vars.len, ctx.env);

        Syntax syn = {
            .type = SAllType,
            .bind_type.bindings = vars,
            .bind_type.body = body,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FSealedType: {
        if (raw.branch.nodes.len < 3) {
            err.range = raw.range;
            err.message = mk_cstr_doc("Sealed type former requires at least 2 arguments!", a);
            throw_pi_error(ctx.point, err);
        }

        SymbolArray vars = mk_symbol_array(8, a);
        if (!get_symbol_list(&vars, raw.branch.nodes.data[1])) {
            err.range = raw.branch.nodes.data[1].range;
            err.message = mk_cstr_doc("Sealed argument list malformed", a);
            throw_pi_error(ctx.point, err);
        }
        shadow_vars(vars, ctx.env);

        size_t body_idx = 2;
        SynArray implicits = mk_syn_array(8, a);
        {
            RawTree raw_implicits = raw.branch.nodes.data[body_idx];
            if (raw_implicits.type == RawBranch && raw_implicits.branch.hint == HImplicit) {
                body_idx++;
                for (size_t i = 0; i < raw_implicits.branch.nodes.len; i++) {
                    SynRef syn = abstract_expr_i(raw_implicits.branch.nodes.data[i], ctx);
                    push_syn(syn, &implicits);
                }
            }
        }

        RawTree* raw_term;
        if (raw.branch.nodes.len == body_idx + 1) {
            raw_term = &raw.branch.nodes.data[body_idx];
        } else {
            raw_term = raw_slice(&raw, body_idx, ctx.pia);
        }
        SynRef body = abstract_expr_i(*raw_term, ctx);
        shadow_pop(vars.len, ctx.env);

        Syntax syn = {
            .type = SSealedType,
            .sealed_type.vars = vars,
            .sealed_type.implicits = implicits,
            .sealed_type.body = body,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FFamily: {
        if (raw.branch.nodes.len < 3) {
            err.range = raw.range;
            err.message = mk_cstr_doc("Family term former requires at least 2 arguments!", a);
            throw_pi_error(ctx.point, err);
        }

        SymbolArray vars = mk_symbol_array(8, a);
        if (!get_symbol_list(&vars, raw.branch.nodes.data[1])) {
            err.range = raw.range;
            err.message = mk_cstr_doc("All argument list malformed", a);
            throw_pi_error(ctx.point, err);
        }

        shadow_vars(vars, ctx.env);

        RawTree* raw_term = (raw.branch.nodes.len == 3)
            ? &raw.branch.nodes.data[2]
            : raw_slice(&raw, 2, ctx.pia);
        
        SynRef body = abstract_expr_i(*raw_term, ctx);
        shadow_pop(vars.len, ctx.env);

        Syntax syn = {
            .type = STypeFamily,
            .bind_type.bindings = vars,
            .bind_type.body = body,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FLiftCType: {
        RawTree* raw_term = (raw.branch.nodes.len == 2)
            ? &raw.branch.nodes.data[1]
            : raw_slice(&raw, 1, ctx.pia);
        SynRef c_type = abstract_expr_i(*raw_term, ctx);
        Syntax syn = {
            .type = SLiftCType,
            .c_type = c_type,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FReinterpretNative:
    case FReinterpretRelic: {
        if (raw.branch.nodes.len < 3) {
            err.range = raw.range;
            err.message = mk_cstr_doc("Reinterpret term former requires at least 2 arguments!", a);
            throw_pi_error(ctx.point, err);
        }

        SynRef type = abstract_expr_i(raw.branch.nodes.data[1], ctx);

        RawTree* raw_term = (raw.branch.nodes.len == 3)
            ? &raw.branch.nodes.data[2]
            : raw_slice(&raw, 2, ctx.pia);

        SynRef body = abstract_expr_i(*raw_term, ctx);

        Syntax syn = {
            .type = SReinterpret,
            .reinterpret.from_native = former == FReinterpretNative,
            .reinterpret.type = type, 
            .reinterpret.body = body,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FConvertNative:
    case FConvertRelic: {
        if (raw.branch.nodes.len < 3) {
            err.range = raw.range;
            err.message = mk_cstr_doc("convert term former requires at least 2 arguments!", a);
            throw_pi_error(ctx.point, err);
        }

        SynRef type = abstract_expr_i(raw.branch.nodes.data[1], ctx);

        RawTree* raw_term = (raw.branch.nodes.len == 3)
            ? &raw.branch.nodes.data[2]
            : raw_slice(&raw, 2, ctx.pia);

        SynRef body = abstract_expr_i(*raw_term, ctx);

        Syntax syn = {
            .type = SConvert,
            .convert.from_native = former == FConvertNative,
            .convert.type = type, 
            .convert.body = body,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FTypeOf: {
        if (raw.branch.nodes.len < 2) {
            err.range = raw.range;
            err.message = mk_cstr_doc("type-of term former requires 1 argument!", a);
            throw_pi_error(ctx.point, err);
        }
        RawTree* raw_term = (raw.branch.nodes.len == 2)
            ? &raw.branch.nodes.data[1]
            : raw_slice(&raw, 1, ctx.pia);

        SynRef body = abstract_expr_i(*raw_term, ctx);

        Syntax syn = {
            .type = STypeOf,
            .type_of = body,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FDescribe: {
        if (raw.branch.nodes.len != 2) {
            err.range = raw.range;
            err.message = mk_cstr_doc("describe term former requires 1 argument!", a);
            throw_pi_error(ctx.point, err);
        }

        SynRef abs = abstract_expr_i(raw.branch.nodes.data[1], ctx);
        SymbolArray* path = try_get_path(abs, ctx);

        if (!path) {
            err.range = raw.branch.nodes.data[1].range;
            err.message = mk_cstr_doc("describe expects argument to be a path!", a);
            throw_pi_error(ctx.point, err);
        }
        Syntax syn = {
            .type = SDescribe,
            .to_describe = *path,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FQuote: {
        if (raw.branch.nodes.len != 2) {
            err.range = raw.range;
            err.message = mk_cstr_doc("quote term former expects 1 argument!", a);
            throw_pi_error(ctx.point, err);
        }

        RawTree body = raw.branch.nodes.data[1];

        Syntax syn = {
            .type = SQuote,
            .quoted = body,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    case FCapture: {
        if (raw.branch.nodes.len != 2) {
            err.range = raw.range;
            err.message = mk_cstr_doc("capture term former expects 1 argument!", a);
            throw_pi_error(ctx.point, err);
        }
        
        void* value = NULL;
        PiType* type = NULL;

        RawTree capture = raw.branch.nodes.data[1];
        if (is_symbol(capture)) {
            ShadowEntry entry = shadow_env_lookup(capture.atom.symbol, ctx.env);
            if (entry.type == SGlobal) {
                value = entry.value;
                type = entry.vtype;
            }
            
            Syntax syn = {
                .type = SCapture,
                .capture.value = value,
                .capture.type = type,
            };
            SynRef res = new_syntax(ctx.tape);
            set_syntax(res, syn , ctx.tape);
            set_range(res, (SynRange){.term = raw.range}, ctx.tape);
            return res;
        }

        SynRef cap = abstract_expr_i(capture, ctx);
        Syntax cap_syn = get_syntax(cap, ctx.tape);
        if (cap_syn.type != SAbsVariable) {
            err.range = capture.range;
            err.message = mk_cstr_doc("Capture term former must capture a symbol or path", a);
            throw_pi_error(ctx.point, err);
        } else {
            Syntax syn = {
                .type = SCapture,
                .capture.value = cap_syn.abvar.value,
                .capture.type = cap_syn.abvar.type,
            };
            SynRef res = new_syntax(ctx.tape);
            set_syntax(res, syn , ctx.tape);
            set_range(res, (SynRange){.term = raw.range}, ctx.tape);
            return res;
        }
    }
    case FDevAnnotation: {
        DevFlag flags = DevNone;
        size_t curr_node = 1;
        while (curr_node < raw.branch.nodes.len && is_special(raw.branch.nodes.data[curr_node])) {
            RawTree curr = raw.branch.nodes.data[curr_node];
            curr_node++;
            if (curr.branch.nodes.len < 1) {
                err.range = curr.range;
                err.message = mv_cstr_doc("Developer node expects annotations to have a type, this one is empty", a);
                throw_pi_error(ctx.point, err);
            }
                
            RawTree head = curr.branch.nodes.data[0];
            if (is_symbol(head) && symbol_eq(head.atom.symbol, string_to_symbol(mv_string("break")))) {
                flags = flags | check_dev_flags(curr, ctx);
            } else if (is_symbol(head) && symbol_eq(head.atom.symbol, string_to_symbol(mv_string("print")))) {
                DevFlag prn_flags = check_dev_flags(curr, ctx);
                flags = flags | (prn_flags << 3);
            } else {
                err.range = head.range;
                err.message = mv_cstr_doc("Expected head of annotation in developer node to be either the symbol 'break' or the symbol 'print'", a);
                throw_pi_error(ctx.point, err);
            }
                      
        }

        if (curr_node >= raw.branch.nodes.len) {
            err.range = raw.range;
            err.message = mv_cstr_doc("This developer node lacks a body!", a);
            throw_pi_error(ctx.point, err);
        }
        if (curr_node + 1!= raw.branch.nodes.len) {
            err.range = raw.branch.nodes.data[curr_node].range;
            err.message = mv_cstr_doc("This developer node has multiple bodies! This should eb the only body", a);
            throw_pi_error(ctx.point, err);
        }

        if (flags & DBAbstract)
            debug_break();
        if (flags & DPAbstract)
            panic(mv_string("not implemented: developer-print abstract."));

        SynRef inner = abstract_expr_i(raw.branch.nodes.data[curr_node], ctx);

        Syntax syn = {
            .type = SDevAnnotation,
            .dev.flags = flags,
            .dev.inner = inner,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = raw.range}, ctx.tape);
        return res;
    }
    }
    panic(mv_string("Invalid term-former provided to mk_term."));
}

MacroResult eval_macro(ComptimeHead head, RawTree raw, AbstractionICtx ctx) {
    // Call the function (uses Pico ABI)
    MacroResult output;
    RawTreePiList input = raw.branch.nodes;
    void* dvars = get_dynamic_memory();
    void* vstack_memory_ptr = ctx.vstack_memory_ptr; 
    void* dynamic_memory_ptr = ctx.dynamic_memory_ptr; 

    PiAllocator old_temp_alloc = set_std_temp_allocator(*ctx.pia);

    // TODO: swap so this is backend independent (use foreign_adapters to call) 
    int64_t out;


#if ARCH == AMD64
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

                         "mov %4, %%r13    \n"
                         "mov %3, %%r12    \n"
                         "mov %2, %%r14    \n"
                         "mov %2, %%r15    \n"

                         // Push output ptr & sizeof (MacroResult), resp
                         "push %6          \n" // output ptr
                         "push %7          \n" // sizeof (MacroResult)

                         // Push arg (array) onto stack
                         //"push 0x30(%4)       \n"
                         "push 0x28(%5)       \n"
                         "push 0x20(%5)       \n"
                         "push 0x18(%5)       \n"
                         "push 0x10(%5)       \n"
                         "push 0x8(%5)        \n"
                         "push (%5)         \n"

                         // First store the function to call in RAX, in case
                         // the compiler has stored it as an offset to rbp
                         // then we can set rbp and call
                         "mov %1, %%rax \n"
                         "mov %%rsp, %%rbp \n"

                         // Call function, this should consume 'Array' from the stack and push
                         // 'Raw Syntax' onto the stack
                         "call *%%rax         \n"

                         // After calling the function, we
                         // expect a RawTree to be atop the stack:
#if ABI == SYSTEM_V_64
                         // memcpy (dest = rdi, src = rsi, size = rdx)
                         // retval = rax 
                         // Note: 0x58 = sizeof(MacroResult)
                         "mov 0x58(%%rsp), %%rdx   \n"
                         "mov 0x60(%%rsp), %%rdi   \n"
                         "mov %%rsp, %%rsi         \n"
                         "call memcpy              \n"

#elif ABI == WIN_64
                         // memcpy (dest = rcx, src = rdx, size = r8)
                         // retval = rax
                         "mov 0x58(%%rsp), %%r8    \n"
                         "mov 0x60(%%rsp), %%rcx   \n"
                         "mov %%rsp, %%rdx         \n"
                         "sub $0x20, %%rsp         \n"
                         "call memcpy              \n"
                         "add $0x20, %%rsp         \n"
#else
#error "Unknown calling convention"
#endif
                         // pop value from stack 
                         // Note: 0x58 = sizeof(MacroResult)
                         "mov 0x58(%%rsp), %%rax   \n"
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
                           , "r" (vstack_memory_ptr)
                           , "r" (dynamic_memory_ptr)
                           , "r" (dvars)
                           , "r" (&input)
                           , "r" (&output)
                           , "c" (sizeof(MacroResult)) 
                           // Clobbers are either registers we change (output cannot be trusted)
                           // or registers we don't want compiler to assign to input values
                         : "rax", "r13", "r14", "r15");
#elif ARCH == AARCH64
    panic(mv_string("TODO: implement macro calls for AARCH64"));
#else
  #error "Calling convention: Unknown Windows ABI"
#endif

    set_std_temp_allocator(old_temp_alloc);
    return output;
}

SynRef abstract_expr_i(RawTree raw, AbstractionICtx ctx) {
    // TODO: add debug checks that shadow_env.len is preserved 
    PicoError err = {.range = raw.range};
    Allocator* a = ctx.gpa;
    // Resolution does not perform type analysis, so we set the type pointer to NULL
    // This is then resolved in the typechecking stage;
    switch (raw.type) {
    case RawAtom: {
        SynRef res = new_syntax(ctx.tape);
        switch(raw.atom.type) {
        case ASymbol: {
            Syntax syn = {
                .type = SVariable,
                .variable = raw.atom.symbol,
            };
            set_syntax(res, syn , ctx.tape);
            set_range(res, (SynRange){.term = raw.range}, ctx.tape);
            break;
        }
        case AIntegral: {
            Syntax syn = {
                .type = SLitUntypedIntegral,
                .integral.value = raw.atom.int_64,
            };
            set_syntax(res, syn , ctx.tape);
            set_range(res, (SynRange){.term = raw.range}, ctx.tape);
            break;
        }
        case AFloating: {
            Syntax syn = {
                .type = SLitUntypedFloating,
                .floating.value = raw.atom.float_64,
            };
            set_syntax(res, syn , ctx.tape);
            set_range(res, (SynRange){.term = raw.range}, ctx.tape);
            break;
        }
        case ABool: {
            Syntax syn = {
                .type = SLitBool,
                .boolean = (bool) raw.atom.int_64,
            };
            set_syntax(res, syn , ctx.tape);
            set_range(res, (SynRange){.term = raw.range}, ctx.tape);
            break;
        }
        case AString: {
            Syntax syn = {
                .type = SLitString,
                .string = raw.atom.string,
            };
            set_syntax(res, syn , ctx.tape);
            set_range(res, (SynRange){.term = raw.range}, ctx.tape);
            break;
        }
        case ACapture: {
            Syntax syn = {
                .type = SAbsVariable,
                .abvar.type = raw.atom.capture.type,
                .abvar.value = raw.atom.capture.value,
                .abvar.index = 0,
                // TODO: symbol?
            };
            set_syntax(res, syn , ctx.tape);
            set_range(res, (SynRange){.term = raw.range}, ctx.tape);
            break;
        }
        default:
            panic(mv_string("Don't know how to make a literal from this atom!."));
        }
        return res;
        break;
    }
    case RawBranch: {
        // Currently, can only have function calls, so all Raw lists compile down to an application
        if (raw.branch.nodes.len < 1) {
            err.range = raw.range;
            err.message = mk_cstr_doc("Raw Syntax must have at least one element!", a);
            throw_pi_error(ctx.point, err);
        }

        if (!is_expr(raw.branch.nodes.data[0])) {
            err.range = raw.branch.nodes.data[0].range;
            err.message = mk_cstr_doc("Unprocessed non-expression: syntax hints '[', ']' or implicits '{' '}'\n" 
                                      "should only be used for term specific syntax or implicit arguments, respectively.", a);
            throw_pi_error(ctx.point, err);
        }
        
        ComptimeHead head = comptime_head(raw.branch.nodes.data[0], ctx);
        switch (head.type) {
        case HeadSyntax:
            return mk_application_body(head.syn, raw, ctx);
        case HeadFormer:
            return mk_term(head.former, raw, ctx);
        case HeadMacro: {
            // Call the function (uses Pico ABI)
            MacroResult output = eval_macro(head, raw, ctx);
            if (output.result_type == 1) {
                return abstract_expr_i(output.term, ctx);
            } else {
                PicoError err = (PicoError) {
                    .message = mk_str_doc(output.err.message, a),
                    .range = output.err.range,
                };
                throw_pi_error(ctx.point, err);
            }
        }
        default:
            panic(mv_string("Bad comptime head given!"));
        }
        break;
    }
    default:
        panic(mv_string("Internal Error: Name Resolution Received an invalid Raw Syntax Tree"));
    }
}


TopLevel mk_toplevel(TermFormer former, RawTree raw, AbstractionICtx ctx) {
    Allocator* a = ctx.gpa;
    PicoError err = {.range = raw.range};
    TopLevel res;
    switch (former) {
    case FDefine: {
        if (raw.branch.nodes.len < 3) {
            err.message = mv_cstr_doc("Definitions expect at least 2 terms", a);
            throw_pi_error(ctx.point, err);
        }

        if (!is_symbol(raw.branch.nodes.data[1])) {
            err.range = raw.branch.nodes.data[1].range;
            err.message = mv_cstr_doc("First argument to definitions should be a symbol", a);
            throw_pi_error(ctx.point, err);
        }

        Symbol sym = raw.branch.nodes.data[1].atom.symbol;
        
        RawTree* raw_term = (raw.branch.nodes.len == 3) ? &raw.branch.nodes.data[2] : raw_slice(&raw, 2, ctx.pia);

        shadow_var(sym, ctx.env);
        SynRef out = abstract_expr_i(*raw_term, ctx);
        shadow_pop(1, ctx.env);

        res = (TopLevel) {
            .type = TLDef,
            .def.bind = sym,
            .def.value = out,
        };

        break;
    }
    case FDeclare: {
        if (raw.branch.nodes.len < 3) {
            err.range = raw.range;
            err.message = mv_cstr_doc("Declarations expect at least 2 terms", a);
            throw_pi_error(ctx.point, err);
        }

        if (!is_symbol(raw.branch.nodes.data[1])) {
            err.range = raw.branch.nodes.data[1].range;
            err.message = mv_cstr_doc("First argument to declaration should be a symbol", a);
            throw_pi_error(ctx.point, err);
        }

        Symbol sym = raw.branch.nodes.data[1].atom.symbol;
        
        // Declaration bodies are a set of field, value pairs, e.g.
        // [.type <Type>]
        // [.optimise <level>]
        // [.inline-hint <hint>]

        shadow_var(sym, ctx.env);
        SymSynAMap properties = mk_sym_syn_amap(raw.branch.nodes.len, a);
        for (size_t i = 2; i < raw.branch.nodes.len; i++) {
            RawTree fdesc = raw.branch.nodes.data[i];
            if (fdesc.type != RawBranch) {
                err.range = fdesc.range;
                err.message = mv_cstr_doc("Declaration expects all property descriptors to be compound terms.", a);
                throw_pi_error(ctx.point, err);
            }
            
            if (fdesc.branch.nodes.len < 2) {
                err.range = fdesc.range;
                err.message = mv_cstr_doc("Declaration expects all property descriptors to have at least 2 elements.", a);
                throw_pi_error(ctx.point, err);
            }

            Symbol field;
            if (!get_fieldname(&fdesc.branch.nodes.data[0], &field)) {
                err.range = fdesc.branch.nodes.data[0].range;
                err.message = mv_cstr_doc("Declaration has malformed property name.", a);
                throw_pi_error(ctx.point, err);
            }

            RawTree* val_desc = fdesc.branch.nodes.len == 2 ? &fdesc.branch.nodes.data[1] : raw_slice(&fdesc, 1, ctx.pia); 
            SynRef syn = abstract_expr_i(*val_desc, ctx);

            sym_syn_insert(field, syn, &properties);
        }
        shadow_pop(1, ctx.env);

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
            throw_pi_error(ctx.point, err);
        }

        ImportClauseArray clauses = mk_import_clause_array(raw.branch.nodes.len - 1, a);
        for (size_t i = 1; i < raw.branch.nodes.len; i++) {
            ImportClause clause = abstract_import_clause(&raw.branch.nodes.data[i], a, ctx.point);
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
            .expr = mk_term(former, raw, ctx),
        };
        break;
    }
    }
    return res;
}

TopLevel abstract_i(RawTree raw, AbstractionICtx ctx) {
    Allocator* a = ctx.gpa;
    // first: try a unique toplevel-form
    bool unique_toplevel = false;

    if (raw.type == RawBranch && raw.branch.nodes.len > 1) {
        ComptimeHead head = comptime_head(raw.branch.nodes.data[0], ctx);
        switch (head.type) {
        case HeadSyntax:
            return (TopLevel) {
                .type = TLExpr,
                .expr = mk_application_body(head.syn, raw, ctx),
            };
        case HeadFormer:
            return mk_toplevel(head.former, raw, ctx);
        case HeadMacro: {
            MacroResult output = eval_macro(head, raw, ctx);
            if (output.result_type == 1) {
                return abstract_i(output.term, ctx);
            } else {
                PicoError err = (PicoError) {
                    .message = mk_str_doc(output.err.message, a),
                    .range = output.err.range,
                };
                throw_pi_error(ctx.point, err);
            }
        }
        }
    } 

    if (!unique_toplevel) {
        return (TopLevel) {
            .type = TLExpr,
            .expr = abstract_expr_i(raw, ctx),
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

SymbolArray get_path(RawTree raw, Allocator *a, PiErrorPoint *point) {
    PicoError err;
    err.range = raw.range;

    SymbolArray path = mk_symbol_array(4, a);
    bool running = true;
    while (running) {
        if (raw.type == RawBranch) {
            if (raw.branch.nodes.len == 3) {
                RawTree rhead = raw.branch.nodes.data[0];
                if (!(rhead.type == RawAtom && rhead.atom.type == ASymbol) ||
                    !symbol_eq(rhead.atom.symbol, string_to_symbol(mv_string(".")))) {
                    err.range = rhead.range;
                    err.message = mv_cstr_doc("Invalid path separator: expected '.'", a);
                    throw_pi_error(point, err);
                }

                RawTree path_part = raw.branch.nodes.data[1];
                if (!(path_part.type == RawAtom && path_part.atom.type == ASymbol)) {
                    err.range = path_part.range;
                    err.message = mv_cstr_doc("Invalid path part: expecting a symbol here.", a);
                    throw_pi_error(point, err);
                }
            
                push_symbol(path_part.atom.symbol, &path);
                raw = raw.branch.nodes.data[2];
            } else {
                err.message = mv_cstr_doc("Invalid module path: expect foo.bar, (. foo bar) or foo.", a);
                throw_pi_error(point, err);
            }
        } else if (raw.type == RawAtom && raw.atom.type == ASymbol) {
            push_symbol(raw.atom.symbol, &path);
            running = false;
        } else {
            err.message = mv_cstr_doc("Invalid module path: expect foo.bar, (. foo bar) or foo.", a);
            throw_pi_error(point, err);
        }
    }

    // Reverse path:
    for (size_t i = 0; i < path.len / 2; i++) {
        Symbol s1 = path.data[i];
        Symbol s2 = path.data[path.len - (i + 1)];
        path.data[i] = s2;
        path.data[path.len - (i + 1)] = s1;
    }
    return path;
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
        // field
        // (. field parent)
        if (raw->branch.nodes.len == 1) {
            SymbolArray path = get_path(*raw, a, point);
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
            // Check for '.'
            if (eq_symbol(&raw->branch.nodes.data[0], string_to_symbol(mv_string(".")))) {
                SymbolArray path = get_module_path(*raw, a, point);
                return (ImportClause) {
                    .type = Import,
                    .path = path,
                };
            } else {
                Symbol middle;
                if(!get_fieldname(&raw->branch.nodes.data[1], &middle)) {
                    err.range = raw->branch.nodes.data[1].range;
                    err.message = mv_cstr_doc("Invalid import clause - expected a keyword such as :as or :only", a);
                    throw_pi_error(point, err);
                }
                if (symbol_eq(middle, string_to_symbol(mv_string("as")))) {
                    Symbol rename;
                    if(!get_fieldname(&raw->branch.nodes.data[2], &rename)) {
                        err.range = raw->branch.nodes.data[0].range;
                        err.message = mv_cstr_doc("Invalid import-as new name", a);
                        throw_pi_error(point, err);
                    }
                    SymbolArray path = get_path(raw->branch.nodes.data[0], a, point);
                    return (ImportClause) {
                        .type = ImportAs,
                        .path = path,
                        .rename = rename,
                    };
                } else if (symbol_eq(middle, string_to_symbol(mv_string("only")))) {
                    RawTree symlist = raw->branch.nodes.data[2]; 
                    if (symlist.type != RawBranch) {
                        err.range = raw->branch.nodes.data[2].range;
                        err.message = mv_cstr_doc("When importing with ':only', exepect a symbol-list here.", a);
                        throw_pi_error(point, err);
                    }

                    SymbolArray members = mk_symbol_array(symlist.branch.nodes.len, a);
                    for (size_t i = 0; i < symlist.branch.nodes.len; i++) {
                        RawTree rsymbol = symlist.branch.nodes.data[i];
                        if (rsymbol.type == RawAtom && rsymbol.atom.type == ASymbol) {
                          push_symbol(rsymbol.atom.symbol, &members);
                        } else {
                          err.range = rsymbol.range;
                          err.message = mv_cstr_doc("Expecting a symbol here.", a);
                          throw_pi_error(point, err);
                        }
                    }
                    SymbolArray path = get_path(raw->branch.nodes.data[0], a, point);
                    return (ImportClause) {
                        .type = ImportMany,
                        .path = path,
                        .members = members,
                    };
                } else {
                    err.range = raw->branch.nodes.data[1].range;
                    err.message = mv_cstr_doc("Invalid import clause: expecting the keyword :as or :only", a);
                    throw_pi_error(point, err);
                }
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
                err.message = mv_cstr_doc("Invalid export clause - expected to get a keyword such as :as or :only", a);
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
ComptimeHead comptime_head(RawTree raw, AbstractionICtx ctx) {
    if (is_symbol(raw)) {
        Symbol sym = raw.atom.symbol;
        ShadowEntry entry = shadow_env_lookup(sym, ctx.env);
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
    SynRef ref = abstract_expr_i(raw, ctx);
    Syntax syn = get_syntax(ref, ctx.tape);
    if (syn.type == SAbsVariable) {
        if (syn.abvar.type->sort == TPrim && syn.abvar.type->prim == TFormer) {
            return (ComptimeHead){.type = HeadFormer, .former = *((TermFormer*)syn.abvar.value)};
        } else if (syn.abvar.type->sort == TPrim && syn.abvar.type->prim == TMacro) {
            return (ComptimeHead){.type = HeadMacro, .macro_addr = *((uint64_t*)syn.abvar.value)};
        }
    }
    return (ComptimeHead){.type = HeadSyntax, .syn = ref};
}

Module* try_get_module(SynRef ref, AbstractionICtx ctx) {
    Syntax syn = get_syntax(ref, ctx.tape);
    switch (syn.type) {
    case SVariable: {
        ShadowEntry se = shadow_env_lookup(syn.variable, ctx.env);
        if ((se.type == SGlobal || se.type == SLocal) && se.is_module) {
            return se.module;
        }
        break;
    }
    case SProjector: {
        Syntax syn = get_syntax(ref, ctx.tape);
        Module *module = try_get_module(syn.projector.val, ctx);
        if (module) {
            ModuleEntry* entry = get_def(syn.projector.field, module);
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

SymbolArray* try_get_path(SynRef ref, AbstractionICtx ctx) {
    SymbolArray arr = mk_symbol_array(8, ctx.gpa);
    bool running = true;
    while (running) {
        Syntax syn = get_syntax(ref, ctx.tape);
        switch (syn.type) {
        case SVariable: {
            push_symbol(syn.variable, &arr);
            running = false;
            break;
        }
        case SProjector: {
            push_symbol(syn.projector.field, &arr);
            ref = syn.projector.val;
            break;
        }
        default:
            return NULL;
        }
    }
    reverse_symbol_array(arr);
    SymbolArray* out = mem_alloc(sizeof(SymbolArray), ctx.gpa);
    *out = arr;
    return out;
}

DevFlag check_dev_flags(RawTree raw, AbstractionICtx ctx) {
    DevFlag flags = DevNone;
    for (size_t i = 1; i < raw.branch.nodes.len; i++) {
        RawTree ann = raw.branch.nodes.data[i];
        if (!is_symbol(ann)) {
            PicoError err = (PicoError) {
                .range = ann.range,
                .message = mv_cstr_doc("Expected a symbol which is either 'abstract', 'typecheck' or 'codegen' here.", ctx.gpa)
            };
            throw_pi_error(ctx.point, err);
        }
        if (symbol_eq(ann.atom.symbol, string_to_symbol(mv_string("abstract")))) {
            flags |= DBAbstract;
        } else if (symbol_eq(ann.atom.symbol, string_to_symbol(mv_string("typecheck")))) {
            flags |= DBTypecheck;
        } else if (symbol_eq(ann.atom.symbol, string_to_symbol(mv_string("codegen")))) {
            flags |= DBGenerate;
        } else {
            PicoError err = (PicoError) {
                .range = ann.range,
                .message = mv_cstr_doc("Expected a symbol which is either 'abstract', 'typecheck' or 'codegen' here.", ctx.gpa)
            };
            throw_pi_error(ctx.point, err);
        }
    }
    return flags;
}

bool is_special(RawTree raw) {
    return raw.type == RawBranch && raw.branch.hint == HSpecial; 
}

SynRef resolve_module_projector(Range range, SynRef source, RawTree* msym, AbstractionICtx ctx) {
    Allocator *a = ctx.gpa;
    Module* m = try_get_module(source, ctx);
    if (m) {
        // TODO (INVESTIGATION): do we have a better way to manage lookup
        // Note: seems like having to check for the special case of
        // Kind/Constraint is causing issues. 
        ModuleEntry* e = get_def(msym->atom.symbol, m);
        if (e) {
            if (e->is_module) {
                Syntax syn = {
                    .type = SProjector,
                    .projector.field = msym->atom.symbol,
                    .projector.val = source,
                };
                SynRef res = new_syntax(ctx.tape);
                set_syntax(res, syn , ctx.tape);
                set_range(res, (SynRange){.term = range}, ctx.tape);
                return res;
            } else {
                Syntax syn = {
                    .type = SAbsVariable,
                    .abvar.index = 0,
                    .abvar.value = (e->type.sort == TKind || e->type.sort == TConstraint) ? &e->value : e->value,
                    .abvar.type = &e->type,
                    .abvar.symbol = msym->atom.symbol,
                };
                SynRef res = new_syntax(ctx.tape);
                set_syntax(res, syn , ctx.tape);
                set_range(res, (SynRange){.term = range}, ctx.tape);
                return res;
            }
        } else {
            PicoError err = (PicoError) {
                .range = msym->range,
                .message = mv_str_doc(string_cat(mv_string("Field not found in module: "),
                                                 symbol_to_string(msym->atom.symbol, a), a), a)
            };
            throw_pi_error(ctx.point, err);
        }
    } else {
        Syntax syn = {
            .type = SProjector,
            .projector.field = msym->atom.symbol,
            .projector.val = source,
        };
        SynRef res = new_syntax(ctx.tape);
        set_syntax(res, syn , ctx.tape);
        set_range(res, (SynRange){.term = range}, ctx.tape);
        return res;
    }
}

void deduce_dimension(U64Array *dims, RawTree nodes, AbstractionICtx ctx) {
  bool on_node = (nodes.type == RawBranch && nodes.branch.hint == HSpecial);
  while (on_node) {
      push_u64(nodes.branch.nodes.len, dims);
      if (nodes.branch.nodes.len > 0) {
          nodes = nodes.branch.nodes.data[0];
          on_node = (nodes.type == RawBranch && nodes.branch.hint == HSpecial);
      } else {
          on_node = false;
      }
  }
}

RawTreePiList next_node_arr(U64Array *index, U64Array dims, RawTree nodes, AbstractionICtx ctx) {
    // TODO: harden?
    if (nodes.type != RawBranch || nodes.branch.hint != HSpecial) {
        array_incorrect_format(nodes, ctx);
    }

    if (dims.len == 1) {
        // Special case: there is only one node array.
        index->data[0] = dims.data[0];
        return nodes.branch.nodes;
    } else {
        // Mulidimensional array case:
        // retrieve the array asoociated with the current index
        for (size_t i = 0; i < dims.len - 1; i++) {
            if (nodes.branch.nodes.len != dims.data[i]) {
                array_incorrect_size(nodes, dims.data[i], ctx);
            }
            nodes = nodes.branch.nodes.data[index->data[i]];
            if (nodes.type != RawBranch || nodes.branch.hint != HSpecial) {
                array_incorrect_format(nodes, ctx);
            }
        }

        // Propagate increment upwards
        size_t layer = 2;
        bool prop = true;
        while (prop & (layer <= dims.len)) {
            index->data[index->len - layer]++;
            // Propagate changes to next layer if we are at the end of the
            // current subarray AND there exist another layer
            prop = (index->data[index->len - layer] >= dims.data[index->len - layer])
                && layer != dims.len;
            if (prop ) {
                index->data[index->len - layer] = 0;
            }
            layer++;
        }
        return nodes.branch.nodes;
    }
}
