#ifndef __PICO_TYPECHECK_TYPE_ERRORS_H
#define __PICO_TYPECHECK_TYPE_ERRORS_H

#include "pico/typecheck/typecheck.h"
#include "pico/typecheck/unify.h"


// ---------------------------------------------------------------------- 
//
//                              Typechecking  
//
// ----------------------------------------------------------------------

typedef enum {
    URNone,
    URCheck,
} UnifyReason_t;

typedef struct {
    PiType *expected;
    PiType *actual;
    Range range;
} UReasonCheck;

typedef struct {
    UnifyReason_t type;
    union {
        UReasonCheck check;
    };
} UnifyReason;

// After unification, checkif typecheck was successful
void check_result_out(UnifyResult out, Range range, UnifyReason reason, Allocator* a, PiErrorPoint* point);


// Variable
_Noreturn void type_error_unexpected_module(Syntax* syn, Module* module, TypeCheckContext ctx);
_Noreturn void type_error_unknown_var(Syntax* syn, TypeCheckContext ctx);

// Declarations
_Noreturn void type_error_invalid_declaration(Symbol type, Syntax* arg, TypeCheckContext ctx);

// Import
_Noreturn void type_error_invalid_import(ImportClause clause, Range range, TypeCheckContext ctx);

// Procedure
_Noreturn void type_error_expecting_instance_arg(size_t implicit_idx, Syntax* proc, TypeCheckContext ctx);
_Noreturn void type_error_proc_incorrect_num_implicits(Syntax* proc, PiType* type, TypeCheckContext ctx);
_Noreturn void type_error_proc_incorrect_num_args(Syntax* proc, PiType* type, TypeCheckContext ctx);

// Application and All Application
typedef enum {InvTypes, InvImplicits, InvValues} InvalidArgType;

_Noreturn void type_error_invalid_application_target(PiType* type, Syntax* app, TypeCheckContext ctx);
_Noreturn void type_error_incorrect_num_args(PiType* type, Syntax *app, InvalidArgType args_type, TypeCheckContext ctx);
_Noreturn void type_error_incorrect_num_args_all_noproc(PiType* type, Syntax* app, bool is_implicit_args, TypeCheckContext ctx);
_Noreturn void type_error_all_app_couldnt_deduce_types(size_t arg, Syntax* app, TypeCheckContext ctx);

// Sealing
_Noreturn void type_error_invalid_seal_type(PiType* type, Syntax* seal, TypeCheckContext ctx);
_Noreturn void type_error_incorrect_num_seal_args(PiType* type, Syntax* seal, TypeCheckContext ctx);

// Sealing
_Noreturn void type_error_invalid_unseal_type(PiType* type, Syntax* unseal, TypeCheckContext ctx);
_Noreturn void type_error_incorrect_num_unseal_binds(PiType* type, Syntax* unseal, TypeCheckContext ctx);

// Constructor / Variant
_Noreturn void type_error_invalid_variant_type(PiType* type, Syntax* variant, TypeCheckContext ctx);
_Noreturn void type_error_incorrect_num_variant_args(PiType* type, Syntax* variant, size_t variant_idx, TypeCheckContext ctx);
_Noreturn void type_error_missing_variant_tag(PiType* type, Syntax* variant, TypeCheckContext ctx);

// Match 
_Noreturn void type_error_match_invalid_type(PiType* type, Syntax* variant, TypeCheckContext ctx);
_Noreturn void type_error_match_duplicate_tag(PiType* type, Syntax* match, size_t variant_idx, TypeCheckContext ctx);
_Noreturn void type_error_match_incorrect_tag(PiType* type, Syntax* match, size_t variant_idx, TypeCheckContext ctx);
_Noreturn void type_error_match_num_binds(PiType* type, Syntax* match, size_t variant_idx, TypeCheckContext ctx);
_Noreturn void type_error_match_missing_variants(PiType* type, Syntax* match, size_t variant_idx, TypeCheckContext ctx);

// Struct
_Noreturn void type_error_struct_invalid_type(PiType* type, Syntax* strct, TypeCheckContext ctx);
_Noreturn void type_error_struct_missing_field(PiType* type, Syntax* strct, TypeCheckContext ctx);
_Noreturn void type_error_struct_dupliate_field(PiType* type, Syntax* strct, TypeCheckContext ctx);
_Noreturn void type_error_struct_extra_field(PiType* type, Syntax* strct, TypeCheckContext ctx);

// Projector
_Noreturn void type_error_proj_invalid_type(PiType* type, Syntax* proj, TypeCheckContext ctx);

// ---------------------------------------------------------------------- 
//
//                              Unifictaion  
//
// ----------------------------------------------------------------------

// Enum
UnifyResult unify_error_variant_name_mismatch(Symbol lhs, Symbol rhs, UnifyContext ctx);

// Named
UnifyResult unify_error_name_has_args_match(PiType* lhs, PiType* rhs, Allocator* a);

#endif

