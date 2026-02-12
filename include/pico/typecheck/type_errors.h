#ifndef __PICO_TYPECHECK_TYPE_ERRORS_H
#define __PICO_TYPECHECK_TYPE_ERRORS_H

#include "pico/typecheck/typecheck.h"

// Variable
_Noreturn void type_error_unexpected_module(Syntax* syn, Module* module, TypeCheckContext ctx);
_Noreturn void type_error_unknown_var(Syntax* syn, TypeCheckContext ctx);

// Declarations
_Noreturn void type_error_invalid_declaration(Symbol type, Syntax* arg, TypeCheckContext ctx);

// Import
_Noreturn void type_error_invalid_import(ImportClause clause, Range range, TypeCheckContext ctx);

// Procedure
_Noreturn void type_error_expecting_instance_arg(size_t implicit_idx, Syntax* proc, TypeCheckContext ctx);

// Application and All Application
_Noreturn void type_error_invalid_application_target(PiType* type, Syntax* app, TypeCheckContext ctx);
_Noreturn void type_error_incorrect_num_args(PiType* type, Syntax *app, bool is_function, bool is_value_args, TypeCheckContext ctx);
_Noreturn void type_error_incorrect_num_args_all_noproc(PiType* type, Syntax* app, bool is_implicit_args, TypeCheckContext ctx);

#endif
