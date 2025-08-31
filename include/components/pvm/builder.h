#ifndef __COMPONENTS_PVM_BUILDER_H
#define __COMPONENTS_PVM_BUILDER_H

#include "components/pvm/pvm.h"

//struct Builder;
typedef struct Builder Builder;

Builder* mk_term_builder(Allocator* a);
Block insert_block(Term term, Builder* builder);

PVMFuncHandle insert_func(PVMType* type, Builder* builder);
PVMOperand const_val(PVMType type, void* val);

// Control flow
/* Not implemented yet
void build_ret(Builder* builder);
PVMOperand build_call(ComparisonOperator op, PVMFuncHandle handle, void* args);
PVMOperand build_branch(PVMOperand op, Block iftrue, Block iffalse);
PVMOperand build_jump(PVMOperand op, Block iftrue, Block iffalse);
*/

// Calculation & Conversion
PVMOperand build_arith(ArithmeticOperator op, PVMOperand arg1, PVMOperand arg2);
PVMOperand build_cmp(ComparisonOperator op, PVMOperand arg1, PVMOperand arg2);
//PVMOperand build_gep(PVMOperand op);
//PVMOperand build_bitcast(PVMType new_type, PVMOperand arg);

// Memory
/* Not implemented yet
PVMOperand build_alloca(PVMType type);
PVMOperand build_load(PVMType type, PVMOperand address);
PVMOperand build_store(PVMOperand val, PVMOperand address);
*/

// Variable size oeprations
/* Not implemented yet
PVMOperand build_vload(ComparisonOperator op, PVMOperand arg1, PVMOperand arg2);
PVMOperand build_vstore(ComparisonOperator op, PVMOperand arg1, PVMOperand arg2);
PVMOperand build_vsubref(ComparisonOperator op, PVMOperand arg1, PVMOperand arg2);
//Operand build_vgep(Operand op); ??
PVMOperand build_vret(PVMOperand op);
*/

#endif
