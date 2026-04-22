#include "platform/machine_info.h"
#if ARCH == AMD64
#include <inttypes.h>
#include <stdio.h>
#include <string.h>

#include "data/num.h"
#include "data/stringify.h"
#include "platform/signals.h"
#include "platform/machine_info.h"
#include "platform/memory/executable.h"
#include "platform/terminal/terminal.h"

#include "data/stream.h"
#include "components/pretty/string_printer.h"
#include "components/pretty/stream_printer.h"

#include "pico/data/error.h"
#include "pico/codegen/backend-direct/generate.h"
#include "pico/codegen/backend-direct/internal.h"
#include "pico/codegen/backend-direct/polymorphic.h"
#include "pico/codegen/backend-direct/foreign_adapters.h"
#include "pico/values/modular.h"
#include "pico/codegen/backend-direct/address_env.h"
#include "pico/eval/call.h"

#define STACK_ALIGN 8

/* Code Generation Assumptions:
 * • All expressions evaluate to integers or functions 
 * • All functions are just an address to call to (or jmp to for tail calls) 
 * • All terms are well-typed
 * 
 * On the calling convention used:
 * Calling a function:
 * • Push all arguments onto the stack in left-right order (i.e. rightmost arg
 *   is top of stack)
 */

// Internal functions
static void generate_entry(size_t out_sz, Target target, Allocator* a, ErrorPoint* point);
static void generate_exit(size_t out_sz, Target target, Allocator* a, ErrorPoint* point);

LinkData bd_generate_toplevel(TopLevel top, Environment* env, CodegenContext ctx) {
    Allocator* a = ctx.a;
    InternalLinkData links = (InternalLinkData) {
        .links = (LinkData) {
            .external_code_links = mk_sym_sarr_amap(8, a),
            .ec_links = mk_link_meta_array(32, a),
            .ed_links = mk_link_meta_array(8, a),
            .cc_links = mk_link_meta_array(32, a),
            .cd_links = mk_link_meta_array(8, a),
            .dd_links = mk_link_meta_array(8, a),
        },
        .gotolinks = mk_sym_sarr_assoc(8, a),
    };

    switch(top.type) {
    case TLDef: {
        // Note: types can only be recursive via 'Name', so we do not recursively bind if
        // generating a type.
        Symbol* recsym = get_type(top.def.value, ctx.tape)->sort != TKind ? 
            &top.def.bind : NULL;
        AddressEnv* a_env = mk_address_env(env, recsym, a);
        size_t out_sz = pi_size_of(*get_type(top.def.value, ctx.tape));

        InternalContext ictx = {
            .tape = ctx.tape,
            .target = ctx.target,
            .links = &links,
            .a = ctx.a,
            .point = ctx.point,
            .logger = ctx.logger,
        };

        generate_entry(out_sz, ctx.target, a, ctx.point);
        generate_i(top.def.value, a_env, ictx);
        generate_exit(out_sz, ctx.target, a, ctx.point);

        delete_address_env(a_env, a);
        break;
    }
    case TLDecl: {
        // Do nothing; open only affects the environment
        break;
    }
    case TLImport: {
        // Do nothing; open only affects the environment
        break;
    }
    case TLExpr: {
        AddressEnv* a_env = mk_address_env(env, NULL, a);

        InternalContext ictx = {
            .tape = ctx.tape,
            .target = ctx.target,
            .links = &links,
            .a = ctx.a,
            .point = ctx.point,
            .logger = ctx.logger,
        };

        size_t out_sz = pi_size_of(*get_type(top.expr, ctx.tape));
        generate_entry(out_sz, ctx.target, a, ctx.point);
        generate_i(top.expr, a_env, ictx);
        generate_exit(out_sz, ctx.target, a, ctx.point);

        delete_address_env(a_env, a);
        break;
    }
    }

    // The data chunk may be moved around during code-generation via 'realloc'
    // if it needs to grow. Thus, we backlink data here, to be safe.
    // TODO (INVESTIGATE BUG): check if also backlinking code makes sense?
    for (size_t i = 0; i < links.links.ed_links.len; i++) {
        LinkMetaData link = links.links.ed_links.data[i];
        void** address_ptr = (void**) ((void*)get_instructions(ctx.target.target).data + link.source_offset);
        set_unaligned_ptr(address_ptr, ctx.target.data_aux->data + link.dest_offset);
    }
    for (size_t i = 0; i < links.links.cd_links.len; i++) {
        LinkMetaData link = links.links.cd_links.data[i];
        void** address_ptr = (void**) ((void*)get_instructions(ctx.target.code_aux).data + link.source_offset);
        set_unaligned_ptr(address_ptr, ctx.target.data_aux->data + link.dest_offset);
    }
    for (size_t i = 0; i < links.links.dd_links.len; i++) {
        LinkMetaData link = links.links.dd_links.data[i];
        void** address_ptr = (void**) ((void*)ctx.target.data_aux->data + link.source_offset);
        set_unaligned_ptr(address_ptr, ctx.target.data_aux->data + link.dest_offset);
    }

    return links.links;
}

LinkData bd_generate_expr(SynRef syn, Environment* env, CodegenContext ctx) {
    Allocator* a = ctx.a;
    
    AddressEnv* a_env = mk_address_env(env, NULL, a);
    InternalLinkData links = (InternalLinkData) {
        .links = (LinkData) {
            .external_code_links = mk_sym_sarr_amap(8, a),
            .ec_links = mk_link_meta_array(8, a),
            .ed_links = mk_link_meta_array(8, a),
            .cc_links = mk_link_meta_array(8, a),
            .cd_links = mk_link_meta_array(8, a),
            .dd_links = mk_link_meta_array(8, a),
        },
        .gotolinks = mk_sym_sarr_assoc(8, a),
    };

    InternalContext ictx = {
        .target = ctx.target,
        .links = &links,
        .a = ctx.a,
        .point = ctx.point,
        .logger = ctx.logger,
    };

    size_t out_sz = pi_size_of(*get_type(syn, ctx.tape));
    generate_entry(out_sz, ctx.target, a, ctx.point);
    generate_i(syn, a_env, ictx);
    generate_exit(out_sz, ctx.target, a, ctx.point);

    delete_address_env(a_env, a);

    // The data chunk may be moved around during code-generation via 'realloc'
    // if it needs to grow. Thus, we backlink data here, to be safe.
    // TODO (INVESTIGATE BUG): check if also backlinking code makes sense?
    for (size_t i = 0; i < links.links.ed_links.len; i++) {
        LinkMetaData link = links.links.ed_links.data[i];
        void** address_ptr = (void**) ((void*)get_instructions(ctx.target.target).data + link.source_offset);
        *address_ptr= ctx.target.data_aux->data + link.dest_offset;
    }
    for (size_t i = 0; i < links.links.cd_links.len; i++) {
        LinkMetaData link = links.links.cd_links.data[i];
        void** address_ptr = (void**) ((void*)get_instructions(ctx.target.code_aux).data + link.source_offset);
        *address_ptr= ctx.target.data_aux->data + link.dest_offset;
    }
    for (size_t i = 0; i < links.links.dd_links.len; i++) {
        LinkMetaData link = links.links.dd_links.data[i];
        void** address_ptr = (void**) ((void*)ctx.target.data_aux->data + link.source_offset);
        *address_ptr= ctx.target.data_aux->data + link.dest_offset;
    }

    return links.links;
}

void bd_generate_type_expr(SynRef syn, TypeEnv* env, CodegenContext ctx) {
    Allocator* a = ctx.a;
    AddressEnv* a_env = mk_type_address_env(env, NULL, a);
    InternalLinkData links = (InternalLinkData) {
        .links = (LinkData) {
            .external_code_links = mk_sym_sarr_amap(8, a),
            .ec_links = mk_link_meta_array(8, a),
            .ed_links = mk_link_meta_array(8, a),
            .cc_links = mk_link_meta_array(8, a),
            .cd_links = mk_link_meta_array(8, a),
            .dd_links = mk_link_meta_array(8, a),
        },
        .gotolinks = mk_sym_sarr_assoc(8, a),
    };

    InternalContext ictx = {
        .tape = ctx.tape,
        .target = ctx.target,
        .links = &links,
        .a = ctx.a,
        .point = ctx.point,
        .logger = ctx.logger,
    };

    size_t out_sz = pi_size_of(*get_type(syn, ctx.tape));
    generate_entry(out_sz, ctx.target, a, ctx.point);
    generate_i(syn, a_env, ictx);
    generate_exit(out_sz, ctx.target, a, ctx.point);

    delete_address_env(a_env, a);
}

static void generate_entry(size_t out_sz, Target target, Allocator *a, ErrorPoint *point) {
    // Generate as if this is a native function called as 
    // void function(void* out_mem, void* dyamic_memor, void* temp_memory, void* shadow_stack_memory)

    Assembler* ass = target.target;
    build_unary_op(Push, reg(RBP, sz_64), ass, a, point);
    build_unary_op(Push, reg(RBX, sz_64), ass, a, point);
    build_unary_op(Push, reg(RDI, sz_64), ass, a, point);
    build_unary_op(Push, reg(RSI, sz_64), ass, a, point);
    build_unary_op(Push, reg(R15, sz_64), ass, a, point);
    build_unary_op(Push, reg(VSTACK_HEAD, sz_64), ass, a, point);
    build_unary_op(Push, reg(DVARS_REGISTER, sz_64), ass, a, point);
    build_unary_op(Push, reg(DMEM_REGISTER, sz_64), ass, a, point);

    // Push the argument onto the stack
#if ABI == SYSTEM_V_64
    if (out_sz != 0) {
        build_unary_op(Push, reg(RDI, sz_64), ass, a, point);
    }

    // Both VSTACK_HEAD and R15 have same value, as the variable stack has not moved
    build_binary_op(Mov, reg(R15, sz_64), reg(RSI, sz_64), ass, a, point);
    build_binary_op(Mov, reg(VSTACK_HEAD, sz_64), reg(RSI, sz_64), ass, a, point);
    build_binary_op(Mov, reg(DVARS_REGISTER, sz_64), reg(RDX, sz_64), ass, a, point);
    build_binary_op(Mov, reg(DMEM_REGISTER, sz_64), reg(RCX, sz_64), ass, a, point);
#elif ABI == WIN_64
    if (out_sz != 0) {
        build_unary_op(Push, reg(RCX, sz_64), ass, a, point);
    }

    build_binary_op(Mov, reg(R15, sz_64), reg(RDX, sz_64), ass, a, point);
    build_binary_op(Mov, reg(VSTACK_HEAD, sz_64), reg(RDX, sz_64), ass, a, point);
    build_binary_op(Mov, reg(DVARS_REGISTER, sz_64), reg(R8, sz_64), ass, a, point);
    build_binary_op(Mov, reg(DMEM_REGISTER, sz_64), reg(R9, sz_64), ass, a, point);
#endif

    // Code generated here may assume $RBP is the base of the stack, they are
    // not aware of all the values we just pushed;
    build_binary_op(Mov, reg(RBP, sz_64), reg(RSP, sz_64), ass, a, point);
}

static void generate_exit(size_t out_sz, Target target, Allocator *a, ErrorPoint *point) {
    Assembler* ass = target.target;

#if ABI == SYSTEM_V_64
    // memcpy (dest = rdi, src = rsi, size = rdx)
    // retval = rax
    if (out_sz != 0) {
        build_binary_op(Mov, reg(RDI, sz_64), rref8(RSP, pi_stack_align(out_sz), sz_64), ass, a, point);
        build_binary_op(Mov, reg(RSI, sz_64), reg(RSP, sz_64), ass, a, point);
        build_binary_op(Mov, reg(RDX, sz_64), imm64((int64_t)out_sz), ass, a, point);

        build_binary_op(Mov, reg(RCX, sz_64), imm64((int64_t)&memcpy), ass, a, point);
        build_unary_op(Call, reg(RCX, sz_64), ass, a, point);
        // pop value from stack
        build_binary_op(Add, reg(RSP, sz_64), imm32(pi_stack_align(out_sz + 8)), ass, a, point);
    }

#elif ABI == WIN_64
    // memcpy (dest = rcx, src = rdx, size = r8)
    // retval = rax
    if (out_sz != 0) {
        build_binary_op(Mov, reg(RCX, sz_64), rref8(RSP, pi_stack_align(out_sz), sz_64), ass,a, point);
        build_binary_op(Mov, reg(RDX, sz_64), reg(RSP, sz_64), ass, a, point);
        build_binary_op(Mov, reg(R8, sz_64), imm64((int64_t)out_sz), ass, a, point);
        build_binary_op(Sub, reg(RSP, sz_64), imm32(32), ass, a, point);

        build_binary_op(Mov, reg(RAX, sz_64), imm64((int64_t)&memcpy), ass, a, point);
        build_unary_op(Call, reg(RAX, sz_64), ass, a, point);
        // pop value from stack
        build_binary_op(Add, reg(RSP, sz_64), imm32(pi_stack_align(out_sz) + 32 + 8), ass, a, point);
    }
#else
#error "unknown ABI"
#endif

        // call function
        generate_c_call(mk_dynamic_var, ass, a, point);

        // Pop value off stack
        build_binary_op(Add, reg(RSP, sz_64), imm32(val_size), ass, a, point);
        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        
        data_stack_grow(env, ADDRESS_SIZE);
        data_stack_shrink(env, val_size);
        break;
    }
    case SDynamicUse: {
        // TODO: check that the dynamic use handles stack alignment correctly
        generate_i(syn.use, env, ictx);

        // We now have a dynamic variable: get its' value as ptr

#if ABI == SYSTEM_V_64 
        // arg1 = rdi
        build_unary_op(Pop, reg(RDI, sz_64), ass, a, point);
#elif ABI == WIN_64 
        // arg1 = rcx
        build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);
#else
#error "unknown ABI"
#endif

        // call function
        generate_c_call(get_dynamic_val, ass, a, point);

        if (is_variable_in(type, env)) {
            panic(mv_string("Not yet generating code for polymorphic dynamic use"));
        } else{
            // Now, allocate space on stack
            size_t val_size = pi_size_of(*type);
            build_binary_op(Sub, reg(RSP, sz_64), imm32(pi_stack_align(val_size)), ass, a, point);
            build_binary_op(Mov, reg(RCX, sz_64), reg(RAX, sz_64), ass, a, point);

            generate_monomorphic_copy(RSP, RCX, val_size, ass, a, point);
            data_stack_shrink(env, ADDRESS_SIZE);
            data_stack_grow(env, val_size);
        }
        break;
    }
    case SDynamicSet: {
        size_t val_size = pi_size_of(*get_type(syn.dynamic_set.new_val, ictx.tape));
        generate_i(syn.dynamic_set.dynamic, env, ictx);
        generate_i(syn.dynamic_set.new_val, env, ictx);

#if ABI == SYSTEM_V_64 
        // arg1 = rdi, arg2 = rsi
        build_binary_op(Mov, reg(RDI, sz_64), rref8(RSP, val_size, sz_64), ass, a, point);
        build_binary_op(Mov, reg(RSI, sz_64), reg(RSP, sz_64), ass, a, point);
        build_binary_op(Mov, reg(RDX, sz_64), imm32(val_size), ass, a, point);
#elif ABI == WIN_64 
        // arg1 = rcx, arg2 = rdx
        build_binary_op(Mov, reg(RCX, sz_64), rref8(RSP, val_size, sz_64), ass, a, point);
        build_binary_op(Mov, reg(RDX, sz_64), reg(RSP, sz_64), ass, a, point);
        build_binary_op(Mov, reg(R8, sz_64), imm32(val_size), ass, a, point);
#else
#error "unknown ABI"
#endif
        // call function
        generate_c_call(set_dynamic_val, ass, a, point);

        if (is_variable_in(type, env)) {
            panic(mv_string("Not yet generating code for polymorphic dynamic set"));
        } else {
            build_binary_op(Add, reg(RSP, sz_64), imm32(val_size + ADDRESS_SIZE), ass, a, point);
            data_stack_shrink(env, ADDRESS_SIZE + val_size);
        }
        break;
    }
    case SDynamicLet: {
        // TODO: check that the dynamic let handles stack alignment correctly

        // Step 1: evaluate all dynamic bindings & bind them
        for (size_t i = 0; i < syn.dyn_let_expr.bindings.len; i++) {
            DynBinding* dbind = syn.dyn_let_expr.bindings.data[i];
            size_t bind_size = pi_size_of(*get_type(dbind->expr, ictx.tape));
            generate_i(dbind->var, env, ictx);
            generate_i(dbind->expr, env, ictx);

            // Copy the value into the dynamic var
            // Currently, the stack looks like:
            // + dynamic-var-index
            //   new-value
            // R15 is the dynamic memory register, move it into RCX
            // Move the index into RAX
            build_binary_op(Mov, reg(RCX, sz_64), reg(DVARS_REGISTER, sz_64), ass, a, point);
            build_binary_op(Mov, reg(RAX, sz_64), rref8(RSP, bind_size, sz_64), ass, a, point);
            // RCX holds the array - we need to index it with index located in RAX
            build_binary_op(Mov, reg(RCX, sz_64), sib(RCX, RAX, 8, sz_64), ass, a, point);
            // Now we have a pointer to the value stored in RCX, swap it with
            // the value on the stack

            // TODO (check if replace with stack copy)
            generate_monomorphic_swap(RCX, RSP, bind_size, ass, a, point);
        }

        // Step 2: generate the body
        generate_i(syn.dyn_let_expr.body, env, ictx);
        size_t val_size = pi_stack_size_of(*get_type(syn.dyn_let_expr.body, ictx.tape));

        // Step 3: unwind the bindings
        //  • Store the (address of the) current dynamic value index to restore in RDX
        build_binary_op(Mov, reg(RDX, sz_64), reg(RSP, sz_64), ass, a, point);
        build_binary_op(Add, reg(RDX, sz_64), imm32(val_size), ass, a, point);

        size_t offset_size = 0;
        for (size_t i = 0; i < syn.dyn_let_expr.bindings.len; i++) {
            size_t idx = syn.dyn_let_expr.bindings.len - (i + 1);

            DynBinding* dbind = syn.dyn_let_expr.bindings.data[idx];
            size_t bind_size = pi_stack_size_of(*get_type(dbind->expr, ictx.tape));

            // Store ptr to dynamic memory (array) in RCX, and the index in RAX
            build_binary_op(Mov, reg(RCX, sz_64), reg(DVARS_REGISTER, sz_64), ass, a, point);
            build_binary_op(Mov, reg(RAX, sz_64), rref8(RDX, bind_size, sz_64), ass, a, point);
            // RCX holds the array - we need to index it with index located in RAX
            build_binary_op(Mov, reg(RCX, sz_64), sib(RCX, RAX, 8, sz_64), ass, a, point);

            // Store ptr to local value to restore in RDX 
            // TODO (check if replace with stack copy)
            generate_monomorphic_swap(RCX, RDX, bind_size, ass, a, point);
            build_binary_op(Add, reg(RDX, sz_64), imm32(bind_size + ADDRESS_SIZE), ass, a, point);

            offset_size += bind_size + ADDRESS_SIZE;
        }

        // Step 4: cleanup: pop bindings
        generate_stack_move(offset_size, 0, val_size, ass, a, point);
        build_binary_op(Add, reg(RSP, sz_64), imm32(offset_size), ass, a, point);
        data_stack_shrink(env, offset_size);
        break;
    }
    case SLet: {
        size_t bind_sz = ADDRESS_SIZE; 

        build_unary_op(Push, reg(VSTACK_HEAD, sz_64), ass, a, point);
        data_stack_grow(env, ADDRESS_SIZE);

        for (size_t i = 0; i < syn.let_expr.bindings.len; i++) {
            SymSynCell elt = syn.let_expr.bindings.data[i];
            generate_i(elt.val, env, ictx);

            if (is_variable_in(get_type(elt.val, ictx.tape), env)) {
                address_bind_relative_index(elt.key, 0, env);
                bind_sz += ADDRESS_SIZE;
            } else {
                size_t stack_sz = pi_stack_size_of(*get_type(elt.val, ictx.tape));
                address_bind_relative(elt.key, 0, env);
                bind_sz += stack_sz;
            }
        }
        generate_i(syn.let_expr.body, env, ictx);

        if (is_variable_in(type, env)) {
            generate_stack_size_of(RAX, type, env, ass, a, point);

            // Grab the value of VSTACK_HEAD we pushed at the beginning - offset bind_sz + stack head 
            build_binary_op(Mov, reg(VSTACK_HEAD, sz_64), rref8(RSP, bind_sz, sz_64), ass, a, point);
            build_binary_op(Sub, reg(VSTACK_HEAD, sz_64), reg(RAX, sz_64), ass, a, point);

            build_binary_op(Mov, reg(R9, sz_64), rref8(RSP, 0, sz_64), ass, a, point);

            generate_poly_move(reg(VSTACK_HEAD, sz_64), reg(R9, sz_64), reg(RAX, sz_64), ass, a, point);

            // Store current index in stack return position
            build_binary_op(Mov, rref8(RSP, bind_sz, sz_64), reg(VSTACK_HEAD, sz_64), ass, a, point);
            build_binary_op(Add, reg(RSP, sz_64), imm8(bind_sz), ass, a, point);
            data_stack_shrink(env, bind_sz);
        } else {
            size_t stack_sz = pi_stack_size_of(*type);
            // HERE IS !!BUG!!
            build_binary_op(Mov, reg(VSTACK_HEAD, sz_64), rref8(RSP, bind_sz + stack_sz - ADDRESS_SIZE, sz_64), ass, a, point);
            generate_stack_move(bind_sz, 0, stack_sz, ass, a, point);
            build_binary_op(Add, reg(RSP, sz_64), imm8(bind_sz), ass, a, point);
            data_stack_shrink(env, bind_sz);
        }
        break;
    }
    case SIf: {
        // Generate the condition
        generate_i(syn.if_expr.condition, env, ictx);

        // Pop the bool into R9; compare with 0
        build_unary_op(Pop, reg(R9, sz_64), ass, a, point);
        build_binary_op(Cmp, reg(R9, sz_8), imm8(0), ass, a, point);
        data_stack_shrink(env, ADDRESS_SIZE);

        // ---------- CONDITIONAL JUMP ----------
        // compare the value to 0
        // jump to false branch if equal to 0 -- the 32-bit immediate is a placeholder
        AsmResult out = build_unary_op(JE, imm32(0), ass, a, point);
        size_t start_pos = get_pos(ass);

        size_t jmp_loc = out.backlink;

        // ---------- TRUE BRANCH ----------
        // now, generate the code to run (if true)
        generate_i(syn.if_expr.true_branch, env, ictx);

        // Generate jump to end of false branch to be backlinked later
        out = build_unary_op(JMP, imm32(0), ass, a, point);

        // calc backlink offset
        size_t end_pos = get_pos(ass);
        if (end_pos - start_pos > INT32_MAX) {
            throw_error(point, mv_cstr_doc("Jump in conditional too large", a));
        } 

        // backlink
        set_i32_backlink(ass, jmp_loc, end_pos - start_pos);
        jmp_loc = out.backlink;
        start_pos = get_pos(ass);

        data_stack_shrink(env, is_variable_in(type, env) ? ADDRESS_SIZE : pi_stack_size_of(*type));

        // ---------- FALSE BRANCH ----------
        // Generate code for the false branch
        generate_i(syn.if_expr.false_branch, env, ictx);

        // calc backlink offset
        end_pos = get_pos(ass);
        if (end_pos - start_pos > INT32_MAX) {
            throw_error(point, mv_cstr_doc("Jump in conditional too large", a));
        } 
        set_i32_backlink(ass, jmp_loc, end_pos - start_pos);
        break;
    }
    case SCond: {
        // Generate the condition
        U64Array end_jumps = mk_u64_array(syn.cond.clauses.len, a);
        U64Array end_jump_locs = mk_u64_array(syn.cond.clauses.len, a);

        for (size_t i = 0; i < syn.cond.clauses.len; i++) {
            CondClause* clause = syn.cond.clauses.data[i];
            generate_i(clause->condition, env, ictx);

            // Pop the bool into R9; compare with 0
            build_unary_op(Pop, reg(R9, sz_64), ass, a, point);
            build_binary_op(Cmp, reg(R9, sz_8), imm8(0), ass, a, point);
            data_stack_shrink(env, ADDRESS_SIZE);

            // ---------- JUMP TO NEXT BRANCH ----------
            // compare the value to 0
            // jump to next condition if equal to 0 -- the 32-bit immediate is a placeholder
            AsmResult out = build_unary_op(JE, imm32(0), ass, a, point);
            size_t prev_false = get_pos(ass);
            size_t prev_false_loc = out.backlink;

            // ---------- BRANCH IF TRUE ----------
            // now, generate the code to run (if true)
            generate_i(clause->branch, env, ictx);
            data_stack_shrink(env, is_variable_in(type, env) ? ADDRESS_SIZE : pi_stack_size_of(*type));

            // ---------- JUMP TO END ----------
            // Generate code for the false branch
            out = build_unary_op(JMP, imm32(0), ass, a, point);
            push_u64(get_pos(ass), &end_jumps);
            push_u64(out.backlink, &end_jump_locs);

            // Jump to here if branch was not true (continue searching for condition)
            size_t curr_pos = get_pos(ass);
            set_i32_backlink(ass, prev_false_loc, curr_pos - prev_false);
        }

        // ---------- OTHERWISE BRANCH ----------
        generate_i(syn.cond.otherwise, env, ictx);

        // All branches should jump to this point
        size_t curr_pos = get_pos(ass);
        for (size_t i = 0; i < end_jumps.len; i++) {
            set_i32_backlink(ass, end_jump_locs.data[i], curr_pos - end_jumps.data[i]);
        }
        break;
    }
    case SLabels: {
        // Labels: The code-generation for labels is as follows: 
        // 1. Push the current variable stack head ($r14)  
        // 2. Add labels to environment
        // 3. Generate expression
        // 4. For each expression:
        //  5.1 Mark Beginning of label expression
        //  6.2 Generate label expressions
        // 7. Backlink all labels.
        
        /* build_unary_op(Push, reg(VSTACK_HEAD, sz_64), ass, a, point); */
        /* data_stack_grow(env, ADDRESS_SIZE); */

        SymbolArray labels = mk_symbol_array(syn.labels.terms.len, a);
        for (size_t i = 0; i < syn.labels.terms.len; i++)  {
            push_symbol(syn.labels.terms.data[i].key, &labels);
            sym_sarr_bind(syn.labels.terms.data[i].key, mk_size_array(4, a), &links->gotolinks);
        }

        address_start_labels(labels, env);

        generate_i(syn.labels.entry, env, ictx);

        SymSizeAssoc label_points = mk_sym_size_assoc(syn.labels.terms.len, a);
        SymSizeAssoc label_jumps = mk_sym_size_assoc(syn.labels.terms.len, a);

        size_t out_size = pi_size_of(*type);
        for (size_t i = 0; i < syn.labels.terms.len; i++) {
            // Clear the stack offset, either from expression or previous label
            data_stack_shrink(env,out_size);
            SymPtrACell cell = syn.labels.terms.data[i];

            // Mark Label
            size_t pos = get_pos(ass);
            sym_size_bind(cell.key, pos, &label_points);
            
            SynLabelBranch* branch = cell.val;

            // Bind Label Vars 
            size_t arg_total = 0;
            SymSizeAssoc arg_sizes = mk_sym_size_assoc(branch->args.len, a);
            for (size_t i = 0; i < branch->args.len; i++) {
                size_t arg_size = pi_size_of(*(PiType*)branch->args.data[i].val);
                sym_size_bind(branch->args.data[i].key, arg_size, &arg_sizes);
                arg_total += arg_size;
            }

            data_stack_grow(env,arg_total);
            address_bind_label_vars(arg_sizes, env);
            generate_i(branch->body, env, ictx);
            address_unbind_label_vars(env);

            LabelEntry lble = label_env_lookup(cell.key, env);
            if (lble.type == Err)
                throw_error(point, mv_cstr_doc("Label not found during codegen!!", a));
            data_stack_shrink(env, arg_total);

            // Copy the result down the stack
            generate_stack_move(arg_total, 0, out_size, ass, a, point);
            build_binary_op(Add, reg(RSP, sz_64), imm8(arg_total), ass, a, point);

            AsmResult out = build_unary_op(JMP, imm32(0), ass, a, point);
            sym_size_bind(cell.key, out.backlink, &label_jumps);
        }

        size_t label_end = get_pos(ass);

        for (size_t i = 0; i < label_points.len; i++)  {
            Symbol sym = label_points.data[i].key;
            size_t dest = label_points.data[i].val;

            // Step 1: make the end of the label jump to the end of the expression.
            {
                size_t backlink = label_jumps.data[i].val;
                size_t origin = backlink + 4; // the + 4 accounts for the 4-byte immediate
                size_t dest = label_end;

                int64_t amt = dest - origin;
                if (amt < INT32_MIN || amt > INT32_MAX) panic(mv_string("Label jump too large!"));
                
                set_i32_backlink(ass, backlink, amt);
            }


            // Step 2: fill out each jump to this label.
            SizeArray* arr = sym_sarr_alookup(sym, links->gotolinks);
            if (!arr) panic(mv_string("Can't find size array when backlinking label!"));

            for (size_t i = 0; i < arr->len; i++) {
                size_t backlink = arr->data[i];
                size_t origin = backlink + 4; // the + 4 accounts for the 4-byte immediate

                int64_t amt = dest - origin;
                if (amt < INT32_MIN || amt > INT32_MAX) panic(mv_string("Label jump too large!"));
                
                set_i32_backlink(ass, backlink, amt);
            }
        }

        sym_sarr_unbindn(syn.labels.terms.len, &links->gotolinks);
        address_end_labels(env);
        break;
    }
    case SGoTo: {
        // Generating code for a goto:
        // 1. Generate args
        size_t arg_total = 0;
        for (size_t i = 0; i < syn.go_to.args.len; i++) {
            SynRef expr = syn.go_to.args.data[i];
            arg_total += pi_stack_size_of(*get_type(expr, ictx.tape));
            generate_i(expr, env, ictx);
        }

        // 2. Backlink the label
        LabelEntry lble = label_env_lookup(syn.go_to.label, env);
        if (lble.type == Ok) {
            size_t delta = lble.stack_offset - arg_total;
            generate_stack_move(delta, 0, arg_total, ass, a, point);
            if (delta != 0) {
                // Adjust the stack so it has the same value that one would have
                // going into a labels expression.
                // TODO handle dynamic variable unbinding (if needed!)
                build_binary_op(Add, reg(RSP, sz_64), imm32(delta), ass, a, point);
            }

            // Stack should "pretend" it pushed a value of type type and
            // consumed all args. This is so that, e.g. if this go-to is inside
            // a seq or if, the other branches of the if or the rest of the seq
            // generates assuming the correct stack offset.
            data_stack_shrink(env, arg_total);
            data_stack_grow(env, pi_size_of(*type));

            AsmResult out = build_unary_op(JMP, imm32(0), ass, a, point);

            backlink_goto(syn.go_to.label, out.backlink, links, a);
        } else {
            throw_error(point, mv_cstr_doc("Label not found during codegen!!", a));
        }
        break;
    }
    case SWithReset: {
        // TODO: check that the with-reset handles stack alignment correctly
        // TODO: make sure that with-reset and reset-to handle DVARS_REGISTER and VSTACK_HEAD correctly
        //                (dynamic vars + index stack)
        // Overview of reset codegen
        // 1. Push as reset point onto the stack
        // 2. Push the ptr to reset point onto the stack - bind it

        // 3. Evaluate Expr
        // 4. Cleanup
        // 5. Insert jump - on expr completion & cleanup, goto end of expr

        // 6. Entry to handler - rework stack binding/understanding of stackstructure 
        // 7. Generate handler code
        // 8. Cleanup after handler and backlink jump from end of expr

        // Step 1.
        //-------
        AsmResult out = build_binary_op(LEA, reg(RAX, sz_64), rref32(RIP, 0, sz_64), ass, a, point); // TODO: backlink N
        size_t reset_backlink = out.backlink;
        size_t reset_instr_end = get_pos(ass);

        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        build_unary_op(Push, reg(RBP, sz_64), ass, a, point);

        // Step 2.
        //-------
        build_unary_op(Push, reg(RSP, sz_64), ass, a, point);
        data_stack_grow(env, 3*ADDRESS_SIZE);
        address_bind_relative(syn.with_reset.point_sym, 0, env);

        // Step 3.
        //--------
        generate_i(syn.with_reset.expr, env, ictx);

        // Step 4.
        //--------

        // Cleanup code for both 
        // At this point, the stack should have the appearance:
        // -- RIP for resetting to
        // -- Old RBP
        // -- Self-ptr + RSP
        // -- Out Value
        // So, the goal is do to a stack move from current RSP to current RSP - 3*ADDRESS

        size_t val_size = pi_size_of(*type);
        size_t in_val_size = pi_size_of(*syn.with_reset.in_arg_ty);
        generate_stack_move(3 * ADDRESS_SIZE, 0, val_size, ass, a, point);
        build_binary_op(Add, reg(RSP, sz_64), imm32(3 * ADDRESS_SIZE), ass, a, point);
        generate_stack_move(3 * ADDRESS_SIZE, 0, val_size, ass, a, point);

        data_stack_shrink(env, 3*ADDRESS_SIZE + val_size);
        address_pop(env);

        // Step 5.
        //--------
        out = build_unary_op(JMP, imm32(0), ass, a, point);
        size_t end_expr_link = out.backlink;
        size_t end_expr_pos = get_pos(ass);

        // Note: 
        const size_t reset_intro_pos = get_pos(ass);
        size_t dist = reset_intro_pos - reset_instr_end; 
        if (dist > INT32_MAX) {
            panic(mv_string("Reset expression jump distance too large."));
        }
        // Note: cast to uint8_t to get correct ptr arithmetic!
        *(int32_t*)((uint8_t*)get_instructions(ass).data + reset_backlink) = (int32_t)dist;

        // TODO: backlink to jump here
        // If we end up here, then we know the stacklooks like
        //     > Value (argument)  / ADRESS_SIZE
        // RSP > Continuation Mark / ADDRESS SIZE

        //----------------------------------------------------------------------
        // Handler Code begins here
        //----------------------------------------------------------------------
        // Step 6.
        //--------
        // Note: from reset-to, the stack now currently looks like
        // value 
        // Bind value + continuation mark
        data_stack_grow(env, ADDRESS_SIZE + in_val_size);
        address_bind_relative(syn.with_reset.cont_sym, 0, env);
        address_bind_relative(syn.with_reset.in_sym, 8, env);
        
        /* Symbol cont_sym; */
        /* Symbol in_sym; */
        generate_i(syn.with_reset.handler, env, ictx);
        data_stack_shrink(env, ADDRESS_SIZE + in_val_size);
        address_pop_n(2, env);

        // Step 7.
        //--------
        // At this point the stack looks like:
        //     > in-value
        //     > return-addr
        // RSP > out value
        // So do some cleanup
        // - generate_stack_move(ADDRESS_SIZE + val_size, 0, val_size, ass, ass, a, point);
        generate_stack_move(ADDRESS_SIZE + in_val_size, 0, val_size, ass, a, point);
        build_binary_op(Add, reg(RSP, sz_64), imm32(ADDRESS_SIZE + in_val_size), ass, a, point);


        // Step 8. 
        //--------
        size_t cleanup_start_pos = get_pos(ass);
        dist = cleanup_start_pos - end_expr_pos;
        if (dist > INT32_MAX) {
            throw_error(point, mv_cstr_doc("Internal error in codegen: jump distance exceeded INT32_MAX", a));
        }
        *(get_instructions(ass).data + end_expr_link) = (int32_t) dist;

        break;
    }
    case SResetTo: {
        generate_i(syn.reset_to.arg, env, ictx);
        generate_i(syn.reset_to.point, env, ictx);
        // there should how be a stack with the following:
        // > arg
        // > Ptr to reset-point

        // Step 1: take the ptr to reset-point
        build_unary_op(Pop, reg(R10, sz_64), ass, a, point);

        // Step 2: stash the current stack ptr in R9 (this will be used to copy the argument )
        build_binary_op(Mov, reg(R9, sz_64), reg(RSP, sz_64), ass, a, point);

        // Step 3: Deref return point ptr to get old RBP, RSP & RIP to jump up 
        build_binary_op(Mov, reg(RSP, sz_64), rref8(R10, -8, sz_64), ass, a, point);
        build_unary_op(Pop, reg(RBP, sz_64), ass, a, point);
        build_unary_op(Pop, reg(RDI, sz_64), ass, a, point); //RDI = jump dest!

        // Step 4: Copy the argument onto the (new) stack.
        PiType* arg_type = get_type(syn.reset_to.arg, ictx.tape);
        size_t asize = pi_size_of(*arg_type);
        build_binary_op(Sub, reg(RSP, sz_64), imm32(asize), ass, a, point);
        // TODO (check if replace with stack copy)
        generate_monomorphic_copy(RSP, R9, asize, ass, a, point);

        // Step 5: Long Jump (call) register
        build_unary_op(Call, reg(RDI, sz_64), ass, a, point);

        // Address environment bookkeeping: shrink the stack appropriately
        data_stack_shrink(env,
                          pi_size_of(*get_type(syn.reset_to.arg, ictx.tape))
                          + pi_size_of(*get_type(syn.reset_to.point, ictx.tape)));
        data_stack_grow(env, pi_size_of(*type));

        break;
    }
    case SSequence: {
        build_unary_op(Push, reg(VSTACK_HEAD, sz_64), ass, a, point);
        data_stack_grow(env, ADDRESS_SIZE);

        size_t bind_sz = 0; 
        for (size_t i = 0; i < syn.sequence.elements.len; i++) {
            SeqElt* elt = syn.sequence.elements.data[i];
            generate_i(elt->expr, env, ictx);

            if (elt->is_binding && i + 1 != syn.sequence.elements.len) {
                if (is_variable_in(get_type(elt->expr, ictx.tape), env)) {
                    address_bind_relative_index(elt->symbol, 0, env);
                    bind_sz += ADDRESS_SIZE;
                } else {
                    size_t stack_sz = pi_stack_size_of(*get_type(elt->expr, ictx.tape));
                    address_bind_relative(elt->symbol, 0, env);
                    bind_sz += stack_sz;
                }
            }

            // Remember: We do not pop off the stack if this is
            // a) a binding (hence this is an else) 
            // b) the last value in the sequence (as this value is preserved)
            else if (i + 1 != syn.sequence.elements.len) {
                if (is_variable_in(get_type(elt->expr, ictx.tape), env)) {
                    generate_stack_size_of(RAX, get_type(elt->expr, ictx.tape), env, ass, a, point);
                    build_binary_op(Add, reg(VSTACK_HEAD, sz_64), reg(RAX, sz_64), ass, a, point);
                    build_binary_op(Add, reg(RSP, sz_64), imm8(ADDRESS_SIZE), ass, a, point);
                    data_stack_shrink(env, ADDRESS_SIZE);
                } else {
                    size_t stack_sz = pi_stack_size_of(*get_type(elt->expr, ictx.tape));
                    build_binary_op(Add, reg(RSP, sz_64), imm8(stack_sz), ass, a, point);
                    data_stack_shrink(env, stack_sz);
                }
            }
            
        }
        if (is_variable_in(type, env)) {
            generate_stack_size_of(RAX, type, env, ass, a, point);

            // Grab the value of VSTACK_HEAD we pushed at the beginning - offset bind_sz + stack head 
            build_binary_op(Mov, reg(VSTACK_HEAD, sz_64), rref8(RSP, bind_sz + ADDRESS_SIZE, sz_64), ass, a, point);
            build_binary_op(Sub, reg(VSTACK_HEAD, sz_64), reg(RAX, sz_64), ass, a, point);

            build_binary_op(Mov, reg(R9, sz_64), rref8(RSP, 0, sz_64), ass, a, point);

            generate_poly_move(reg(VSTACK_HEAD, sz_64), reg(R9, sz_64), reg(RAX, sz_64), ass, a, point);

            // Store current index in stack return position
            generate_stack_move(bind_sz, 0, ADDRESS_SIZE, ass, a, point);
            build_binary_op(Add, reg(RSP, sz_64), imm8(bind_sz + ADDRESS_SIZE), ass, a, point);
            data_stack_shrink(env, bind_sz + ADDRESS_SIZE);
        } else {
            size_t stack_sz = pi_stack_size_of(*type);
            build_binary_op(Mov, reg(VSTACK_HEAD, sz_64), rref32(RSP, bind_sz + stack_sz, sz_64), ass, a, point);
            generate_stack_move(bind_sz + ADDRESS_SIZE, 0, stack_sz, ass, a, point);
            build_binary_op(Add, reg(RSP, sz_64), imm32(bind_sz + ADDRESS_SIZE), ass, a, point);
            data_stack_shrink(env, bind_sz + ADDRESS_SIZE);
        }
        break;
    }
    case SIs:
        generate_i(syn.is.val, env, ictx);
        break;
    case SInTo:
        generate_i(syn.into.val, env, ictx);
        break;
    case SOutOf:
        generate_i(syn.out_of.val, env, ictx);
        break;
    case SName:
        generate_i(syn.name.body, env, ictx);
        break;
    case SUnName:
        generate_i(syn.unname, env, ictx);
        break;
    case SWiden:
        if (is_variable_in(type, env)) {
            // TODO: throw error with range/report in typecheck phase?
            panic(mv_string("Can't generate code for polymorphic-widen"));
        }

        // TODO (BUG): appropriately widen (sign-extend/double broaden)
        generate_i(syn.widen.val, env, ictx);
        if (get_type(syn.widen.val, ictx.tape)->prim < UInt_8) {
            // is signed, 
            panic(mv_string("can't widen signed ints yet!"));
        } else if (get_type(syn.widen.val, ictx.tape)->prim < Float_32) {
            // Is unsigned, so the movzx instruction will do:
            switch (get_type(syn.widen.val, ictx.tape)->prim) {
            case UInt_8:
                // Use MovZx (only available for bytes + words)
                build_binary_op(Mov, reg(RAX, sz_64), rref8(RSP, 0, sz_64), ass, a, point);
                build_binary_op(MovZx, reg(RCX, sz_64), reg(RAX, sz_8), ass, a, point);
                build_binary_op(Mov, rref8(RSP, 0, sz_64), reg(RCX, sz_64), ass, a, point);
                break;
            case UInt_16:
                // Use MovZx (only available for bytes + words)
                build_binary_op(Mov, reg(RAX, sz_64), rref8(RSP, 0, sz_64), ass, a, point);
                build_binary_op(MovZx, reg(RCX, sz_64), reg(RAX, sz_16), ass, a, point);
                build_binary_op(Mov, rref8(RSP, 0, sz_64), reg(RCX, sz_64), ass, a, point);
                break;
            case UInt_32:
                // In the case of 32-bit registers, moving a 32-bit register
                // into a 32-bit register does the zero-elimination 
                build_binary_op(Mov, reg(RAX, sz_64), rref8(RSP, 0, sz_64), ass, a, point);
                build_binary_op(Mov, reg(RCX, sz_32), reg(RAX, sz_32), ass, a, point);
                build_binary_op(Mov, rref8(RSP, 0, sz_64), reg(RCX, sz_64), ass, a, point);
                break;
            case UInt_64:
                // Widening from U64 to U64 = no-op
                break;
            default:
                panic(mv_string("impossible code path"));
            }

        } else {
            panic(mv_string("can't widen this yet!"));
        }

        break;
    case SNarrow:
        if (is_variable_in(type, env)) {
            // TODO: throw error with range/report in typecheck phase?
            panic(mv_string("Can't generate code for polymorphic-narrow"));
        }
        generate_i(syn.narrow.val, env, ictx);
        // TODO: bounds check?
        // TODO: if signed -ve => 0??

        // TODO (BUG): generate appropriate assembly to convert between floating
        // point values
        if (type->sort == TPrim && type->prim == Float_32 &&
            get_type(syn.narrow.val, ictx.tape)->sort == TPrim &&
            get_type(syn.narrow.val, ictx.tape)->prim == Float_64) {
            build_binary_op(CvtSD2SS, reg(XMM0, sz_32), rref8(RSP, 0, sz_64), ass, a, point);
            build_binary_op(MovSS, rref8(RSP, 0, sz_32), reg(XMM0, sz_32), ass, a, point);
        }
        // point values!
        break;
    case SSizeOf: {
        generate_size_of(RAX, get_syntax(syn.size, ictx.tape).type_val, env, ass, a, point);
        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        data_stack_grow(env, ADDRESS_SIZE);
        break;
    }
    case SAlignOf: {
        generate_align_of(RAX, get_syntax(syn.size, ictx.tape).type_val, env, ass, a, point);
        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        data_stack_grow(env, ADDRESS_SIZE);
        break;
    }
    case SOffsetOf: {
        PiType* struct_type = strip_type(get_syntax(syn.offset_of.body, ictx.tape).type_val);
        generate_offset_of(RAX, syn.offset_of.field, struct_type->structure.fields, env, ass, a, point);
        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        data_stack_grow(env, ADDRESS_SIZE);
        break;
    }
    case SDynAlloc: {
        // TODO: check size/provide overflow error?
        generate_i(syn.size, env, ictx);
        build_unary_op(Pop, reg(RAX, sz_64), ass, a, point);
        build_unary_op(Push, reg(DMEM_REGISTER, sz_64), ass, a, point);
        build_binary_op(Add, reg(DMEM_REGISTER, sz_64), reg(RAX, sz_64), ass, a, point);
        break;
    }
    case SCheckedType: {
        build_binary_op(Mov, reg(R9, sz_64), imm64((uint64_t)syn.type_val), ass, a, point);
        build_unary_op(Push, reg(R9, sz_64), ass, a, point);
        data_stack_grow(env, ADDRESS_SIZE);
        break;
    }
    case SProcType: {
        // Generate proc type: start by malloc'ing size for args
        generate_tmp_malloc(reg(RAX, sz_64), imm32(syn.proc_type.args.len * ADDRESS_SIZE), ass, a, point);
        build_binary_op(Mov, reg(RCX, sz_64), imm32(0), ass, a, point);

        for (size_t i = 0; i < syn.proc_type.args.len; i++) {
            SynRef arg = syn.proc_type.args.data[i];

            // Second, generate & move the type (note: stash & pop RCX)
            build_unary_op(Push, reg(RCX, sz_64), ass, a, point);
            build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
            data_stack_grow(env, 2*ADDRESS_SIZE);
            generate_i(arg, env, ictx);

            data_stack_shrink(env, 3*ADDRESS_SIZE);
            build_unary_op(Pop, reg(R9, sz_64), ass, a, point);
            build_unary_op(Pop, reg(RAX, sz_64), ass, a, point);
            build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);

            build_binary_op(Mov, sib(RAX, RCX, 8, sz_64), reg(R9, sz_64), ass, a, point);

            // Now, incremenet index by 1
            build_binary_op(Add, reg(RCX, sz_64), imm32(1), ass, a, point);
        }
        // Stash RAX
        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        data_stack_grow(env, ADDRESS_SIZE);
        generate_i(syn.proc_type.return_type, env, ictx);
        data_stack_shrink(env, 2*ADDRESS_SIZE);
        build_unary_op(Pop, reg(R9, sz_64), ass, a, point);
        build_unary_op(Pop, reg(RAX, sz_64), ass, a, point);

        // Finally, generate function call to make type
        gen_mk_proc_ty(reg(RAX, sz_64), imm32(syn.proc_type.args.len), reg(RAX, sz_64), reg(R9, sz_64), ass, a, point);
        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        data_stack_grow(env, ADDRESS_SIZE);

        break;
    }
    case SStructType: {
        // Generate struct type: for each element of the struct type
        // First, malloc enough data for the array:
        generate_tmp_malloc(reg(RAX, sz_64), imm32(syn.struct_type.fields.len * 3 * ADDRESS_SIZE), ass, a, point);
        build_binary_op(Mov, reg(RCX, sz_64), imm32(0), ass, a, point);

        for (size_t i = 0; i < syn.struct_type.fields.len; i++) {
            SymSynCell field = syn.struct_type.fields.data[i];
            // First, move the field name
            build_binary_op(Mov, sib8(RAX, RCX, 8, 0, sz_64), imm32(field.key.name), ass, a, point);
            build_binary_op(Mov, sib8(RAX, RCX, 8, 8, sz_64), imm32(field.key.did), ass, a, point);

            // Second, generate & move the type (note: stash & pop RCX)
            build_unary_op(Push, reg(RCX, sz_64), ass, a, point);
            build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
            data_stack_grow(env, 2*ADDRESS_SIZE);
            generate_i(field.val, env, ictx);

            data_stack_shrink(env, 3*ADDRESS_SIZE);
            build_unary_op(Pop, reg(R9, sz_64), ass, a, point);
            build_unary_op(Pop, reg(RAX, sz_64), ass, a, point);
            build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);

            build_binary_op(Mov, sib8(RAX, RCX, 8, 16, sz_64), reg(R9, sz_64), ass, a, point);

            // Now, incremenet index by 3 (to account for struct size!)
            build_binary_op(Add, reg(RCX, sz_64), imm32(3), ass, a, point);
        }

        // Finally, generate function call to make type
        gen_mk_struct_ty(reg(RAX, sz_64), imm32(syn.struct_type.fields.len), reg(RAX, sz_64), syn.struct_type.packed, ass, a, point);
        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        data_stack_grow(env, ADDRESS_SIZE);
        break;
    }
    case SEnumType: {
        // Generate enum type: malloc array for enum
        // First, malloc enough data for the array:
        generate_tmp_malloc(reg(RAX, sz_64), imm32(syn.enum_type.variants.len * 3 * ADDRESS_SIZE), ass, a, point);
        build_binary_op(Mov, reg(RCX, sz_64), imm32(0), ass, a, point);

        for (size_t i = 0; i < syn.enum_type.variants.len; i++) {
            SymPtrCell field = syn.enum_type.variants.data[i];
            // First, move the field name
            build_binary_op(Mov, sib8(RAX, RCX, 8, 0, sz_64), imm32(field.key.name), ass, a, point);
            build_binary_op(Mov, sib8(RAX, RCX, 8, 8, sz_64), imm32(field.key.did), ass, a, point);

            // Second, generate & variant (note: stash & pop RCX)
            build_unary_op(Push, reg(RCX, sz_64), ass, a, point);
            build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
            data_stack_grow(env, 2*ADDRESS_SIZE);

            SynArray variant = *(SynArray*)field.val;
            generate_tmp_malloc(reg(RAX, sz_64), imm32(variant.len * ADDRESS_SIZE), ass, a, point);
            build_binary_op(Mov, reg(RCX, sz_64), imm32(0), ass, a, point);

            for (size_t i = 0; i < variant.len; i++) {

                build_unary_op(Push, reg(RCX, sz_64), ass, a, point);
                build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
                data_stack_grow(env, 2*ADDRESS_SIZE);

                generate_i(variant.data[i], env, ictx);

                data_stack_shrink(env, 3*ADDRESS_SIZE);
                build_unary_op(Pop, reg(R9, sz_64), ass, a, point);
                build_unary_op(Pop, reg(RAX, sz_64), ass, a, point);
                build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);

                build_binary_op(Mov, sib(RAX, RCX, 8, sz_64), reg(R9, sz_64), ass, a, point);

                // Now, incremenet index
                build_binary_op(Add, reg(RCX, sz_64), imm32(1), ass, a, point);
            }

            // The variant was just stored in RAX, move it to R9
            build_binary_op(Mov, reg(R9, sz_64), reg(RAX, sz_64), ass, a, point);

            build_unary_op(Pop, reg(RAX, sz_64), ass, a, point);
            build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);
            data_stack_shrink(env, 2*ADDRESS_SIZE);

            build_binary_op(Mov, sib8(RAX, RCX, 8, 16, sz_64), reg(R9, sz_64), ass, a, point);

            // Now, incremenet index by 3 (to account for ptr + symbol)
            build_binary_op(Add, reg(RCX, sz_64), imm32(3), ass, a, point);
        }

        // Finally, generate function call to make type
        gen_mk_enum_ty(reg(RAX, sz_64), syn.enum_type, syn.enum_type.tag_size, reg(RAX, sz_64), ass, a, point);
        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        data_stack_grow(env, ADDRESS_SIZE);
        break;
    }
    case SResetType:
        generate_i(syn.reset_type.in, env, ictx);
        generate_i(syn.reset_type.out, env, ictx);
        gen_mk_reset_ty(ass, a, point);
        data_stack_shrink(env, ADDRESS_SIZE);
        break;
    case SDynamicType:
        generate_i(syn.dynamic_type, env, ictx);
        gen_mk_dynamic_ty(ass, a, point);
        break;
    case SAllType:
        // Forall type structure: (array symbol) body
        for (size_t i = 0; i < syn.bind_type.bindings.len; i++) {
            address_bind_type(syn.bind_type.bindings.data[i], env);
        }
        generate_i(syn.bind_type.body, env, ictx);
        gen_mk_forall_ty(syn.bind_type.bindings, ass, a, point);
        address_pop_n(syn.bind_type.bindings.len, env);
        break;
    case SSealedType: {
        for (size_t i = 0; i < syn.sealed_type.vars.len; i++) {
            address_bind_type(syn.bind_type.bindings.data[i], env);
        }

        generate_tmp_malloc(reg(RAX, sz_64), imm32(syn.sealed_type.implicits.len * ADDRESS_SIZE), ass, a, point);
        build_binary_op(Mov, reg(RCX, sz_64), imm32(0), ass, a, point);

        for (size_t i = 0; i < syn.sealed_type.implicits.len; i++) {
            SynRef arg = syn.sealed_type.implicits.data[i];

            // Second, generate & move the type (note: stash & pop RCX)
            build_unary_op(Push, reg(RCX, sz_64), ass, a, point);
            build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
            data_stack_grow(env, 2*ADDRESS_SIZE);
            generate_i(arg, env, ictx);

            data_stack_shrink(env, 3*ADDRESS_SIZE);
            build_unary_op(Pop, reg(R9, sz_64), ass, a, point);
            build_unary_op(Pop, reg(RAX, sz_64), ass, a, point);
            build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);

            build_binary_op(Mov, sib(RAX, RCX, 8, sz_64), reg(R9, sz_64), ass, a, point);

            // Now, incremenet index by 1
            build_binary_op(Add, reg(RCX, sz_64), imm32(1), ass, a, point);
        }

        // Stash RAX
        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        data_stack_grow(env, ADDRESS_SIZE);
        generate_i(syn.sealed_type.body, env, ictx);
        data_stack_shrink(env, 2*ADDRESS_SIZE);

        gen_mk_sealed_ty(syn.sealed_type.vars, imm32(syn.sealed_type.implicits.len), ass, a, point);
        address_pop_n(syn.sealed_type.vars.len, env);
        data_stack_grow(env, ADDRESS_SIZE);
        break;
    }
    case STypeFamily:
        // Family type structure: (array symbol) body
        for (size_t i = 0; i < syn.bind_type.bindings.len; i++) {
            address_bind_type(syn.bind_type.bindings.data[i], env);
        }
        generate_i(syn.bind_type.body, env, ictx);
        gen_mk_fam_ty(syn.bind_type.bindings, ass, a, point);
        address_pop_n(syn.bind_type.bindings.len, env);
        break;
    case SLiftCType: {
        generate_i(syn.c_type, env, ictx);
        gen_mk_c_ty(ass, a, point);
        // Now, the type lies atop the stack, and we must pop the ctype out from
        // under it
        size_t cts = pi_stack_size_of(*get_type(syn.c_type, ictx.tape));

        // TODO (IMPROVEMENT) this assumes address-size == 64 bits 
        build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);
        build_binary_op(Add, reg(RSP, sz_64), imm32(cts), ass, a, point);
        build_unary_op(Push, reg(RCX, sz_64), ass, a, point);

        data_stack_shrink(env, cts - ADDRESS_SIZE);
        break;
    }
    case SNamedType: {
        build_binary_op(Mov, reg(RAX, sz_64), imm64(syn.named_type.name.did), ass, a, point);
        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        build_binary_op(Mov, reg(RAX, sz_64), imm64(syn.named_type.name.name), ass, a, point);
        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        data_stack_grow(env, sizeof(Symbol));

        address_bind_type(syn.named_type.name, env);
        generate_i(syn.named_type.body, env, ictx);
        gen_mk_named_ty(ass, a, point);
        data_stack_shrink(env, sizeof(Symbol));
        address_pop(env);
        break;
    }
    case SDistinctType:
        build_binary_op(Mov, reg(RAX, sz_64), imm64(syn.distinct_type.name.did), ass, a, point);
        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        build_binary_op(Mov, reg(RAX, sz_64), imm64(syn.distinct_type.name.name), ass, a, point);
        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        data_stack_grow(env, sizeof(Symbol));

        address_bind_type(syn.distinct_type.name, env);
        generate_i(syn.distinct_type.body, env, ictx);
        gen_mk_distinct_ty(ass, a, point);
        data_stack_shrink(env, sizeof(Symbol));
        address_pop(env);
        break;
    case SOpaqueType:
        build_binary_op(Mov, reg(RAX, sz_64), imm64(syn.opaque_type.name.did), ass, a, point);
        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        build_binary_op(Mov, reg(RAX, sz_64), imm64(syn.opaque_type.name.name), ass, a, point);
        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        data_stack_grow(env, sizeof(Symbol));

        address_bind_type(syn.opaque_type.name, env);
        generate_i(syn.opaque_type.body, env, ictx);
        gen_mk_opaque_ty(ass, a, point);
        data_stack_shrink(env, sizeof(Symbol));
        address_pop(env);
        break;
    case STraitType: {
        // Generate trait type: first bind relevant variables
        address_bind_type(syn.trait.name, env);
        for (size_t i = 0; i < syn.bind_type.bindings.len; i++) {
            address_bind_type(syn.bind_type.bindings.data[i], env);
        }

        // First, malloc enough data for the array:
        generate_tmp_malloc(reg(RAX, sz_64), imm32(syn.trait.fields.len * (sizeof(Symbol) + ADDRESS_SIZE)), ass, a, point);
        build_binary_op(Mov, reg(RCX, sz_64), imm32(0), ass, a, point);

        for (size_t i = 0; i < syn.trait.fields.len; i++) {
            SymSynCell field = syn.trait.fields.data[i];
            // First, move the field name
            build_binary_op(Mov, sib8(RAX, RCX, 8, 0, sz_64), imm32(field.key.name), ass, a, point);
            build_binary_op(Mov, sib8(RAX, RCX, 8, 8, sz_64), imm32(field.key.did), ass, a, point);

            // Second, generate & move the type (note: stash & pop RCX)
            build_unary_op(Push, reg(RCX, sz_64), ass, a, point);
            build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
            data_stack_grow(env, 2*ADDRESS_SIZE);
            generate_i(field.val, env, ictx);

            data_stack_shrink(env, 3*ADDRESS_SIZE);
            build_unary_op(Pop, reg(R9, sz_64), ass, a, point);
            build_unary_op(Pop, reg(RAX, sz_64), ass, a, point);
            build_unary_op(Pop, reg(RCX, sz_64), ass, a, point);

            build_binary_op(Mov, sib8(RAX, RCX, 8, 16, sz_64), reg(R9, sz_64), ass, a, point);

            // Now, incremenet index by 2 (to account for trait size!)
            build_binary_op(Add, reg(RCX, sz_64), imm32(3), ass, a, point);
        }

        // Finally, generate function call to make type
        gen_mk_trait_ty(syn.trait.name, syn.trait.vars, reg(RAX, sz_64), imm32(syn.trait.fields.len), reg(RAX, sz_64), ass, a, point);
        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        data_stack_grow(env, ADDRESS_SIZE);

        address_pop_n(syn.trait.vars.len + 1, env);
        break;
    }

    case SReinterpret:
        // reinterpret has no associated codegen
        generate_i(syn.reinterpret.body, env, ictx);
        break;
    case SConvert: {
        if (syn.convert.from_native) {
            void* cfn = *(void**)const_fold(syn.convert.body, env, ictx);
            void* proc_address = get_instructions(target.code_aux).data;
            proc_address += get_instructions(target.code_aux).len;

            // Generate procedure value (push the address onto the stack)
            AsmResult out = build_binary_op(Mov, reg(RAX, sz_64), imm64((uint64_t)proc_address), ass, a, point);
            backlink_code(target, out.backlink, links);
            build_unary_op(Push, reg(RAX, sz_64), ass, a, point);

            // Now, change the target and the assembler, such that code is now
            // generated in the 'code segment'. Then, generate the function body
            ass = target.code_aux;
            target.target = target.code_aux;
            ictx.target = target;

            bd_convert_c_fn(cfn, &get_type(syn.convert.body, ictx.tape)->c_type, type, ass, a, point);
        } else {
            panic(mv_string("Cannot yet convert pico value to c value."));
        }
        break;
    }
    case STypeOf: {
        build_binary_op(Mov, reg(R9, sz_64), imm64((uint64_t)get_type(syn.type_of, ictx.tape)), ass, a, point);
        build_unary_op(Push, reg(R9, sz_64), ass, a, point);
        data_stack_grow(env, pi_size_of(*type));
        break;
    }
    case SDescribe: {
        Environment* base = get_addr_base(env);
        if (syn.to_describe.len == 0) panic(mv_string("unexepcted empty path provided to describe"));

        EnvEntry entry = env_lookup(syn.to_describe.data[0], base);
        for (size_t i = 1; i < syn.to_describe.len; i++) {
            if (entry.is_module) {
                ModuleEntry* mentry = get_def(syn.to_describe.data[i], entry.value);
                if (mentry) {
                    entry.is_module = mentry->is_module;
                    entry.value = mentry->value;
                    entry.type = &mentry->type;
                } else {
                    throw_error(point, mv_cstr_doc("Unknown symbol in path to describe.", a));
                }
            } else {
                throw_error(point, mv_cstr_doc("Unknown symbol in path to describe.", a));
            }
        }
        String immediate;

        if (entry.success == Ok) {
          if (entry.is_module) {
              SymbolArray syms = get_defined_symbols(entry.value, a);
              PtrArray lines = mk_ptr_array(syms.len + 8, a);
              {
                  PtrArray moduledesc = mk_ptr_array(2, a);
                  String m_name = symbol_to_string(module_name(entry.value), a);
                  push_ptr(mk_str_doc(mv_string("Module: "), a), &moduledesc);
                  push_ptr(mk_str_doc(m_name, a), &moduledesc);
                  push_ptr(mv_sep_doc(moduledesc, a), &lines);
              }
              push_ptr(mk_str_doc(mv_string("────────────────────────────────────────────"), a), &lines);

              for (size_t i = 0; i < syms.len; i++) {
                  Symbol symbol = syms.data[i];
                  ModuleEntry* mentry = get_def(symbol, entry.value);
                  if (mentry) {
                      PtrArray desc = mk_ptr_array(3, a);
                      if (mentry->is_module) {
                          push_ptr(mk_str_doc(mv_string("Module"), a), &desc);
                          push_ptr(mk_str_doc(symbol_to_string(symbol, a), a), &desc);
                      } else {
                          push_ptr(mk_str_doc(symbol_to_string(symbol, a), a), &desc);
                          push_ptr(mk_str_doc(mv_string(":"), a), &desc);
                          push_ptr(mv_nest_doc(2, pretty_type(&mentry->type, default_ptp, a), a), &desc);
                      }
                      push_ptr(mv_hsep_doc(desc, a), &lines);
                  } else {
                      // TODO: report error - exported symbol not in module?
                  }
              }

              push_ptr(mk_str_doc(mv_string("────────────────────────────────────────────\n"), a), &lines);

              Document* doc = mv_grouped_vsep_doc(lines, a);
              immediate = doc_to_str(doc, 80, a);

          } else {
              PtrArray lines = mk_ptr_array(8, a);
              {
                  PtrArray header = mk_ptr_array(syn.to_describe.len + 1, a);
                  push_ptr(mk_str_doc(mv_string("Path: "), a), &header);
                  for (size_t i = 0; i < syn.to_describe.len; i++) {
                      push_ptr(mk_str_doc(symbol_to_string(syn.to_describe.data[i], a), a), &header);
                  }
                  push_ptr(mv_sep_doc(header, a), &lines);
              }
              push_ptr(mk_str_doc(mv_string("────────────────────────────────────────────"), a), &lines);
              {
                  PtrArray moduledesc = mk_ptr_array(2, a);
                  String m_name = symbol_to_string(module_name(entry.source), a);
                  push_ptr(mk_str_doc(mv_string("Source Module: "), a), &moduledesc);
                  push_ptr(mk_str_doc(m_name, a), &moduledesc);
                  push_ptr(mv_sep_doc(moduledesc, a), &lines);
              }
              {
                  PtrArray typedesc = mk_ptr_array(2, a);
                  push_ptr(mk_str_doc(mv_string("Type: "), a), &typedesc);
                  push_ptr(pretty_type(entry.type, default_ptp, a), &typedesc);
                  push_ptr(mv_sep_doc(typedesc, a), &lines);
              }
              {
                  PtrArray valdesc = mk_ptr_array(2, a);
                  push_ptr(mk_str_doc(mv_string("Value: "), a), &valdesc);
                  push_ptr(pretty_pi_value(entry.value, entry.type, default_pvp, a), &valdesc);
                  push_ptr(mv_sep_doc(valdesc, a), &lines);
              }

              // end line
              push_ptr(mk_str_doc(mv_string("────────────────────────────────────────────\n"), a), &lines);

              Document* doc = mv_grouped_vsep_doc(lines, a);
              immediate = doc_to_str(doc, 120, a);
          }
        } else {
            immediate = mv_string("Local variable.");
        }


        if (immediate.memsize > UINT32_MAX) 
            throw_error(point, mv_cstr_doc("Codegen: String literal length must fit into less than 32 bits", a));

        // Push the string onto the stack
        // '0' is used as a placeholder, as the correct address will be
        // substituted when backlinking!
        AsmResult out = build_binary_op(Mov, reg(RAX, sz_64), imm64(0), ass, a, point);

        // Backlink the data & copy the bytes into the data-segment.
        backlink_data(target, out.backlink, links);
        add_u8_chunk(immediate.bytes, immediate.memsize, target.data_aux);

        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        build_unary_op(Push, imm32(immediate.memsize), ass, a, point);

        data_stack_grow(env, pi_stack_size_of(*type));
        break;
    }
    case SQuote: {
        // Setup: push the memory address (in data) of the Syntax value into stack.
        AsmResult out = build_binary_op(Mov, reg(RAX, sz_64), imm64(0), ass, a, point);
        backlink_data(target, out.backlink, links);

        // Now copy the entire concrete syntax tree into the data-segment,
        // setting allocators to null
        add_rawtree(syn.quoted, target, links);

        build_binary_op(Sub, reg(RSP, sz_64), imm8(sizeof(RawTree)), ass, a, point);
        for (size_t i = 0; i < sizeof(RawTree); i += sizeof(uint64_t)) {
            build_binary_op(Mov, reg(RCX, sz_64), rref8(RAX, i, sz_64), ass, a, point);
            build_binary_op(Mov, rref8(RSP, i, sz_64), reg(RCX, sz_64), ass, a, point);
        }
        data_stack_grow(env, pi_stack_size_of(*type));
        break;
    }
    case SCapture: {
        // Setup: push the memory address (in data) of the Syntax value into stack.
        AsmResult out = build_binary_op(Mov, reg(RAX, sz_64), imm64(0), ass, a, point);
        backlink_data(target, out.backlink, links);

        RawTree raw = (RawTree) {
          .type = RawAtom,
          .atom = (Atom) {
              .type = ACapture,
              .capture = (Capture) {
                  .type = syn.capture.type,
                  .value = syn.capture.value,
              }
          },
        };
        // Now copy the entire concrete syntax tree into the data-segment,
        // setting allocators to null
        add_rawtree(raw, target, links);

        build_binary_op(Sub, reg(RSP, sz_64), imm8(sizeof(RawTree)), ass, a, point);
        for (size_t i = 0; i < sizeof(RawTree); i += sizeof(uint64_t)) {
            build_binary_op(Mov, reg(RCX, sz_64), rref8(RAX, i, sz_64), ass, a, point);
            build_binary_op(Mov, rref8(RSP, i, sz_64), reg(RCX, sz_64), ass, a, point);
        }
        data_stack_grow(env, pi_stack_size_of(*type));
        break;
    }
    case SDevAnnotation: {
        if (syn.dev.flags & DBGenerate)
            debug_break();

        size_t current_loc = get_pos(ass);
        generate_i(syn.dev.inner, env, ictx);

        if (syn.dev.flags & DPGenerate) {

            U8Array instrs = get_instructions(ass);
            PtrArray nodes = mk_ptr_array(4 + (instrs.len - current_loc), a);

            write_string(mv_string("Developer Debug Aid: Generated section (bytes "), get_stdout_stream());
            write_string(string_u64(current_loc, a), get_stdout_stream());
            write_string(mv_string(" - "), get_stdout_stream());
            write_string(string_u64(instrs.len, a), get_stdout_stream());
            write_string(mv_string(")\n"), get_stdout_stream());

            for (size_t i = current_loc; i < instrs.len; i++) {
                int len = snprintf(NULL, 0, "%02x", instrs.data[i]) + 1;
                char* str = (char*)mem_alloc(sizeof(char) * len, a);
                snprintf(str, len, "%02" PRIx8, instrs.data[i]);
                Document* arg = mv_cstr_doc(str, a);

                push_ptr(arg, &nodes);
            }
             
            Document* doc = mv_hsep_doc(nodes, a);
            write_doc_formatted(doc, 120, get_formatted_stdout());
            write_string(mv_string("\n"), get_stdout_stream());
        }
        break;
    }

    default: {
        panic(mv_string("Invalid abstract term in polymorphic codegen."));
    }
    }

#ifdef DEBUG_ASSERT
    int64_t new_head = get_stack_head(env);
    int64_t diff = old_head - new_head;
    if (diff < 0) panic(mv_string("diff < 0!"));

    size_t stack_sz = is_variable_in(type, env) ?
        ADDRESS_SIZE : pi_stack_size_of(*type);

    // Justification for cast: stack size of is extremely unlikely to be 2^63 bytes
    //  in size!
    if (diff != (int64_t)stack_sz) {
        String expected = string_u64(stack_sz, a);
        String actual = string_i64(diff, a);
        String message = string_ncat(a, 4,
                                     mv_string("Address environment constraint violated: expected size of the stack is wrong!\nExpected "),
                                     expected, mv_string(" but got "), actual);
        panic(message);
    }
#endif
}

size_t calc_variant_stack_size(PtrArray* types) {
    size_t total = 0;
    for (size_t i = 0; i < types->len; i++) {
        total += pi_stack_size_of(*(PiType*)types->data[i]);
    }
    return total;
}

// Const_fold: evaluate and place 
void* const_fold(SynRef ref, AddressEnv *env, InternalContext ictx) {
    InternalLinkData* links = ictx.links;
    Target target = ictx.target;

    Allocator exalloc = mk_executable_allocator(ictx.a);

    // Catch error here; so can cleanup after self before further unwinding.
    ErrorPoint cleanup_point;
    if (catch_error(cleanup_point)) goto on_error;

    // As we will The 
    Target gen_target = {
        .target = mk_assembler(current_cpu_feature_flags(), &exalloc),
        .code_aux = ictx.target.code_aux,
        .data_aux = ictx.target.data_aux,
    };

    generate_i(ref, env, ictx);
    
    // The data chunk may be moved around during code-generation via 'realloc'
    // if it needs to grow. Thus, we backlink data here, to be safe.
    // TODO (INVESTIGATE BUG): check if also backlinking code makes sense?
    for (size_t i = 0; i < links->links.ed_links.len; i++) {
        LinkMetaData link = links->links.ed_links.data[i];
        void** address_ptr = (void**) ((void*)get_instructions(target.target).data + link.source_offset);
        *address_ptr= target.data_aux->data + link.dest_offset;
    }
    for (size_t i = 0; i < links->links.cd_links.len; i++) {
        LinkMetaData link = links->links.cd_links.data[i];
        void** address_ptr = (void**) ((void*)get_instructions(target.code_aux).data + link.source_offset);
        *address_ptr= target.data_aux->data + link.dest_offset;
    }
    for (size_t i = 0; i < links->links.dd_links.len; i++) {
        LinkMetaData link = links->links.dd_links.data[i];
        void** address_ptr = (void**) ((void*)target.data_aux->data + link.source_offset);
        *address_ptr= target.data_aux->data + link.dest_offset;
    }

    void* result = pico_run_expr(gen_target, pi_size_of(*get_type(ref, ictx.tape)), ictx.a, &cleanup_point);

    delete_assembler(gen_target.target);
    release_executable_allocator(exalloc);
    return result;

 on_error:
    delete_assembler(gen_target.target);
    release_executable_allocator(exalloc);
    throw_error(ictx.point, cleanup_point.error_message);
}

#endif
