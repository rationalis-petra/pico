#include <string.h>

#include "data/num.h"
#include "data/stringify.h"
#include "platform/signals.h"
#include "platform/machine_info.h"
#include "platform/memory/executable.h"

#include "components/pretty/string_printer.h"

#include "pico/data/error.h"
#include "pico/codegen/backend-direct/generate.h"
#include "pico/codegen/backend-direct/internal.h"
#include "pico/codegen/backend-direct/polymorphic.h"
#include "pico/codegen/backend-direct/foreign_adapters.h"
#include "pico/binding/address_env.h"
#include "pico/eval/call.h"

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
static void* const_fold(Syntax *syn, AddressEnv *env, Target target, InternalLinkData* links, Allocator *a, ErrorPoint *point);

LinkData bd_generate_toplevel(TopLevel top, Environment* env, Target target, Allocator* a, ErrorPoint* point) {
    InternalLinkData links = (InternalLinkData) {
        .links = (LinkData) {
            .external_links = mk_sym_sarr_amap(8, a),
            .ec_links = mk_link_meta_array(32, a),
            .ed_links = mk_link_meta_array(8, a),
            .cc_links = mk_link_meta_array(32, a),
            .cd_links = mk_link_meta_array(8, a),
            .dd_links = mk_link_meta_array(8, a),
        },
        .gotolinks = mk_sym_sarr_amap(8, a),
    };

    switch(top.type) {
    case TLDef: {
        // Note: types can only be recursive via 'Name', so we do not recursively bind if
        // generating a type.
        Symbol* recsym = top.def.value->ptype->sort != TKind ? 
            &top.def.bind : NULL;
        AddressEnv* a_env = mk_address_env(env, recsym, a);
        size_t out_sz = pi_size_of(*top.def.value->ptype);

        generate_entry(out_sz, target, a, point);
        generate_i(*top.def.value, a_env, target, &links, a, point);
        generate_exit(out_sz, target, a, point);

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

        size_t out_sz = pi_size_of(*top.expr->ptype);
        generate_entry(out_sz, target, a, point);
        generate_i(*top.expr, a_env, target, &links, a, point);
        generate_exit(out_sz, target, a, point);

        delete_address_env(a_env, a);
        break;
    }
    }

    // The data chunk may be moved around during code-generation via 'realloc'
    // if it needs to grow. Thus, we backlink data here, to be safe.
    // TODO (INVESTIGATE BUG): check if also backlinking code makes sense?
    for (size_t i = 0; i < links.links.ed_links.len; i++) {
        LinkMetaData link = links.links.ed_links.data[i];
        void** address_ptr = (void**) ((void*)get_instructions(target.target).data + link.source_offset);
        set_unaligned_ptr(address_ptr, target.data_aux->data + link.dest_offset);
    }
    for (size_t i = 0; i < links.links.cd_links.len; i++) {
        LinkMetaData link = links.links.cd_links.data[i];
        void** address_ptr = (void**) ((void*)get_instructions(target.code_aux).data + link.source_offset);
        set_unaligned_ptr(address_ptr, target.data_aux->data + link.dest_offset);
    }
    for (size_t i = 0; i < links.links.dd_links.len; i++) {
        LinkMetaData link = links.links.dd_links.data[i];
        void** address_ptr = (void**) ((void*)target.data_aux->data + link.source_offset);
        set_unaligned_ptr(address_ptr, target.data_aux->data + link.dest_offset);
    }

    return links.links;
}

LinkData bd_generate_expr(Syntax* syn, Environment* env, Target target, Allocator* a, ErrorPoint* point) {
    
    AddressEnv* a_env = mk_address_env(env, NULL, a);
    InternalLinkData links = (InternalLinkData) {
        .links = (LinkData) {
            .external_links = mk_sym_sarr_amap(8, a),
            .ec_links = mk_link_meta_array(8, a),
            .ed_links = mk_link_meta_array(8, a),
            .cc_links = mk_link_meta_array(8, a),
            .cd_links = mk_link_meta_array(8, a),
            .dd_links = mk_link_meta_array(8, a),
        },
        .gotolinks = mk_sym_sarr_amap(8, a),
    };

    size_t out_sz = pi_size_of(*syn->ptype);
    generate_entry(out_sz, target, a, point);
    generate_i(*syn, a_env, target, &links, a, point);
    generate_exit(out_sz, target, a, point);

    delete_address_env(a_env, a);

    // The data chunk may be moved around during code-generation via 'realloc'
    // if it needs to grow. Thus, we backlink data here, to be safe.
    // TODO (INVESTIGATE BUG): check if also backlinking code makes sense?
    for (size_t i = 0; i < links.links.ed_links.len; i++) {
        LinkMetaData link = links.links.ed_links.data[i];
        void** address_ptr = (void**) ((void*)get_instructions(target.target).data + link.source_offset);
        *address_ptr= target.data_aux->data + link.dest_offset;
    }
    for (size_t i = 0; i < links.links.cd_links.len; i++) {
        LinkMetaData link = links.links.cd_links.data[i];
        void** address_ptr = (void**) ((void*)get_instructions(target.code_aux).data + link.source_offset);
        *address_ptr= target.data_aux->data + link.dest_offset;
    }
    for (size_t i = 0; i < links.links.dd_links.len; i++) {
        LinkMetaData link = links.links.dd_links.data[i];
        void** address_ptr = (void**) ((void*)target.data_aux->data + link.source_offset);
        *address_ptr= target.data_aux->data + link.dest_offset;
    }

    return links.links;
}

void bd_generate_type_expr(Syntax* syn, TypeEnv* env, Target target, Allocator* a, ErrorPoint* point) {
    AddressEnv* a_env = mk_type_address_env(env, NULL, a);
    InternalLinkData links = (InternalLinkData) {
        .links = (LinkData) {
            .external_links = mk_sym_sarr_amap(8, a),
            .ec_links = mk_link_meta_array(8, a),
            .ed_links = mk_link_meta_array(8, a),
            .cc_links = mk_link_meta_array(8, a),
            .cd_links = mk_link_meta_array(8, a),
            .dd_links = mk_link_meta_array(8, a),
        },
        .gotolinks = mk_sym_sarr_amap(8, a),
    };

    size_t out_sz = pi_size_of(*syn->ptype);
    generate_entry(out_sz, target, a, point);
    generate_i(*syn, a_env, target, &links, a, point);
    generate_exit(out_sz, target, a, point);

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
    build_unary_op(Push, reg(R14, sz_64), ass, a, point);
    build_unary_op(Push, reg(R13, sz_64), ass, a, point);
    build_unary_op(Push, reg(R12, sz_64), ass, a, point);

    // Push the argument onto the stack
#if ABI == SYSTEM_V_64
    if (out_sz != 0) {
        build_unary_op(Push, reg(RDI, sz_64), ass, a, point);
    }

    // Both R14 and R15 have same value, as the variable stack has not moved
    build_binary_op(Mov, reg(R15, sz_64), reg(RSI, sz_64), ass, a, point);
    build_binary_op(Mov, reg(R14, sz_64), reg(RSI, sz_64), ass, a, point);
    build_binary_op(Mov, reg(R13, sz_64), reg(RCX, sz_64), ass, a, point);
#elif ABI == WIN_64
    if (out_sz != 0) {
        build_unary_op(Push, reg(RCX, sz_64), ass, a, point);
    }

    build_binary_op(Mov, reg(R15, sz_64), reg(RDX, sz_64), ass, a, point);
    build_binary_op(Mov, reg(R14, sz_64), reg(RDX, sz_64), ass, a, point);
    build_binary_op(Mov, reg(R13, sz_64), reg(R8, sz_64), ass, a, point);
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
#error "Unknown calling convention"
#endif

    build_unary_op(Pop, reg(R12, sz_64), ass, a, point);
    build_unary_op(Pop, reg(R13, sz_64), ass, a, point);
    build_unary_op(Pop, reg(R14, sz_64), ass, a, point);
    build_unary_op(Pop, reg(R15, sz_64), ass, a, point);
    build_unary_op(Pop, reg(RSI, sz_64), ass, a, point);
    build_unary_op(Pop, reg(RDI, sz_64), ass, a, point);
    build_unary_op(Pop, reg(RBX, sz_64), ass, a, point);
    build_unary_op(Pop, reg(RBP, sz_64), ass, a, point);

    // Return
    build_nullary_op(Ret, ass, a, point);
}

void generate_i(Syntax syn, AddressEnv* env, Target target, InternalLinkData* links, Allocator* a, ErrorPoint* point) {
#ifdef DEBUG_ASSERT
    int64_t old_head = get_stack_head(env);
#endif

    Assembler* ass = target.target;
    switch (syn.type) {
    case SLitUntypedIntegral: 
    case SLitTypedIntegral:
    case SLitUntypedFloating: 
    case SLitTypedFloating: 
    case SLitString: 
    case SLitBool: 
    case SLitUnit: 
    case SLitArray: 
    case SVariable: 
    case SAbsVariable: 
    case SProcedure: 
    case SAll: 
    case SMacro: 
    case SApplication:
    case SAllApplication: /* { */
    case SExists:
    case SUnpack: 
    case SConstructor: 
    case SVariant:
    case SMatch: 
    case SStructure: 
    case SProjector: 
        generate_polymorphic_i(syn, env, target, links, a, point);
        break;
    case SInstance: {
        // TODO: check that the instance handles stack alignment correctly
        /* Instances work as follows:
         * • Instances as values are expected to be passed as pointers and allocated temporarily. 
         * • Non-parametric instances are simply pointers : codegen generates a
         *   malloc and then assigns all values.
         * • Parametric instances are functions (which may be instantiated by
         *   the runtime)
         */

        size_t immediate_sz = 0;
        for (size_t i = 0; i < syn.ptype->instance.fields.len; i++) {
            immediate_sz = pi_size_align(immediate_sz, pi_align_of(*(PiType*)syn.ptype->instance.fields.data[i].val));
            immediate_sz += pi_size_of(*(PiType*)syn.ptype->instance.fields.data[i].val);
        }
        build_binary_op(Mov, reg(RSI, sz_64), imm32(immediate_sz), ass, a, point);
        generate_tmp_malloc(reg(RAX, sz_64), reg(RSI, sz_64), ass, a, point);
        build_binary_op(Mov, reg(RCX, sz_64), reg(RAX, sz_64), ass, a, point);

        // Grow by address size to account for the fact that the for loop
        // keeps a stack of the address, which is updated each iteration.
        data_stack_grow(env, ADDRESS_SIZE);
        build_unary_op(Push, reg(RCX, sz_64), ass, a, point);

        // TODO (BUG): generate code that doesn't assume fields are in same order   

        // Alignment
        size_t index_offset = 0;
        for (size_t i = 0; i < syn.ptype->instance.fields.len; i++) {
            // Generate field
            Syntax* val = syn.instance.fields.data[i].val;
            generate_i(*val, env, target, links, a, point);

            // The offset tells us how far up the stack we look to find the instance ptr
            size_t offset = pi_stack_size_of(*val->ptype);

            // Retrieve index (ptr) 
            // TODO (BUG): Check offset is < int8_t max.
            build_binary_op(Mov, reg(RCX, sz_64), rref8(RSP, pi_stack_align(offset), sz_64), ass, a, point);

            // Align RCX
            size_t aligned_offset = pi_size_align(index_offset, pi_align_of(*val->ptype));
            if (index_offset != aligned_offset) {
                size_t align = aligned_offset - index_offset;
                build_binary_op(Add, reg(RCX, sz_64), imm32(align), ass, a, point);
            }

            // TODO (check if replace with stack copy)
            generate_monomorphic_copy(RCX, RSP, offset, ass, a, point);

            // We need to increment the current field index to be able ot access
            // the next
            build_binary_op(Add, reg(RCX, sz_64), imm32(offset), ass, a, point);
            index_offset += offset;

            // Pop value from stack
            build_binary_op(Add, reg(RSP, sz_64), imm32(pi_stack_align(offset)), ass, a, point);
            data_stack_shrink(env, offset);

            // Override index with new value
            build_binary_op(Mov, rref8(RSP, 0, sz_64), reg(RCX, sz_64), ass, a, point);
        }

        build_binary_op(Mov, reg(RCX, sz_64), rref8(RSP, 0, sz_64), ass, a, point);
        build_binary_op(Sub, reg(RCX, sz_64), imm32(immediate_sz), ass, a, point);
        build_binary_op(Mov, rref8(RSP, 0, sz_64), reg(RCX, sz_64), ass, a, point);

        // Note: we don't shrink as the final address (on stack) is accounted
        // for by the 'grow' prior to the above for-loop
        break;
    }
    case SDynamic:
    case SDynamicUse: 
    case SDynamicSet: 
        generate_polymorphic_i(syn, env, target, links, a, point);
        break;
    case SDynamicLet: {
        // TODO: check that the dynamic let handles stack alignment correctly

        // Step 1: evaluate all dynamic bindings & bind them
        for (size_t i = 0; i < syn.dyn_let_expr.bindings.len; i++) {
            DynBinding* dbind = syn.dyn_let_expr.bindings.data[i];
            size_t bind_size = pi_size_of(*dbind->expr->ptype);
            generate_i(*dbind->var, env, target, links, a, point);
            generate_i(*dbind->expr, env, target, links, a, point);

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
        generate_i(*syn.let_expr.body, env, target, links, a, point);
        size_t val_size = pi_size_of(*syn.dyn_let_expr.body->ptype);

        // Step 3: unwind the bindings
        //  • Store the (address of the) current dynamic value index to restore in RDX
        build_binary_op(Mov, reg(RDX, sz_64), reg(RSP, sz_64), ass, a, point);
        build_binary_op(Add, reg(RDX, sz_64), imm32(val_size), ass, a, point);

        size_t offset_size = 0;
        for (size_t i = 0; i < syn.let_expr.bindings.len; i++) {
            DynBinding* dbind = syn.dyn_let_expr.bindings.data[i];
            size_t bind_size = pi_size_of(*dbind->expr->ptype);

            // Store ptr to dynamic memory (array) in RCX, and the index in RAX
            build_binary_op(Mov, reg(RCX, sz_64), reg(DVARS_REGISTER, sz_64), ass, a, point);
            build_binary_op(Mov, reg(RAX, sz_64), rref8(RDX, 0, sz_64), ass, a, point);
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
    case SLet: 
    case SIf: 
    case SLabels: 
    case SGoTo: 
        generate_polymorphic_i(syn, env, target, links, a, point);
        break;
    case SWithReset: {
        // TODO: check that the with-reset handles stack alignment correctly
        // TODO: make sure that with-reset and reset-to handle R13 and R14 correctly
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
        generate_i(*syn.with_reset.expr,env, target, links, a, point);

        // Step 4.
        //--------

        // Cleanup code for both 
        // At this point, the stack should have the appearance:
        // -- RIP for resetting to
        // -- Old RBP
        // -- Self-ptr + RSP
        // -- Out Value
        // So, the goal is do to a stack move from current RSP to current RSP - 3*ADDRESS

        size_t val_size = pi_size_of(*syn.ptype);
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
        generate_i(*syn.with_reset.handler, env, target, links, a, point);
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
            throw_error(point, mv_string("Internal error in codegen: jump distance exceeded INT32_MAX"));
        }
        *(get_instructions(ass).data + end_expr_link) = (int32_t) dist;

        break;
    }
    case SResetTo: {
        generate_i(*syn.reset_to.arg, env, target, links, a, point);
        generate_i(*syn.reset_to.point, env, target, links, a, point);
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
        PiType* arg_type = syn.reset_to.arg->ptype;
        size_t asize = pi_size_of(*arg_type);
        build_binary_op(Sub, reg(RSP, sz_64), imm32(asize), ass, a, point);
        // TODO (check if replace with stack copy)
        generate_monomorphic_copy(RSP, R9, asize, ass, a, point);

        // Step 5: Long Jump (call) register
        build_unary_op(Call, reg(RDI, sz_64), ass, a, point);

        // Address environment bookkeeping: shrink the stack appropriately
        data_stack_shrink(env, pi_size_of(*syn.reset_to.arg->ptype) + pi_size_of(*syn.reset_to.point->ptype));
        data_stack_grow(env, pi_size_of(*syn.ptype));

        break;
    }
    case SSequence: {
        size_t binding_size = 0;
        size_t num_bindings = 0;
        size_t last_size = 0;
        for (size_t i = 0; i < syn.sequence.elements.len; i++) {
            SeqElt* elt = syn.sequence.elements.data[i];
            generate_i(*elt->expr, env, target, links, a, point);

            size_t sz = pi_stack_size_of(*elt->expr->ptype);
            if (elt->is_binding) {
                num_bindings++;
                binding_size += i + 1 == syn.sequence.elements.len ? 0 : sz;
                address_bind_relative(elt->symbol, 0, env);
            }
            else if (i + 1 != syn.sequence.elements.len) {
                if (sz != 0) {
                    build_binary_op(Add, reg(RSP, sz_64), imm32(sz), ass, a, point);
                }
                data_stack_shrink(env, sz);
            }

            if (i + 1 == syn.sequence.elements.len) {
                last_size = sz;
            }
            
        }
        if (binding_size != 0) {
            generate_stack_move(binding_size, 0, last_size, ass, a, point);
            build_binary_op(Add, reg(RSP, sz_64), imm32(binding_size), ass, a, point);
            data_stack_shrink(env, binding_size);
            address_pop_n(num_bindings, env);
        }
        break;
    }
    case SIs:
    case SInTo:
    case SOutOf:
    case SName:
    case SUnName:
    case SWiden:
    case SNarrow:
    case SSizeOf:
    case SAlignOf: 
    case SOffsetOf: 
    case SCheckedType: 
    case SProcType:
    case SStructType:
    case SEnumType:
    case SResetType:
    case SDynamicType:
    case SAllType:
    case SExistsType: 
    case STypeFamily:
    case SLiftCType:
    case SNamedType:
    case SDistinctType:
    case SOpaqueType:
    case STraitType: 
        generate_polymorphic_i(syn, env, target, links, a, point);
        break;

    case SReinterpret:
        // reinterpret has no associated codegen
        generate_i(*syn.reinterpret.body, env, target, links, a, point);
        break;
    case SConvert: {
        if (syn.convert.from_native) {
            void* cfn = *(void**)const_fold(syn.convert.body, env, target, links, a, point);
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

            bd_convert_c_fn(cfn, &syn.convert.body->ptype->c_type, syn.ptype, ass, a, point);
        } else {
            panic(mv_string("Cannot yet convert pico value to c value."));
        }
        break;
    }
    case STypeOf: {
        build_binary_op(Mov, reg(R9, sz_64), imm64((uint64_t)syn.type_of->ptype), ass, a, point);
        build_unary_op(Push, reg(R9, sz_64), ass, a, point);
        data_stack_grow(env, pi_size_of(*syn.ptype));
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
                    throw_error(point, mv_string("Unknown symbol in path to describe."));
                }
            } else {
                throw_error(point, mv_string("Unknown symbol in path to describe."));
            }
        }
        String immediate;

        if (entry.success == Ok) {
          if (entry.is_module) {
              SymbolArray syms = get_defined_symbols(entry.value, a);
              PtrArray lines = mk_ptr_array(syms.len + 8, a);
              {
                  PtrArray moduledesc = mk_ptr_array(2, a);
                  String module_name = get_name(entry.value, a);
                  push_ptr(mk_str_doc(mv_string("Module: "), a), &moduledesc);
                  push_ptr(mk_str_doc(module_name, a), &moduledesc);
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
                          push_ptr(mv_nest_doc(2, pretty_type(&mentry->type, a), a), &desc);
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
                  String module_name = get_name(entry.source, a);
                  push_ptr(mk_str_doc(mv_string("Source Module: "), a), &moduledesc);
                  push_ptr(mk_str_doc(module_name, a), &moduledesc);
                  push_ptr(mv_sep_doc(moduledesc, a), &lines);
              }
              {
                  PtrArray typedesc = mk_ptr_array(2, a);
                  push_ptr(mk_str_doc(mv_string("Type: "), a), &typedesc);
                  push_ptr(pretty_type(entry.type, a), &typedesc);
                  push_ptr(mv_sep_doc(typedesc, a), &lines);
              }
              {
                  PtrArray valdesc = mk_ptr_array(2, a);
                  push_ptr(mk_str_doc(mv_string("Value: "), a), &valdesc);
                  push_ptr(pretty_pi_value(entry.value, entry.type, a), &valdesc);
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
            throw_error(point, mv_string("Codegen: String literal length must fit into less than 32 bits"));

        // Push the string onto the stack
        // '0' is used as a placeholder, as the correct address will be
        // substituted when backlinking!
        AsmResult out = build_binary_op(Mov, reg(RAX, sz_64), imm64(0), ass, a, point);

        // Backlink the data & copy the bytes into the data-segment.
        backlink_data(target, out.backlink, links);
        add_u8_chunk(immediate.bytes, immediate.memsize, target.data_aux);

        build_unary_op(Push, reg(RAX, sz_64), ass, a, point);
        build_unary_op(Push, imm32(immediate.memsize), ass, a, point);

        data_stack_grow(env, pi_stack_size_of(*syn.ptype));
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
        data_stack_grow(env, pi_stack_size_of(*syn.ptype));
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
        data_stack_grow(env, pi_stack_size_of(*syn.ptype));
        break;
    }
    default: {
        panic(mv_string("Invalid abstract supplied to monomorphic codegen."));
    }
    }

#ifdef DEBUG_ASSERT
    int64_t new_head = get_stack_head(env);
    int64_t diff = old_head - new_head;
    if (diff < 0) panic(mv_string("diff < 0!"));

    // Justification: stack size of is extremely unlikely to be 2^63 bytes
    //  in size!
    if (diff != (int64_t)pi_stack_size_of(*syn.ptype)) {
        String expected = string_u64(pi_stack_size_of(*syn.ptype), a);
        String actual = string_i64(diff, a);
        String message = string_ncat(a, 4,
                                     mv_string("Address environment constraint violated: expected size of the stack is wrong!\nExpected "),
                                     expected, mv_string(" but got "), actual);
        panic(message);
    }
#endif
}

size_t calc_variant_size(PtrArray* types) {
    size_t total = sizeof(uint64_t);
    for (size_t i = 0; i < types->len; i++) {
        size_t field_align;
        Result_t res = pi_maybe_align_of(*(PiType*)types->data[i], &field_align);
        if (res != Ok) return res;
        total = pi_size_align(total, field_align);
        size_t field_size;
        res = pi_maybe_size_of(*(PiType*)types->data[i], &field_size);
        if (res != Ok) return res;
        total += field_size;
    }
    return total;
}

size_t calc_variant_stack_size(PtrArray* types) {
    size_t total = sizeof(uint64_t);
    for (size_t i = 0; i < types->len; i++) {
        total += pi_stack_size_of(*(PiType*)types->data[i]);
    }
    return total;
}

// Const_fold: evaluate and place 
void *const_fold(Syntax *syn, AddressEnv *env, Target target, InternalLinkData* links, Allocator *a, ErrorPoint *point) {
    Allocator exalloc = mk_executable_allocator(a);

    // Catch error here; so can cleanup after self before further unwinding.
    ErrorPoint cleanup_point;
    if (catch_error(cleanup_point)) goto on_error;

    // As we will The 
    Target gen_target = {
        .target = mk_assembler(current_cpu_feature_flags(), &exalloc),
        .code_aux = target.code_aux,
        .data_aux = target.data_aux,
    };

    generate_i(*syn, env, gen_target, links, a, point);
    
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

    void* result = pico_run_expr(gen_target, pi_size_of(*syn->ptype), a, &cleanup_point);

    delete_assembler(gen_target.target);
    release_executable_allocator(exalloc);
    return result;

 on_error:
    delete_assembler(gen_target.target);
    release_executable_allocator(exalloc);
    throw_error(point, cleanup_point.error_message);
}

