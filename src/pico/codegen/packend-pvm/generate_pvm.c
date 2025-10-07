#include "data/num.h"
#include "data/meta/array_impl.h"
#include "platform/signals.h"

#include "pico/data/error.h"
#include "pico/codegen/backend-pvm/generate.h"
#include "pico/binding/address_env.h"

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

int compare_link_meta(LinkMetaData lhs, LinkMetaData rhs) {
    long int diff = lhs.source_offset - rhs.source_offset;
    if (diff) return diff;
    return lhs.dest_offset - rhs.dest_offset;
}

ARRAY_CMP_IMPL(LinkMetaData, compare_link_meta, link_meta, LinkMeta)

// Implementation details
static void generate(Syntax syn, AddressEnv* env, Target target, LinkData* links, Allocator* a, ErrorPoint* point);

LinkData pvm_generate_toplevel(TopLevel top, Environment* env, Target target, Allocator* a, ErrorPoint* point) {
    LinkData links = (LinkData) {
        .external_links = mk_sym_sarr_amap(8, a),
        .ec_links = mk_link_meta_array(32, a),
        .ed_links = mk_link_meta_array(8, a),
        .cc_links = mk_link_meta_array(32, a),
        .cd_links = mk_link_meta_array(8, a),
        .dd_links = mk_link_meta_array(8, a),
    };

    switch(top.type) {
    case TLDef: {
        // Note: types can only be recursive via 'Name', so we do not recursively bind if
        // generating a type.
        Symbol* recsym = top.def.value->ptype->sort != TKind ? 
            &top.def.bind : NULL;
        AddressEnv* a_env = mk_address_env(env, recsym, a);
        generate(*top.def.value, a_env, target, &links, a, point);
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
        generate(*top.expr, a_env, target, &links, a, point);
        delete_address_env(a_env, a);
        break;
    }
    }

    // The data chunk may be moved around during code-generation via 'realloc'
    // if it needs to grow. Thus, we backlink data here, to be safe.
    // TODO (INVESTIGATE BUG): check if also backlinking code makes sense?
    for (size_t i = 0; i < links.ed_links.len; i++) {
        LinkMetaData link = links.ed_links.data[i];
        void** address_ptr = (void**) ((void*)get_instructions(target.target).data + link.source_offset);
        set_unaligned_ptr(address_ptr, target.data_aux->data + link.dest_offset);
    }
    for (size_t i = 0; i < links.cd_links.len; i++) {
        LinkMetaData link = links.cd_links.data[i];
        void** address_ptr = (void**) ((void*)get_instructions(target.code_aux).data + link.source_offset);
        set_unaligned_ptr(address_ptr, target.data_aux->data + link.dest_offset);
    }
    for (size_t i = 0; i < links.dd_links.len; i++) {
        LinkMetaData link = links.dd_links.data[i];
        void** address_ptr = (void**) ((void*)target.data_aux->data + link.source_offset);
        set_unaligned_ptr(address_ptr, target.data_aux->data + link.dest_offset);
    }

    return links;
}

LinkData pvm_generate_expr(Syntax* syn, Environment* env, Target target, Allocator* a, ErrorPoint* point) {
    
    AddressEnv* a_env = mk_address_env(env, NULL, a);
    LinkData links = (LinkData) {
        .external_links = mk_sym_sarr_amap(8, a),
        .ec_links = mk_link_meta_array(8, a),
        .ed_links = mk_link_meta_array(8, a),
        .cc_links = mk_link_meta_array(8, a),
        .cd_links = mk_link_meta_array(8, a),
        .dd_links = mk_link_meta_array(8, a),
    };
    generate(*syn, a_env, target, &links, a, point);
    delete_address_env(a_env, a);

    // The data chunk may be moved around during code-generation via 'realloc'
    // if it needs to grow. Thus, we backlink data here, to be safe.
    // TODO (INVESTIGATE BUG): check if also backlinking code makes sense?
    for (size_t i = 0; i < links.ed_links.len; i++) {
        LinkMetaData link = links.ed_links.data[i];
        void** address_ptr = (void**) ((void*)get_instructions(target.target).data + link.source_offset);
        *address_ptr= target.data_aux->data + link.dest_offset;
    }
    for (size_t i = 0; i < links.cd_links.len; i++) {
        LinkMetaData link = links.cd_links.data[i];
        void** address_ptr = (void**) ((void*)get_instructions(target.code_aux).data + link.source_offset);
        *address_ptr= target.data_aux->data + link.dest_offset;
    }
    for (size_t i = 0; i < links.dd_links.len; i++) {
        LinkMetaData link = links.dd_links.data[i];
        void** address_ptr = (void**) ((void*)target.data_aux->data + link.source_offset);
        *address_ptr= target.data_aux->data + link.dest_offset;
    }

    return links;
}

void pvm_generate_type_expr(Syntax* syn, TypeEnv* env, Target target, Allocator* a, ErrorPoint* point) {
    AddressEnv* a_env = mk_type_address_env(env, NULL, a);
    LinkData links = (LinkData) {
        .external_links = mk_sym_sarr_amap(8, a),
        .ec_links = mk_link_meta_array(8, a),
        .ed_links = mk_link_meta_array(8, a),
        .cc_links = mk_link_meta_array(8, a),
        .cd_links = mk_link_meta_array(8, a),
        .dd_links = mk_link_meta_array(8, a),
    };
    generate(*syn, a_env, target, &links, a, point);
    delete_address_env(a_env, a);
}

void generate(Syntax syn, AddressEnv* env, Target target, LinkData* links, Allocator* a, ErrorPoint* point) {
    switch (syn.type) {
    case SLitUntypedIntegral: 
        panic(mv_string("Internal Error: Codegen provided untyped integral"));
    case SLitTypedIntegral: {
        not_implemented(mv_string("PVM Integral Constant Generation"));
        break;
    }
    case SLitUntypedFloating: 
        panic(mv_string("Internal Error: Codegen provided untyped float"));
    case SLitTypedFloating: {
        not_implemented(mv_string("PVM Floating Constant Generation"));
        break;
    }
    case SLitString: {
        not_implemented(mv_string("PVM String Constant Generation"));
        break;
    }
    case SLitBool: {
        not_implemented(mv_string("PVM Bool Constant Generation"));
        break;
    }
    case SLitUnit: {
        not_implemented(mv_string("PVM Unit Generation"));
        break;
    }
    case SLitArray: {
        not_implemented(mv_string("PVM Array Constant Generation"));
        break;
    }
    case SVariable: 
    case SAbsVariable: {
        not_implemented(mv_string("PVM Variable Generation"));
        break;
    }
    case SProcedure: {
        not_implemented(mv_string("PVM Procedure Generation"));
        break;
    }
    case SAll: {
        not_implemented(mv_string("PVM All Generation"));
        break;
    }
    case SMacro: {
        generate(*syn.transformer, env, target, links, a, point);
        break;
    }
    case SApplication: {
        not_implemented(mv_string("PVM Funcall Generation"));
        break;
    }
    case SAllApplication: {
        not_implemented(mv_string("PVM Funcall/all Generation"));
        break;
    }
    case SExists: {
        not_implemented(mv_string("PVM exists Generation"));
        break;
    }
    case SUnpack: {
        not_implemented(mv_string("PVM unpack Generation"));
        break;
    }
    case SConstructor: {
        not_implemented(mv_string("PVM Constructor Generation"));
        break;
    }
    case SVariant: {
        not_implemented(mv_string("PVM Variant Generation"));
        break;
    }
    case SMatch: {
        not_implemented(mv_string("PVM Match Generation"));
        break;
    }
    case SStructure: {
        not_implemented(mv_string("PVM Structure Generation"));
        break;
    }
    case SProjector: {
        not_implemented(mv_string("PVM Projector Generation"));
        break;
    }
    case SInstance: {
        not_implemented(mv_string("PVM Instance Generation"));
        break;
    }
    case SDynamic: {
        not_implemented(mv_string("PVM Dynamic Generation"));
        break;
    }
    case SDynamicUse: {
        not_implemented(mv_string("PVM Dynamic-Use Generation"));
        break;
    }
    case SDynamicSet: {
        not_implemented(mv_string("PVM Dynamic-Set Generation"));
        break;
    }
    case SDynamicLet: {
        not_implemented(mv_string("PVM Dynamic-Let Generation"));
        break;
    }
    case SLet: {
        not_implemented(mv_string("PVM Let Generation"));
        break;
    }
    case SIf: {
        not_implemented(mv_string("PVM If Generation"));
        break;
    }
    case SLabels: {
        not_implemented(mv_string("PVM Labels Generation"));
        break;
    }
    case SGoTo: {
        not_implemented(mv_string("PVM GoTo Generation"));
        break;
    }
    case SWithReset: {
        not_implemented(mv_string("PVM With-Reset Generation"));
        break;
    }
    case SResetTo: {
        not_implemented(mv_string("PVM Reset-To Generation"));
        break;
    }
    case SSequence: {
        not_implemented(mv_string("PVM Is Generation"));
        break;
    }
    case SIs:
        generate(*syn.is.val, env, target, links, a, point);
        break;
    case SInTo:
        generate(*syn.into.val, env, target, links, a, point);
        break;
    case SOutOf:
        generate(*syn.out_of.val, env, target, links, a, point);
        break;
    case SName:
        generate(*syn.name.val, env, target, links, a, point);
        break;
    case SUnName:
        generate(*syn.unname, env, target, links, a, point);
        break;
    case SWiden:
        not_implemented(mv_string("PVM Widen Generation"));
        break;
    case SNarrow:
        not_implemented(mv_string("PVM Narrow Generation"));
        break;
    case SSizeOf: {
        not_implemented(mv_string("PVM Size-Of Generation"));
        break;
    }
    case SAlignOf: {
        not_implemented(mv_string("PVM Align-Of Generation"));
        break;
    }
    case SOffsetOf: {
        not_implemented(mv_string("PVM Offset-Of Generation"));
        break;
    }
    case SCheckedType: {
        not_implemented(mv_string("PVM Checked-Type Generation"));
        break;
    }
    case SProcType:
        not_implemented(mv_string("PVM Proc-Type Generation"));
        break;
    case SStructType:
        not_implemented(mv_string("PVM Struct-Type Generation"));
        break;
    case SEnumType:
        not_implemented(mv_string("PVM Enum-Type Generation"));
        break;
    case SResetType:
        not_implemented(mv_string("PVM Reset-Type Generation"));
        break;
    case SDynamicType:
        not_implemented(mv_string("PVM Dynamic-Type Generation"));
        break;
    case SAllType:
        not_implemented(mv_string("PVM All-Type Generation"));
        break;
    case SExistsType:
        not_implemented(mv_string("PVM Exists-Type Generation"));
        break;
    case STypeFamily:
        not_implemented(mv_string("PVM Type-Fam Generation"));
        break;
    case SLiftCType:
        not_implemented(mv_string("PVM Lift-CType Generation"));
        break;
    case SNamedType:
        not_implemented(mv_string("PVM Named-Type Generation"));
        break;
    case SDistinctType:
        not_implemented(mv_string("PVM Distinct-Type Generation"));
        break;
    case SOpaqueType:
        not_implemented(mv_string("PVM Opaque-Type Generation"));
        break;
    case STraitType:
        not_implemented(mv_string("PVM Trait-Type Generation"));
        break;
    case SReinterpret:
        generate(*syn.reinterpret.body, env, target, links, a, point);
        break;
    case SConvert: {
        not_implemented(mv_string("PVM Convert Generation"));
        break;
    }
    case STypeOf: {
        not_implemented(mv_string("PVM Type-of Generation"));
        break;
    }
    case SDescribe: {
        not_implemented(mv_string("PVM Describe Generation"));
        break;
    }
    case SQuote: {
        not_implemented(mv_string("PVM Quote Generation"));
        break;
    }
    case SCapture: {
        not_implemented(mv_string("PVM Capture Generation"));
        break;
    }
    default: {
        panic(mv_string("Invalid abstract supplied to codegen."));
    }
    }
}
