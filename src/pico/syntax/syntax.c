#include "data/string.h"
#include "platform/signals.h"
#include "components/pretty/standard_types.h"

#include "pico/values/array.h"
#include "pico/values/modular.h"
#include "pico/syntax/syntax.h"

String syntax_type_to_string(Syntax_t type) {
    switch (type) {
        // Atoms
    case SLitUntypedIntegral:
        return mv_string("<untyped int>");
    case SLitTypedIntegral:
        return mv_string("<typed int>");
    case SLitUntypedFloating:
        return mv_string("<untyped float>");
    case SLitTypedFloating:
        return mv_string("<typed float>");
    case SLitString:
        return mv_string("<literal string>");
    case SLitBool:
        return mv_string("<literal bool>");
    case SLitUnit:
        return mv_string("<literal unit>");
    case SLitArray:
        return mv_string("<literal array>");
    case SVariable:
        return mv_string("<variable>");
    case SAbsVariable:
        return mv_string("<aboslute variable>");

        // Terms & term formers
    case SProcedure:
        return mv_string("proc");
    case SAll:
        return mv_string("all");
    case SMacro:
        return mv_string("macro");
    case SApplication:
        return mv_string("<application>");
    case SAllApplication:
        return mv_string("<all application>");
    case SSeal:
        return mv_string("seal");
    case SUnseal:
        return mv_string("unseal");
    case SConstructor:
        return mv_string("<constructor>");
    case SVariant:
        return mv_string("<variant>");
    case SMatch:
        return mv_string("match");
    case SStructure:
        return mv_string("struct");
    case SProjector:
        return mv_string("<projector> (.)");
    case SInstance:
        return mv_string("instance");
    case SDynamic:
        return mv_string("dynamic");
    case SDynamicUse:
        return mv_string("use");
    case SDynamicSet:
        return mv_string("modfiy");
    case SDynamicLet:
        return mv_string("bind");

        // Control Flow & Binding
    case SLet:
        return mv_string("let");
    case SIf:
        return mv_string("if");
    case SCond:
        return mv_string("cond");
    case SLabels:
        return mv_string("labels");
    case SGoTo:
        return mv_string("go-to");
    case SSequence:
        return mv_string("seq");
    case SWithReset:
        return mv_string("with-reset");
    case SResetTo:
        return mv_string("reset-to");

        // Special
    case SIs:
        return mv_string("is");
    case SInTo:
        return mv_string("into");
    case SOutOf:
        return mv_string("out-of");
    case SName:
        return mv_string("name");
    case SUnName:
        return mv_string("unname");
    case SWiden:
        return mv_string("widen");
    case SNarrow:
        return mv_string("narrow");
    case SSizeOf:
        return mv_string("size-of");
    case SAlignOf:
        return mv_string("align-of");
    case SOffsetOf:
        return mv_string("offset-of");
    case SModule:
        return mv_string("module");

        // Types & Type formers
    case SProcType:
        return mv_string("Proc");
    case SStructType:
        return mv_string("Struct");
    case SEnumType:
        return mv_string("Enum");
    case SResetType:
        return mv_string("Reset");
    case SDynamicType:
        return mv_string("Dynamic");
    case SNamedType:
        return mv_string("Named");
    case SDistinctType:
        return mv_string("Distinct");
    case SOpaqueType:
        return mv_string("Opaque");
    case STraitType:
        return mv_string("Trait");
    case SAllType:
        return mv_string("All");
    case SSealedType:
        return mv_string("Sealed");
    case STypeFamily:
        return mv_string("Family");
    case SLiftCType:
        return mv_string("Lift");

    case SCheckedType:
        return mv_string("<checked type>");

    case SAnnotation:
        return mv_string("ann");

        // Should be moved to macros!(?)
    case SReinterpret:
        return mv_string("reinterpret");
    case SConvert:
        return mv_string("convert");

        // Meta/reflection
    case STypeOf:
        return mv_string("type-of");
    case SDescribe:
        return mv_string("describe");
    case SQuote:
        return mv_string("quote");
    case SCapture:
        return mv_string("capture");
    case SDevAnnotation:
        return mv_string("developer-annnotation");
    }
    panic(mv_string("Invalid syntax type provided to syntax_type_to_string"));
}

typedef struct SyntaxCall {
    Symbol field;
    Syntax* fnc;
    Syntax* arg;
} SyntaxCall;

Syntax* mk_lit_untyped_int_syn(const int64_t value, Allocator* a) {
    Syntax* out = (Syntax*)mem_alloc(sizeof(Syntax), a);
    *out = (Syntax) {
        .type = SLitUntypedIntegral,
        .integral.value = value,
    };
    return out;
}

Syntax* mk_lit_typed_int_syn(const int64_t value, PrimType prim, Allocator* a) {
    Syntax* out = (Syntax*)mem_alloc(sizeof(Syntax), a);
    *out = (Syntax) {
        .type = SLitTypedIntegral,
        .integral.value = value,
        .integral.type = prim,
    };
    return out;
}

typedef struct {
    bool should_wrap;
} PrettyContext;

Document* pretty_syntax_internal(Syntax* syntax, PrettyContext ctx, Allocator* a);
Document* pretty_syntax_callback(Syntax* syntax, void* ctx, Allocator* a) {
    PrettyContext pctx = {
        .should_wrap = false,
    };
    return pretty_syntax_internal(syntax, pctx, a);
}

Document* pretty_syntax_internal(Syntax* syntax, PrettyContext ctx, Allocator* a) {
    bool should_wrap = ctx.should_wrap;
    ctx.should_wrap = true;

    Document* out = NULL;
    DocStyle former_style = scolour(colour(60, 190, 24), dstyle);
    DocStyle ty_former_style = scolour(colour(209, 118, 219), dstyle);
    DocStyle field_style = scolour(colour(60, 120, 210), dstyle);
    DocStyle const_style = scolour(colour(120, 170, 210), dstyle);
    DocStyle var_style = scolour(colour(212, 130, 42), dstyle);

    switch (syntax->type) {
    case SLitUntypedIntegral: {
    case SLitTypedIntegral: 
        out = mv_style_doc(const_style, pretty_i64(syntax->integral.value, a), a);
        break;
    }
    case SLitUntypedFloating: {
    case SLitTypedFloating: 
        out = mv_style_doc(const_style, pretty_f64(syntax->floating.value, a), a);
        break;
    }
    case SLitBool: {
        if (syntax->integral.value == 0) {
            out = mv_style_doc(field_style, mk_str_doc(mv_string(":false"), a), a);
        } else {
            out = mv_style_doc(field_style, mk_str_doc(mv_string(":true"), a), a);
        }
        break;
    }
    case SLitUnit: {
        out = mv_style_doc(field_style, mk_str_doc(mv_string(":unit"), a), a);
        break;
    }
    case SLitArray: {
        PrettyElem pelem = (PrettyElem) {
            .print_elem = (print_element)pretty_syntax_callback,
            .context = NULL,
        };
        Array array = (Array) {
            .shape.len = syntax->array_lit.shape.len,
            .shape.data = syntax->array_lit.shape.data,
            .data = syntax->array_lit.subterms.data,
        };
        size_t index_size = sizeof(void*);
        return pretty_array(array, index_size, pelem, a);
        break;
    }
    case SLitString: {
        Document* delimiter = mk_str_doc(mv_string("\""), a);
        PtrArray nodes = mk_ptr_array(3, a);
        push_ptr(delimiter, &nodes);
        push_ptr(mv_str_doc(syntax->string, a), &nodes);
        push_ptr(delimiter, &nodes);
        out = mk_cat_doc(nodes, a);
        break;
    }
    case SVariable: {
        out = mk_str_doc(symbol_to_string(syntax->variable, a), a);
        break;
    }
    case SAbsVariable: {
        PtrArray nodes = mk_ptr_array(4, a);
        push_ptr(mk_str_doc(mv_string("#abv"), a), &nodes);
        // TODO (BUG LOGIC): replace with pretty_size.
        push_ptr(mk_str_doc(symbol_to_string(syntax->abvar.symbol, a), a), &nodes);
        push_ptr(pretty_u64(syntax->abvar.index, a), &nodes);
        push_ptr(pretty_ptr(syntax->abvar.value, a), &nodes);
        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SProcedure: {
        PtrArray head_nodes = mk_ptr_array(3, a);
        push_ptr(mv_style_doc(former_style, mv_str_doc((mk_string("proc", a)), a), a), &head_nodes);

        if (syntax->procedure.implicits.len != 0) {
            PtrArray impl_nodes = mk_ptr_array(syntax->procedure.implicits.len, a);
            for (size_t i = 0; i < syntax->procedure.implicits.len; i++) {
                Document* arg = mk_str_doc(symbol_to_string(syntax->procedure.implicits.data[i].key, a), a);
                push_ptr(mv_nest_doc(2, arg, a), &impl_nodes);
            }
            push_ptr(mk_paren_doc("{", "}", mv_sep_doc(impl_nodes, a), a), &head_nodes);
        }

        PtrArray arg_nodes = mk_ptr_array(syntax->procedure.args.len, a);
        for (size_t i = 0; i < syntax->procedure.args.len; i++) {
            SymPtrACell cell = syntax->procedure.args.data[i];
            Document* arg = mv_style_doc(var_style, mk_str_doc(symbol_to_string(cell.key, a), a), a);
            if (cell.val) {
                PtrArray tyarg_nodes = mk_ptr_array(2, a);
                push_ptr(arg, &tyarg_nodes);
                push_ptr(mv_nest_doc(2, pretty_syntax(cell.val, a), a), &tyarg_nodes);
                arg = mv_group_doc(mk_paren_doc("(", ")", mv_sep_doc(tyarg_nodes, a), a), a);
            }
            push_ptr(arg, &arg_nodes);
        }
        push_ptr(mv_nest_doc(2, mk_paren_doc("[", "]", mv_nest_doc(1, mv_sep_doc(arg_nodes, a), a), a), a), &head_nodes);

        PtrArray nodes = mk_ptr_array(2, a);
        push_ptr(mv_group_doc(mv_sep_doc(head_nodes, a), a), &nodes);
        push_ptr(mv_nest_doc(2, pretty_syntax_internal(syntax->procedure.body, ctx, a), a), &nodes);
        out = mv_group_doc(mv_sep_doc(nodes, a), a);
        if (should_wrap) out = mk_paren_doc("(", ")", out, a);
        break;
    }
    case SAll: {
        PtrArray head_nodes = mk_ptr_array(2, a);
        PtrArray arg_nodes = mk_ptr_array(syntax->all.args.len, a);
        push_ptr(mv_style_doc(former_style, mv_str_doc((mk_string("all", a)), a), a), &head_nodes);
        for (size_t i = 0; i < syntax->all.args.len; i++) {
            Document* arg = mv_style_doc(var_style, mk_str_doc(symbol_to_string(syntax->all.args.data[i], a), a), a);
            push_ptr(arg, &arg_nodes);
        }

        PtrArray nodes = mk_ptr_array(2, a);
        push_ptr(mk_paren_doc("[", "]", mv_sep_doc(arg_nodes, a), a), &head_nodes);
        push_ptr(mv_group_doc(mv_sep_doc(head_nodes, a), a), &nodes);

        ctx.should_wrap = false;
        push_ptr(mv_nest_doc(2, pretty_syntax_internal(syntax->all.body, ctx, a), a), &nodes);
        out = mv_sep_doc(nodes, a);
        if (should_wrap) out = mk_paren_doc("(", ")", out, a);
        break;
    }
    case SMacro: {
        PtrArray nodes = mk_ptr_array(2, a);
        push_ptr(mv_str_doc((mk_string("macro", a)), a), &nodes);
        push_ptr(mv_nest_doc(2, pretty_syntax_internal(syntax->transformer, ctx, a), a), &nodes);
        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SApplication: {
        Document* head = pretty_syntax_internal(syntax->application.function, ctx, a);
        PtrArray nodes = mk_ptr_array(2, a);
        push_ptr(head, &nodes);
        PtrArray args = mk_ptr_array(syntax->application.args.len, a);
        for (size_t i = 0; i < syntax->application.args.len; i++) {
            Document* node = pretty_syntax_internal(syntax->application.args.data[i], ctx, a);
            push_ptr(node, &args);
        }
        push_ptr(mv_nest_doc(2, mv_sep_doc(args, a), a), &nodes);
        out = mk_paren_doc("(", ")", mv_hsep_doc(nodes, a), a);
        break;
    }
    case SAllApplication: {
        Document* head = pretty_syntax_internal(syntax->all_application.function, ctx, a);
        bool print_types = syntax->all_application.types.len != 0;
        PtrArray nodes = mk_ptr_array((print_types ? 3 : 1) + syntax->all_application.types.len + syntax->all_application.args.len, a);
        push_ptr(head, &nodes);

        PtrArray impl_args = mk_ptr_array(syntax->all_application.types.len, a);
        for (size_t i = 0; i < syntax->all_application.types.len; i++) {
            Document* node = pretty_syntax_internal(syntax->all_application.types.data[i], ctx, a);
            push_ptr(node, &impl_args);
        }
        if (print_types) push_ptr(mk_paren_doc("{", "}", mv_sep_doc(impl_args, a), a), &nodes);

        for (size_t i = 0; i < syntax->all_application.args.len; i++) {
            Document* node = pretty_syntax_internal(syntax->all_application.args.data[i], ctx, a);
            push_ptr(node, &nodes);
        }
        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SSeal: {
        PtrArray nodes = mk_ptr_array(4, a);
        push_ptr(mv_cstr_doc("seal", a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->seal.type, ctx, a), &nodes);
        {
            PtrArray types = mk_ptr_array(syntax->seal.types.len, a);
            for (size_t i = 0; i < syntax->seal.types.len; i++) {
                push_ptr(pretty_syntax_internal(syntax->seal.types.data[i], ctx, a), &types);
            }
            push_ptr(mk_paren_doc("[","]", mv_sep_doc(types, a), a), &nodes);
        }
        {
            PtrArray implicits = mk_ptr_array(syntax->seal.implicits.len, a);
            for (size_t i = 0; i < syntax->seal.implicits.len; i++) {
                push_ptr(pretty_syntax_internal(syntax->seal.implicits.data[i], ctx, a), &implicits);
            }
            push_ptr(mk_paren_doc("{","}", mv_sep_doc(implicits, a), a), &nodes);
        }
        push_ptr(pretty_syntax_internal(syntax->seal.body, ctx, a), &nodes);
        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SUnseal: {
        PtrArray nodes = mk_ptr_array(4, a);
        push_ptr(pretty_syntax_internal(syntax->unseal.sealed, ctx, a), &nodes);

        PtrArray types = mk_ptr_array(syntax->unseal.types.len, a);
        for (size_t i = 0; i < syntax->all.args.len; i++) {
            Document* arg = mk_str_doc(symbol_to_string(syntax->unseal.types.data[i], a), a);
            push_ptr(arg, &nodes);
        }
        push_ptr(mk_paren_doc("[","]", mv_sep_doc(types, a), a), &nodes);

        if (syntax->unseal.implicits.len > 0) {
            PtrArray implicits = mk_ptr_array(syntax->unseal.implicits.len, a);
            for (size_t i = 0; i < syntax->unseal.implicits.len; i++) {
                Document* arg = mk_str_doc(symbol_to_string(syntax->unseal.implicits.data[i], a), a);
                push_ptr(arg, &nodes);
            }
            push_ptr(mk_paren_doc("{", "}", mv_sep_doc(types, a), a), &implicits);
        }

        push_ptr(pretty_syntax_internal(syntax->unseal.body, ctx, a), &nodes);
        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SConstructor: {
        PtrArray nodes = mk_ptr_array(3, a);
        if (syntax->constructor.enum_type) {
            push_ptr(pretty_syntax_internal(syntax->constructor.enum_type, ctx, a), &nodes);
        }
        push_ptr(mk_str_doc(mv_string(":"), a), &nodes);
        push_ptr(mk_str_doc(symbol_to_string(syntax->variant.tagname, a), a), &nodes);
        out = mv_style_doc(field_style, mv_cat_doc(nodes, a), a);
        break;
    }
    case SVariant: {
        PtrArray nodes = mk_ptr_array(syntax->variant.args.len + 1, a);
        PtrArray ctor_parts = mk_ptr_array(3, a);

        if (syntax->variant.enum_type) {
            push_ptr(pretty_syntax_internal(syntax->variant.enum_type, ctx, a), &ctor_parts);
        }
        push_ptr(mk_str_doc(mv_string(":") , a), &ctor_parts);
        push_ptr(mk_str_doc(symbol_to_string(syntax->variant.tagname, a), a), &ctor_parts);
        push_ptr(mv_style_doc(field_style, mv_cat_doc(ctor_parts, a), a), &nodes);

        for (size_t i = 0; i < syntax->variant.args.len; i++) {
            push_ptr(pretty_syntax_internal(syntax->variant.args.data[i], ctx, a), &nodes);
        }
        
        if (syntax->variant.args.len > 0) {
            out = mk_paren_doc("(", ")",  mv_sep_doc(nodes, a), a);
        } else {
            out = mv_sep_doc(nodes, a);
        }
        break;
    }
    case SMatch: {
        // Nodes = (match¹ val² expr expr ... )³, hence len + 3
        PtrArray nodes = mk_ptr_array(syntax->match.clauses.len + 3, a);

        push_ptr(mk_str_doc(mv_string("(match"), a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->match.val, ctx, a), &nodes);

        for (size_t i = 0; i < syntax->match.clauses.len; i++) {
            SynClause* clause = syntax->match.clauses.data[i];
            
            // the 4 comes from (, pattern, body, )
            PtrArray clause_nodes = mk_ptr_array(4, a);
            push_ptr(mk_str_doc(mv_string("("), a), &clause_nodes);
            
            PtrArray ptn_nodes = mk_ptr_array(4 + clause->vars.len, a);
            push_ptr(mk_str_doc(mv_string("[:"), a), &ptn_nodes);
            push_ptr(mk_str_doc(symbol_to_string(clause->tagname, a), a), &ptn_nodes);
            for (size_t j = 0; j < clause->vars.len; j++) {
                push_ptr(mk_str_doc(symbol_to_string(clause->vars.data[j], a), a), &ptn_nodes);
            }
            push_ptr(mk_str_doc(mv_string("]"), a), &ptn_nodes);

            push_ptr(mv_sep_doc(ptn_nodes, a), &clause_nodes);
            push_ptr(pretty_syntax_internal(clause->body, ctx, a), &clause_nodes);
            push_ptr(mk_str_doc(mv_string(")"), a), &clause_nodes);

            push_ptr(mv_cat_doc(clause_nodes, a), &nodes);
        }

        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case SStructure: {
        PtrArray nodes = mk_ptr_array(2 + syntax->structure.fields.len, a);
        push_ptr(mv_style_doc(former_style, mv_cstr_doc("struct", a), a), &nodes);

        for (size_t i = 0; i < syntax->structure.fields.len; i++) {
            PtrArray field_nodes = mk_ptr_array(2, a);

            push_ptr(mv_style_doc(field_style, mk_str_doc(symbol_to_string(syntax->structure.fields.data[i].key, a), a), a), &field_nodes);
            push_ptr(pretty_syntax_internal(syntax->structure.fields.data[i].val, ctx, a), &field_nodes);

            Document* fdoc = mv_nest_doc(2, mv_group_doc(mk_paren_doc("[.", "]", mv_sep_doc(field_nodes, a), a), a), a);
            push_ptr(fdoc, &nodes);
        }
        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SProjector: {
        PtrArray nodes = mk_ptr_array(3, a);

        push_ptr(pretty_syntax_internal(syntax->projector.val, ctx, a), &nodes);
        push_ptr(mk_str_doc(mv_string("."), a), &nodes);
        push_ptr(mk_str_doc(symbol_to_string(syntax->projector.field, a), a), &nodes);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case SInstance: {
        PtrArray nodes = mk_ptr_array(4 + syntax->instance.fields.len, a);

        push_ptr(mk_str_doc(mv_string("instance "), a), &nodes);

        if (syntax->instance.params.len > 0) {
            const SymbolArray params = syntax->instance.params;
            PtrArray pnodes = mk_ptr_array(params.len, a);
            for (size_t i = 0; i < params.len; i++) {
                push_ptr(mk_str_doc(symbol_to_string(params.data[i], a), a), &pnodes);
            }
            push_ptr(mk_paren_doc("[", "]", mv_sep_doc(pnodes, a), a), &nodes);
        }

        if (syntax->instance.implicits.len > 0) {
            const SymPtrAssoc implicits = syntax->instance.implicits;
            PtrArray inodes = mk_ptr_array(implicits.len, a);
            for (size_t i = 0; i < implicits.len; i++) {
                if (implicits.data[i].val) {
                    PtrArray anodes = mk_ptr_array(2, a);
                    push_ptr(mk_str_doc(symbol_to_string(implicits.data[i].key, a), a), &anodes);
                    push_ptr(pretty_syntax_internal(implicits.data[i].val, ctx, a), &anodes);
                    push_ptr(mk_paren_doc("(", ")", mv_sep_doc(anodes, a), a), &inodes);
                } else {
                    push_ptr(mk_str_doc(symbol_to_string(implicits.data[i].key, a), a), &inodes);
                }
            }
            push_ptr(mk_paren_doc("{", "}", mv_sep_doc(inodes, a), a), &nodes);
        }

        push_ptr(pretty_syntax_internal(syntax->instance.constraint, ctx, a), &nodes);

        for (size_t i = 0; i < syntax->instance.fields.len; i++) {
            PtrArray fnodes = mk_ptr_array(2, a);
            push_ptr(mk_str_doc(symbol_to_string(syntax->instance.fields.data[i].key, a), a), &fnodes);
            push_ptr(pretty_syntax_internal(syntax->instance.fields.data[i].val, ctx, a), &fnodes);
            push_ptr(mk_paren_doc("[.", "]", mv_sep_doc(fnodes, a), a), &nodes);
        }

        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SDynamic: {
        PtrArray nodes = mk_ptr_array(3, a);

        push_ptr(mv_style_doc(former_style, mk_cstr_doc("dynamic", a), a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->dynamic, ctx, a), &nodes);
        out = mv_sep_doc(nodes, a);
        if (should_wrap) out = mk_paren_doc("(", ")", out, a);
        break;
    }
    case SDynamicUse: {
        PtrArray nodes = mk_ptr_array(2, a);
        push_ptr(mv_style_doc(former_style, mk_cstr_doc("use", a), a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->use, ctx, a), &nodes);
        out = mv_sep_doc(nodes, a);
        if (should_wrap) out = mk_paren_doc("(", ")", out, a);
        break;
    }
    case SDynamicSet: {
        PtrArray nodes = mk_ptr_array(3, a);
        push_ptr(mv_style_doc(former_style, mk_cstr_doc("modify", a), a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->dynamic_set.dynamic, ctx, a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->dynamic_set.new_val, ctx, a), &nodes);
        out = mv_sep_doc(nodes, a);
        if (should_wrap) out = mk_paren_doc("(", ")", out, a);
        break;
    }
    case SLet: {
        PtrArray nodes = mk_ptr_array(3 + syntax->let_expr.bindings.len, a);
        push_ptr(mv_style_doc(former_style, mk_cstr_doc("let", a), a), &nodes);
        for (size_t i = 0; i < syntax->let_expr.bindings.len; i++) {
            Symbol name = syntax->let_expr.bindings.data[i].key;
            Syntax* expr = syntax->let_expr.bindings.data[i].val;
            PtrArray let_nodes = mk_ptr_array(2, a);
            push_ptr(mk_str_doc(symbol_to_string(name, a), a), &let_nodes);
            push_ptr(pretty_syntax_internal(expr, ctx, a), &nodes);

            push_ptr(mv_nest_doc(4, mv_group_doc(mk_paren_doc("[", "]", mv_sep_doc(let_nodes, a), a), a), a), &nodes);
        }
        push_ptr(mv_nest_doc(2, pretty_syntax_internal(syntax->let_expr.body, ctx, a), a), &nodes);

        out = mv_sep_doc(nodes, a);
        if (should_wrap) out = mk_paren_doc("(", ")", out, a);
        break;
    }
    case SDynamicLet: {
        PtrArray nodes = mk_ptr_array(3 + syntax->let_expr.bindings.len, a);
        push_ptr(mv_style_doc(former_style, mk_cstr_doc("bind", a), a), &nodes);
        for (size_t i = 0; i < syntax->dyn_let_expr.bindings.len; i++) {
            DynBinding* bind = syntax->dyn_let_expr.bindings.data[i];
            PtrArray let_nodes = mk_ptr_array(2, a);
            push_ptr(pretty_syntax_internal(bind->var, ctx, a), &let_nodes);
            push_ptr(pretty_syntax_internal(bind->expr, ctx, a), &let_nodes);

            push_ptr(mv_nest_doc(4, mv_group_doc(mk_paren_doc("[", "]", mv_sep_doc(let_nodes, a), a), a), a), &nodes);
        }
        push_ptr(mv_nest_doc(2, pretty_syntax_internal(syntax->let_expr.body, ctx, a), a), &nodes);

        out = mv_sep_doc(nodes, a);
        if (should_wrap )out = mk_paren_doc("(", ")", out, a);
        break;
    }
    case SIf: {
        PtrArray nodes = mk_ptr_array(4, a);
        push_ptr(mv_style_doc(former_style, mk_cstr_doc("if", a), a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->if_expr.condition, ctx, a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->if_expr.true_branch, ctx, a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->if_expr.false_branch, ctx, a), &nodes);
        out = mv_sep_doc(nodes, a);
        if (should_wrap )out = mk_paren_doc("(", ")", out, a);
        break;
    }
    case SCond: {
        PtrArray nodes = mk_ptr_array(2 + syntax->cond.clauses.len, a);
        push_ptr(mv_style_doc(former_style, mk_cstr_doc("cond", a), a), &nodes);
        for (size_t i = 0; i < syntax->cond.clauses.len; i++) {
            PtrArray clause_nodes = mk_ptr_array(2, a);
            CondClause* clause = syntax->cond.clauses.data[i];
            push_ptr(pretty_syntax_internal(clause->condition, ctx, a), &nodes);
            push_ptr(pretty_syntax_internal(clause->branch, ctx, a), &nodes);
            push_ptr(mv_group_doc(mv_nest_doc(2, mk_paren_doc("[", "]", mv_sep_doc(clause_nodes, a), a), a), a), &nodes);
        }
        out = mv_sep_doc(nodes, a);
        if (should_wrap )out = mk_paren_doc("(", ")", out, a);
        break;
    }
    case SIs: {
        PtrArray nodes = mk_ptr_array(2, a);
        push_ptr(pretty_syntax_internal(syntax->is.val, ctx, a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->is.type, ctx, a), &nodes);
        out = mk_paren_doc("(is ", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SInTo: {
        PtrArray nodes = mk_ptr_array(2, a);
        push_ptr(pretty_syntax_internal(syntax->into.type, ctx, a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->into.val, ctx, a), &nodes);
        out = mk_paren_doc("(into ", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SOutOf: {
        PtrArray nodes = mk_ptr_array(2, a);
        push_ptr(pretty_syntax_internal(syntax->out_of.type, ctx, a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->out_of.val, ctx, a), &nodes);
        out = mk_paren_doc("(out-of ", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SName: {
        PtrArray nodes = mk_ptr_array(2, a);
        push_ptr(pretty_syntax_internal(syntax->name.type, ctx, a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->name.val, ctx, a), &nodes);
        out = mk_paren_doc("(name ", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SUnName: {
        out = mk_paren_doc("(unname ", ")", pretty_syntax_internal(syntax->unname, ctx, a), a);
        break;
    }
    case SWiden: {
        PtrArray nodes = mk_ptr_array(2, a);
        push_ptr(pretty_syntax_internal(syntax->widen.val, ctx, a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->widen.type, ctx, a), &nodes);
        out = mk_paren_doc("(widen ", ")", mv_nest_doc(2, mv_sep_doc(nodes, a), a), a);
        break;
    }
    case SNarrow: {
        PtrArray nodes = mk_ptr_array(2, a);
        push_ptr(pretty_syntax_internal(syntax->narrow.val, ctx, a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->narrow.type, ctx, a), &nodes);
        out = mk_paren_doc("(narrow ", ")", mv_nest_doc(2, mv_sep_doc(nodes, a), a), a);
        break;
    }
    case SSizeOf: {
        out = mk_paren_doc("(size-of ", ")", pretty_syntax_internal(syntax->size, ctx, a), a);
        break;
    }
    case SAlignOf: {
        out = mk_paren_doc("(align-of ", ")", pretty_syntax_internal(syntax->size, ctx, a), a);
        break;
    }
    case SOffsetOf: {
        PtrArray nodes = mk_ptr_array(2, a);
        push_ptr(mk_str_doc(symbol_to_string(syntax->offset_of.field, a), a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->offset_of.body, ctx, a), &nodes);
        out = mk_paren_doc("(offset-of ", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SModule: {
        out = mv_str_doc(mv_string("Pretty syntax not implemented for modules!"), a);
        break;
    }
    case SLabels: {
        PtrArray head_nodes = mk_ptr_array(3, a);
        push_ptr(mv_style_doc(former_style, mk_cstr_doc("labels", a), a), &head_nodes);
        push_ptr(mv_nest_doc(2, pretty_syntax_internal(syntax->labels.entry, ctx, a), a), &head_nodes);

        PtrArray nodes = mk_ptr_array(3, a);
        push_ptr(mv_group_doc(mv_sep_doc(head_nodes, a), a), &nodes);
        PtrArray label_terms = mk_ptr_array(syntax->labels.terms.len, a);
        for (size_t i = 0; i < syntax->labels.terms.len; i++) {
            PtrArray label_nodes = mk_ptr_array(3, a);
            SymPtrACell cell = syntax->labels.terms.data[i];
            SynLabelBranch* branch = cell.val;

            PtrArray arg_nodes = mk_ptr_array(branch->args.len, a);
            for (size_t i = 0; i < branch->args.len; i++) {
                Document* arg = mk_str_doc(symbol_to_string(branch->args.data[i].key, a), a);
                push_ptr(arg, &arg_nodes);
            }

            push_ptr(mv_style_doc(var_style, mk_str_doc(symbol_to_string(cell.key, a), a), a), &label_nodes);
            if (arg_nodes.len > 0) {
                push_ptr(mv_nest_doc(2, mk_paren_doc("[", "]", mv_sep_doc(arg_nodes, a), a), a), &label_nodes);
            }
            ctx.should_wrap = false;
            push_ptr(mv_nest_doc(2, pretty_syntax_internal(branch->body, ctx, a), a), &label_nodes);
            push_ptr(mk_paren_doc("[", "]", mv_hsep_doc(label_nodes, a), a), &label_terms);
        }
        push_ptr(mv_nest_doc(2, mv_vsep_doc(label_terms, a), a), &nodes);
        out = mv_sep_doc(nodes, a);
        if (should_wrap) out = mk_paren_doc("(", ")", out, a);
        break;
    }
    case SGoTo: {
        PtrArray nodes = mk_ptr_array(3, a);
        push_ptr(mv_style_doc(former_style, mk_cstr_doc("go-to", a), a), &nodes);
        push_ptr(mv_style_doc(var_style, mk_str_doc(symbol_to_string(syntax->go_to.label, a), a), a), &nodes);

        PtrArray args = mk_ptr_array(syntax->go_to.args.len, a);
        for (size_t i = 0; i < syntax->go_to.args.len; i++) {
            push_ptr(mv_nest_doc(2, pretty_syntax_internal(syntax->go_to.args.data[i], ctx, a), a), &args);
        }
        if (args.len > 0) {
            push_ptr(mv_sep_doc(args, a), &nodes);
        }
        out = mk_paren_doc("(", ")", mv_hsep_doc(nodes, a), a);
        break;
    }
    case SWithReset: {
        PtrArray nodes = mk_ptr_array(3, a);

        push_ptr(mk_str_doc(mv_string("(with-reset ["), a), &nodes);
        push_ptr(mk_str_doc(symbol_to_string(syntax->with_reset.point_sym, a), a), &nodes);
        push_ptr(mk_str_doc(mv_string("]"), a), &nodes);

        push_ptr(pretty_syntax_internal(syntax->with_reset.expr, ctx, a), &nodes);

        push_ptr(mk_str_doc(mv_string("[handler"), a), &nodes);
        push_ptr(mk_str_doc(symbol_to_string(syntax->with_reset.in_sym, a), a), &nodes);
        push_ptr(mk_str_doc(symbol_to_string(syntax->with_reset.cont_sym, a), a), &nodes);
        push_ptr(mk_str_doc(mv_string("]"), a), &nodes);

        push_ptr(pretty_syntax_internal(syntax->with_reset.handler, ctx, a), &nodes);
        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case SResetTo: {
        PtrArray nodes = mk_ptr_array(3, a);
        push_ptr(mk_str_doc(mv_string("(reset-to "), a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->reset_to.point, ctx, a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->reset_to.arg, ctx, a), &nodes);
        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case SSequence: {
        PtrArray nodes = mk_ptr_array(1 + syntax->sequence.elements.len, a);
        push_ptr(mv_style_doc(former_style, mv_cstr_doc("seq", a), a), &nodes);
        for (size_t i = 0; i < syntax->sequence.elements.len; i++) {
            SeqElt* elt = syntax->sequence.elements.data[i];
            if (elt->is_binding) {
                PtrArray let_nodes = mk_ptr_array(3, a);
                push_ptr(mv_style_doc(former_style, mv_cstr_doc("let!", a), a), &let_nodes);
                push_ptr(mv_style_doc(var_style, mk_str_doc(symbol_to_string(elt->symbol, a), a), a), &let_nodes);
                push_ptr(pretty_syntax_internal(elt->expr, ctx, a), &let_nodes);

                push_ptr(mv_group_doc(mk_paren_doc("[", "]", mv_nest_doc(2, mv_sep_doc(let_nodes, a), a), a), a), &nodes);
            } else {
                push_ptr(pretty_syntax_internal(elt->expr, ctx, a), &nodes);
            }
        }
        out = mk_paren_doc("(", ")", mv_nest_doc(2, mv_sep_doc(nodes, a), a), a);
        break;
    }
    case SProcType: {
        PtrArray head_nodes = mk_ptr_array(4, a) ;
        push_ptr(mv_style_doc(ty_former_style, mv_str_doc(mk_string("Proc", a), a), a), &head_nodes);
        
        PtrArray arg_nodes = mk_ptr_array(syntax->proc_type.args.len, a);
        for (size_t i = 0; i < syntax->proc_type.args.len ; i++)  {
            push_ptr(pretty_syntax_internal(syntax->proc_type.args.data[i], ctx, a), &arg_nodes);
        }
        push_ptr(mk_paren_doc("[", "]", mv_sep_doc(arg_nodes, a), a), &head_nodes);

        PtrArray nodes = mk_ptr_array(4, a) ;
        push_ptr(mv_group_doc(mv_sep_doc(head_nodes, a), a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->proc_type.return_type, ctx, a), &nodes);
        out = mv_sep_doc(nodes, a);
        if (should_wrap) out = mk_paren_doc("(", ")", out, a);
        break;
    }
    case SStructType: {
        PtrArray head_nodes = mk_ptr_array(2, a) ;
        push_ptr(mv_style_doc(ty_former_style, mv_str_doc(mk_string("Struct", a), a), a), &head_nodes);
        if (syntax->struct_type.packed) {
            push_ptr(mv_str_doc(mk_string("packed", a), a), &head_nodes);
        }
        
        PtrArray field_nodes = mk_ptr_array(syntax->struct_type.fields.len, a) ;
        for (size_t i = 0; i < syntax->struct_type.fields.len ; i++)  {
            PtrArray fnodes = mk_ptr_array(4, a);
            PtrArray field_desc = mk_ptr_array(2, a);
            push_ptr(mk_str_doc(mv_string("."), a), &field_desc);
            push_ptr(mk_str_doc(symbol_to_string(syntax->struct_type.fields.data[i].key, a), a), &field_desc);
            push_ptr(mk_cat_doc(field_desc, a), &fnodes);

            push_ptr(pretty_syntax_internal(syntax->struct_type.fields.data[i].val, ctx, a), &fnodes);

            push_ptr(mv_group_doc(mk_paren_doc("[", "]", mv_sep_doc(fnodes, a), a), a), &field_nodes);
        }
        PtrArray nodes = mk_ptr_array(syntax->struct_type.fields.len + 2, a) ;
        push_ptr(mv_group_doc(mv_sep_doc(head_nodes, a), a), &nodes);
        push_ptr(mv_nest_doc(2, mv_sep_doc(field_nodes, a), a), &nodes);
        out = mv_sep_doc(nodes, a);
        if (should_wrap) out = mk_paren_doc("(", ")", out, a);
        break;
    }
    case SEnumType: {
        PtrArray head_nodes = mk_ptr_array(syntax->enum_type.variants.len + 2, a);
        push_ptr(mv_style_doc(former_style, mv_str_doc(mk_string("Enum", a), a), a), &head_nodes);
        push_ptr(mv_style_doc(const_style, pretty_u8(syntax->enum_type.tag_size, a), a), &head_nodes);

        PtrArray nodes = mk_ptr_array(syntax->enum_type.variants.len + 1, a);
        push_ptr(mv_group_doc(mv_sep_doc(head_nodes, a), a), &nodes);
        
        for (size_t i = 0; i < syntax->enum_type.variants.len ; i++)  {
            PtrArray fnodes = mk_ptr_array(4, a);
            {
                PtrArray snodes = mk_ptr_array(2, a);
                push_ptr(mk_str_doc(mv_string(":"), a), &snodes);
                push_ptr(mk_str_doc(symbol_to_string(syntax->enum_type.variants.data[i].key, a), a), &snodes);

                Document* symdoc = mv_style_doc(field_style, mv_cat_doc(snodes, a), a);
                push_ptr(symdoc, &fnodes);
            }

            PtrArray* args = syntax->enum_type.variants.data[i].val;
            PtrArray anodes = mk_ptr_array(args->len, a);
            for (size_t j = 0; j < args->len; j++) {
                push_ptr(pretty_syntax_internal(args->data[j], ctx, a), &anodes);
            }
            if (anodes.len > 0) {
                push_ptr(mv_nest_doc(2, mv_sep_doc(anodes, a), a),  &fnodes);
                push_ptr(mv_nest_doc(2, mv_group_doc(mk_paren_doc("[","]", mv_sep_doc(fnodes, a), a), a), a), &nodes);
            } else {
                push_ptr(mv_nest_doc(2, mv_cat_doc(fnodes, a), a), &nodes);
            }

        }

        out = mv_sep_doc(nodes, a);
        if (should_wrap) out = mk_paren_doc("(", ")", out, a);
        break;
    }
    case SResetType: {
        PtrArray nodes = mk_ptr_array(4, a) ;
        push_ptr(mv_str_doc(mk_string("(Reset ", a), a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->reset_type.in, ctx, a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->reset_type.out, ctx, a), &nodes);
        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case SDynamicType: {
        PtrArray nodes = mk_ptr_array(2, a) ;
        push_ptr(mk_str_doc(mv_string("Dynamic"), a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->dynamic_type, ctx, a), &nodes);
        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SNamedType: {
        PtrArray nodes = mk_ptr_array(2, a) ;
        push_ptr(mk_str_doc(mv_string("Named"), a), &nodes);
        push_ptr(mk_str_doc(symbol_to_string(syntax->named_type.name, a), a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->named_type.body, ctx, a), &nodes);
        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SDistinctType: {
        PtrArray nodes = mk_ptr_array(2, a) ;
        push_ptr(mk_str_doc(mv_string("Distinct"), a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->distinct_type, ctx, a), &nodes);
        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SOpaqueType: {
        PtrArray nodes = mk_ptr_array(2, a) ;
        push_ptr(mk_str_doc(mv_string("Opaque"), a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->opaque_type, ctx, a), &nodes);
        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case STraitType: {
        PtrArray nodes = mk_ptr_array(2 + syntax->trait.fields.len, a) ;
        push_ptr(mk_str_doc(mv_string("Trait "), a), &nodes);

        PtrArray pnodes = mk_ptr_array(syntax->trait.vars.len, a) ;
        for (size_t i = 0; i < syntax->trait.vars.len; i++) {
            push_ptr(mk_str_doc(symbol_to_string(syntax->trait.vars.data[i], a), a), &pnodes);
        }
        push_ptr(mk_paren_doc("[", "]", mv_sep_doc(pnodes, a), a), &nodes);

        for (size_t i = 0; i < syntax->trait.fields.len ; i++)  {
            PtrArray fnodes = mk_ptr_array(2, a);
            push_ptr(mk_str_doc(symbol_to_string(syntax->trait.fields.data[i].key, a), a), &fnodes);
            push_ptr(pretty_syntax_internal(syntax->trait.fields.data[i].val, ctx, a), &fnodes);
            push_ptr(mk_paren_doc("[.", "]", mv_sep_doc(fnodes, a), a), &nodes);
        }

        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SAllType: {
        PtrArray nodes = mk_ptr_array(5, a) ;
        push_ptr(mk_str_doc(mv_string("(All ["), a), &nodes);

        SymbolArray arr = syntax->bind_type.bindings;
        PtrArray arg_nodes = mk_ptr_array(arr.len, a);
        for (size_t i = 0; i < arr.len; i++) {
            push_ptr(mk_str_doc(symbol_to_string(arr.data[i], a), a), &arg_nodes);
        }
        push_ptr(mv_sep_doc(arg_nodes, a), &nodes);
        push_ptr(mk_str_doc(mv_string("]"), a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->bind_type.body, ctx, a), &nodes);
        push_ptr(mk_str_doc(mv_string(")"), a), &nodes);
        out = mv_sep_doc(nodes, a);
        break;
    }
    case SSealedType: {
        PtrArray nodes = mk_ptr_array(4, a) ;
        push_ptr(mv_style_doc(former_style, mk_str_doc(mv_string("Sealed"), a), a), &nodes);

        SymbolArray arr = syntax->sealed_type.vars;
        PtrArray arg_nodes = mk_ptr_array(arr.len, a);
        for (size_t i = 0; i < arr.len; i++) {
            push_ptr(mv_style_doc(var_style, mk_str_doc(symbol_to_string(arr.data[i], a), a), a), &arg_nodes);
        }
        push_ptr(mk_paren_doc("[", "]", mv_sep_doc(arg_nodes, a), a), &nodes);

        PtrArray impls = syntax->sealed_type.implicits;
        PtrArray impl_nodes = mk_ptr_array(impls.len, a);
        for (size_t i = 0; i < impls.len; i++) {
            push_ptr(pretty_syntax_internal(impls.data[i], ctx, a), &impl_nodes);
        }
        push_ptr(mk_paren_doc("{", "}", mv_sep_doc(impl_nodes, a), a), &nodes);

        push_ptr(pretty_syntax_internal(syntax->sealed_type.body, ctx, a), &nodes);
        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case STypeFamily: {
        PtrArray nodes = mk_ptr_array(4, a) ;
        push_ptr(mk_str_doc(mv_string("Family"), a), &nodes);

        SymbolArray arr = syntax->bind_type.bindings;
        PtrArray arg_nodes = mk_ptr_array(arr.len, a);
        for (size_t i = 0; i < arr.len; i++) {
            push_ptr(mk_str_doc(symbol_to_string(arr.data[i], a), a), &arg_nodes);
        }
        push_ptr(mk_paren_doc("[", "]", mv_sep_doc(arg_nodes, a), a), &nodes);

        push_ptr(pretty_syntax_internal(syntax->bind_type.body, ctx, a), &nodes);
        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SLiftCType: {
        PtrArray nodes = mk_ptr_array(2, a) ;
        push_ptr(mk_str_doc(mv_string("C-Type"), a), &nodes);

        push_ptr(pretty_syntax_internal(syntax->c_type, ctx, a), &nodes);
        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SCheckedType: {
        Document* doc_ty = pretty_type(syntax->type_val, a);
        out = mk_paren_doc("<checked-type: ", ">", doc_ty, a);
        break;
    }
    case SAnnotation: {
        out = mv_str_doc(mk_string("Pretty syntax not implemented for annotation!", a), a);
        break;
    }
    case SReinterpret: {
        PtrArray nodes = mk_ptr_array(4, a);
        if (syntax->reinterpret.from_native) {
            push_ptr(mk_str_doc(mv_string("reinterpret-native"), a), &nodes);
        } else {
            push_ptr(mk_str_doc(mv_string("reinterpret-relic"), a), &nodes);
        }

        push_ptr(pretty_syntax_internal(syntax->reinterpret.type, ctx, a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->reinterpret.body, ctx, a), &nodes);

        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SConvert: {
        PtrArray nodes = mk_ptr_array(4, a);
        if (syntax->convert.from_native) {
            push_ptr(mk_str_doc(mv_string("convert-native"), a), &nodes);
        } else {
            push_ptr(mk_str_doc(mv_string("convert-relic"), a), &nodes);
        }

        push_ptr(pretty_syntax_internal(syntax->convert.type, ctx, a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->convert.body, ctx, a), &nodes);

        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case STypeOf: {
        PtrArray nodes = mk_ptr_array(4, a);
        push_ptr(mk_str_doc(mv_string("type-of"), a), &nodes);
        push_ptr(pretty_syntax_internal(syntax->type_of, ctx, a), &nodes);
        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SDescribe: {
        PtrArray nodes = mk_ptr_array(4, a);
        push_ptr(mk_str_doc(mv_string("describe"), a), &nodes);
        PtrArray path_nodes = mk_ptr_array(syntax->to_describe.len, a);
        for (size_t i = 0; i < syntax->to_describe.len; i++) {
            push_ptr(mk_str_doc(symbol_to_string(syntax->to_describe.data[i], a), a), &path_nodes);
            if (i + 1 != syntax->to_describe.len)
                push_ptr(mk_str_doc(mv_string("."), a), &path_nodes);
        }
        push_ptr(mv_cat_doc(path_nodes, a), &nodes);
        out = mk_paren_doc("(", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    case SQuote: {
        Document* raw = pretty_rawtree(syntax->quoted, a);
        out = mk_paren_doc("(quote ", ")", mv_nest_doc(2, raw, a), a);
        break;
    }
    case SCapture: {
        RawTree raw = (RawTree) {
          .type = RawAtom,
          .atom = (Atom) {
              .type = ACapture,
              .capture = (Capture) {
                  .type = syntax->capture.type,
                  .value = syntax->capture.value,
              }
          },
        };
        Document* raw_doc = pretty_rawtree(raw, a);
        out = mk_paren_doc("(capture ", ")", mv_nest_doc(2, raw_doc, a), a);
        break;
    }
    case SDevAnnotation: {
        Document* raw = pretty_syntax(syntax->dev.inner, a);
        PtrArray nodes = mk_ptr_array(2, a);
        DevFlag flags = syntax->dev.flags;
        {
            // Breakpoints
            PtrArray bp_nodes = mk_ptr_array(4, a);
            push_ptr(mv_cstr_doc(".break", a), &bp_nodes);
            if (flags & DBAbstract) push_ptr(mv_cstr_doc(":abstraction", a), &bp_nodes);
            if (flags & DBTypecheck) push_ptr(mv_cstr_doc(":typecheck", a), &bp_nodes);
            if (flags & DBGenerate) push_ptr(mv_cstr_doc(":codegen", a), &bp_nodes);
            if (bp_nodes.len == 1) push_ptr(mv_cstr_doc(":none", a), &bp_nodes);
            push_ptr(mv_group_doc(mv_nest_doc(2, mk_paren_doc("[", "]", mv_sep_doc(bp_nodes, a), a), a), a), &nodes);
        }

        {
            // Diagnostic Printing
            PtrArray dp_nodes = mk_ptr_array(4, a);
            push_ptr(mv_cstr_doc(".print", a), &dp_nodes);
            if (flags & DPAbstract) push_ptr(mv_cstr_doc(":abstraction", a), &dp_nodes);
            if (flags & DPTypecheck) push_ptr(mv_cstr_doc(":typecheck", a), &dp_nodes);
            if (flags & DPGenerate) push_ptr(mv_cstr_doc(":codegen", a), &dp_nodes);
            if (dp_nodes.len == 1) push_ptr(mv_cstr_doc(":none", a), &dp_nodes);
            push_ptr(mv_group_doc(mv_nest_doc(2, mk_paren_doc("[", "]", mv_sep_doc(dp_nodes, a), a), a), a), &nodes);
        }

        push_ptr(mv_nest_doc(2, raw, a), &nodes);
        out = mk_paren_doc("(dev ", ")", mv_sep_doc(nodes, a), a);
        break;
    }
    }

    if (out == NULL)  {
        panic(mv_string("Internal Error in pretty_syntax_internal: Unknown syntax Type"));
    }
    out = mv_group_doc(out, a);
    return out;
}

Document* pretty_syntax(Syntax* syntax, Allocator* a) {
    DocStyle text_style = scolour(colour(195, 195, 209), dstyle);
    PrettyContext ctx = {
        .should_wrap = true,
    };
    return mv_style_doc(text_style, pretty_syntax_internal(syntax, ctx, a), a);
}

Document* pretty_def(Definition* def, Allocator* a) {
    PtrArray nodes = mk_ptr_array(3, a);
    push_ptr(mv_str_doc(mk_string("def", a), a), &nodes);
    push_ptr(mk_str_doc(symbol_to_string(def->bind, a), a), &nodes);
    push_ptr(mv_nest_doc(2, pretty_syntax(def->value, a), a), &nodes);
    return mk_paren_doc("(", ")", mv_hsep_doc(nodes, a), a);
}

Document* pretty_decl(Declaration* decl, Allocator* a) {
    PtrArray nodes = mk_ptr_array(3, a);
    push_ptr(mv_str_doc(mk_string("declare", a), a), &nodes);
    push_ptr(mk_str_doc(symbol_to_string(decl->bind, a), a), &nodes);
    for (size_t i = 0; i < decl->decls.len; i++) {
        ModuleDecl* dec = decl->decls.data[i];
        PtrArray prop_nodes = mk_ptr_array(2, a);
        switch (dec->sort) {
        case DeclType:
            push_ptr(mv_cstr_doc("type", a), &prop_nodes);
            push_ptr(pretty_type(dec->type, a), &prop_nodes);
            break;
        }
        push_ptr(mk_paren_doc("[.", "]", mv_hsep_doc(nodes, a), a), &nodes);
    }
    return mk_paren_doc("(", ")", mv_hsep_doc(nodes, a), a);
}

Document* pretty_toplevel(TopLevel* toplevel, Allocator* a) {
    Document* out = NULL;
    switch (toplevel->type) {
    case TLDef:
        out = pretty_def(&toplevel->def, a);
        break;
    case TLDecl:
        out = pretty_decl(&toplevel->decl, a);
        break;
    case TLImport: {
        PtrArray docs = mk_ptr_array(toplevel->import.clauses.len, a);
        for (size_t i = 0; i < toplevel->import.clauses.len; i++) {
            push_ptr(pretty_import_clause(toplevel->import.clauses.data[i], a), &docs);
        }
        out = mk_paren_doc("(import ", ")", mv_sep_doc(docs, a), a);
        break;
    }
    case TLExpr:
        out = pretty_syntax(toplevel->expr, a);
        break;
    }
    return out;
}

PiType* toplevel_type(TopLevel top) {
    PiType* out = NULL;
    switch (top.type) {
    case TLExpr:
        out = top.expr->ptype;
        break;
    case TLDef:
        out = top.def.value->ptype;
        break;
    case TLDecl:
        out = NULL;
        break;
    case TLImport:
        out = NULL;
        break;
    }
    return out;
}
