#include "platform/signals.h"
#include "rune/analysis/elaboration.h"


static void elaborate_rune_to(RExpr rich, RExprPool* rpool, ExprPool* pool, RegionAllocator* region, PiErrorPoint* point, Expr* out);
static ExprOption elaborate_rune_opt_expr(RExprOption rich, RExprPool* rpool, ExprPool* pool, RegionAllocator* region, PiErrorPoint* point);

/**
 * Elaboate the richer surface syntax into the kernel representation. Key differences:
 * • The kernel is call by push value
 * • The kernel contains fewer types & terms:
 *   - Functions / Application become codata.
 *   - Records beccome data with a single constructor.
 *   - CoData become a corecord with a single eliminator. 
 */
ExprRef elaborate_rune_expr(RExprRef rich, RExprPool* rpool, ExprPool* pool, RegionAllocator* region, PiErrorPoint* point) {
    ExprRef ref = new_expr(pool);
    Expr expr = {};
    elaborate_rune_to(get_rexpr(rich, rpool), rpool, pool, region, point, &expr);
    set_expr(ref, expr, pool);
    return ref;
}

Def elaborate_rune_def(RDef rich, RExprPool* rpool, ExprPool* pool, RegionAllocator* region, PiErrorPoint* point) {
    ExprRef ref = elaborate_rune_expr(rich.expr, rpool, pool, region, point);
    return (Def) {
        .name = rich.name,
        .expr = ref,
    };
}

static ExprOption elaborate_rune_opt_expr(RExprOption rich, RExprPool* rpool, ExprPool* pool, RegionAllocator* region, PiErrorPoint* point) {
    ExprOption out = {.type = None};
    if (rich.type == Some)  {
        out = (ExprOption) {
            .type = Some,
            .val = elaborate_rune_expr(rich.val, rpool, pool, region, point),
        };
    }
    return out;
}

void elaborate_rune_to(RExpr rich, RExprPool* rpool, ExprPool* pool, RegionAllocator* region, PiErrorPoint* point, Expr* out) {
    Allocator a = ra_to_gpa(region);
    switch (rich.type) {
    case REVar:
        *out = (Expr) {
            .type = EVar,
            .var = rich.var,
        };
        break;
    case REFn:
        *out = (Expr) {
            .type = EFn,
            .fn.args = rich.fn.args,
            .fn.body = elaborate_rune_expr(rich.fn.body, rpool, pool, region, point),
        };
        break;
    case REApp: {
        ExprSlice args = new_expr_slice(rich.app.args.len, pool);
        for (size_t i = 0; i < rich.app.args.len; i++) {
            Expr out;
            RExpr in = get_rexpr(get_rexpr_elt(rich.app.args, i), rpool);
            elaborate_rune_to(in, rpool, pool, region, point, &out);
            set_expr_elt(args, i, out, pool);
        }
        *out = (Expr) {
            .type = EApp,
            .app.fn = elaborate_rune_expr(rich.app.fn, rpool, pool, region, point),
            .app.args = args,
        };
        break;
    }
    case RECtor: {
        *out = (Expr) {
            .type = ECtor,
            .ctor.name = rich.ctor.name,
            .ctor.type = elaborate_rune_opt_expr(rich.ctor.type, rpool, pool, region, point),
        };
        break;
    }
    case RECase:
        panic(mv_string("TODO: case elaboration"));
        break;
    case RECoRecord:
        panic(mv_string("TODO: corecord elaboration"));
        break;
    case REProject:
        panic(mv_string("TODO: project elaboration"));
        break;
    case RERecur:
        panic(mv_string("TODO: recur elaboration"));
        break;

    // Rich types/values - need lowering
    // Types
    case RERecord: {
        if (rich.record.type.type != Some) {
            PicoError err = {
                .message = mv_cstr_doc("Records require a type when dynamic mode.", &a),
            };
            throw_pi_error(point, err);
        }
        ExprRef type = elaborate_rune_expr(rich.record.type.val, rpool, pool, region, point);

        ExprMap fields = new_expr_map(rich.record.fields.len, pool);
        for (size_t i = 0; i < rich.record.fields.len; i++) {
            RExprCell cell = get_rexpr_map_elt(rich.record.fields, i, rpool);
            ExprCell out = {.name = cell.name};
            elaborate_rune_to(cell.expr, rpool, pool, region, point, &out.expr);
            set_expr_map_elt(fields, i, out, pool);
        }
        *out = (Expr) {
            .type = ERecord,
            .record.type = type,
            .record.fields = fields,
        };
        break;
    }
    case RECoData:
        panic(mv_string("TODO: codata elaboration"));
        break;

        // Embedded Values
    case REVal:
        *out = (Expr) {
            .type = EVal,
            .value = rich.value,
        };
        break;

    case REDataType:
        panic(mv_string("TODO: re data type elaboration"));
        break;
    case RECoDataType:
        panic(mv_string("TODO: re codata type elaboration"));
        break;
    case RERecordType: {
        if (rich.record_ty.type.type != None) {
            panic(mv_string("TODO: Record Type with set universe"));
        }
        ExprNames names = new_expr_names(rich.record_ty.fields.len, pool);
        AlgebraicTypeSpec spec = new_alg_spec(1, pool);
        AlgebraOp op = {
            .name = string_to_name(mv_string("record")),
            .args = new_expr_map(names.len, pool),
        };
        set_alg_spec_op(spec, 0, op, pool);
        for (size_t i = 0; i < rich.record_ty.fields.len; i++) {
            RExprCell cell = get_rexpr_map_elt(rich.record_ty.fields, i, rpool);
            set_expr_name(names, i, cell.name, pool);
            ExprCell new_cell = {.name = cell.name,};
            elaborate_rune_to(cell.expr, rpool, pool, region, point, &new_cell.expr);
            set_expr_map_elt(op.args, i, new_cell, pool);
        }
        *out = (Expr) {
          .type = EDataType,
          .data_ty = {
              .type = {.type = None},
              .is_record = true,
              .names = names,
              .constructors = spec,
          },
        };
        break;
    }
    case RECoRecordType:
        panic(mv_string("TODO: re corecord type elaboration"));
        break;

    case REInt:
        *out = (Expr) {
            .type = EInt,
            .num = rich.num,
        };
        break;
    case REString:
        *out = (Expr) {
            .type = EString,
            .string = rich.string,
        };
        break;
    case REList: {
        ExprSlice args = new_expr_slice(rich.list.len, pool);
        for (size_t i = 0; i < rich.list.len; i++) {
            Expr out;
            RExpr in = get_rexpr(get_rexpr_elt(rich.list, i), rpool);
            elaborate_rune_to(in, rpool, pool, region, point, &out);
            set_expr_elt(args, i, out, pool);
        }
        *out = (Expr) {
            .type = EList,
            .list = args,
        };
        break;
    }
    }
}

