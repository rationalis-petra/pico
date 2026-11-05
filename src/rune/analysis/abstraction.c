#include "platform/signals.h"

#include "pico/abstraction/helpers.h"
#include "rune/analysis/abstraction.h"
#include "rune/analysis/abstraction_errors.h"

// Functional Core
static void mk_fn_expr(RawTree raw, HostCallbackData host_data, RExprPool* pool, RegionAllocator* region, PiErrorPoint* point, RExpr* out);
static void mk_ctor_expr(RawTree raw, HostCallbackData host_data, RExprPool* pool, RegionAllocator* region, PiErrorPoint* point, RExpr* out);
static void mk_cocase_expr(RawTree raw, HostCallbackData host_data, RExprPool* pool, RegionAllocator* region, PiErrorPoint* point, RExpr* out);
static void mk_record_expr(RawTree raw, HostCallbackData host_data, RExprPool* pool, RegionAllocator* region, PiErrorPoint* point, RExpr* out);
static void mk_app_expr(RawTree raw, HostCallbackData host_data, RExprPool* pool, RegionAllocator* region, PiErrorPoint* point, RExpr* out);

// Type Constructors
static void mk_record_ty_expr(RawTree raw, HostCallbackData host_data, RExprPool* pool, RegionAllocator* region, PiErrorPoint* point, RExpr* out);

// Literals
static void mk_list_expr(RawTree raw, HostCallbackData host_data, RExprPool* pool, RegionAllocator* region, PiErrorPoint* point, RExpr* out);

RDef abstract_rune_def(RawTree raw, HostCallbackData host_data, RExprPool* pool, RegionAllocator* region, PiErrorPoint* point) {
    Allocator ra = ra_to_gpa(region);
    Allocator* a = &ra;
    if (raw.type != RawBranch || raw.branch.nodes.len < 3) {
        PicoError err = {
            .range = raw.range,
            .message = mv_cstr_doc("Definitions (toplevel values) are expected to have the form (def <name> expr‥).", a),
        };
        throw_pi_error(point, err);
    }
    RawTree head = raw.branch.nodes.data[0];
    if (!eq_symbol(&head, string_to_symbol(mv_string("def")))) {
        PicoError err = {
            .range = raw.range,
            .message = mv_cstr_doc("Definitions (toplevel values) are expected to have the form (def <name> expr‥).", a),
        };
        throw_pi_error(point, err);
    }

    RawTree name = raw.branch.nodes.data[1];
    if (!is_symbol(name)) {
      PicoError err = {
        .range = raw.range,
        .message = mv_cstr_doc("Definitions (toplevel values) are expected to have the form (def <name> expr‥)."
                               "However, the 'name' value is not a symbol.", a),
        };
        throw_pi_error(point, err);
    }

    RawTree expr = raw.branch.nodes.len == 3
        ? raw.branch.nodes.data[2]
        : raw_slice(&raw, 2);
    RExprRef ref = abstract_rune_expr(expr, host_data, pool, region, point);
    return (RDef) {
        .name = name.atom.symbol.name,
        .expr = ref 
    };
}

void abstract_rune_to(RawTree raw, HostCallbackData host_data, RExprPool* pool, RegionAllocator* region, PiErrorPoint* point, RExpr* out) {
    Allocator ra = ra_to_gpa(region);
    Allocator* a = &ra;
    switch (raw.type) {
    case RawBranch: {
        if (raw.branch.nodes.len < 1) {
            PicoError err = {
                .range = raw.range,
                .message = mv_cstr_doc("Composite terms like this require some arugments.", a),
            };
            throw_pi_error(point, err);
        }
        RawTree head = raw.branch.nodes.data[0];
        if (eq_symbol(&head, string_to_symbol(mv_string("fn")))) {
            mk_fn_expr(raw, host_data, pool, region, point, out);
        } else if (eq_symbol(&head, string_to_symbol(mv_string(":")))) {
            mk_ctor_expr(raw, host_data, pool, region, point, out);
        // } else if (eq_symbol(&head, string_to_symbol(mv_string(";")))) {
        //     mk_coctor_expr(raw, host_data, pool, region, point, out);
        } else if (eq_symbol(&head, string_to_symbol(mv_string("corecord")))) {
            mk_cocase_expr(raw, host_data, pool, region, point, out);
        } else if (eq_symbol(&head, string_to_symbol(mv_string("record")))) {
            mk_record_expr(raw, host_data, pool, region, point, out);
        } else if (eq_symbol(&head, string_to_symbol(mv_string("list")))) {
            mk_list_expr(raw, host_data, pool, region, point, out);

        // Types
        // TODO: Data, Codata, Corecord
        } else if (eq_symbol(&head, string_to_symbol(mv_string("Record")))) {
            mk_record_ty_expr(raw, host_data, pool, region, point, out);
        } else {
            if (is_symbol(head)) {
                for (size_t i = 0; i < host_data.names.len; i++) {
                    if (host_data.names.data[i] == head.atom.symbol.name) {
                        host_data.abstract(raw, host_data, pool, region, point, host_data.closure_data, out);
                        return;
                    }
                }
            }
            // No matches locally; must be an application
            mk_app_expr(raw, host_data, pool, region, point, out);
        }
        return;
        break;
    }
    case RawAtom: {
        switch (raw.atom.type) {
        case ABool: {
            PicoError err = {
                .range = raw.range,
                .message = mv_cstr_doc("Boolean literals are not (yet) supported by rune.", a),
            };
            throw_pi_error(point, err);
            break;
        }
        case AIntegral: {
            *out = (RExpr) {
                .type = REInt,
                .num = raw.atom.int_64,
            };
            return;
        }
        case AFloating: {
            PicoError err = {
                .range = raw.range,
                .message = mv_cstr_doc("Floating point literals are not supported by rune.", a),
            };
            throw_pi_error(point, err);
            break;
        }
        case ASymbol: {
            *out = (RExpr) {
                .type = REVar,
                .var = raw.atom.symbol.name,
            };
            return;
        }
        case AString: {
            *out = (RExpr) {
                .type = REString,
                .string = raw.atom.string,
            };
            return;
        }
        case ACapture:
            panic(mv_string("It should not be possible for captures to mainfest in rune."));
            break;
        }
        panic(mv_string("Invalid atom type provided to rune."));
    }
    }
    panic(mv_string("Invalid raw syntax provided to rune."));
}

RExprRef abstract_rune_expr(RawTree raw, HostCallbackData host_data, RExprPool* pool, RegionAllocator* region, PiErrorPoint* point) {
    RExprRef ref = new_rexpr(pool);
    RExpr expr;
    abstract_rune_to(raw, host_data, pool, region, point, &expr);
    set_rexpr(ref, expr, pool);
    return ref;
}


static void mk_fn_expr(RawTree raw, HostCallbackData host_data, RExprPool* pool, RegionAllocator* region, PiErrorPoint* point, RExpr* out) {
    Allocator ra = ra_to_gpa(region);
    Allocator* a = &ra;
    if (raw.branch.nodes.len < 3) {
        PicoError err = {
            .range = raw.range,
            .message = mv_cstr_doc("fn terms should look like (fn [args] expr‥).", a),
        };
        throw_pi_error(point, err);
    }
    RawTree raw_args = raw.branch.nodes.data[1];
    RawTree raw_body = raw.branch.nodes.len == 3
        ? raw.branch.nodes.data[2]
        : raw_slice(&raw, 2);
    NameArray arr; 
    if (!get_name_list(&arr, raw_args, a)) {
        PicoError err = {
            .range = raw.range,
            .message = mv_cstr_doc("The 'fn' argument list is malformed. Functions should look like (fn [args] expr‥).", a),
        };
        throw_pi_error(point, err);
    }
    RExprRef body = abstract_rune_expr(raw_body, host_data, pool, region, point);
    *out = (RExpr) {
        .type = REFn,
        .fn.args = arr,
        .fn.body = body,
    };
}

static void mk_ctor_expr(RawTree raw, HostCallbackData host_data, RExprPool* pool, RegionAllocator* region, PiErrorPoint* point, RExpr* out) {
    Allocator ra = ra_to_gpa(region);
    Allocator* a = &ra;
    if (raw.branch.nodes.len > 3 || raw.branch.nodes.len < 2) {
        PicoError err = {
            .range = raw.range,
            .message = mv_cstr_doc("Constructor terms should look like :constructor or Type:constructor.", a),
        };
        throw_pi_error(point, err);
    }
    Symbol name;
    if (!get_symbol(raw.branch.nodes.data[1], &name)) {
        PicoError err = {
            .range = raw.range,
            .message = mv_cstr_doc("Constructor terms should look like :constructor or Type:constructor.", a),
        };
        throw_pi_error(point, err);
    }
    RExprOption body = {.type = None};
    if (raw.branch.nodes.len == 3) {
      body = (RExprOption) {
          .type = Some,
          .val = abstract_rune_expr(raw.branch.nodes.data[2], host_data, pool, region, point),
      };
    }
    *out = (RExpr) {
        .type = RECtor,
        .ctor.name = name.name,
        .ctor.type = body,
    };
}

static void mk_cocase_expr(RawTree raw, HostCallbackData host_data, RExprPool* pool, RegionAllocator* region, PiErrorPoint* point, RExpr* out) {
    RExprOption body = {.type = None};
    size_t pad = 1;
    if (raw.branch.nodes.len > 1) {
        RawTree maybe_type = raw.branch.nodes.data[1];
        if (maybe_type.type != RawBranch || maybe_type.branch.hint == HExpression) {
            pad++;
            body = (RExprOption) {
                .type = Some,
                .val = abstract_rune_expr(maybe_type, host_data, pool, region, point),
            };
        }
    }

    RPatternMap fields = new_rpat_map(raw.branch.nodes.len - pad, pool);
    for (size_t i = 0; i < fields.len; i++) {
        RawTree fdesc = raw.branch.nodes.data[i + pad];
        if (fdesc.type != RawBranch) {
            record_bad_fdesc_type(fdesc, point, region);
        }

        if (fdesc.branch.nodes.len < 2) {
            record_bad_fdesc_len(fdesc, point, region);
        }

        Symbol field;
        if (!get_fieldname(&fdesc.branch.nodes.data[0], FDot, &field)) {
            record_bad_fdesc_fieldname(fdesc, point, region);
        }

        RawTree val_desc = fdesc.branch.nodes.len == 2 ? fdesc.branch.nodes.data[1] : raw_slice(&fdesc, 1); 
        RExprRef fexpr = abstract_rune_expr(val_desc, host_data, pool, region, point);
        RPatternCell cell = {
            .name = field.name,
            .pattern.body = fexpr,
        };
        set_rpat_map_elt(fields, i, cell, pool);
    }
    *out = (RExpr) {
        .type = RECoRecord,
        .corecord.type = body,
        .corecord.fields = fields,
    };
}

static void mk_record_expr(RawTree raw, HostCallbackData host_data, RExprPool* pool, RegionAllocator* region, PiErrorPoint* point, RExpr* out) {
    RExprOption body = {.type = None};
    size_t pad = 1;
    if (raw.branch.nodes.len > 1) {
        RawTree maybe_type = raw.branch.nodes.data[1];
        if (maybe_type.type != RawBranch || maybe_type.branch.hint == HExpression) {
            pad++;
            body = (RExprOption) {
                .type = Some,
                .val = abstract_rune_expr(maybe_type, host_data, pool, region, point),
            };
        }
    }

    RExprMap fields = new_rexpr_map(raw.branch.nodes.len - pad, pool);
    for (size_t i = 0; i < fields.len; i++) {
        RawTree fdesc = raw.branch.nodes.data[i + pad];
        if (fdesc.type != RawBranch) {
            record_bad_fdesc_type(fdesc, point, region);
        }

        if (fdesc.branch.nodes.len < 2) {
            record_bad_fdesc_len(fdesc, point, region);
        }

        Symbol field;
        if (!get_fieldname(&fdesc.branch.nodes.data[0], FDot, &field)) {
            record_bad_fdesc_fieldname(fdesc, point, region);
        }

        RawTree val_desc = fdesc.branch.nodes.len == 2 ? fdesc.branch.nodes.data[1] : raw_slice(&fdesc, 1);
        RExpr fexpr;
        abstract_rune_to(val_desc, host_data, pool, region, point, &fexpr);
        RExprCell cell = {
            .name = field.name,
            .expr = fexpr,
        };
        set_rexpr_map_elt(fields, i, cell, pool);
    }
    *out = (RExpr) {
        .type = RERecord,
        .record.type = body,
        .record.fields = fields,
    };
}

static void mk_app_expr(RawTree raw, HostCallbackData host_data, RExprPool* pool, RegionAllocator* region, PiErrorPoint* point, RExpr* out) {
    RawTree head = raw.branch.nodes.data[0];
    RExprRef fn = abstract_rune_expr(head, host_data, pool, region, point);

    RExprSlice args = new_rexpr_slice(raw.branch.nodes.len - 1, pool);
    for (size_t i = 1; i < raw.branch.nodes.len; i++) {
        RExpr out;
        abstract_rune_to(raw.branch.nodes.data[i], host_data, pool, region, point, &out);
        set_rexpr_elt(args, i - 1, out, pool);
    }
    *out = (RExpr) {
        .type = REApp,
        .app.fn = fn,
        .app.args = args,
    };
}

static void mk_list_expr(RawTree raw, HostCallbackData host_data, RExprPool* pool, RegionAllocator* region, PiErrorPoint* point, RExpr* out) {
    RExprSlice args = new_rexpr_slice(raw.branch.nodes.len - 1, pool);
    for (size_t i = 1; i < raw.branch.nodes.len; i++) {
        RExpr out;
        abstract_rune_to(raw.branch.nodes.data[i], host_data, pool, region, point, &out);
        set_rexpr_elt(args, i - 1, out, pool);
    }
    *out = (RExpr) {
        .type = REList,
        .list = args,
    };
}

static void mk_record_ty_expr(RawTree raw, HostCallbackData host_data, RExprPool* pool, RegionAllocator* region, PiErrorPoint* point, RExpr* out) {
    RExprOption body = {.type = None};
    size_t pad = 1;
    if (raw.branch.nodes.len > 1) {
        RawTree maybe_type = raw.branch.nodes.data[1];
        if (maybe_type.type != RawBranch || maybe_type.branch.hint == HExpression) {
            pad++;
            body = (RExprOption) {
                .type = Some,
                .val = abstract_rune_expr(maybe_type, host_data, pool, region, point),
            };
        }
    }

    RExprMap fields = new_rexpr_map(raw.branch.nodes.len - pad, pool);
    for (size_t i = 0; i < fields.len; i++) {
        RawTree fdesc = raw.branch.nodes.data[i + pad];
        if (fdesc.type != RawBranch) {
            record_bad_fdesc_type(fdesc, point, region);
        }

        if (fdesc.branch.nodes.len < 2) {
            record_bad_fdesc_len(fdesc, point, region);
        }

        Symbol field;
        if (!get_fieldname(&fdesc.branch.nodes.data[0], FDot, &field)) {
            record_bad_fdesc_fieldname(fdesc, point, region);
        }

        RawTree val_desc = fdesc.branch.nodes.len == 2 ? fdesc.branch.nodes.data[1] : raw_slice(&fdesc, 1);
        RExpr fexpr;
        abstract_rune_to(val_desc, host_data, pool, region, point, &fexpr);
        RExprCell cell = {
            .name = field.name,
            .expr = fexpr,
        };
        set_rexpr_map_elt(fields, i, cell, pool);
    }
    *out = (RExpr) {
        .type = RERecordType,
        .record_ty.type = body,
        .record_ty.fields = fields,
    };
}
