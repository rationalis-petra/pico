#include "platform/signals.h"

#include "pico/abstraction/helpers.h"
#include "rune/analysis/abstraction.h"

// Functional Core
static void mk_fn_expr(RawTree raw, HostCallbackData host_data, ExprPool* pool, RegionAllocator* region, PiErrorPoint* point, Expr* out);
static void mk_ctor_expr(RawTree raw, HostCallbackData host_data, ExprPool* pool, RegionAllocator* region, PiErrorPoint* point, Expr* out);
static void mk_app_expr(RawTree raw, HostCallbackData host_data, ExprPool* pool, RegionAllocator* region, PiErrorPoint* point, Expr* out);

// Literals
static void mk_list_expr(RawTree raw, HostCallbackData host_data, ExprPool* pool, RegionAllocator* region, PiErrorPoint* point, Expr* out);

Def abstract_rune_def(RawTree raw, HostCallbackData host_data, ExprPool* pool, RegionAllocator* region, PiErrorPoint* point) {
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
    ExprRef ref = abstract_rune_expr(expr, host_data, pool, region, point);
    return (Def) {
        .name = name.atom.symbol.name,
        .expr = ref 
    };
}

void abstract_rune_to(RawTree raw, HostCallbackData host_data, ExprPool* pool, RegionAllocator* region, PiErrorPoint* point, Expr* out) {
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
        } else if (eq_symbol(&head, string_to_symbol(mv_string("list")))) {
            mk_list_expr(raw, host_data, pool, region, point, out);
        } else {
            if (is_symbol(head)) {
                for (size_t i = 0; i < host_data.names.len; i++) {
                    if (host_data.names.data[i] == head.atom.symbol.name) {
                        host_data.abstract(raw, host_data, pool, region, point, host_data.closure_data, out);
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
            PicoError err = {
                .range = raw.range,
                .message = mv_cstr_doc("Intergral literals are not (yet) supported by rune.", a),
            };
            throw_pi_error(point, err);
            break;
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
            *out = (Expr) {
                .type = EVar,
                .var = raw.atom.symbol.name,
            };
            return;
        }
        case AString: {
            *out = (Expr) {
                .type = EString,
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

ExprRef abstract_rune_expr(RawTree raw, HostCallbackData host_data, ExprPool* pool, RegionAllocator* region, PiErrorPoint* point) {
    ExprRef ref = new_expr(pool);
    Expr expr;
    abstract_rune_to(raw, host_data, pool, region, point, &expr);
    set_expr(ref, expr, pool);
    return ref;
}


static void mk_fn_expr(RawTree raw, HostCallbackData host_data, ExprPool* pool, RegionAllocator* region, PiErrorPoint* point, Expr* out) {
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
    ExprRef body = abstract_rune_expr(raw_body, host_data, pool, region, point);
    *out = (Expr) {
        .type = EFn,
        .fn.args = arr,
        .fn.body = body,
    };
}

static void mk_ctor_expr(RawTree raw, HostCallbackData host_data, ExprPool* pool, RegionAllocator* region, PiErrorPoint* point, Expr* out) {
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
    ExprOption body = {.type = None};
    if (raw.branch.nodes.len == 3) {
      body = (ExprOption) {
          .type = Some,
          .val = abstract_rune_expr(raw.branch.nodes.data[2], host_data, pool, region, point),
      };
    }
    *out = (Expr) {
        .type = ECtor,
        .ctor.name = name.name,
        .ctor.type = body,
    };
}

static void mk_app_expr(RawTree raw, HostCallbackData host_data, ExprPool* pool, RegionAllocator* region, PiErrorPoint* point, Expr* out) {
    RawTree head = raw.branch.nodes.data[0];
    ExprRef fn = abstract_rune_expr(head, host_data, pool, region, point);

    ExprSlice args = new_expr_slice(raw.branch.nodes.len - 1, pool);
    for (size_t i = 1; i < raw.branch.nodes.len; i++) {
        Expr out;
        abstract_rune_to(raw.branch.nodes.data[i], host_data, pool, region, point, &out);
        set_expr_elt(args, i, out, pool);
    }
    *out = (Expr) {
        .type = EApp,
        .app.fn = fn,
        .app.args = args,
    };
}

static void mk_list_expr(RawTree raw, HostCallbackData host_data, ExprPool* pool, RegionAllocator* region, PiErrorPoint* point, Expr* out) {
    ExprSlice args = new_expr_slice(raw.branch.nodes.len - 1, pool);
    for (size_t i = 1; i < raw.branch.nodes.len; i++) {
        Expr out;
        abstract_rune_to(raw.branch.nodes.data[i], host_data, pool, region, point, &out);
        set_expr_elt(args, i, out, pool);
    }
    *out = (Expr) {
        .type = EList,
        .list = args,
    };
}
