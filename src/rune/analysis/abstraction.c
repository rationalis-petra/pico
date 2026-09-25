#include "platform/signals.h"

#include "pico/abstraction/helpers.h"
#include "rune/analysis/abstraction.h"

static ExprRef mk_fn_expr(RawTree raw, HostCallbackData host_data, ExprPool* pool, RegionAllocator* region, PiErrorPoint* point);
static ExprRef mk_app_expr(RawTree raw, HostCallbackData host_data, ExprPool* pool, RegionAllocator* region, PiErrorPoint* point);

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

ExprRef abstract_rune_expr(RawTree raw, HostCallbackData host_data, ExprPool* pool, RegionAllocator* region, PiErrorPoint* point) {
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
            return mk_fn_expr(raw, host_data, pool, region, point);
        } else {
            if (is_symbol(head)) {
                for (size_t i = 0; i < host_data.names.len; i++) {
                    if (host_data.names.data[i] == head.atom.symbol.name) {
                        ExprRef eref = new_expr(pool);
                        HostRef ref = host_data.abstract(raw, host_data, pool, region, point);
                        Expr expr = {
                            .type = EHost,
                            .host = ref,
                        };
                        set_expr(eref, expr, pool);
                        return eref;
                    }
                }
            }
            // No matches locally; must be an application
            return mk_app_expr(raw, host_data, pool, region, point);
        }
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
            Expr sym = {
                .type = EVar,
                .var = raw.atom.symbol.name,
            };
            ExprRef ref = new_expr(pool);
            set_expr(ref, sym, pool);
            return ref;
            break;
        }
        case AString:
            panic(mv_string("TODO: string interpolation"));
            break;
        case ACapture:
            panic(mv_string("It should not be possible for captures to mainfest in rune."));
            break;
        }
        panic(mv_string("Invalid atom type provided to rune."));
    }
    }
    panic(mv_string("Invalid raw syntax provided to rune."));
}


static ExprRef mk_fn_expr(RawTree raw, HostCallbackData host_data, ExprPool* pool, RegionAllocator* region, PiErrorPoint* point) {
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
    ExprRef fn = new_expr(pool);
    NameArray arr; 
    if (!get_name_list(&arr, raw_args, a)) {
        PicoError err = {
            .range = raw.range,
            .message = mv_cstr_doc("The 'fn' argument list is malformed. Functions should look like (fn [args] expr‥).", a),
        };
        throw_pi_error(point, err);
    }
    ExprRef body = abstract_rune_expr(raw_body, host_data, pool, region, point);
    Expr fne = {
        .type = EFn,
        .fn.args = arr,
        .fn.body = body,
    };
    set_expr(fn, fne, pool);
    return fn;
}

static ExprRef mk_app_expr(RawTree raw, HostCallbackData host_data, ExprPool* pool, RegionAllocator* region, PiErrorPoint* point) {
    Allocator ra = ra_to_gpa(region);
    Allocator* a = &ra;

    RawTree head = raw.branch.nodes.data[0];
    ExprRef app = new_expr(pool);
    ExprRef fn = abstract_rune_expr(head, host_data, pool, region, point);

    ExprArray args = mk_expr_array(raw.branch.nodes.len - 1, a);
    for (size_t i = 1; i < raw.branch.nodes.len; i++) {
        push_expr(abstract_rune_expr(raw.branch.nodes.data[i], host_data, pool, region, point), &args);
    }
    Expr appe = {
        .type = EApp,
        .app.fn = fn,
        .app.args = args,
    };
    set_expr(app, appe, pool);
    return app;
}

