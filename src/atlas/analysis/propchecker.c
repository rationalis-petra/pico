#include "platform/signals.h"
#include "data/meta/array_header.h"
#include "data/meta/array_impl.h"

#include "pico/abstraction/helpers.h"
#include "atlas/analysis/propchecker.h"

typedef enum {
    PExpr,
    PExprOption,
    PExprArray,

    PString,

    PName,
    PNameOption,
    PNameArray,

    PCallback,
} PropType;

typedef struct {
    PropCb callback;
    void* in;
    void* out;
} CallbackInfo;

typedef struct {
    String name;
    union {
        CallbackInfo cb_info; 
        void* location;
    };
    PropType type;
} Prop;

ARRAY_HEADER(Prop, prop, Prop);
ARRAY_COMMON_IMPL(Prop, prop, Prop);

struct PropSet {
    PropArray props;
    Allocator* gpa;
};

PropSet* make_prop_set(size_t numprops, Allocator* a) {
    PropSet* out = mem_alloc(sizeof(PropSet), a);

    *out = (PropSet) {
        .props = mk_prop_array(8, a),
        .gpa = a,
    };
    return out;
}

void delete_prop_set(PropSet *set) {
    mem_free(set, set->gpa);
}

void add_expr_prop(String propname, ExprRef* location, PropSet* props) {
    Prop prop = {
        .name = propname,
        .location = location,
        .type = PExpr,
    };
    push_prop(prop, &props->props);
}

void add_expr_option_prop(String propname, ExprRef* location, PropSet* props) {
    Prop prop = {
        .name = propname,
        .location = location,
        .type = PExprOption,
    };
    push_prop(prop, &props->props);
}

void add_expr_array_prop(String propname, ExprRef* location, PropSet* props) {
    Prop prop = {
        .name = propname,
        .location = location,
        .type = PExprArray,
    };
    push_prop(prop, &props->props);
}

void add_string_prop(String propname, String* location, PropSet* props) {
    Prop prop = {
        .name = propname,
        .location = location,
        .type = PString,
    };
    push_prop(prop, &props->props);
}

void add_name_prop(String propname, Name* location, PropSet* props) {
    Prop prop = {
        .name = propname,
        .location = location,
        .type = PName,
    };
    push_prop(prop, &props->props);
}

void add_name_option_prop(String propname, NameOption* location, PropSet* props) {
    Prop prop = {
        .name = propname,
        .location = location,
        .type = PNameOption,
    };
    push_prop(prop, &props->props);
}

void add_name_array_prop(String propname, NameArray* location, PropSet* props) {
    Prop prop = {
        .name = propname,
        .location = location,
        .type = PNameArray,
    };
    push_prop(prop, &props->props);
}

void add_callback_prop(String propname, PropCb callback, void* in, void* out, PropSet* props) {
    Prop prop = {
        .name = propname,
        .cb_info = {.callback = callback, .in = in, .out = out},
        .type = PCallback,
    };
    push_prop(prop, &props->props);
}

void parse_prop(RawTree term, PropSet* props, bool checks[], HostCallbackData host_data, ExprPool* pool, PiErrorPoint* point, RegionAllocator* region) {
    Allocator alc = ra_to_gpa(region);
    Allocator* a = &alc;
    // Step 1: confirm is branch
    if (term.type != RawBranch) {
        PicoError err = {
            .range = term.range,
            .message = mk_str_doc(mv_string("expected compound term but got atom."), a),
        };
        throw_pi_error(point, err);
    }

    if (term.branch.nodes.len == 0) {
        PicoError err = {
            .range = term.range,
            .message = mk_str_doc(mv_string("unexpected empty compound term."), a),
        };
        throw_pi_error(point, err);
    }

    RawTree head = term.branch.nodes.data[0];
    if (head.type != RawAtom || head.atom.type != ASymbol) {
        PicoError err = {
            .range = head.range,
            .message = mk_str_doc(mv_string("Expected a property name here."), a),
        };
        throw_pi_error(point, err);
    }
    
    Symbol propsym = head.atom.symbol;
    String propname = view_symbol_string(propsym);

    bool found = false;
    for (size_t i = 0; i < props->props.len; i++) {
        if (string_cmp(propname, props->props.data[i].name) == 0) {
            found = true;

            Prop prop = props->props.data[i];
            checks[i] = true;
            switch (prop.type) {
            case PString: {
                if (term.branch.nodes.len != 2) {
                    PicoError err = {
                        .range = term.range,
                        .message = mk_str_doc(mv_string("This property expects a single string, but got multiple values."), a),
                    };
                    throw_pi_error(point, err);
                }

                RawTree rstr = term.branch.nodes.data[1];
                if (rstr.type != RawAtom || rstr.atom.type != AString) {
                    PicoError err = {
                        .range = term.range,
                        .message = mk_str_doc(mv_string("This property expects a single string, but got a different type of value."), a),
                    };
                    throw_pi_error(point, err);
                }

                String* dest = prop.location;
                *dest = rstr.atom.string;
                break;
            }
            case PName: {
                if (term.branch.nodes.len != 2) {
                    PicoError err = {
                        .range = term.range,
                        .message = mk_str_doc(mv_string("This property expects a single symbol, but got multiple values."), a),
                    };
                    throw_pi_error(point, err);
                }

                RawTree rstr = term.branch.nodes.data[1];
                if (rstr.type != RawAtom || rstr.atom.type != ASymbol) {
                    PicoError err = {
                        .range = term.range,
                        .message = mk_str_doc(mv_string("This property expects a single symbol, but got a different type of value."), a),
                    };
                    throw_pi_error(point, err);
                }

                Symbol* dest = prop.location;
                *dest = rstr.atom.symbol;
                break;
            }
            case PNameOption:
                panic(mv_string("not parsing symbol option yet!"));
                break;
            case PNameArray: {
                NameArray arr = mk_name_array(term.branch.nodes.len - 1, a);

                for (size_t i = 1; i < term.branch.nodes.len; i++) {
                    RawTree rstr = term.branch.nodes.data[i];
                    if (rstr.type != RawAtom || rstr.atom.type != ASymbol) {
                        PicoError err = {
                            .range = rstr.range,
                            .message = mk_str_doc(mv_string("This property expects an array of symbols, but got a different type of value."), a),
                        };
                        throw_pi_error(point, err);
                    }
                    // TODO: ensure did == 0
                    push_name(rstr.atom.symbol.name, &arr);
                }

                NameArray* dest = prop.location;
                *dest = arr;
                break;
            }
            case PExpr: {
                RawTree expr = (term.branch.nodes.len == 2)
                    ? term.branch.nodes.data[1]
                    : raw_slice(&term, 1);

                ExprRef* dest = prop.location;
                *dest = abstract_rune_expr(expr, host_data, pool, region, point);
                break;
            }
            case PExprOption: {
                RawTree expr = (term.branch.nodes.len == 2)
                    ? term.branch.nodes.data[1]
                    : raw_slice(&term, 1);


                ExprRef* dest = prop.location;
                if (is_key_symbol(expr, string_to_symbol(mv_string("none")))) {
                    *dest = new_expr(pool);
                    set_expr(*dest,
                             (Expr) {
                                 .type = ECtor,
                                 .ctor.name = string_to_name(mv_string("none")),
                             },
                             pool);
                } else {
                    ExprRef ctor_ref = new_expr(pool);
                    Expr ctor = {
                        .type = ECtor,
                        .ctor.name = string_to_name(mv_string("some")),
                    };
                    set_expr(ctor_ref, ctor, pool);
                    ExprSlice members = new_expr_slice(1, pool);
                    Expr val;
                    abstract_rune_to(expr, host_data, pool, region, point, &val);
                    set_expr_elt(members, 0, val, pool);

                    *dest = new_expr(pool);
                    set_expr(*dest,
                             (Expr){
                                 .type = EApp,
                                 .app.fn = ctor_ref,
                                 .app.args = members,
                             },
                             pool);
                }
                break;
            }
            case PExprArray: {
                ExprSlice slice = new_expr_slice(term.branch.nodes.len - 1, pool);
                for (size_t i = 1; i < term.branch.nodes.len; i++) {
                    Expr expr;
                    abstract_rune_to(term.branch.nodes.data[i], host_data, pool, region, point, &expr);
                    set_expr_elt(slice, i, expr, pool);
                }

                ExprRef* dest = prop.location;
                    *dest = new_expr(pool);
                    set_expr(*dest,
                             (Expr){.type = EList, .list = slice},
                             pool);
                break;
            }
            case PCallback: {
                prop.cb_info.callback(term, point, prop.cb_info.in, prop.cb_info.out);
                break;
            }
            }
        }
    }

    if (!found) {
        PicoError err = {
            .range = head.range,
            .message = mk_str_doc(mv_string("Property of this type not available for this stanza."), a),
        };
        throw_pi_error(point, err);
    }

}

bool is_mandatory(PropType type) {
    switch (type) {
    case PName:
    case PExpr:
        return true;
    default:
        return false;
    }
}

void check_props(PropSet* props, bool checks[], Range range, PiErrorPoint* point, RegionAllocator* region) {
    Allocator alc = ra_to_gpa(region);
    Allocator* a = &alc;
    for (size_t i = 0; i < props->props.len; i++) {
        Prop prop = props->props.data[i];

        if (!checks[i] && is_mandatory(prop.type)) {
            PtrArray nodes = mk_ptr_array(4, a);
            push_ptr(mv_str_doc(mv_string("Missing stanza clause: "), a), &nodes);
            push_ptr(mk_str_doc(props->props.data[i].name, a), &nodes);

            Document* message = mv_sep_doc(nodes, a);
            
            PicoError err = {
                .range = range,
                .message = message,
            };
            throw_pi_error(point, err);
        }
    }
}
