#include "platform/signals.h"
#include "data/meta/array_header.h"
#include "data/meta/array_impl.h"

#include "atlas/analysis/propchecker.h"

typedef enum {
    PSymbol,
    PSymbolOption,
    PSymbolArray,

    PString,
    PStringOption,
    PStringArray,
} PropType;

typedef struct {
    String name;
    void* location;
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

void add_string_prop(String propname, String* location, PropSet* props) {
    Prop prop = {
        .name = propname,
        .location = location,
        .type = PString,
    };
    push_prop(prop, &props->props);
}

void add_string_option_prop(String propname, StringOption* location, PropSet* props) {
    Prop prop = {
        .name = propname,
        .location = location,
        .type = PStringOption,
    };
    push_prop(prop, &props->props);
}

void add_string_array_prop(String propname, StringArray* location, PropSet* props) {
    Prop prop = {
        .name = propname,
        .location = location,
        .type = PStringArray,
    };
    push_prop(prop, &props->props);

}

void add_symbol_prop(String propname, Symbol* location, PropSet* props) {
    Prop prop = {
        .name = propname,
        .location = location,
        .type = PSymbol,
    };
    push_prop(prop, &props->props);
}

void add_symbol_option_prop(String propname, SymbolOption* location, PropSet* props) {
    Prop prop = {
        .name = propname,
        .location = location,
        .type = PSymbolOption,
    };
    push_prop(prop, &props->props);
}

void add_symbol_array_prop(String propname, SymbolArray* location, PropSet* props) {
    Prop prop = {
        .name = propname,
        .location = location,
        .type = PSymbolArray,
    };
    push_prop(prop, &props->props);

}

void parse_prop(RawAtlas term, PropSet* props, bool checks[], PiErrorPoint* point, Allocator* a) {
    // Step 1: confirm is branch
    if (term.type != AtlBranch) {
        PicoError err = {
            .range = term.range,
            .message = mk_str_doc(mv_string("expected compound term but got atom."), a),
        };
        throw_pi_error(point, err);
    }

    if (term.branch.len == 0) {
        PicoError err = {
            .range = term.range,
            .message = mk_str_doc(mv_string("unexpected empty compound term."), a),
        };
        throw_pi_error(point, err);
    }

    RawAtlas head = term.branch.data[0];
    if (head.type != AtlAtom || head.atom.type != AtSymbol) {
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
            case PSymbol:
                if (term.branch.len != 2) {
                    PicoError err = {
                        .range = term.range,
                        .message = mk_str_doc(mv_string("This property expects a single symbol, but got multiple values."), a),
                    };
                    throw_pi_error(point, err);
                }

                RawAtlas rstr = term.branch.data[1];
                if (rstr.type != AtlAtom || rstr.atom.type != AtSymbol) {
                    PicoError err = {
                        .range = term.range,
                        .message = mk_str_doc(mv_string("This property expects a single symbol, but got a different type of value."), a),
                    };
                    throw_pi_error(point, err);
                }

                Symbol* dest = prop.location;
                *dest = rstr.atom.symbol;
                break;
            case PSymbolOption:
                
                panic(mv_string("not parsing symbol option yet!"));
                break;
            case PSymbolArray: {
                SymbolArray arr = mk_symbol_array(term.branch.len - 1, a);

                for (size_t i = 1; i < term.branch.len; i++) {
                    RawAtlas rstr = term.branch.data[i];
                    if (rstr.type != AtlAtom || rstr.atom.type != AtSymbol) {
                        PicoError err = {
                            .range = rstr.range,
                            .message = mk_str_doc(mv_string("This property expects an array of symbols, but got a different type of value."), a),
                        };
                        throw_pi_error(point, err);
                    }
                    push_symbol(rstr.atom.symbol, &arr);
                }

                SymbolArray* dest = prop.location;
                *dest = arr;
                break;
            }
            case PString: {
                if (term.branch.len != 2) {
                    PicoError err = {
                        .range = term.range,
                        .message = mk_str_doc(mv_string("This property expects a single string, but got multiple values."), a),
                    };
                    throw_pi_error(point, err);
                }

                RawAtlas rstr = term.branch.data[1];
                if (rstr.type != AtlAtom || rstr.atom.type != AtString) {
                    PicoError err = {
                        .range = term.range,
                        .message = mk_str_doc(mv_string("This property expects a single string, but got a different type of value."), a),
                    };
                    throw_pi_error(point, err);
                }

                String* dest = prop.location;
                String src = rstr.atom.string;
                *dest = src;
                break;
            }
            case PStringOption: {
                if (term.branch.len != 2) {
                    PicoError err = {
                        .range = term.range,
                        .message = mk_str_doc(mv_string("This property expects an (optional) string, but got multiple values."), a),
                    };
                    throw_pi_error(point, err);
                }
                RawAtlas rstr = term.branch.data[1];

                if (rstr.type == AtlAtom && rstr.atom.type == AtKeyword
                    && string_cmp(view_symbol_string(rstr.atom.keyword), mv_string("none")) == 0) {
                    StringOption* opt = prop.location;
                    *opt = (StringOption) {.type = None};
                } else {
                    if (rstr.type != AtlAtom || rstr.atom.type != AtString) {
                        PicoError err = {
                            .range = rstr.range,
                            .message = mk_str_doc(mv_string("This property expects an (optional) single string, but got a different type of value."), a),
                        };
                        throw_pi_error(point, err);
                    }

                    StringOption* dest = prop.location;
                    String src = rstr.atom.string;
                    *dest = (StringOption) {.type = Some, .value = src};
                }
                break;
            }
            case PStringArray: {
                StringArray arr = mk_string_array(term.branch.len - 1, a);

                for (size_t i = 1; i < term.branch.len; i++) {
                    RawAtlas rstr = term.branch.data[i];
                    if (rstr.type != AtlAtom || rstr.atom.type != AtString) {
                        PicoError err = {
                            .range = rstr.range,
                            .message = mk_str_doc(mv_string("This property expects an array of single strings, but got a different type of value."), a),
                        };
                        throw_pi_error(point, err);
                    }
                    push_string(rstr.atom.string, &arr);
                }

                StringArray* dest = prop.location;
                *dest = arr;
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

void check_props(PropSet* props, bool checks[], Range range, PiErrorPoint* point, Allocator* a) {
    for (size_t i = 0; i < props->props.len; i++) {
        if (!checks[i]) {
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
