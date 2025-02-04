#include "pico/parse/parse.h"

/* High Level overview of the grammar:
 *
 * 
 *
 *
 *
 */


// The 'actual' parser funciton
ParseResult parse_main(IStream* is, SourcePos* parse_state, Allocator* a);

ParseResult parse_rawtree(IStream* is, Allocator* a) {
    SourcePos start_state;
    start_state.col = 0;
    start_state.row = 0;
    return parse_main(is, &start_state, a);
}

// The three main parsing functions, which parse:
// + lists
// + numbers
// + symbols
// The 'main' parser does lookahead to dispatch on the appropriate parsing function.
ParseResult parse_list(IStream* is, SourcePos* parse_state, uint32_t terminator, SyntaxHint hint, Allocator* a);
ParseResult parse_atom(IStream* is, SourcePos* parse_state, Allocator* a);
ParseResult parse_number(IStream* is, SourcePos* parse_state, Allocator* a);
ParseResult parse_prefix(char prefix, IStream* is, SourcePos* parse_state, Allocator* a);
ParseResult parse_string(IStream* is, SourcePos* parse_state, Allocator* a);
ParseResult parse_char(IStream* is, SourcePos* parse_state);

// Helper functions
StreamResult consume_whitespace(IStream* is, SourcePos* parse_state);
bool is_numchar(uint32_t codepoint);
bool is_whitespace(uint32_t codepoint);
bool is_symchar(uint32_t codepoint);

ParseResult parse_main(IStream* is, SourcePos* parse_state, Allocator* a) {
    ParseResult out;
    uint32_t point;

    consume_whitespace(is, parse_state);
    StreamResult result;
    PtrArray terms = mk_ptr_array(8, a);
    bool running = true;

    while (running && ((result = peek(is, &point)) == StreamSuccess)) {
        switch (peek(is, &point)) {
        case StreamSuccess:
            if (point == '(') {
                out = parse_list(is, parse_state, ')', HExpression, a);
            }
            else if (point == '[') {
                out = parse_list(is, parse_state, ']', HSpecial, a);
            }
            else if (point == '{') {
                out = parse_list(is, parse_state, '}', HImplicit, a);
            }
            else if (point == ':') {
                if (terms.len == 0) {
                    out = parse_prefix(':', is, parse_state, a);
                } else {
                    next(is, &point);

                    out = (ParseResult) {
                        .type = ParseSuccess,
                        .data.result = (RawTree) {
                            .type = RawAtom,
                            .hint = HNone,
                            .atom.type = ASymbol,
                            .atom.symbol = string_to_symbol(mv_string(":")),
                        }
                    };
                }
            }
            else if (point == '.') {
                if (terms.len == 0) {
                    out = parse_prefix('.', is, parse_state, a);
                } else {
                    next(is, &point);

                    out = (ParseResult) {
                        .type = ParseSuccess,
                        .data.result = (RawTree) {
                            .type = RawAtom,
                            .hint = HNone,
                            .atom.type = ASymbol,
                            .atom.symbol = string_to_symbol(mv_string(".")),
                        }
                    };
                }
            }
            else if (point == '"') {
                out = parse_string(is, parse_state, a);
            }
            else if (point == '#') {
                out = parse_char(is, parse_state);
            }
            else if (is_numchar(point) || point == '-') {
                out = parse_number(is, parse_state, a);
            }
            else if (is_whitespace(point)) {
                out.type = ParseNone;
                running = false;
                break;
            }
            else if (is_symchar(point)){
                out = parse_atom(is, parse_state, a);
            } else {
                out.type = ParseNone;
                running = false;
                break;
            }
            break;

        case StreamEnd: {
            out.type = ParseNone;
            running = false;
            break;
        }
        
        default: {
            out = (ParseResult) {
                .type = ParseFail,
                .data.range.start = *parse_state,
                .data.range.end = *parse_state,
            };
            running = false;
        } break;
        }

        if (out.type == ParseSuccess) {
            RawTree* term = mem_alloc(sizeof(RawTree), a);
            *term = out.data.result;
            push_ptr(term, &terms);
        }
    }

    if (out.type == ParseSuccess && terms.len == 0) {
        out.type = ParseNone;
    } else if (out.type == ParseNone && terms.len == 1) {
        out.type = ParseSuccess;
        out.data.result = *(RawTree*)terms.data[0];
    } else if ((out.type == ParseSuccess || out.type == ParseNone) && terms.len > 1) {
        // Now that the list has been accumulated, 'unroll' the list appropriately, 
        // meaning that (num : i64 . +) becomes (. + (: num i64))
        RawTree* current = terms.data[0];
        for (size_t i = 1; terms.len - i != 0; i += 2) {
            PtrArray children = mk_ptr_array(3, a);
            push_ptr(terms.data[i], &children);
            push_ptr(terms.data[i+1], &children);
            push_ptr(current, &children);

            current = mem_alloc(sizeof(RawTree), a);
            *current = (RawTree) {
                .type = RawList,
                .hint = HNone,
                .nodes = children,
            };
        };

        out = (ParseResult) {
            .type = ParseSuccess,
            .data.result = *current,
        };
    }
    return out;
}

ParseResult parse_list(IStream* is, SourcePos* parse_state, uint32_t terminator, SyntaxHint hint, Allocator* a) {
    ParseResult res;
    res.type = ParseSuccess;
    res.data.range.start = *parse_state;
    ParseResult out;
    PtrArray nodes = mk_ptr_array(8, a);
    uint32_t codepoint;

    // Assume '(' is next character
    next(is, &codepoint);
    consume_whitespace(is, parse_state);
    StreamResult sres;

    while ((sres = peek(is, &codepoint)) == StreamSuccess && (codepoint != terminator)) {
        res = parse_main(is, parse_state, a);

        if (res.type == ParseFail) {
            out = res;
            break;
        } else {
            RawTree* node = (RawTree*)mem_alloc(sizeof(RawTree), a);
            *node = res.data.result;
            push_ptr(node, &nodes);
        }
        consume_whitespace(is, parse_state);
    }

    if (sres != StreamSuccess) {
        out.type = ParseFail;
        out.data.range.start = *parse_state;
        out.data.range.end = *parse_state;
    } else if (res.type == ParseFail) {
        out = res;
    } else {
        out.type = ParseSuccess;
        out.data.result = (RawTree) {
            .type = RawList,
            .hint = hint,
            .nodes = nodes,
        };
        // consume closing ')'
        next(is, &codepoint);
    }
    return out;
}

ParseResult parse_atom(IStream* is, SourcePos* parse_state, Allocator* a) {
    /* The parse_atom function is responsible for parsing symbols and 'symbol conglomerates'
     * These may be 'true' atoms such as num, + or foo. Strings separated by '.'
     * and ':' are also considered by the parser as 'atoms' as these elements are not separated
     * by spaces and bind tightly.
     * 
     * The general approach is as follows:
     */
    uint32_t codepoint;
    StreamResult result;
    ParseResult out;
    U32Array arr = mk_u32_array(16, a);

    PtrArray terms = mk_ptr_array(8, a);

    // Accumulate a list of symbols, so, for example, 
    // num:i64.+ becomes {'num', ':', 'i64', '.', '+'}

    while (((result = peek(is, &codepoint)) == StreamSuccess)) {
        if (is_symchar(codepoint)) {
            next(is, &codepoint);
            push_u32(codepoint, &arr);
        } else if (codepoint == '.' || codepoint == ':') {
            next(is, &codepoint);

            // Store symbol
            RawTree* val = mem_alloc(sizeof(RawTree), a);
            String str = string_from_UTF_32(arr, a);
            *val = (RawTree) {
                .type = RawAtom,
                .hint = HNone,
                .atom.type = ASymbol,
                .atom.symbol = string_to_symbol(str),
            };
            push_ptr(val, &terms);
            arr.len = 0; // reset array

            RawTree* op = mem_alloc(sizeof(RawTree), a);
            *op = (RawTree) {
                .type = RawAtom,
                .hint = HNone,
                .atom.type = ASymbol,
                .atom.symbol = codepoint == '.'
                  ? string_to_symbol(mv_string("."))
                  : string_to_symbol(mv_string(":")),
            };
            push_ptr(op, &terms);
        } else {
            RawTree* val = mem_alloc(sizeof(RawTree), a);
            String str = string_from_UTF_32(arr, a);
            *val = (RawTree) {
                .type = RawAtom,
                .hint = HNone,
                .atom.type = ASymbol,
                .atom.symbol = string_to_symbol(str),
            };
            push_ptr(val, &terms);

            // We are done; break out of loop
            break;
        }
    }

    if (result != StreamSuccess) {
        out = (ParseResult) {
            .type = ParseFail,
            .data.range.start = *parse_state,
            .data.range.end = *parse_state,
        };
    } else {
        // Now that the list has been accumulated, 'unroll' the list appropriately, 
        // meaning that (num : i64 . +) becomes (. + (: num i64))
        RawTree* current = terms.data[0];
        for (size_t i = 1; terms.len - i != 0; i += 2) {
            PtrArray children = mk_ptr_array(3, a);
            push_ptr(terms.data[i], &children);
            push_ptr(terms.data[i+1], &children);
            push_ptr(current, &children);

            current = mem_alloc(sizeof(RawTree), a);
            *current = (RawTree) {
                .type = RawList,
                .hint = HNone,
                .nodes = children,
            };
        };

        out = (ParseResult) {
            .type = ParseSuccess,
            .data.result = *current,
        };
    }
    return out;
}

ParseResult parse_number(IStream* is, SourcePos* parse_state, Allocator* a) {
    uint32_t codepoint;
    StreamResult result;
    U8Array arr = mk_u8_array(10, a);
    bool is_positive = true;
    bool just_negation = true;

    result = peek(is, &codepoint);
    if (result == StreamSuccess && codepoint == '-') {
        next(is, &codepoint);
        is_positive = false;
    }

    while (((result = peek(is, &codepoint)) == StreamSuccess) && is_numchar(codepoint)) {
        just_negation = false;
        next(is, &codepoint);
        // the cast is safe as is-numchar ensures codepoint < 256
        uint8_t val = (uint8_t) codepoint - 48;
        push_u8(val, &arr);
    }

    if (just_negation) {
        return (ParseResult) {
            .type = ParseSuccess,
            .data.result.type = RawAtom,
            .data.result.atom.type = ASymbol,
            .data.result.atom.symbol = string_to_symbol(mv_string("-")),
        };
    }


    if (result != StreamSuccess && result != StreamEnd) {
        return (ParseResult) {
            .type = ParseFail,
            .data.range.start = *parse_state,
            .data.range.end = *parse_state,
        };
    }

    int64_t int_result = 0;
    uint64_t tens = 1;
    for (size_t i = arr.len; i > 0; i--) {
        int_result += tens * arr.data[i-1];
        tens *= 10;
    }
    int_result *= is_positive ? 1 : -1;

    return (ParseResult) {
        .type = ParseSuccess,
        .data.result.type = RawAtom,
        .data.result.atom.type = AIntegral,
        .data.result.atom.int_64 = int_result,
    };
}

ParseResult parse_prefix(char prefix, IStream* is, SourcePos* parse_state, Allocator* a) {
    uint32_t codepoint;
    StreamResult result;
    ParseResult out;
    U32Array arr = mk_u32_array(10, a);

    next(is, &codepoint); // consume token

    while (((result = peek(is, &codepoint)) == StreamSuccess) && is_symchar(codepoint)) {
        next(is, &codepoint);
        push_u32(codepoint, &arr);
    }

    if (result != StreamSuccess) {
        out.type = ParseFail;
        out.data.range.start = *parse_state;
        out.data.range.end = *parse_state;
    } else if (arr.len == 0) {
        char cstr[2] = {prefix, '\0'};
        String str = mv_string(cstr);
        Symbol sym_result = string_to_symbol(str);

        out.type = ParseSuccess;
        out.data.result.type = RawAtom;
        out.data.result.atom.type = ASymbol;
        out.data.result.atom.symbol = sym_result;

    } else {
        char cstr[2] = {prefix, '\0'};
        String str = mv_string(cstr);
        Symbol sym_result = string_to_symbol(str);
        RawTree* proj = mem_alloc(sizeof(RawTree), a);
        proj->type = RawAtom;
        proj->atom.type = ASymbol;
        proj->atom.symbol = sym_result;

        str = string_from_UTF_32(arr, a);
        sym_result = string_to_symbol(str);
        RawTree* field = mem_alloc(sizeof(RawTree), a);
        field->type = RawAtom;
        field->atom.type = ASymbol;
        field->atom.symbol = sym_result;

        PtrArray nodes = mk_ptr_array(2, a);
        push_ptr(proj, &nodes);
        push_ptr(field, &nodes);

        out.type = ParseSuccess;
        out.data.result = (RawTree) {
            .type = RawList,
            .hint = HNone,
            .nodes = nodes,
        };
    }
    
    return out;
}

ParseResult parse_string(IStream* is, SourcePos* parse_state, Allocator* a) {
    StreamResult result;
    U32Array arr = mk_u32_array(64, a);
    uint32_t codepoint;
    next(is, &codepoint); // consume token (")

    while (((result = peek(is, &codepoint)) == StreamSuccess) && codepoint != '"') {
        next(is, &codepoint);
        push_u32(codepoint, &arr);
    }

    if (result != StreamSuccess) {
        return (ParseResult) {
            .type = ParseFail,
            .data.range.start = *parse_state,
            .data.range.end = *parse_state,
        };
    }

    next(is, &codepoint); // consume token (")
    return (ParseResult) {
        .type = ParseSuccess,
        .data.result.type = RawAtom,
        .data.result.hint = HNone,
        .data.result.atom.type = AString,
        .data.result.atom.string = string_from_UTF_32(arr, a),
    };
}

ParseResult parse_char(IStream* is, SourcePos* parse_state) {
    StreamResult result;
    uint32_t codepoint;
    next(is, &codepoint); // consume token #)
    result = next(is, &codepoint);

    if (result != StreamSuccess) {
        return (ParseResult) {
            .type = ParseFail,
            .data.range.start = *parse_state,
            .data.range.end = *parse_state,
        };
    }

    //next(is, &codepoint); // consume token (")
    return (ParseResult) {
        .type = ParseSuccess,
        .data.result.type = RawAtom,
        .data.result.hint = HNone,
        .data.result.atom.type = AIntegral,
        .data.result.atom.int_64 = codepoint,
    };
}

StreamResult consume_whitespace(IStream* is, SourcePos* parse_state) {
    uint32_t codepoint;
    StreamResult result;
    while ((result = peek(is, &codepoint)) == StreamSuccess) {
        if (is_whitespace(codepoint)) {
            // TODO: Check if newline!
            parse_state->col++;
            result = next(is, &codepoint);
        }
        else {
            break;
        }
    }
    return result;
}

bool is_numchar(uint32_t codepoint) {
    return (48 <= codepoint && codepoint <= 57);
}

bool is_whitespace(uint32_t codepoint) {
    return codepoint == 32 || (9 <= codepoint && codepoint <= 13);
}

bool is_symchar(uint32_t codepoint) {
    return !is_whitespace(codepoint) && !(codepoint == '('
                                          || codepoint == ')'
                                          || codepoint == '['
                                          || codepoint == ']'
                                          || codepoint == '{'
                                          || codepoint == '}'
                                          || codepoint == '.'
                                          || codepoint == ':');
}
