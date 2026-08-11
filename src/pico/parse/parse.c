#include <math.h>
#include "platform/signals.h"
#include "pico/parse/parse.h"

// The main parsing functions, which parse different types of expressions. 
// The entry point, parse_expr, which an arbitrary expression, inspects the head
// of the input sream and delegates to a number of secondary parsing functions:  
// + list - for whitespace separated lists 
// + atom - for symbols OR prefixed operators such as .field or :variant
//   + parse_sybol - for symbols, is called by parse_atom
// + numbers - for decimal or floating-point numbers

static ParseResult parse_expr(IStream* is, uint32_t expected, PiAllocator* pia, Allocator* a);
static ParseResult parse_list(IStream* is, uint32_t terminator, SyntaxHint hint, PiAllocator* pia, Allocator* a);
static ParseResult parse_atom(IStream* is, PiAllocator* pia, Allocator* a);
static ParseResult parse_number(uint8_t base, IStream* is, PiAllocator* pia, Allocator* a);
static ParseResult parse_string(IStream* is, PiAllocator* pia, Allocator* a);
static ParseResult parse_rawstring(IStream* is, PiAllocator* pia, Allocator* a);
static ParseResult parse_hash(IStream* is, PiAllocator* pia, Allocator* a);

// Helper functions
StreamResult consume_until(uint32_t stop, IStream* is);
StreamResult consume_whitespace(IStream* is);
bool is_numchar(uint32_t codepoint, uint8_t base);
bool is_whitespace(uint32_t codepoint);
bool is_comment_start(uint32_t codepoint);
bool is_symchar(uint32_t codepoint);
bool is_special_char(uint32_t codepoint);

ParseResult parse_rawtree(IStream* is, PiAllocator* pia, Allocator* a) {
    return parse_expr(is, '\0', pia, a);
}

ParseResult parse_expr(IStream* is, uint32_t expected, PiAllocator* pia, Allocator* a) {
    // default if we never enter loop body 
    ParseResult out = (ParseResult) {.type = ParseNone};
    uint32_t point;

    consume_whitespace(is);
    StreamResult result;
    RawTreePiList terms = mk_rawtree_list(8, pia);
    bool running = true;

    while (running && ((result = peek(is, &point)) == StreamSuccess)) {
        switch (peek(is, &point)) {
        case StreamSuccess:
            if (point == '(') {
                out = parse_list(is, ')', HExpression, pia, a);
            }
            else if (point == '[') {
                out = parse_list(is, ']', HSpecial, pia, a);
            }
            else if (point == '{') {
                out = parse_list(is, '}', HImplicit, pia, a);
            }
            //  0x27E8 = ⟨, 0x27E9 = ⟩
            else if (point == 0x27E8) {
                out = parse_list(is, 0x27E9, HData, pia, a);
            }
            else if ((point == ':') | (point == '.') | (point == '^')) {
                size_t start = bytecount(is);
                next(is, &point);
                U32Array chars = {.len = 0, .size = 1, .data = &point };
                push_u32(point, &chars);
                Symbol sym = string_to_symbol(string_from_UTF_32(chars, a));

                out = (ParseResult) {
                    .type = ParseSuccess,
                    .result = (RawTree) {
                        .type = RawAtom,
                        .range.start = start,
                        .range.end = bytecount(is),
                        .atom.type = ASymbol,
                        .atom.symbol = sym,
                    }
                };
            }
            else if (point == '"') {
                out = parse_string(is, pia, a);
            }
            else if (point == '~') {
                out = parse_rawstring(is, pia, a);
            }
            else if (point == '#') {
                out = parse_hash(is, pia, a);
            }
            else if (is_numchar(point, 10) || point == '-') {
                out = parse_number(10, is, pia, a);
            }
            else if (is_whitespace(point) || is_comment_start(point)) {
                // Whitespace always terminates a unit, e.g. 
                // "foo.bar" is pared as a single unit (. foo bar), while
                // "foo . bar" requires parse_expr to be called thrice.
                out.type = ParseNone;
                running = false;
                break;
            }
            else if (is_symchar(point)) {
                out = parse_atom(is, pia, a);
            } else if (point == expected) {
                // We couldn't do a parse!
                out.type = ParseNone;
                running = false;
                break;
            } else {
                // We couldn't do a parse!
                size_t range_start = bytecount(is);
                next(is, &point);

                String actual_string = string_from_codepoint(point, a);
                String expected_string = string_from_codepoint(expected, a);
                String message = string_ncat(a, 5,
                                             mv_string("Unexpected character: '"),
                                             actual_string,
                                             mv_string("', expected: '"),
                                             expected_string,
                                             mv_string("'"));

                out = (ParseResult) {
                    .type = ParseFail,
                    .error.range.start = range_start,
                    .error.range.end = bytecount(is),
                    .error.message = mv_str_doc(message, a),
                };
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
                .error.message = mv_cstr_doc("Stream result was in unexpected state.", a),
                .error.range.start = bytecount(is),
                .error.range.end = bytecount(is),
            };
            running = false;
        } break;
        }

        if (out.type == ParseSuccess) {
            push_rawtree(out.result, &terms);
        } else if (out.type == ParseFail) {
            running = false;
        }
    }

    if (out.type != ParseFail && terms.len == 0) {
        out.type = ParseNone;
    } else if (out.type == ParseNone && terms.len == 1) {
        out.type = ParseSuccess;
        out.result = terms.data[0];
    } else if ((out.type == ParseSuccess || out.type == ParseNone) && terms.len > 1) {
        /**
         * Unrolling
         * Now that the list has been accumulated, 'unroll' the list appropriately, 
         * e.g. num:i64.+ => [num, :, i64, ., +] => (. + (: num i64))
         * For a more in-depth, worked example, take
         * bar.^foo.baz => (. baz (^ (. foo bar)))
         * 
         * We first get the list ['bar', '.', '^', 'foo', '.', 'baz']
         * We start with `final` undefined, and `current = &final`
         * 
         * We start on the RHS, with 'baz', and look to the left the symbol is
         * '.' (an infix operator), and there are more symbols, so we determine
         * it is infix, and set *current = (. baz ?) and current = &?
         * 
         * Then, we proceed and decrement 'i' by 2 (baz + .), and look again.
         * This time, the symbol under the index is 'foo' and one to the left is
         * '^' (a prefix operator). There are still more symbols left, so we set
         * *out = (^ ?) and SWAP foo & ^. final is now (. baz (^ ?)). We
         * decrement 'i' by only 1 (prefix) and set current = &?. 
         *
         * Now, the value at the index is stil 'foo' (remember we swapped it
         * with '^', but the value to the left is '.', so we proceed as we did
         * for baz: *out = (. foo ?), giving final = (. baz (^ (. foo ?))). 
         * 
         * Finally, we get to 'bar' (i = 0). There are no more symbols to the
         * left of bar, so it just gets inserted in, giving the final expression
         *
         *  (. baz (^ (. foo ?)))
         *
         */
        RawTree final;
        RawTree* current = &final;
        for (size_t i = terms.len - 1; i > 0;) {
            RawTree raw_token = terms.data[i - 1];
            if (raw_token.type != RawAtom || raw_token.atom.type != ASymbol) {
                return (ParseResult) {
                    .type = ParseFail,
                    .error.message = mv_cstr_doc("Unexpected infix symbol: expected '.', ':' or '^'.", a),
                    .error.range = raw_token.range,
                };
            }
            Symbol tok = raw_token.atom.symbol;
            bool is_infix_sym = symbol_eq(tok, string_to_symbol(mv_string(".")))
                || symbol_eq(tok, string_to_symbol(mv_string(":")));
            bool is_prefix_sym = symbol_eq(tok, string_to_symbol(mv_string("^")));
            if (!is_infix_sym & !is_prefix_sym) {
                return (ParseResult) {
                    .type = ParseFail,
                    .error.message = mv_cstr_doc("Unexpected infix symbol: expected '.', ':' or '^'.", a),
                    .error.range = raw_token.range,
                };
            }
            if (is_prefix_sym || i == 1) {
                RawTreePiList nodes = mk_rawtree_list(2, pia);
                RawTree rhs = terms.data[i];
                terms.data[i - 1] = rhs;
                terms.data[i] = raw_token;
                push_rawtree(raw_token, &nodes);
                push_rawtree(rhs,  &nodes);
                *current = (RawTree) {
                    .type = RawBranch,
                    .range.end = rhs.range.end,
                    .range.start = terms.data[0].range.start,
                    .branch.hint = HExpression,
                    .branch.nodes = nodes,
                };
                current = &nodes.data[1];
                i -= 1;
            } else {
                RawTreePiList nodes = mk_rawtree_list(3, pia);
                RawTree rhs = terms.data[i];
                push_rawtree(raw_token, &nodes);
                push_rawtree(rhs,  &nodes);
                push_rawtree(rhs,  &nodes);
                *current = (RawTree) {
                    .type = RawBranch,
                    .range.end = rhs.range.end,
                    .range.start = terms.data[0].range.start,
                    .branch.hint = HExpression,
                    .branch.nodes = nodes,
                };
                current = &nodes.data[2];
                i -= 2;
            }

        };
        *current = terms.data[0];

        out = (ParseResult) {
            .type = ParseSuccess,
            .result = final,
        };
    }
    return out;
}

ParseResult parse_list(IStream* is, uint32_t terminator, SyntaxHint hint, PiAllocator* pia, Allocator* a) {
    ParseResult res;
    res.type = ParseSuccess;
    ParseResult out;
    RawTreePiList nodes = mk_rawtree_list(8, pia);
    uint32_t codepoint;

    // Assume '(' is next character
    size_t start = bytecount(is);
    next(is, &codepoint);
    consume_whitespace(is);
    StreamResult sres;

    while ((sres = peek(is, &codepoint)) == StreamSuccess && (codepoint != terminator)) {
        res = parse_expr(is, terminator, pia, a);

        if (res.type == ParseFail) {
            out = res;
            break;
        } else {
            push_rawtree(res.result, &nodes);
        }
        consume_whitespace(is);
    }

    if (sres == StreamEnd) {
        out = (ParseResult) {
            .type = ParseFail,
            .error.message = mv_cstr_doc("Unexpected end of stream. List started here was still parsing", a),
            .error.range.start = start,
            .error.range.end = start + 1,
        };
    } else if (sres != StreamSuccess) {
        out = (ParseResult) {
            .type = ParseFail,
            .error.message = mv_cstr_doc("Input stream failure", a),
            .error.range.start = bytecount(is),
            .error.range.end = bytecount(is),
        };
    } else if (res.type == ParseFail) {
        out = res;
    } else {
        // consume closing ')'
        next(is, &codepoint);

        out.type = ParseSuccess;
        out.result = (RawTree) {
            .type = RawBranch,
            .range.start = start,
            .range.end = bytecount(is),
            .branch.hint = hint,
            .branch.nodes = nodes,
        };
    }
    return out;
}

static ParseResult parse_atom_prepped(U32Array symchars, size_t start, IStream* is, PiAllocator* pia, Allocator* a);

ParseResult parse_atom(IStream* is, PiAllocator* pia, Allocator* a) {
    U32Array symchars = mk_u32_array(16, a);
    size_t start = bytecount(is);
    return parse_atom_prepped(symchars, start, is, pia, a);
}

ParseResult parse_atom_prepped(U32Array symchars, size_t start, IStream* is, PiAllocator* pia, Allocator* a) {
    /* The parse_atom function is responsible for parsing symbols and 'symbol conglomerates'
     * These may be 'true' atoms such as bar, + or foo. Strings separated by '.'
     * and ':' such as Maybe:none and foo.var are also considered by the parser
     * as 'atoms'
     * as these elements are not separated by spaces and bind tightly.
     * 
     * The general approach is as follows:
     */
    uint32_t codepoint;
    StreamResult result;
    ParseResult out = {.type = ParseNone};

    RawTreePiList terms = mk_rawtree_list(8, pia);

    // Accumulate a list of symbols, so, for example, 
    // num:i64.+ becomes {'num', ':', 'i64', '.', '+'}

    while (((result = peek(is, &codepoint)) == StreamSuccess)) {
        if (is_symchar(codepoint)) {
            next(is, &codepoint);
            push_u32(codepoint, &symchars);
        } else { 
            // Empty symchars *should* only occur when parsing the '^' symbol
            if (symchars.len != 0) {
                String str = string_from_UTF_32(symchars, a);
                RawTree val = (RawTree) {
                    .type = RawAtom,
                    .range.start = start,
                    .range.end = bytecount(is),
                    .atom.type = ASymbol,
                    .atom.symbol = string_to_symbol(str),
                };
                push_rawtree(val, &terms);
            } else {
                panic(mv_string("Unexpected Case encountered while parsing: empty symbol"));
            }

            // We are done; break out of loop
            break;
        }
    }
    if (result == StreamEnd) {
        if (symchars.len == 0) {
            panic(mv_string("Unexpected Case encountered while parsing: empty symbol"));
        } else {
            String str = string_from_UTF_32(symchars, a);
            RawTree val = (RawTree) {
                .type = RawAtom,
                .range.start = start,
                .range.end = bytecount(is),
                .atom.type = ASymbol,
                .atom.symbol = string_to_symbol(str),
            };
            push_rawtree(val, &terms);
        }
    }

    if (result != StreamSuccess && result != StreamEnd) {
        out = (ParseResult) {
            .type = ParseFail,
            .error.message = mv_cstr_doc("Stream failure.", a),
            .error.range.start = bytecount(is),
            .error.range.end = bytecount(is),
        };
    } else {
        // Now that the list has been accumulated, 'unroll' the list appropriately, 
        // meaning that (num : i64 . +) becomes (. + (: num i64))
        RawTree current = terms.data[0];
        for (size_t i = 1; terms.len - i != 0; i += 2) {
            RawTreePiList children = mk_rawtree_list(3, pia);
            push_rawtree(terms.data[i], &children);
            push_rawtree(terms.data[i+1], &children);
            push_rawtree(current, &children);

            current = (RawTree) {
                .type = RawBranch,
                .range.start = current.range.start,
                .range.end = terms.data[i+1].range.end,
                .branch.hint = HExpression,
                .branch.nodes = children,
            };
        };

        out = (ParseResult) {
            .type = ParseSuccess,
            .result = current,
        };
    }
    return out;
}

ParseResult parse_number(uint8_t base, IStream* is, PiAllocator* pia, Allocator* a) {
    uint32_t codepoint;
    StreamResult result;
    U8Array lhs = mk_u8_array(10, a);
    U8Array rhs = mk_u8_array(10, a);
    bool is_positive = true;
    bool just_negation = true;
    bool floating = false;
    size_t start = bytecount(is);

    result = peek(is, &codepoint);
    if (result == StreamSuccess && codepoint == '-') {
        next(is, &codepoint);
        is_positive = false;
    }

    while (((result = peek(is, &codepoint)) == StreamSuccess) && (is_numchar(codepoint, base) || codepoint == '_')) {
        just_negation = false;
        next(is, &codepoint);
        if (codepoint != '_') {
            // The cast is safe as is-numchar ensures codepoint < 256
            uint8_t val = is_numchar(codepoint, 10)
                ? (uint8_t) codepoint - 48
                /**
                 * e.g. B = 11
                 * B | 32 = b (converts to lowercase)
                 * b - 97 = 1 (97 = a), so b is one above a
                 * 1 + 10 = 11, the correct value for B as a number
                 */
                : (uint8_t) ((codepoint | 32) - 97) + 10; 
            push_u8(val, &lhs);
        }
    }

    if (result == StreamSuccess && codepoint == '.') {
        floating = true;
        just_negation = false;
        next(is, &codepoint);
        while (((result = peek(is, &codepoint)) == StreamSuccess) && (is_numchar(codepoint, base) || codepoint == '_')) {
            next(is, &codepoint);
            if (codepoint != '_') {
                // The cast is safe as is-numchar ensures codepoint < 256
                uint8_t val = (uint8_t) codepoint - 48;
                push_u8(val, &rhs);
            }
        }
    }

    if (just_negation) {
        if (is_whitespace(codepoint)) {
            return (ParseResult) {
                .type = ParseSuccess,
                .result.type = RawAtom,
                .result.range.start = start,
                .result.range.end = bytecount(is),
                .result.atom.type = ASymbol,
                .result.atom.symbol = string_to_symbol(mv_string("-")),
            };
        } else {
            U32Array symchars = mk_u32_array(16, a);
            push_u32('-', &symchars);
            return parse_atom_prepped(symchars, start, is, pia, a);
        }
    }


    if (result != StreamSuccess && result != StreamEnd) {
        return (ParseResult) {
            .type = ParseFail,
            .error.message = mv_cstr_doc("Stream failure", a),
            .error.range.start = bytecount(is),
            .error.range.end = bytecount(is),
        };
    }

    if (floating) {
        int64_t lhs_result = 0;
        uint64_t rhs_result = 0;
        uint64_t exp = 1;
        for (size_t i = lhs.len; i > 0; i--) {
            if (lhs.data[i - 1] >= base) {
                return (ParseResult) {
                    .type = ParseFail,
                    .error.message = mv_cstr_doc("Digit size exceeds base size in number literal", a),
                    .error.range.start = bytecount(is),
                    .error.range.end = bytecount(is),
                };
            }
            lhs_result += exp * lhs.data[i-1];
            exp *= base;
        }
        exp = 1;
        for (size_t i = rhs.len; i > 0; i--) {
            if (rhs.data[i - 1] >= base) {
                return (ParseResult) {
                    .type = ParseFail,
                    .error.message = mv_cstr_doc("Digit size exceeds base size in number literal", a),
                    .error.range.start = bytecount(is),
                    .error.range.end = bytecount(is),
                };
            }
            rhs_result += exp * rhs.data[i-1];
            exp *= 10;
        }
        double dlhs = (double)lhs_result;
        double drhs = (double)rhs_result;
        drhs = drhs / powl(10, rhs.len);
        double total = dlhs + drhs; 
        total *= is_positive ? 1 : -1;

        return (ParseResult) {
            .type = ParseSuccess,
            .result.type = RawAtom,
            .result.range.start = start,
            .result.range.end = bytecount(is),
            .result.atom.type = AFloating,
            .result.atom.float_64 = total,
        };
    } else {
        int64_t int_result = 0;
        uint64_t exp = 1;
        for (size_t i = lhs.len; i > 0; i--) {
            if (lhs.data[i - 1] >= base) {
                return (ParseResult) {
                    .type = ParseFail,
                    .error.message = mv_cstr_doc("Digit size exceeds base size in number literal", a),
                    .error.range.start = bytecount(is),
                    .error.range.end = bytecount(is),
                };
            }
            int_result += exp * lhs.data[i-1];
            exp *= base;
        }
        int_result *= is_positive ? 1 : -1;

        return (ParseResult) {
            .type = ParseSuccess,
            .result.type = RawAtom,
            .result.range.start = start,
            .result.range.end = bytecount(is),
            .result.atom.type = AIntegral,
            .result.atom.int_64 = int_result,
        };
    }
}

ParseResult parse_string(IStream* is, PiAllocator* pia, Allocator* a) {
    StreamResult result;
    U32Array arr = mk_u32_array(64, a);
    uint32_t codepoint;
    size_t start = bytecount(is);
    next(is, &codepoint); // consume token (")

    while (((result = peek(is, &codepoint)) == StreamSuccess) && codepoint != '"') {
        if (codepoint == '\\') {
            next(is, &codepoint);
            size_t escape_start = bytecount(is);
            if ((result = peek(is, &codepoint)) != StreamSuccess)
                break;
            switch (codepoint) {
            case 'n':
                codepoint = '\n';
                break;
            case 't':
                codepoint = '\t';
                break;
            case '"':
                codepoint = '\"';
                break;
            case '\\':
                codepoint = '\\';
                break;
            default:
                return (ParseResult) {
                    .type = ParseFail,
                    .error.message = mv_cstr_doc("Unrecognized escape character", a),
                    .error.range.start = escape_start,
                    .error.range.end = bytecount(is),
                };
            }
        }
        push_u32(codepoint, &arr);
        next(is, &codepoint);
    }

    if (result != StreamSuccess) {
        return (ParseResult) {
            .type = ParseFail,
            .error.message = mv_cstr_doc("Stream failed", a),
            .error.range.start = start,
            .error.range.end = bytecount(is),
        };
    }

    next(is, &codepoint); // consume token (")
    return (ParseResult) {
        .type = ParseSuccess,
        .result.type = RawAtom,
        .result.range.start = start,
        .result.range.end = bytecount(is),
        .result.atom.type = AString,
        .result.atom.string = string_from_UTF_32(arr, a),
    };
}

ParseResult parse_rawstring(IStream* is, PiAllocator* pia, Allocator* a) {
    StreamResult result;
    U32Array arr = mk_u32_array(64, a);
    uint32_t codepoint;
    size_t start = bytecount(is);
    next(is, &codepoint); // consume token ~
    next(is, &codepoint); // consume token "
    if (codepoint != '"') {
        return (ParseResult) {
            .type = ParseFail,
            .error.message = mv_cstr_doc("Raw string parse failed: expect '\"' after ~", a),
            .error.range.start = start,
            .error.range.end = bytecount(is),
        };
    }

    while (((result = peek(is, &codepoint)) == StreamSuccess) && codepoint != '"') {
        push_u32(codepoint, &arr);
        next(is, &codepoint);
    }

    if (result != StreamSuccess) {
        return (ParseResult) {
            .type = ParseFail,
            .error.message = mv_cstr_doc("Stream failed", a),
            .error.range.start = start,
            .error.range.end = bytecount(is),
        };
    }

    next(is, &codepoint); // consume token (")
    return (ParseResult) {
        .type = ParseSuccess,
        .result.type = RawAtom,
        .result.range.start = start,
        .result.range.end = bytecount(is),
        .result.atom.type = AString,
        .result.atom.string = string_from_UTF_32(arr, a),
    };
}

ParseResult build_char_lit(int64_t codepoint, Range range) {
    return (ParseResult) {
        .type = ParseSuccess,
        .result.type = RawAtom,
        .result.range = range,
        .result.atom.type = AIntegral,
        .result.atom.int_64 = codepoint,
    };
}

ParseResult parse_hash(IStream* is, PiAllocator* pia, Allocator* a) {
    StreamResult result;
    uint32_t codepoint;
    size_t start = bytecount(is);
    next(is, &codepoint); // consume token #)
    result = next(is, &codepoint);

    if (result != StreamSuccess) {
        return (ParseResult) {
            .type = ParseFail,
            .error.message = mv_cstr_doc("Stream failed", a),
            .error.range.start = bytecount(is),
            .error.range.end = bytecount(is),
        };
    }

    uint32_t char_lit = codepoint;
    peek(is, &codepoint);
    if (is_whitespace(codepoint) || is_special_char(codepoint)) {
        Range range = {
            .start = start,
            .end = bytecount(is),
        };
        return build_char_lit(char_lit, range);
    } else if (codepoint == '_') {
        next(is, &codepoint); // Consume '_'
        switch (char_lit) {
        case 'b':
            return parse_number(2, is, pia, a);
        case 'o':
            return parse_number(8, is, pia, a);
        case 'x':
            return parse_number(16, is, pia, a);
        default:
            return (ParseResult) {
                .type = ParseFail,
                .error.message = mv_cstr_doc("Invalid base indicator: please use one of (b)inary, (o)ctal or he(x)adecimal", a),
                .error.range.start = bytecount(is),
                .error.range.end = bytecount(is),
            };
            break;
        }
    } else {
        U32Array u32_name = mk_u32_array(8, a);
        push_u32(char_lit, &u32_name);
        while (!(is_whitespace(codepoint) || is_special_char(codepoint))) {
            push_u32(codepoint, &u32_name);
            next(is, &codepoint); // consume current token
            peek(is, &codepoint); // to check if next token is end
        }
        String name = string_from_UTF_32(u32_name, a);

        Range range = {
            .start = start,
            .end = bytecount(is),
        };
        if (string_eq(mv_string("null"), name)) {
            return build_char_lit('\0', range);
        } else if (string_eq(mv_string("space"), name)) {
            return build_char_lit(' ', range);
        } else if (string_eq(mv_string("tab"), name)) {
            return build_char_lit('\t', range);
        } else if (string_eq(mv_string("newline"), name)) {
            return build_char_lit('\n', range);
        } else if (string_eq(mv_string("return"), name)) {
            return build_char_lit('\r', range);
        } else {
            return (ParseResult) {
                .type = ParseFail,
                .error.message = mv_cstr_doc("Unexpected charater literall name '#': most character literals\n "
                                             "need there is only one character following the '#' then a space or .\n"
                                             "special character.\n If you wanted a numeric literal, follow the '#' \n"
                                             "  with a base indicator character then an underscore, e.g. #b_11 for binary 3.\n"
                                             "If you wanted a named charater, such as #space or #tab, please check \n"
                                             "  the spelling, and the documentation, to ensure your charater is supported."
                                             , a),
                .error.range = range,
            };
        }
    }
}

StreamResult consume_until(uint32_t stop, IStream* is) {
    uint32_t codepoint;
    StreamResult result;

    next(is, &codepoint); // consume token #)
    result = next(is, &codepoint);

    while ((result = peek(is, &codepoint)) == StreamSuccess) {
        if (codepoint != stop) {
            result = next(is, &codepoint);
        }
        else {
            break;
        }
    }
    return result;
}

StreamResult consume_whitespace(IStream* is) {
    uint32_t codepoint;
    StreamResult result;
    while ((result = peek(is, &codepoint)) == StreamSuccess) {
        if (is_whitespace(codepoint) ) {
            result = next(is, &codepoint);
        } else if (codepoint == ';') {
            result = consume_until('\n', is);
        } else {
            break;
        }
    }
    return result;
}

bool is_numchar(uint32_t codepoint, uint8_t base) {
    uint32_t lower = codepoint | 32;
    return (base <= 10)
        ? ((48 <= codepoint) & (codepoint < (48u + base)))
        : ((48 <= codepoint) & (codepoint < 58)) | ((97u <= lower) & (lower < 97u + (base - 10)));
}

bool is_whitespace(uint32_t codepoint) {
    // Take note of the (codepoint == 0). This is because we may encounter a
    // NULL character in a file. If this happens, we treat it as whitespace (for now).
    // We *may* want to change this to report an error instead. 
    // (possibly for all control/non-displayable characters?)
    return (codepoint == 32) | (9 <= codepoint && codepoint <= 13) | (codepoint == 0);
}

bool is_comment_start(uint32_t codepoint) {
    // Take note of the (codepoint == 0). This is because we may encounter a
    // NULL character in a file. If this happens, we treat it as whitespace (for now).
    // We *may* want to change this to report an error instead. 
    // (possibly for all control/non-displayable characters?)
    return codepoint == ';';
}

bool is_symchar(uint32_t codepoint) {
    return !is_whitespace(codepoint) && !is_special_char(codepoint);
}

bool is_special_char(uint32_t codepoint) {
    return (codepoint == '('
            || codepoint == ')'
            || codepoint == '['
            || codepoint == ']'
            || codepoint == '{'
            || codepoint == '}'
            || codepoint == 0x27E8
            || codepoint == 0x27E9
            || codepoint == '.'
            || codepoint == ':'
            || codepoint == '^');
}
