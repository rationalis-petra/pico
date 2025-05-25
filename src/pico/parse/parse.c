#include <math.h>
#include "pico/parse/parse.h"

/* High Level overview of the grammar:
 *
 * 
 *
 *
 *
 */


// The 'actual' parser funciton
ParseResult parse_main(IStream* is, Allocator* a);

ParseResult parse_rawtree(IStream* is, Allocator* a) {
    return parse_main(is, a);
}

// The three main parsing functions, which parse:
// + lists
// + numbers
// + symbols
// The 'main' parser does lookahead to dispatch on the appropriate parsing function.
ParseResult parse_expr(IStream* is, Allocator* a, uint32_t expected);
ParseResult parse_list(IStream* is, uint32_t terminator, SyntaxHint hint, Allocator* a);
ParseResult parse_atom(IStream* is, Allocator* a);
ParseResult parse_number(IStream* is, Allocator* a);
ParseResult parse_prefix(char prefix, IStream* is, Allocator* a);
ParseResult parse_string(IStream* is, Allocator* a);
ParseResult parse_char(IStream* is);

// Helper functions
StreamResult consume_until(uint32_t stop, IStream* is);
StreamResult consume_whitespace(IStream* is);
bool is_numchar(uint32_t codepoint);
bool is_whitespace(uint32_t codepoint);
bool is_symchar(uint32_t codepoint);

ParseResult parse_main(IStream* is, Allocator* a) {
    return parse_expr(is, a, '\0');
}

ParseResult parse_expr(IStream* is, Allocator* a, uint32_t expected) {
    // default if we never enter loop body 
    ParseResult out = (ParseResult) {.type = ParseNone};
    uint32_t point;

    consume_whitespace(is);
    StreamResult result;
    RawTreeArray terms = mk_rawtree_array(8, a);
    bool running = true;

    while (running && ((result = peek(is, &point)) == StreamSuccess)) {
        switch (peek(is, &point)) {
        case StreamSuccess:
            if (point == '(') {
                out = parse_list(is, ')', HExpression, a);
            }
            else if (point == '[') {
                out = parse_list(is, ']', HSpecial, a);
            }
            else if (point == '{') {
                out = parse_list(is, '}', HImplicit, a);
            }
            else if (point == ':') {
                if (terms.len == 0) {
                    out = parse_prefix(':', is, a);
                } else {
                    size_t start = bytecount(is);
                    next(is, &point);

                    out = (ParseResult) {
                        .type = ParseSuccess,
                        .result = (RawTree) {
                            .type = RawAtom,
                            .range.start = start,
                            .range.end = bytecount(is),
                            .atom.type = ASymbol,
                            .atom.symbol = string_to_symbol(mv_string(":")),
                        }
                    };
                }
            }
            else if (point == '.') {
                if (terms.len == 0) {
                    out = parse_prefix('.', is, a);
                } else {
                    size_t start = bytecount(is);
                    next(is, &point);

                    out = (ParseResult) {
                        .type = ParseSuccess,
                        .result = (RawTree) {
                            .type = RawAtom,
                            .range.start = start,
                            .range.end = bytecount(is),
                            .atom.type = ASymbol,
                            .atom.symbol = string_to_symbol(mv_string(".")),
                        }
                    };
                }
            }
            else if (point == '"') {
                out = parse_string(is, a);
            }
            else if (point == '#') {
                out = parse_char(is);
            }
            else if (is_numchar(point) || point == '-') {
                out = parse_number(is, a);
            }
            else if (is_whitespace(point)) {
                // Whitespace always terminates a unit, e.g. 
                // "foo.bar" is pared as a single unit (. foo bar), while
                // "foo . bar" requires parse_expr to be called thrice.
                out.type = ParseNone;
                running = false;
                break;
            }
            else if (is_symchar(point)){
                out = parse_atom(is, a);
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
                    .error.message = message,
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
                .error.message = mv_string("Stream result was in unexpected state."),
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

    // Post-run cleanup: reduce the term list to a single term (or ParseNone)
    if (out.type != ParseFail && terms.len == 0) {
        out.type = ParseNone;
    } else if (out.type == ParseNone && terms.len == 1) {
        out.type = ParseSuccess;
        out.result = terms.data[0];
    } else if ((out.type == ParseSuccess || out.type == ParseNone) && terms.len > 1) {
        // Check that there is an appropriate (odd) number of terms for infix operator
        // unrolling to function
        if (terms.len % 2 == 0) {
          out = (ParseResult) {
            .type = ParseFail,
            .error.message = mv_string("Inappropriate number of terms for infix-operator: "),
            .error.range.start = bytecount(is),
            .error.range.end = bytecount(is),
          };
          return out;
        }

        // Now that the list has been accumulated, 'unroll' the list appropriately, 
        // meaning that (num : i64 . +) becomes (. + (: num i64))
        RawTree current = terms.data[0];
        for (size_t i = 1; terms.len - i != 0; i += 2) {
            RawTreeArray children = mk_rawtree_array(3, a);
            push_rawtree(terms.data[i], &children);
            push_rawtree(terms.data[i+1], &children);
            push_rawtree(current, &children);

            current = (RawTree) {
                .type = RawBranch,
                .range.start = terms.data[0].range.start,
                .range.end = current.range.end,
                .branch.hint = HNone,
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

ParseResult parse_list(IStream* is, uint32_t terminator, SyntaxHint hint, Allocator* a) {
    ParseResult res;
    res.type = ParseSuccess;
    ParseResult out;
    RawTreeArray nodes = mk_rawtree_array(8, a);
    uint32_t codepoint;

    // Assume '(' is next character
    size_t start = bytecount(is);
    next(is, &codepoint);
    consume_whitespace(is);
    StreamResult sres;

    while ((sres = peek(is, &codepoint)) == StreamSuccess && (codepoint != terminator)) {
        res = parse_expr(is, a, terminator);

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
            .error.message = mv_string("Unexpected end of stream. List started here was still parsing"),
            .error.range.start = start,
            .error.range.end = start,
        };
    } else if (sres != StreamSuccess) {
        out = (ParseResult) {
            .type = ParseFail,
            .error.message = mv_string("Input stream failure"),
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

ParseResult parse_atom(IStream* is, Allocator* a) {
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
    ParseResult out;
    U32Array arr = mk_u32_array(16, a);

    RawTreeArray terms = mk_rawtree_array(8, a);
    size_t start = bytecount(is);

    // Accumulate a list of symbols, so, for example, 
    // num:i64.+ becomes {'num', ':', 'i64', '.', '+'}

    while (((result = peek(is, &codepoint)) == StreamSuccess)) {
        if (is_symchar(codepoint)) {
            next(is, &codepoint);
            push_u32(codepoint, &arr);
        } else if (codepoint == '.' || codepoint == ':') {
            size_t op_start = bytecount(is);
            next(is, &codepoint);
            size_t op_end = bytecount(is);

            RawTree op = (RawTree) {
                .type = RawAtom,
                .range.start = op_start,
                .range.end = op_end,
                .atom.type = ASymbol,
                .atom.symbol = codepoint == '.'
                  ? string_to_symbol(mv_string("."))
                  : string_to_symbol(mv_string(":")),
            };

            // Store symbol
            String str = string_from_UTF_32(arr, a);
            RawTree val = (RawTree) {
                .type = RawAtom,
                .range.start = start,
                .range.end = op_start,
                .atom.type = ASymbol,
                .atom.symbol = string_to_symbol(str),
            };
            // new start bytecount(is)
            start = op_end;

            push_rawtree(val, &terms);
            arr.len = 0; // reset array

            push_rawtree(op, &terms);
        } else {
            String str = string_from_UTF_32(arr, a);
            RawTree val = (RawTree) {
                .type = RawAtom,
                .range.start = start,
                .range.end = bytecount(is),
                .atom.type = ASymbol,
                .atom.symbol = string_to_symbol(str),
            };
            push_rawtree(val, &terms);

            // We are done; break out of loop
            break;
        }
    }

    if (result != StreamSuccess) {
        out = (ParseResult) {
            .type = ParseFail,
            .error.message = mv_string("Stream failure."),
            .error.range.start = bytecount(is),
            .error.range.end = bytecount(is),
        };
    } else {
        // Now that the list has been accumulated, 'unroll' the list appropriately, 
        // meaning that (num : i64 . +) becomes (. + (: num i64))
        RawTree current = terms.data[0];
        for (size_t i = 1; terms.len - i != 0; i += 2) {
            RawTreeArray children = mk_rawtree_array(3, a);
            push_rawtree(terms.data[i], &children);
            push_rawtree(terms.data[i+1], &children);
            push_rawtree(current, &children);

            current = (RawTree) {
                .type = RawBranch,
                .range.start = current.range.start,
                .range.end = terms.data[i+1].range.end,
                .branch.hint = HNone,
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

ParseResult parse_number(IStream* is, Allocator* a) {
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

    while (((result = peek(is, &codepoint)) == StreamSuccess) && is_numchar(codepoint)) {
        just_negation = false;
        next(is, &codepoint);
        // the cast is safe as is-numchar ensures codepoint < 256
        uint8_t val = (uint8_t) codepoint - 48;
        push_u8(val, &lhs);
    }

    if (result == StreamSuccess && codepoint == '.') {
        floating = true;
        just_negation = false;
        next(is, &codepoint);
        while (((result = peek(is, &codepoint)) == StreamSuccess) && is_numchar(codepoint)) {
            next(is, &codepoint);
            // the cast is safe as is-numchar ensures codepoint < 256
            uint8_t val = (uint8_t) codepoint - 48;
            push_u8(val, &rhs);
        }
    }

    if (just_negation) {
        return (ParseResult) {
            .type = ParseSuccess,
            .result.type = RawAtom,
            .result.range.start = start,
            .result.range.end = bytecount(is),
            .result.atom.type = ASymbol,
            .result.atom.symbol = string_to_symbol(mv_string("-")),
        };
    }


    if (result != StreamSuccess && result != StreamEnd) {
        return (ParseResult) {
            .type = ParseFail,
            .error.message = mv_string("Stream failure"),
            .error.range.start = bytecount(is),
            .error.range.end = bytecount(is),
        };
    }

    if (floating) {
        int64_t lhs_result = 0;
        uint64_t rhs_result = 0;
        uint64_t tens = 1;
        for (size_t i = lhs.len; i > 0; i--) {
            lhs_result += tens * lhs.data[i-1];
            tens *= 10;
        }
        tens = 1;
        for (size_t i = rhs.len; i > 0; i--) {
            rhs_result += tens * rhs.data[i-1];
            tens *= 10;
        }
        lhs_result *= is_positive ? 1 : -1;
        double dlhs = (double)lhs_result;
        double drhs = (double)rhs_result;
        drhs = drhs / powl(10, rhs.len);

        return (ParseResult) {
            .type = ParseSuccess,
            .result.type = RawAtom,
            .result.range.start = start,
            .result.range.end = bytecount(is),
            .result.atom.type = AFloating,
            .result.atom.float_64 = dlhs + drhs,
        };
    } else {
        int64_t int_result = 0;
        uint64_t tens = 1;
        for (size_t i = lhs.len; i > 0; i--) {
            int_result += tens * lhs.data[i-1];
            tens *= 10;
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

ParseResult parse_prefix(char prefix, IStream* is, Allocator* a) {
    uint32_t codepoint;
    StreamResult result;
    ParseResult out;
    U32Array arr = mk_u32_array(10, a);

    size_t start = bytecount(is);
    next(is, &codepoint); // consume token

    while (((result = peek(is, &codepoint)) == StreamSuccess) && is_symchar(codepoint)) {
        next(is, &codepoint);
        push_u32(codepoint, &arr);
    }

    if (result != StreamSuccess) {
        out.type = ParseFail;
        out.error.message = mv_string("Stream failed");
        out.error.range.start = bytecount(is);
        out.error.range.end = bytecount(is);
    } else if (arr.len == 0) {
        char cstr[2] = {prefix, '\0'};
        String str = mv_string(cstr);
        Symbol sym_result = string_to_symbol(str);

        out.type = ParseSuccess;
        out.result.type = RawAtom;
        out.result.atom.type = ASymbol;
        out.result.atom.symbol = sym_result;

    } else {
        char cstr[2] = {prefix, '\0'};
        String str = mv_string(cstr);
        Symbol sym_result = string_to_symbol(str);
        RawTree proj = (RawTree) {
            .type = RawAtom,
            .range.start = start,
            .range.end = bytecount(is),
            .atom.type = ASymbol,
            .atom.symbol = sym_result,
        };

        str = string_from_UTF_32(arr, a);
        sym_result = string_to_symbol(str);
        RawTree field = (RawTree) {
            .type = RawAtom,
            .range.start = start,
            .range.end = bytecount(is),
            .atom.type = ASymbol,
            .atom.symbol = sym_result,
        };

        RawTreeArray nodes = mk_rawtree_array(2, a);
        push_rawtree(proj, &nodes);
        push_rawtree(field, &nodes);

        out.type = ParseSuccess;
        out.result = (RawTree) {
            .type = RawBranch,
            .range.start = start,
            .range.end = bytecount(is),
            .branch.hint = HNone,
            .branch.nodes = nodes,
        };
    }
    
    return out;
}

ParseResult parse_string(IStream* is, Allocator* a) {
    StreamResult result;
    U32Array arr = mk_u32_array(64, a);
    uint32_t codepoint;
    size_t start = bytecount(is);
    next(is, &codepoint); // consume token (")

    while (((result = peek(is, &codepoint)) == StreamSuccess) && codepoint != '"') {
        next(is, &codepoint);
        push_u32(codepoint, &arr);
    }

    if (result != StreamSuccess) {
        return (ParseResult) {
            .type = ParseFail,
            .error.message = mv_string("Stream failed"),
            .error.range.start = bytecount(is),
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

ParseResult parse_char(IStream* is) {
    StreamResult result;
    uint32_t codepoint;
    size_t start = bytecount(is);
    next(is, &codepoint); // consume token #)
    result = next(is, &codepoint);

    if (result != StreamSuccess) {
        return (ParseResult) {
            .type = ParseFail,
            .error.message = mv_string("Stream failed"),
            .error.range.start = bytecount(is),
            .error.range.end = bytecount(is),
        };
    }

    //next(is, &codepoint); // consume token (")
    return (ParseResult) {
        .type = ParseSuccess,
        .result.type = RawAtom,
        .result.range.start = start,
        .result.range.end = bytecount(is),
        .result.atom.type = AIntegral,
        .result.atom.int_64 = codepoint,
    };
}

StreamResult consume_until(uint32_t stop, IStream* is) {
    uint32_t codepoint;
    StreamResult result;

    next(is, &codepoint); // consume token #)
    result = next(is, &codepoint);

    while ((result = peek(is, &codepoint)) == StreamSuccess) {
        if (codepoint != stop) {
            // TODO: Check if newline!
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
            // TODO: Check if newline!
            result = next(is, &codepoint);
        } else if (codepoint == ';') {
            result = consume_until('\n', is);
        } else {
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
