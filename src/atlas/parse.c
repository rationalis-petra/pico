#include "atlas/parse.h"

static AtParseResult parse_expr(IStream* is, uint32_t expected, RegionAllocator* region);
static AtParseResult parse_list(IStream* is, uint32_t terminator, RegionAllocator* region);
static AtParseResult parse_atom(IStream* is, bool keyword, RegionAllocator* region);
static AtParseResult parse_string(IStream* is, RegionAllocator* region);

static StreamResult consume_until(uint32_t stop, IStream* is);
static StreamResult consume_whitespace(IStream* is);
static bool is_whitespace(uint32_t codepoint);
static bool is_symchar(uint32_t codepoint);

AtParseResult parse_atlas_defs(IStream *is, RegionAllocator *region) {
    return parse_expr(is, '\0', region);
}

AtParseResult parse_expr(IStream* is, uint32_t expected, RegionAllocator* region) {
    Allocator gpa = ra_to_gpa(region);

    // Default if we never enter loop body 
    AtParseResult out = (AtParseResult) {.type = ParseNone};
    uint32_t point;

    consume_whitespace(is);
    StreamResult result;
    RawAtlasArray terms = mk_rawatlas_array(8, &gpa);
    bool running = true;

    while (running && ((result = peek(is, &point)) == StreamSuccess)) {
        switch (peek(is, &point)) {
        case StreamSuccess:
            if (point == '(') {
                out = parse_list(is, ')', region);
            }
            else if (point == '"') {
                out = parse_string(is, region);
            } else if (point == ':') {
                out = parse_atom(is, true, region);
            }
            else if (is_whitespace(point)) {
                // Whitespace always terminates a unit, e.g. 
                out.type = ParseNone;
                running = false;
                break;
            }
            else if (is_symchar(point)){
                out = parse_atom(is, false, region);
            } else if (point == expected) {
                // We couldn't do a parse!
                out.type = ParseNone;
                running = false;
                break;
            } else {
                // We couldn't do a parse!
                size_t range_start = bytecount(is);
                next(is, &point);

                String actual_string = string_from_codepoint(point, &gpa);
                String expected_string = string_from_codepoint(expected, &gpa);
                String message = string_ncat(&gpa, 5,
                                             mv_string("Unexpected character: '"),
                                             actual_string,
                                             mv_string("', expected: '"),
                                             expected_string,
                                             mv_string("'"));

                out = (AtParseResult) {
                    .type = ParseFail,
                    .error.range.start = range_start,
                    .error.range.end = bytecount(is),
                    .error.message = mv_str_doc(message, &gpa),
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
            out = (AtParseResult) {
                .type = ParseFail,
                .error.message = mv_cstr_doc("Stream result was in unexpected state.", &gpa),
                .error.range.start = bytecount(is),
                .error.range.end = bytecount(is),
            };
            running = false;
        } break;
        }

        if (out.type == ParseSuccess) {
            push_rawatlas(out.result, &terms);
        } else if (out.type == ParseFail) {
            running = false;
        }
    }
    return out;
}

static AtParseResult parse_list(IStream* is, uint32_t terminator, RegionAllocator* region) {
    Allocator gpa = ra_to_gpa(region);
    AtParseResult res;
    res.type = ParseSuccess;
    AtParseResult out;
    RawAtlasArray nodes = mk_rawatlas_array(8, &gpa);
    uint32_t codepoint;

    // Assume '(' is next character
    size_t start = bytecount(is);
    next(is, &codepoint);
    consume_whitespace(is); 
    StreamResult sres;

    while ((sres = peek(is, &codepoint)) == StreamSuccess && (codepoint != terminator)) {
        res = parse_expr(is, terminator, region);

        if (res.type == ParseFail) {
            out = res;
            break;
        } else {
            push_rawatlas(res.result, &nodes);
        }
        consume_whitespace(is);
    }

    if (sres == StreamEnd) {
        out = (AtParseResult) {
            .type = ParseFail,
            .error.message = mv_cstr_doc("Unexpected end of stream. List started here was still parsing", &gpa),
            .error.range.start = start,
            .error.range.end = start + 1,
        };
    } else if (sres != StreamSuccess) {
        out = (AtParseResult) {
            .type = ParseFail,
            .error.message = mv_cstr_doc("Input stream failure", &gpa),
            .error.range.start = bytecount(is),
            .error.range.end = bytecount(is),
        };
    } else if (res.type == ParseFail) {
        out = res;
    } else {
        // consume closing ')'
        next(is, &codepoint);

        out.type = ParseSuccess;
        out.result = (RawAtlas) {
            .type = AtlBranch,
            .range.start = start,
            .range.end = bytecount(is),
            .branch = nodes,
        };
    }
    return out;
}

static AtParseResult parse_atom(IStream* is, bool is_keyword, RegionAllocator* region) {
    /* The parse_atom function is responsible for parsing symbols and 'symbol conglomerates'
     * These may be 'true' atoms such as bar, + or foo. Strings separated by '.'
     * and ':' such as Maybe:none and foo.var are also considered by the parser
     * as 'atoms'
     * as these elements are not separated by spaces and bind tightly.
     * 
     * The general approach is as follows:
     */
    Allocator gpa = ra_to_gpa(region);
    uint32_t codepoint;
    StreamResult result;
    AtParseResult out;
    U32Array arr = mk_u32_array(16, &gpa);

    RawAtlasArray terms = mk_rawatlas_array(8, &gpa);
    size_t start = bytecount(is);

    // Accumulate a list of symbols, so, for example, 
    // num:i64.+ becomes {'num', ':', 'i64', '.', '+'}

    // consume starting ':' if keyword
    if (is_keyword) next(is, &codepoint);

    while (((result = peek(is, &codepoint)) == StreamSuccess)) {
        if (is_symchar(codepoint)) {
            next(is, &codepoint);
            push_u32(codepoint, &arr);
        } else {
            String str = string_from_UTF_32(arr, &gpa);
            RawAtlas val = (RawAtlas) {
                .type = AtlAtom,
                .range.start = start,
                .range.end = bytecount(is),
                .atom.type = is_keyword ? AtKeyword : AtSymbol,
                .atom.symbol = string_to_symbol(str),
            };
            push_rawatlas(val, &terms);

            // We are done; break out of loop
            break;
        }
    }

    if (result == StreamEnd) {
        String str = string_from_UTF_32(arr, &gpa);
        RawAtlas val = (RawAtlas) {
            .type = AtlAtom,
            .range.start = start,
            .range.end = bytecount(is),
            .atom.type = is_keyword ? AtKeyword : AtSymbol,
            .atom.symbol = string_to_symbol(str),
        };
        push_rawatlas(val, &terms);
    }

    if (result != StreamSuccess && result != StreamEnd) {
        out = (AtParseResult) {
            .type = ParseFail,
            .error.message = mv_cstr_doc("Stream failure.", &gpa),
            .error.range.start = bytecount(is),
            .error.range.end = bytecount(is),
        };
    } else {
        // Now that the list has been accumulated, 'unroll' the list appropriately, 
        // meaning that (num : i64 . +) becomes (. + (: num i64))
        RawAtlas current = terms.data[0];
        for (size_t i = 1; terms.len - i != 0; i += 2) {
            RawAtlasArray children = mk_rawatlas_array(3, &gpa);
            push_rawatlas(terms.data[i], &children);
            push_rawatlas(terms.data[i+1], &children);
            push_rawatlas(current, &children);

            current = (RawAtlas) {
                .type = AtlBranch,
                .range.start = current.range.start,
                .range.end = terms.data[i+1].range.end,
                .branch = children,
            };
        };

        out = (AtParseResult) {
            .type = ParseSuccess,
            .result = current,
        };
    }
    return out;
}

static AtParseResult parse_string(IStream *is, RegionAllocator *a) {
    Allocator gpa = ra_to_gpa(a);
    StreamResult result;
    U32Array arr = mk_u32_array(64, &gpa);
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
                return (AtParseResult) {
                    .type = ParseFail,
                    .error.message = mv_cstr_doc("Unrecognized escape character", &gpa),
                    .error.range.start = escape_start,
                    .error.range.end = bytecount(is),
                };
            }
        }
        push_u32(codepoint, &arr);
        next(is, &codepoint);
    }

    if (result != StreamSuccess) {
        return (AtParseResult) {
            .type = ParseFail,
            .error.message = mv_cstr_doc("Stream failed", &gpa),
            .error.range.start = start,
            .error.range.end = bytecount(is),
        };
    }

    next(is, &codepoint); // consume token (")
    return (AtParseResult) {
        .type = ParseSuccess,
        .result.type = AtlAtom,
        .result.range.start = start,
        .result.range.end = bytecount(is),
        .result.atom.type = AtString,
        .result.atom.string = string_from_UTF_32(arr, &gpa),
    };
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

bool is_whitespace(uint32_t codepoint) {
    return (codepoint == 32) | (9 <= codepoint && codepoint <= 13);
}

bool is_symchar(uint32_t codepoint) {
    return !is_whitespace(codepoint) && !(codepoint == '('
                                          || codepoint == ')'
                                          || codepoint == '['
                                          || codepoint == ']'
                                          || codepoint == '{'
                                          || codepoint == '}'
                                          || codepoint == ':');
}
