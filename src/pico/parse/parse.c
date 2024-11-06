#include "pico/parse/parse.h"


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
ParseResult parse_list(IStream* is, SourcePos* parse_state, uint32_t term, SyntaxHint hint, Allocator* a);
ParseResult parse_atom(IStream* is, SourcePos* parse_state, Allocator* a);
ParseResult parse_number(IStream* is, SourcePos* parse_state, Allocator* a);
ParseResult parse_prefix(char prefix, IStream* is, SourcePos* parse_state, Allocator* a);
ParseResult parse_string(IStream* is, SourcePos* parse_state, Allocator* a);

// Helper functions
StreamResult consume_whitespace(IStream* is, SourcePos* parse_state);
bool is_numchar(uint32_t codepoint);
bool is_whitespace(uint32_t codepoint);
bool is_symchar(uint32_t codepoint);

ParseResult parse_main(IStream* is, SourcePos* parse_state, Allocator* a) {
    ParseResult res;
    uint32_t point;

    consume_whitespace(is, parse_state);
    switch (peek(is, &point)) {
    case StreamSuccess:
        if (point == '(') {
            res = parse_list(is, parse_state, ')', HExpression, a);
        }
        else if (point == '[') {
            res = parse_list(is, parse_state, ']', HSpecial, a);
        }
        else if (point == '{') {
            res = parse_list(is, parse_state, '}', HImplicit, a);
        }
        else if (is_numchar(point) || point == '-') {
            res = parse_number(is, parse_state, a);
        }
        else if (point == ':') {
            res = parse_prefix(':', is, parse_state, a);
        }
        else if (point == '.') {
            res = parse_prefix('.', is, parse_state, a);
        }
        else if (point == '"') {
            res = parse_string(is, parse_state, a);
        }
        else {
            res = parse_atom(is, parse_state, a);
        }
        break;

    default: {
        res.type = ParseFail;
        res.data.range.start = *parse_state;
        res.data.range.end = *parse_state;
    } break;
    }
    return res;
}

ParseResult parse_list(IStream* is, SourcePos* parse_state, uint32_t term, SyntaxHint hint, Allocator* a) {
    ParseResult res;
    res.type = ParseFail;
    res.data.range.start = *parse_state;
    ParseResult out;
    PtrArray nodes = mk_ptr_array(5, a);
    uint32_t codepoint;

    // Assume '(' is next character
    next(is, &codepoint);
    consume_whitespace(is, parse_state);
    StreamResult sres;
    while ((sres = peek(is, &codepoint)) == StreamSuccess && !(codepoint == term)) {
        res = parse_main(is, parse_state, a);

        if (res.type == ParseFail) {
            out = res;
            break;
        }
        else {
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
    }
    else if (res.type == ParseFail) {
        out = res;
    }
    else {
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
    uint32_t codepoint;
    StreamResult result;
    ParseResult out;
    U32Array arr = mk_u32_array(10, a);

    PtrArray terms = mk_ptr_array(2, a);
    RawTree* rhs = NULL;

    while (((result = peek(is, &codepoint)) == StreamSuccess)) {
        if (is_symchar(codepoint)) {
            next(is, &codepoint);
            push_u32(codepoint, &arr);
        } else if (codepoint == '.') {
            next(is, &codepoint);
            out = parse_atom(is, parse_state, a);
            if (out.type != ParseSuccess)
                return out;
            RawTree* op = mem_alloc(sizeof(RawTree), a);
            *op = (RawTree) {
                .type = RawAtom,
                .hint = HNone,
                .atom.type = ASymbol,
                .atom.symbol = string_to_symbol(mv_string(".")),
            };
            push_ptr(op, &terms);

            rhs = mem_alloc(sizeof(RawTree), a);
            *rhs = out.data.result;
            break;
        } else if (codepoint == ':') {
            next(is, &codepoint);
            out = parse_atom(is, parse_state, a);
            if (out.type != ParseSuccess)
                return out;
            RawTree* op = mem_alloc(sizeof(RawTree), a);
            *op = (RawTree) {
                .type = RawAtom,
                .hint = HNone,
                .atom.type = ASymbol,
                .atom.symbol = string_to_symbol(mv_string(":")),
            };
            push_ptr(op, &terms);

            rhs = mem_alloc(sizeof(RawTree), a);
            *rhs = out.data.result;
            break;
        } else {
            break;
        }
    }
    if (result != StreamSuccess) {
        out.type = ParseFail;
        out.data.range.start = *parse_state;
        out.data.range.end = *parse_state;
    }
    else {
        String str = string_from_UTF_32(arr, a);
        Symbol sym_result = string_to_symbol(str);

        if (terms.len == 0) {
            out.type = ParseSuccess;
            out.data.result.type = RawAtom;
            out.data.result.atom.type = ASymbol;
            out.data.result.atom.symbol = sym_result;
        } else {
            RawTree* lhs = mem_alloc(sizeof(RawTree), a);
            *lhs = (RawTree) {
                .type = RawAtom,
                .hint = HNone,
                .atom.type = ASymbol,
                .atom.symbol = sym_result,
            };
            push_ptr(rhs, &terms);
            push_ptr(lhs, &terms);

            out.type = ParseSuccess;
            out.data.result.type = RawList;
            out.data.result.nodes = terms;
        }
    }
    return out;
}

ParseResult parse_number(IStream* is, SourcePos* parse_state, Allocator* a) {
    uint32_t codepoint;
    StreamResult result;
    U8Array arr = mk_u8_array(10, a);
    bool is_positive = true;

    result = peek(is, &codepoint);
    if (result == StreamSuccess && codepoint == '-') {
        next(is, &codepoint);
        is_positive = false;
    }

    while (((result = peek(is, &codepoint)) == StreamSuccess) && is_numchar(codepoint)) {
        next(is, &codepoint);
        // the cast is safe as is-numchar ensures codepoint < 256
        uint8_t val = (uint8_t) codepoint - 48;
        push_u8(val, &arr);
    }


    if (result != StreamSuccess) {
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

    next(is, &codepoint); // consume token (")
    return (ParseResult) {
        .type = ParseSuccess,
        .data.result.type = RawAtom,
        .data.result.hint = HNone,
        .data.result.atom.type = AString,
        .data.result.atom.string = string_from_UTF_32(arr, a),
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
