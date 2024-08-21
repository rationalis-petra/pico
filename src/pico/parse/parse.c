#include "pico/parse/parse.h"


// The 'actual' parser funciton
parse_result parse_main(istream* is, sourcepos* parse_state, allocator a);

parse_result parse_rawtree(istream* is, allocator a) {
    sourcepos start_state;
    start_state.col = 0;
    start_state.row = 0;
    return parse_main(is, &start_state, a);
}

// The three main parsing functions, which parse:
// + lists
// + numbers
// + symbols
// The 'main' parser does lookahead to dispatch on the appropriate parsing function.
parse_result parse_list(istream* is, sourcepos* parse_state, uint32_t term, allocator a);
parse_result parse_atom(istream* is, sourcepos* parse_state, allocator a);
parse_result parse_number(istream* is, sourcepos* parse_state, allocator a);
parse_result parse_prefix(char prefix, istream* is, sourcepos* parse_state, allocator a);

// Helper functions
stream_result consume_whitespace(istream* is, sourcepos* parse_state);
bool is_numchar(uint32_t codepoint);
bool is_whitespace(uint32_t codepoint);
bool is_symchar(uint32_t codepoint);

parse_result parse_main(istream* is, sourcepos* parse_state, allocator a) {
    parse_result res;
    uint32_t point;

    consume_whitespace(is, parse_state);
    switch (peek(is, &point)) {
    case StreamSuccess:
        if (point == '(') {
            res = parse_list(is, parse_state, ')', a);
        }
        else if (point == '[') {
            res = parse_list(is, parse_state, ']', a);
        }
        else if (is_numchar(point)) {
            res = parse_number(is, parse_state, a);
        }
        else if (point == ':') {
            res = parse_prefix(':', is, parse_state, a);
        }
        else if (point == '.') {
            res = parse_prefix('.', is, parse_state, a);
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

parse_result parse_list(istream* is, sourcepos* parse_state, uint32_t term, allocator a) {
    parse_result res;
    res.type = ParseFail;
    res.data.range.start = *parse_state;
    parse_result out;
    ptr_array nodes = mk_ptr_array(5, a);
    uint32_t codepoint;

    // Assume '(' is next character
    next(is, &codepoint);
    consume_whitespace(is, parse_state);
    stream_result sres;
    while ((sres = peek(is, &codepoint)) == StreamSuccess && !(codepoint == term)) {
        res = parse_main(is, parse_state, a);

        if (res.type == ParseFail) {
            out = res;
            break;
        }
        else {
            pi_rawtree* node = (pi_rawtree*)mem_alloc(sizeof(pi_rawtree), a);
            node->type = res.data.result.type;
            node->data = res.data.result.data;
            push_ptr(node, &nodes, a);
        }
        consume_whitespace(is, parse_state);
    }
    if (sres != StreamSuccess) {
        out.type = ParseFail;
        out.data.range.start = *parse_state;
        out.data.range.end = *parse_state;
        delete_ptr_array(nodes, (void(*)(void*, allocator))delete_rawtree_ptr, a);
    }
    else if (res.type == ParseFail) {
        out = res;
        delete_ptr_array(nodes, (void(*)(void*, allocator))delete_rawtree_ptr, a);
    }
    else {
        out.type = ParseSuccess;
        out.data.result.type = RawList;
        out.data.result.data.nodes = nodes;
        // consume closing ')'
        next(is, &codepoint);
    }
    return out;
}

parse_result parse_atom(istream* is, sourcepos* parse_state, allocator a) {
    uint32_t codepoint;
    stream_result result;
    parse_result out;
    u32_array arr = mk_u32_array(10, a);

    ptr_array terms = mk_ptr_array(2, a);
    pi_rawtree* rhs;

    while (((result = peek(is, &codepoint)) == StreamSuccess)) {
        if (is_symchar(codepoint)) {
            next(is, &codepoint);
            push_u32(codepoint, &arr, a);
        } else if (codepoint == '.') {
            next(is, &codepoint);
            out = parse_atom(is, parse_state, a);
            if (out.type != ParseSuccess)
                return out;
            pi_rawtree* op = mem_alloc(sizeof(pi_rawtree), a);
            *op = (pi_rawtree) {
                .type = RawAtom,
                .data.atom.type = ASymbol,
                .data.atom.symbol = string_to_symbol(mv_string(".")),
            };
            push_ptr(op, &terms, a);

            rhs = mem_alloc(sizeof(pi_rawtree), a);
            *rhs = out.data.result;
            break;
        } else if (codepoint == ':') {
            next(is, &codepoint);
            out = parse_atom(is, parse_state, a);
            if (out.type != ParseSuccess)
                return out;
            pi_rawtree* op = mem_alloc(sizeof(pi_rawtree), a);
            *op = (pi_rawtree) {
                .type = RawAtom,
                .data.atom.type = ASymbol,
                .data.atom.symbol = string_to_symbol(mv_string(":")),
            };
            push_ptr(op, &terms, a);

            rhs = mem_alloc(sizeof(pi_rawtree), a);
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
        string str = string_from_UTF_32(arr, a);
        pi_symbol sym_result = string_to_symbol(str);
        sdelete_u32_array(arr, a);
        delete_string(str, a);

        if (terms.len == 0) {
            out.type = ParseSuccess;
            out.data.result.type = RawAtom;
            out.data.result.data.atom.type = ASymbol;
            out.data.result.data.atom.symbol = sym_result;
            sdelete_ptr_array(terms, a);
        } else {
            pi_rawtree* lhs = mem_alloc(sizeof(pi_rawtree), a);
            *lhs = (pi_rawtree) {
                .type = RawAtom,
                .data.atom.type = ASymbol,
                .data.atom.symbol = sym_result,
            };
            push_ptr(rhs, &terms, a);
            push_ptr(lhs, &terms, a);

            out.type = ParseSuccess;
            out.data.result.type = RawList;
            out.data.result.data.nodes = terms;
        }
    }
    return out;
}

parse_result parse_number(istream* is, sourcepos* parse_state, allocator a) {
    uint32_t codepoint;
    stream_result result;
    parse_result out;
    u8_array arr = mk_u8_array(10, a);
    while (((result = peek(is, &codepoint)) == StreamSuccess) && is_numchar(codepoint)) {
        next(is, &codepoint);
        // the cast is safe as is-numchar ensures codepoint < 256
        uint8_t val = (uint8_t) codepoint - 48;
        push_u8(val, &arr, a);
    }
    if (result != StreamSuccess) {
        out.type = ParseFail;
        out.data.range.start = *parse_state;
        out.data.range.end = *parse_state;
    }
    else {
        int64_t int_result = 0;
        uint64_t tens = 1;
        for (size_t i = arr.len; i > 0; i--) {
            int_result += tens * aref_u8(i-1, arr);
            tens *= 10;
        }
        out.type = ParseSuccess;
        out.data.result.type = RawAtom;
        out.data.result.data.atom.type = AI64;
        out.data.result.data.atom.int_64 = int_result;
    }
    sdelete_u8_array(arr, a);
    return out;
}

parse_result parse_prefix(char prefix, istream* is, sourcepos* parse_state, allocator a) {
    uint32_t codepoint;
    stream_result result;
    parse_result out;
    u32_array arr = mk_u32_array(10, a);

    next(is, &codepoint); // consume token

    while (((result = peek(is, &codepoint)) == StreamSuccess) && is_symchar(codepoint)) {
        next(is, &codepoint);
        push_u32(codepoint, &arr, a);
    }

    if (result != StreamSuccess) {
        out.type = ParseFail;
        out.data.range.start = *parse_state;
        out.data.range.end = *parse_state;
    } else if (arr.len == 0) {
        char cstr[2] = {prefix, '\0'};
        string str = mv_string(cstr);
        pi_symbol sym_result = string_to_symbol(str);

        out.type = ParseSuccess;
        out.data.result.type = RawAtom;
        out.data.result.data.atom.type = ASymbol;
        out.data.result.data.atom.symbol = sym_result;

    } else {
        char cstr[2] = {prefix, '\0'};
        string str = mv_string(cstr);
        pi_symbol sym_result = string_to_symbol(str);
        pi_rawtree* proj = mem_alloc(sizeof(pi_rawtree), a);
        proj->type = RawAtom;
        proj->data.atom.type = ASymbol;
        proj->data.atom.symbol = sym_result;

        str = string_from_UTF_32(arr, a);
        sym_result = string_to_symbol(str);
        delete_string(str, a);
        pi_rawtree* field = mem_alloc(sizeof(pi_rawtree), a);
        field->type = RawAtom;
        field->data.atom.type = ASymbol;
        field->data.atom.symbol = sym_result;

        ptr_array nodes = mk_ptr_array(2, a);
        push_ptr(proj, &nodes, a);
        push_ptr(field, &nodes, a);

        out.type = ParseSuccess;
        out.data.result.type = RawList;
        out.data.result.data.nodes = nodes;
    }

    // cleanup
    sdelete_u32_array(arr, a);
    
    return out;
}

stream_result consume_whitespace(istream* is, sourcepos* parse_state) {
    uint32_t codepoint;
    stream_result result;
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
                                          || codepoint == '.'
                                          || codepoint == ':');
}
