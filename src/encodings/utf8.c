#include "encodings/utf8.h"

void encode_point_utf8(uint8_t* dest, uint8_t* size, uint32_t codepoint) {
    // Encode with 1 byte (7 bits)
    // encoded as 0xxxxxxx
    if (codepoint < 128 ) {
        dest[0] = (uint8_t)codepoint; // downcast is safe, as codepoint < 128
        *size = 1;
    }
    // Encode with 2 bytes (11 bits)
    // encoded as 110xxxxx 10xxxxxx
    else if (codepoint < 2048) {
        dest[0] = ((0x7c0 & codepoint) >> 6) | 0xc0;
        dest[1] = (0x3f & codepoint) | 0x80;
        *size = 2;
    }
    // Encode with 3 bytes (16 bits)
    // encoded as 1110xxxx 10xxxxxx 10xxxxxx
    else if (codepoint < 65536) {
        dest[0] = ((0xf000 & codepoint) >> 12) | 0xe0; 
        dest[1] = ((0xfc0 & codepoint) >> 6) | 0x80;
        dest[2] = (0x3f & codepoint) | 0x80;
        *size = 3;
    }
    // Encode with 4 bytes (21 bits)
    // encoded as 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
    else {
        dest[0] = ((0x1c0000 & codepoint) >> 18) | 0xf0;
        dest[1] = ((0x3f000 & codepoint) >> 12) | 0x80;
        dest[2] = ((0xfc0 & codepoint) >> 6) | 0x80;
        dest[3] = (0x3f & codepoint) | 0x80;
        *size = 4;
    }
}

bool decode_point_utf8(uint8_t* size, uint8_t* str, uint32_t* out) {
    bool success = true;
    uint8_t num_bytes = num_bytes_utf8(str[0]);
    uint32_t codepoint = 0;

    switch (num_bytes) {
    case 1: {
        *size = 1;
        codepoint = str[0];
    } break;
    case 2: {
        *size = 2;
        codepoint = codepoint | (str[0] & 0x1F);
        codepoint = codepoint << 6;
        codepoint = codepoint | (str[1] & 127);
    } break;
    case 3: {
        *size = 3;
        codepoint = codepoint | (str[0] & 0xF);
        codepoint = codepoint << 6;
        codepoint = codepoint | (str[1] & 127);
        codepoint = codepoint << 6;
        codepoint = codepoint | (str[2] & 127);
    } break;
    case 4: {
        *size = 4;
        codepoint = codepoint | (str[0] & 0x7);
        codepoint = codepoint << 6;
        codepoint = codepoint | (str[1] & 127);
        codepoint = codepoint << 6;
        codepoint = codepoint | (str[2] & 127);
        codepoint = codepoint << 6;
        codepoint = str[3] & 127;
    } break;
    default:
        success = false;
    }
    *out = codepoint;
    return success;
}

uint8_t num_bytes_utf8(uint8_t head) {
    // head = 0bbbbbbb => head & 10000000 = 0
    if ((head & 0x80) == 0) {
        return 1;
    }
    // head = 110bbbbb => ((head ^ 0b001bbbbb) & 0b11100000) 
    // # 0b00100000 = 0x20, 1110000 = 0xE0
    else if (((head ^ 0x20) & 0xE0) == 0xE0) {
        return 2;
    }
    // head = 1110bbbb => ~(head ^ 0x00010000 & 11110000)
    // # 0x00010000 = 0x10, 11110000 = 0xF0
    else if (((head ^ 0x10) & 0xF0) == 0xF0) {
        return 3;
    }
    // head = 11110bbb => (head ^ 00001000) & 11111000
    // # 00001000 = 8, 11111000 = 0xF8
    else if (((head ^ 0x8) & 0xF8) == 0xF8) {
        return 4;
    }
    // indicate error occured
    else {
        return 0;
    }
}

uint8_t point_size_utf8(uint32_t codepoint) {
    if (codepoint < 0x80) {
        return 1;
    }
    else if (codepoint < 0x800) {
        return 2;
    }
    else if (codepoint < 0x10000) {
        return 3;
    }
    else {
        return 4;
    }
}
