#include "encodings/utf8.h"

void encode_point_utf8(uint8_t* dest, uint8_t* size, uint32_t codepoint) {
    // Encode with 1 byte (7 bits)
    // encoded as 0xxxxxxx
    if (codepoint < 128 ) {
        dest[0] = codepoint;
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
    else if (codepoint < 2097152) {
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
        codepoint = codepoint << 5;
        codepoint = codepoint | (str[1] & 127);
    } break;
    case 3: {
        *size = 3;
        codepoint = codepoint | (str[0] & 0xF);
        codepoint = codepoint << 4;
        codepoint = codepoint | (str[1] & 127);
        codepoint = codepoint << 7;
        codepoint = codepoint | (str[2] & 127);
    } break;
    case 4: {
        *size = 4;
        codepoint = codepoint | (str[0] & 0x7);
        codepoint = codepoint << 3;
        codepoint = codepoint | (str[1] & 127);
        codepoint = codepoint << 7;
        codepoint = codepoint | (str[2] & 127);
        codepoint = codepoint << 7;
        codepoint = str[3] & 127;
    } break;
    default:
        success = false;
    }
    *out = codepoint;
    return success;
}

char num_bytes_utf8(uint8_t head) {
    // # 128 = 10000000
    if ((head & 128) == 0) {
        return 1;
    }
    // # (128 + 64) = 11000000
    else if (head & (128 + 64)) {
        return 2;
    }
    // # (128 + 64 + 32) = 11100000
    else if (head & (128 + 64 + 32)) {
        return 3;
    }
    // # (128 + 64 + 32 + 16) = 11110000
    else if (head & (128 + 64 + 32 + 16)) {
        return 4;
    }
    // indicate error occured
    else {
        return 0;
    }
}

char point_size_utf8(uint32_t codepoint) {
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
