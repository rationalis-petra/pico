#ifndef __ENCODINGS_UTF8_H
#define __ENCODINGS_UTF8_H

#include <stdbool.h> 
#include <stdint.h> 

// Given a unicode codepoint (encoded as a 32-bit unsigned int), encode it as utf-8 into the 'dest' 
// out parameter. The size out parameter should be updated to indicate how many bytes were used.
void encode_point_utf8(uint8_t* dest, uint8_t* size, uint32_t codepoint);

// Given an array of bytes (str), read out a single 32-bit unicode codepoint
// the size out parameter is updated to indicate how many bytes were read.
// the size out parameter is set to 0 if the encoding is invalid
bool decode_point_utf8(uint8_t* size, uint8_t* str, uint32_t* out);

// Given a byte, assumed to be the first byte of a (potentially) multibyte character, return the number of 
// bytes the rest of the encoding will use.
// returns 0 if head is an invalid start of a utf-8 string.
char num_bytes_utf8(uint8_t head);

// Given a byte, assumed to be the first byte of a (potentially) multibyte character, return the number of 
// bytes the rest of the encoding will use.
// returns 0 if head is an invalid start of a utf-8 string.
char point_size_utf8(uint32_t codepoint);

// encode_string
// decode_string

#endif