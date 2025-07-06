#ifndef __DATA_NUM_H
#define __DATA_NUM_H

#include <stdint.h>

void set_unaligned_ptr(void** dest, void* src);

void set_unaligned_u32(uint32_t* dest, uint32_t src);
void set_unaligned_i32(int32_t* dest, int32_t src);

#endif
