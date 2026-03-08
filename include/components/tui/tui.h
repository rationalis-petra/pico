#ifndef __COMPONENTS_TUI
#define __COMPONENTS_TUI

#include <stdint.h>

typedef enum {
    Regular, // │┌ 
    Round,   // │╭ 
    Thick,   // ┃┏ 
    Double,  // ║╔   
} BoxType;

void draw_rectangle(BoxType boxtype, uint16_t col, uint16_t row, uint16_t width, uint16_t height) {
    
}

#endif
