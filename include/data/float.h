#ifndef __DATA_FLOAT_H
#define __DATA_FLOAT_H

#include <math.h>

// TODO: proper float32/float64 types??
//typedef _Float32 float32_t;
//typedef _Float64 float64_t;
typedef float float32_t;
typedef double float64_t;

float32_t sin_f32(float32_t val); 
float64_t sin_f64(float64_t val); 

float32_t cos_f32(float32_t val); 
float64_t cos_f64(float64_t val); 

#endif
