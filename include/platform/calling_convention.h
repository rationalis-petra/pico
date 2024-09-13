#ifndef __PLATFORM_CALLING_CONVENTION_H 
#define __PLATFORM_CALLING_CONVENTION_H 

#ifdef __unix__
#define ABI_SYSTEM_V_64
#elif defined(_WIN32) || defined(WIN32)
#define ABI_WIN_64
#else 
#error "Only support Linux/Windows"
#endif

#endif
