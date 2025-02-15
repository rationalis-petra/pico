#include "platform/signals.h"
#include "platform/machine_info.h"

#include "pico/codegen/foreign-adapters.h"

void* convert_c_fn(CType* ctype, PiType* ptype, Assembler* ass, Allocator* a) {
    
}

bool can_reinterpret_prim(CPrim ctype, PrimType ptype) {
#if ABI == SYSTEM_V_64 
    switch (ptype) {
    case Unit:  {
        return false;
    }
    case Bool:  {
        return ctype == CChar || ctype == CUChar;
    }
    case Address: {
        // Address comparisons need to be caught earlier.
        // this is fine to enforce as a contract, as the only caller of this
        // function is can_reinterpret (no risk of many callers being confused
        // by overly complex contract.)
        return false;
    }
    case Int_64: {
        return ctype == CLong; 
    }
    case Int_32: {
        return ctype == CInt; 
    }
    case Int_16: {
        return ctype == CShort; 
    }
    case Int_8: {
        return ctype == CChar; // TODO: is char signed by default?
    }
    case UInt_64: {
        return ctype == CULong;
    }
    case UInt_32: {
        return ctype == CUInt;
    }
    case UInt_16: {
        return ctype == CUInt;
    }
    case UInt_8: {
        return ctype == CUChar;
    }
    case TFormer:  {
        // TODO (FEATURE): check for enum?
        return false;
    }
    case TMacro:  {
        return false;
    }
    }
#elif ABI == WIN_64 
#error "can_reinterpret_prim not imlemented for Win 64"
#else
#error "can_reinterpret_prim not implemented for unknonw arch"
#endif
    // TODO (FEATURE): as this can be invoked by user code with user values, 
    //   perhaps abort on invalid type is too harsh? (throw or return false?) 
    //   perhaps depends on debug state?
    panic(mv_string("Invalid prim provided to can_reinterpret_type"));
}

bool can_reinterpret(CType* ctype, PiType* ptype) {
    // C doesn't have a concept of distinct types, so filter those out. 
    // TODO (BUG LOGIC): possibly don't allow opaque to be converted unless
    // TODO (FEATURE): check for well-formedness of types in debug mode?
    while (ptype->sort == TDistinct) ptype = ptype->distinct.type;

    switch (ptype->sort) {
    case TPrim: 
        if (ctype->sort == CSPrim) {
            return can_reinterpret_prim(ctype->prim, ptype->prim);
        }
        else if (ctype->sort == CSPtr && ptype->prim == Address) {
            return true;
        } else {
            return false;
        }
        break;
    default:
        return false;
    }
}
