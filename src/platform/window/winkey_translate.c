#include "platform/machine_info.h"
#ifdef WINDOW_SYSTEM
#if (OS_FAMILY == WINDOWS)

#include "platform/window/winkey_translate.h"
#include "platform/window/window.h"
#include "platform/signals.h"

RawKey keycode_to_rawkey(WPARAM windows_keycode) {
    switch (windows_keycode) {
    case VK_BACK:
        return RKEY_BACKSPACE;
    case VK_RETURN:
        return RKEY_ENTER;
    case VK_SHIFT:
        return RKEY_SHIFT;
    case VK_SPACE:
        return RKEY_SPACE;
    case 0x30:
        return RKEY_0;
    case 0x31:
        return RKEY_1;
    case 0x32:
        return RKEY_2;
    case 0x33:
        return RKEY_3;
    case 0x34:
        return RKEY_4;
    case 0x35:
        return RKEY_5;
    case 0x36:
        return RKEY_6;
    case 0x37:
        return RKEY_7;
    case 0x38:
        return RKEY_8;
    case 0x39:
        return RKEY_9;
        
    case 'A':
        return RKEY_A;
    case 'B':
        return RKEY_B;
    case 'C':
        return RKEY_C;
    case 'D':
        return RKEY_D;
    case 'E':
        return RKEY_E;
    case 'F':
        return RKEY_F;
    case 'G':
        return RKEY_G;
    case 'H':
        return RKEY_H;
    case 'I':
        return RKEY_I;
    case 'J':
        return RKEY_J;
    case 'K':
        return RKEY_K;
    case 'L':
        return RKEY_L;
    case 'M':
        return RKEY_M;
    case 'N':
        return RKEY_N;
    case 'O':
        return RKEY_O;
    case 'P':
        return RKEY_P;
    case 'Q':
        return RKEY_Q;
    case 'R':
        return RKEY_R;
    case 'S':
        return RKEY_S;
    case 'T':
        return RKEY_T;
    case 'U':
        return RKEY_U;
    case 'V':
        return RKEY_V;
    case 'W':
        return RKEY_W;
    case 'X':
        return RKEY_X;
    case 'Y':
        return RKEY_Y;
    case 'Z':
        return RKEY_Z;
        
    case VK_OEM_PLUS:
        return RKEY_PLUS;
    case VK_OEM_COMMA:
        return RKEY_COMMA;
    case VK_OEM_MINUS:
        return RKEY_MINUS;
    case VK_OEM_PERIOD:
        return RKEY_DOT;
    case VK_OEM_2:
        return RKEY_DIVIDE;

    }
    panic(mv_string("don't recognize this virtual keyode!"));
}

WPARAM rawkey_to_keycode(RawKey rawkey) {
    switch (rawkey) {
    case RKEY_0:
        return 0x30;
    case RKEY_1:
        return 0x31;
    case RKEY_2:
        return 0x32;
    case RKEY_3:
        return 0x33;
    case RKEY_4:
        return 0x34;
    case RKEY_5:
        return 0x35;
    case RKEY_6:
        return 0x36;
    case RKEY_7:
        return 0x37;
    case RKEY_8:
        return 0x38;
    case RKEY_9:
        return 0x39;
        
    case RKEY_A:
        return 'A';
    case RKEY_B:
        return 'B';
    case RKEY_C:
        return 'C';
    case RKEY_D:
        return 'D';
    case RKEY_E:
        return 'E';
    case RKEY_F:
        return 'F';
    case RKEY_G:
        return 'G';
    case RKEY_H:
        return 'H';
    case RKEY_I:
        return 'I';
    case RKEY_J:
        return 'J';
    case RKEY_K:
        return 'K';
    case RKEY_L:
        return 'L';
    case RKEY_M:
        return 'M';
    case RKEY_N:
        return 'N';
    case RKEY_O:
        return 'O';
    case RKEY_P:
        return 'P';
    case RKEY_Q:
        return 'Q';
    case RKEY_R:
        return 'R';
    case RKEY_S:
        return 'S';
    case RKEY_T:
        return 'T';
    case RKEY_U:
        return 'U';
    case RKEY_V:
        return 'V';
    case RKEY_W:
        return 'W';
    case RKEY_X:
        return 'X';
    case RKEY_Y:
        return 'Y';
    case RKEY_Z:
        return 'Z';

        /*
    RKEY_EXCLAMATION,
    RKEY_AT,
    RKEY_HASH,
    RKEY_DOLLAR,
    RKEY_PERCENT,
    RKEY_CARET,
    RKEY_AMPERSAND,
    RKEY_ASTERISK,
    RKEY_LPAREN,
    RKEY_RPAREN,
    */
    case RKEY_MINUS:
        return VK_OEM_MINUS;
    case RKEY_PLUS:
        return VK_OEM_PLUS;

    //RKEY_LBRACE,
    //RKEY_RBRACE,
    //RKEY_COLON,
    //RKEY_SEMICOLON,
    case RKEY_COMMA:
        return VK_OEM_COMMA;
    case RKEY_DOT:
        return VK_OEM_PERIOD;
    case RKEY_DIVIDE:
        return VK_OEM_2;
    //    return VK_

    case RKEY_SPACE:
        return VK_SPACE;

    case RKEY_SHIFT:
        return VK_SHIFT;
    case RKEY_BACKSPACE:
        return VK_BACK;
    case RKEY_ENTER:
        return VK_RETURN;
    default:
        panic(mv_string("Virtual Keycode not recognized when translating to 'raw' key"));
    }

    panic(mv_string("don't recognize this rawkey!"));
}

Key translate_win_keycode(RawKey key, uint16_t unicode, bool use_unicode) {
  if (use_unicode) {
      switch (unicode) {
        case 8:
            return PKEY_BACKSPACE;
        case 13:
            return PKEY_ENTER;
        case ' ':
            return PKEY_SPACE;
        case '!':
            return PKEY_EXCLAMATION;
        case '#':
            return PKEY_HASH;
        case '$':
            return PKEY_DOLLAR;
        case '%':
            return PKEY_PERCENT;
        case '&':
            return PKEY_AMPERSAND;
        case '(':
            return PKEY_LPAREN;
        case ')':
            return PKEY_RPAREN;
        case '*':
            return PKEY_ASTERISK;
        case '+':
            return PKEY_PLUS;
        case ',':
            return PKEY_COMMA;
        case '-':
            return PKEY_MINUS;
        case '.':
            return PKEY_DOT;
        case '/':
            return PKEY_DIVIDE;
        case '0':
            return PKEY_0;
        case '1':
            return PKEY_1;
        case '2':
            return PKEY_2;
        case '3':
            return PKEY_3;
        case '4':
            return PKEY_4;
        case '5':
            return PKEY_5;
        case '6':
            return PKEY_6;
        case '7':
            return PKEY_7;
        case '8':
            return PKEY_8;
        case '9':
            return PKEY_9;
        case ':':
            return PKEY_COLON;
        case ';':
            return PKEY_SEMICOLON;
        case '?':
            return PKEY_QUERY;
        case '@':
            return PKEY_AT;
        case 'A':
            return PKEY_A;
        case 'B':
            return PKEY_B;
        case 'C':
            return PKEY_C;
        case 'D':
            return PKEY_D;
        case 'E':
            return PKEY_E;
        case 'F':
            return PKEY_F;
        case 'G':
            return PKEY_G;
        case 'H':
            return PKEY_H;
        case 'I':
            return PKEY_I;
        case 'J':
            return PKEY_J;
        case 'K':
            return PKEY_K;
        case 'L':
            return PKEY_L;
        case 'M':
            return PKEY_M;
        case 'N':
            return PKEY_N;
        case 'O':
            return PKEY_O;
        case 'P':
            return PKEY_P;
        case 'Q':
            return PKEY_Q;
        case 'R':
            return PKEY_R;
        case 'S':
            return PKEY_S;
        case 'T':
            return PKEY_T;
        case 'U':
            return PKEY_U;
        case 'V':
            return PKEY_V;
        case 'W':
            return PKEY_W;
        case 'X':
            return PKEY_X;
        case 'Y':
            return PKEY_Y;
        case 'Z':
            return PKEY_Z;
        case '[':
            return PKEY_LBRACE;
        case ']':
            return PKEY_RBRACE;
        case '^':
            return PKEY_CARET;
        case 'a':
            return PKEY_A_Lower;
        case 'b':
            return PKEY_B_Lower;
        case 'c':
            return PKEY_C_Lower;
        case 'd':
            return PKEY_D_Lower;
        case 'e':
            return PKEY_E_Lower;
        case 'f':
            return PKEY_F_Lower;
        case 'g':
            return PKEY_G_Lower;
        case 'h':
            return PKEY_H_Lower;
        case 'i':
            return PKEY_I_Lower;
        case 'j':
            return PKEY_J_Lower;
        case 'k':
            return PKEY_K_Lower;
        case 'l':
            return PKEY_L_Lower;
        case 'm':
            return PKEY_M_Lower;
        case 'n':
            return PKEY_N_Lower;
        case 'o':
            return PKEY_O_Lower;
        case 'p':
            return PKEY_P_Lower;
        case 'q':
            return PKEY_Q_Lower;
        case 'r':
            return PKEY_R_Lower;
        case 's':
            return PKEY_S_Lower;
        case 't':
            return PKEY_T_Lower;
        case 'u':
            return PKEY_U_Lower;
        case 'v':
            return PKEY_V_Lower;
        case 'w':
            return PKEY_W_Lower;
        case 'x':
            return PKEY_X_Lower;
        case 'y':
            return PKEY_Y_Lower;
        case 'z':
            return PKEY_Z_Lower;
        default:
            panic(mv_string("Unhandled Unicode(UTF-16) when translating to Processed Key"));
        }
  } else {
      switch (key) {
      case RKEY_BACKSPACE:
          return PKEY_BACKSPACE;
      case RKEY_SHIFT:
          return PKEY_SHIFT;
      case RKEY_ENTER:
          return PKEY_ENTER;
      case RKEY_SPACE:
          return PKEY_SPACE;
      case RKEY_0:
          return PKEY_0;
      case RKEY_1:
          return PKEY_1;
      case RKEY_2:
          return PKEY_2;
      case RKEY_3:
          return PKEY_3;
      case RKEY_4:
          return PKEY_4;
      case RKEY_5:
          return PKEY_5;
      case RKEY_6:
          return PKEY_6;
      case RKEY_7:
          return PKEY_7;
      case RKEY_8:
          return PKEY_8;
      case RKEY_9:
          return PKEY_9;
        
      case RKEY_A:
          return PKEY_A_Lower;
      case RKEY_B:
          return PKEY_B_Lower;
      case RKEY_C:
          return PKEY_C_Lower;
      case RKEY_D:
          return PKEY_D_Lower;
      case RKEY_E:
          return PKEY_E_Lower;
      case RKEY_F:
          return PKEY_F_Lower;
      case RKEY_G:
          return PKEY_G_Lower;
      case RKEY_H:
          return PKEY_H_Lower;
      case RKEY_I:
          return PKEY_I_Lower;
      case RKEY_J:
          return PKEY_J_Lower;
      case RKEY_K:
          return PKEY_K_Lower;
      case RKEY_L:
          return PKEY_L_Lower;
      case RKEY_M:
          return PKEY_M_Lower;
      case RKEY_N:
          return PKEY_N_Lower;
      case RKEY_O:
          return PKEY_O_Lower;
      case RKEY_P:
          return PKEY_P_Lower;
      case RKEY_Q:
          return PKEY_Q_Lower;
      case RKEY_R:
          return PKEY_R_Lower;
      case RKEY_S:
          return PKEY_S_Lower;
      case RKEY_T:
          return PKEY_T_Lower;
      case RKEY_U:
          return PKEY_U_Lower;
      case RKEY_V:
          return PKEY_V_Lower;
      case RKEY_W:
          return PKEY_W_Lower;
      case RKEY_X:
          return PKEY_X_Lower;
      case RKEY_Y:
          return PKEY_Y_Lower;
      case RKEY_Z:
          return PKEY_Z_Lower;
      default:
        panic(mv_string("Unhandled RawKey when translating to Processed Key"));
      }
  }
  panic(mv_string("failed to translate keycode"));
}

#endif
#endif
