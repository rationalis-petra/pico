#include "platform/window/xkb_translate.h"

Key translate_xkb_keycode(xkb_keysym_t key) {
    Key out = UINT32_MAX;
    switch (key) {
    case XKB_KEY_a:
        out = PKEY_A_Lower;
        break;
    case XKB_KEY_b:
        out = PKEY_B_Lower;
        break;
    case XKB_KEY_c:
        out = PKEY_C_Lower;
        break;
    case XKB_KEY_d:
        out = PKEY_D_Lower;
        break;
    case XKB_KEY_e:
        out = PKEY_E_Lower;
        break;
    case XKB_KEY_f:
        out = PKEY_F_Lower;
        break;
    case XKB_KEY_g:
        out = PKEY_G_Lower;
        break;
    case XKB_KEY_h:
        out = PKEY_H_Lower;
        break;
    case XKB_KEY_i:
        out = PKEY_I_Lower;
        break;
    case XKB_KEY_j:
        out = PKEY_J_Lower;
        break;
    case XKB_KEY_k:
        out = PKEY_K_Lower;
        break;
    case XKB_KEY_l:
        out = PKEY_L_Lower;
        break;
    case XKB_KEY_m:
        out = PKEY_M_Lower;
        break;
    case XKB_KEY_n:
        out = PKEY_N_Lower;
        break;
    case XKB_KEY_o:
        out = PKEY_O_Lower;
        break;
    case XKB_KEY_p:
        out = PKEY_P_Lower;
        break;
    case XKB_KEY_q:
        out = PKEY_Q_Lower;
        break;
    case XKB_KEY_r:
        out = PKEY_R_Lower;
        break;
    case XKB_KEY_s:
        out = PKEY_S_Lower;
        break;
    case XKB_KEY_t:
        out = PKEY_T_Lower;
        break;
    case XKB_KEY_u:
        out = PKEY_U_Lower;
        break;
    case XKB_KEY_v:
        out = PKEY_V_Lower;
        break;
    case XKB_KEY_w:
        out = PKEY_W_Lower;
        break;
    case XKB_KEY_x:
        out = PKEY_X_Lower;
        break;
    case XKB_KEY_y:
        out = PKEY_Y_Lower;
        break;
    case XKB_KEY_z:
        out = PKEY_Z_Lower;
        break;

    case XKB_KEY_A:
        out = PKEY_A;
        break;
    case XKB_KEY_B:
        out = PKEY_B;
        break;
    case XKB_KEY_C:
        out = PKEY_C;
        break;
    case XKB_KEY_D:
        out = PKEY_D;
        break;
    case XKB_KEY_E:
        out = PKEY_E;
        break;
    case XKB_KEY_F:
        out = PKEY_F;
        break;
    case XKB_KEY_G:
        out = PKEY_G;
        break;
    case XKB_KEY_H:
        out = PKEY_H;
        break;
    case XKB_KEY_I:
        out = PKEY_I;
        break;
    case XKB_KEY_J:
        out = PKEY_J;
        break;
    case XKB_KEY_K:
        out = PKEY_K;
        break;
    case XKB_KEY_L:
        out = PKEY_L;
        break;
    case XKB_KEY_M:
        out = PKEY_M;
        break;
    case XKB_KEY_N:
        out = PKEY_N;
        break;
    case XKB_KEY_O:
        out = PKEY_O;
        break;
    case XKB_KEY_P:
        out = PKEY_P;
        break;
    case XKB_KEY_Q:
        out = PKEY_Q;
        break;
    case XKB_KEY_R:
        out = PKEY_R;
        break;
    case XKB_KEY_S:
        out = PKEY_S;
        break;
    case XKB_KEY_T:
        out = PKEY_T;
        break;
    case XKB_KEY_U:
        out = PKEY_U;
        break;
    case XKB_KEY_V:
        out = PKEY_V;
        break;
    case XKB_KEY_W:
        out = PKEY_W;
        break;
    case XKB_KEY_X:
        out = PKEY_X;
        break;
    case XKB_KEY_Y:
        out = PKEY_Y;
        break;
    case XKB_KEY_Z:
        out = PKEY_Z;
        break;

    case XKB_KEY_KP_1:
    case XKB_KEY_1:
        out = PKEY_1;
        break;
    case XKB_KEY_KP_2:
    case XKB_KEY_2:
        out = PKEY_2;
        break;
    case XKB_KEY_KP_3:
    case XKB_KEY_3:
        out = PKEY_3;
        break;
    case XKB_KEY_KP_4:
    case XKB_KEY_4:
        out = PKEY_4;
        break;
    case XKB_KEY_KP_5:
    case XKB_KEY_5:
        out = PKEY_5;
        break;
    case XKB_KEY_KP_6:
    case XKB_KEY_6:
        out = PKEY_6;
        break;
    case XKB_KEY_KP_7:
    case XKB_KEY_7:
        out = PKEY_7;
        break;
    case XKB_KEY_KP_8:
    case XKB_KEY_8:
        out = PKEY_8;
        break;
    case XKB_KEY_KP_9:
    case XKB_KEY_9:
        out = PKEY_9;
        break;
    case XKB_KEY_KP_0:
    case XKB_KEY_0:
        out = PKEY_0;
        break;

    case XKB_KEY_exclam:
        out= PKEY_EXCLAMATION;
        break;
    /* case PKEY_AT: */
    /* case PKEY_HASH: */
    /* case PKEY_DOLLAR: */
    /* case PKEY_PERCENT: */
    /* case PKEY_CARET: */
    /* case PKEY_AMPERSAND: */
    /* case PKEY_ASTERISK: */
    case XKB_KEY_parenleft:
        out = PKEY_LPAREN;
        break;
    case XKB_KEY_parenright:
        out = PKEY_RPAREN;
        break;
    case XKB_KEY_minus:
        out = PKEY_MINUS;
        break;
    case XKB_KEY_plus:
        out = PKEY_PLUS;
        break;

    /* case PKEY_LBRACE: */
    /* case PKEY_RBRACE: */
    case XKB_KEY_colon:
        out = PKEY_COLON;
        break;
    case XKB_KEY_semicolon:
        out = PKEY_SEMICOLON;
        break;
    case XKB_KEY_comma:
        out = PKEY_COMMA;
        break;
    /* case PKEY_DOT: */
    /* case PKEY_QUERY: */

    case XKB_KEY_space:
        out = PKEY_SPACE;
        break;

    case XKB_KEY_Return:
    case XKB_KEY_KP_Enter:
        out = PKEY_ENTER;
        break;
    case XKB_KEY_BackSpace:
        out = PKEY_BACKSPACE;
        break;
    }
    return out;
}
