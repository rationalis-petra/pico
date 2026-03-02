#include "platform/machine_info.h"

#ifdef WINDOW_SYSTEM
#if (OS_FAMILY == UNIX)
#include <linux/input.h>

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
        out = PKEY_EXCLAMATION;
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

RawKey scancode_to_rawkey(uint32_t scancode) {
    RawKey outkey = UINT32_MAX;
    switch (scancode) {
    case KEY_A:
        outkey = RKEY_A;
        break;
    case KEY_B:
        outkey = RKEY_B;
        break;
    case KEY_C:
        outkey = RKEY_C;
        break;
    case KEY_D:
        outkey = RKEY_D;
        break;
    case KEY_E:
        outkey = RKEY_E;
        break;
    case KEY_F:
        outkey = RKEY_F;
        break;
    case KEY_G:
        outkey = RKEY_G;
        break;
    case KEY_H:
        outkey = RKEY_H;
        break;
    case KEY_I:
        outkey = RKEY_I;
        break;
    case KEY_J:
        outkey = RKEY_J;
        break;
    case KEY_K:
        outkey = RKEY_K;
        break;
    case KEY_L:
        outkey = RKEY_L;
        break;
    case KEY_M:
        outkey = RKEY_M;
        break;
    case KEY_N:
        outkey = RKEY_N;
        break;
    case KEY_O:
        outkey = RKEY_O;
        break;
    case KEY_P:
        outkey = RKEY_P;
        break;
    case KEY_Q:
        outkey = RKEY_Q;
        break;
    case KEY_R:
        outkey = RKEY_R;
        break;
    case KEY_S:
        outkey = RKEY_S;
        break;
    case KEY_T:
        outkey = RKEY_T;
        break;
    case KEY_U:
        outkey = RKEY_U;
        break;
    case KEY_V:
        outkey = RKEY_V;
        break;
    case KEY_W:
        outkey = RKEY_W;
        break;
    case KEY_X:
        outkey = RKEY_X;
        break;
    case KEY_Y:
        outkey = RKEY_Y;
        break;
    case KEY_Z:
        outkey = RKEY_Z;
        break;
    case KEY_1:
        outkey = RKEY_1;
        break;
    case KEY_2:
        outkey = RKEY_2;
        break;
    case KEY_3:
        outkey = RKEY_3;
        break;
    case KEY_4:
        outkey = RKEY_4;
        break;
    case KEY_5:
        outkey = RKEY_5;
        break;
    case KEY_6:
        outkey = RKEY_6;
        break;
    case KEY_7:
        outkey = RKEY_7;
        break;
    case KEY_8:
        outkey = RKEY_8;
        break;
    case KEY_9:
        outkey = RKEY_9;
        break;
    case KEY_SPACE:
        outkey = RKEY_SPACE;
        break;

    case KEY_MINUS:
        outkey = RKEY_MINUS;
        break;
    case KEY_KPPLUS:
        outkey = RKEY_PLUS;
        break;

    case KEY_SEMICOLON:
        outkey = RKEY_SEMICOLON;
        break;
    case KEY_COMMA:
        outkey = RKEY_COMMA;
        break;
    case KEY_ENTER:
        outkey = RKEY_ENTER;
        break;
    case KEY_BACKSPACE:
        outkey = RKEY_BACKSPACE;
        break;
    }
    return outkey;
}

static uint32_t invert_key[] = {
    KEY_A,
    KEY_B,
    KEY_C,
    KEY_D,
    KEY_E,
    KEY_F,
    KEY_G,
    KEY_H,
    KEY_I,
    KEY_J,
    KEY_K,
    KEY_L,
    KEY_M,
    KEY_N,
    KEY_O,
    KEY_P,
    KEY_Q,
    KEY_R,
    KEY_S,
    KEY_T,
    KEY_U,
    KEY_V,
    KEY_W,
    KEY_X,
    KEY_Y,
    KEY_Z,

    KEY_1,
    KEY_2,
    KEY_3,
    KEY_4,
    KEY_5,
    KEY_6,
    KEY_7,
    KEY_8,
    KEY_9,
    KEY_0,

    0,/* KEY_EXCLAMATION, */
    0,/* KEY_AT, */
    0,/* KEY_HASH, */
    0, /* KEY_DOLLAR, */
    0,/* KEY_PERCENT, */
    0,/* KEY_CARET, */
    0,/* KEY_AMPERSAND, */
    0,/* KEY_ASTERISK, */
    0,/* KEY_LPAREN, */
    0,/* KEY_RPAREN, */
    KEY_MINUS,
    KEY_KPPLUS,

    0,/* KEY_LBRACE, */
    0,/* KEY_RBRACE, */
    0,/* KEY_COLON, */
    KEY_SEMICOLON,
    KEY_COMMA,
    KEY_DOT,
    0,/* KEY_QUERY, */

    KEY_SPACE,

    KEY_ENTER,
    KEY_BACKSPACE,
};

uint32_t rawkey_to_scancode(RawKey rawkey) {
    return invert_key[rawkey];
}

#endif
#endif