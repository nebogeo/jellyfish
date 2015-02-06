const char lc_map[] = {
    0, 0, '1', '2', '3', '4', '5', '6', '7', '8', '9', '0',
    '-', '=', '\b', '\t', 'q', 'w', 'e', 'r', 't', 'y', 'u', 'i',
    'o', 'p', '[', ']', '\n', 0, 'a', 's', 'd', 'f', 'g', 'h',
    'j', 'k', 'l', ';', '\'', '\n', 0, '\\', 'z', 'x', 'c', 'v',
    'b', 'n', 'm', ',', '.', '/', 0, '*', 0, ' ', 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, '\r'
};

const char uc_map[] = {
    0, 0, '!', '@', '#', '$', '%', '^', '&', '*', '(', ')',
    '_', '+', '\b', '\t', 'Q', 'W', 'E', 'R', 'T', 'Y', 'U', 'I',
    'O', 'P', '{', '}', '\n', 0, 'A', 'S', 'D', 'F', 'G', 'H',
    'J', 'K', 'L', ':', '"', '\n', 0, '\\', 'Z', 'X', 'C', 'V',
    'B', 'N', 'M', '<', '>', '?', 0, '*', 0, ' ', 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, '\r'
};

#define KEY_ESC 1
#define KEY_ONE 2
#define KEY_TWO 3
#define KEY_THREE 4
#define KEY_FOUR 5
#define KEY_FIVE 6
#define KEY_SIX 7
#define KEY_SEVEN 8
#define KEY_EIGHT 9
#define KEY_NINE 10
#define KEY_ZERO 11
#define KEY_MINUS 12

#define KEY_TAB 15
#define KEY_Q 16
#define KEY_W 17
#define KEY_E 18
#define KEY_R 19
#define KEY_T 20
#define KEY_Y 21
#define KEY_U 22
#define KEY_I 23
#define KEY_O 24
#define KEY_P 25
#define KEY_LBRACKET 26
#define KEY_RBRACKET 27
#define KEY_RETURN 28
#define KEY_LCTRL 29
#define KEY_A 30
#define KEY_S 31
#define KEY_D 32
#define KEY_F 33
#define KEY_G 34
#define KEY_H 35
#define KEY_J 36
#define KEY_K 37
#define KEY_L 38
#define KEY_SEMICOLON 39
#define KEY_APOST 40
#define KEY_BACKTICK 41
#define KEY_LSHIFT 42
#define KEY_HASH 43
#define KEY_Z 44
#define KEY_X 45
#define KEY_C 46
#define KEY_V 47
#define KEY_B 48
#define KEY_N 49
#define KEY_M 50
#define KEY_COMMA 51
#define KEY_PERIOD 52
#define KEY_BSLASH 52
#define KEY_RSHIFT 54
#define KEY_NUMMULT
#define KEY_LALT 56
#define KEY_SPACE 57
#define KEY_CAPS 58
#define KEY_F1 59
#define KEY_F2 60
#define KEY_F3 61
#define KEY_F4 62
#define KEY_F5 63
#define KEY_F6 64
#define KEY_F7 65
#define KEY_F8 66
#define KEY_F9 67
#define KEY_F10 68
#define KEY_NUMLOCK 69
#define KEY_SCLOCK 70
#define KEY_NUM7 // TODO my mini keyboard don't have these!
#define KEY_NUM8
#define KEY_NUM9
#define KEY_NUMMINUS
#define KEY_NUM4
#define KEY_NUM5
#define KEY_NUM6
#define KEY_NUMPLUS
#define KEY_NUM1
#define KEY_NUM2
#define KEY_NUM3
#define KEY_NUMZERO
#define KEY_NUMPERIOD

#define KEY_FSLASH 86
#define KEY_F11 87
#define KEY_F12 88

#define KEY_NUMENTER
#define KEY_RCTRL
#define KEY_NUMSLASH
#define KEY_SYSRQ 99
#define KEY_ALTGR 100

#define KEY_HOME

#define KEY_PGUP

#define KEY_END

#define KEY_PGDOWN
#define KEY_INSERT
#define KEY_DELETE 111

#define KEY_BREAK 119

#define KEY_LMETA 125
#define KEY_RMETA
#define KEY_MENU 127

#define KEY_CURSL 105
#define KEY_CURSR 106
#define KEY_CURSU 103
#define KEY_CURSD 108
