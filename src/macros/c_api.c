#include <stddef.h>

// This file defines all the data required to compile proc macro shared object

typedef enum {
    T_IDENT,
    T_INTEGER,
    T_STRING,

    T_ASSIGN,
    T_PLUS,
    T_MINUS,
    T_BANG,
    T_ASTERISK,
    T_SLASH,
    T_ARROW,
    T_PERIOD,
    T_TILDE,
    T_AMPERSAND,
    T_BAR,
    T_EQUAL,
    T_NOTEQUAL,
    T_LESSTHAN,
    T_GREATERTHAN,
    T_LESSEQUAL,
    T_GREATEREQUAL,
    T_AND,
    T_OR,
    T_SHL,
    T_SHR,
    T_COMMA,
    T_SEMICOLON,
    T_COLON,
    T_LPAREN,
    T_RPAREN,
    T_LBRACE,
    T_RBRACE,
    T_LBRACKET,
    T_RBRACKET,

    T_CONST,
    T_TRUE,
    T_FALSE,
    T_LET,
    T_FN,
    T_ENUM,
    T_STRUCT,
    T_IF,
    T_WHILE,
    T_FOR,
    T_ELSE,
    T_RETURN,
    T_AS,
    T_CONTINUE,
    T_BREAK,

    T_U8,
    T_U16,
    T_U32,
    T_U64,
    T_I8,
    T_I16,
    T_I32,
    T_I64,
    T_USIZE,
    T_ISIZE,
    T_BOOL,
    T_VOID,

    T_NULL,
} TokenTreeTag;

typedef struct {
    TokenTreeTag tag;
    char *payload;
} Token;

typedef struct {
    void *ptr;
    size_t len;
} Slice;

typedef Slice (*MacroFn)(Slice);

typedef struct {
    const char *name;
    MacroFn fn;
} Macro;

Macro _macros[] = {};

// Compiler expects macros symbol to be defined in a shared object
Slice macros = {
    .ptr = &_macros,
    .len = sizeof(_macros) / sizeof(_macros[0])
};
