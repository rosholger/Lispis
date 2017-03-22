#ifndef TOKEN_H
#define TOKEN_H

#include "common.h"

enum TokenType {
    TOK_EOF = 0,
    TOK_STRING,
    TOK_INT,
    TOK_FLOAT,
    TOK_SYMBOL,
    TOK_LPAREN,
    TOK_RPAREN,
    TOK_QUOTE_ABR,
    TOK_QUASIQUOTE_ABR,
    TOK_UNQUOTE_ABR,
    TOK_UNQUOTE_SPLICE_ABR,
    TOK_LSQUARE,
    TOK_RSQUARE,
    TOK_LBRACKET,
    TOK_RBRACKET,
    
    TOK_DOT,
    TOK_UNKNOWN
};

struct Token {
    union {
        struct {
            String str;
        };
        int intVal;
        float floatVal;
    };
    TokenType tokenType;
    int line;
};

struct Lexer {
    char *buf;
    Token currTok;
    int currLine;
};

Token lexToken(Lexer *lexer);
Token eatToken(Lexer *lexer);
Token peekToken(Lexer *lexer);

#endif