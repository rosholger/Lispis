#include "lexer.h"
#include <cstdio>
#include <cstdlib>
#include <cassert>
#include <cctype>
#include <cstring>
#include <cmath>

void eatComment(Lexer *lexer) {
    while (*lexer->buf && *lexer->buf == ';') {
        while (*lexer->buf && *lexer->buf != '\n') {
            lexer->buf++;
        }
    }
}

void eatWhitespace(Lexer *lexer) {
    eatComment(lexer);
    while(isspace(*lexer->buf)) {
        lexer->buf++;
        eatComment(lexer);
    }
}

bool isSpaceOrComment(char c) {
    return (c == ';' || isspace(c));
}    

Token lexString(Lexer *lexer) {
    Token ret = {};
    ret.tokenType = TOK_STRING;
    ret.str.val = lexer->buf;
    lexer->buf++;
    for (;*lexer->buf && *lexer->buf != '"'; lexer->buf++) {
        ret.str.length++;
    }
    if (*lexer->buf == '"') {
        lexer->buf++;
    }
    return ret;
}

bool startOfSymbol(char c) {
    return (!isSpaceOrComment(c) && !isdigit(c) && c != '.');
}

bool isSymbolChar(char c) {
    return (c && !isSpaceOrComment(c) &&
            c != '(' && c != ')' && c != '\'');
}

Token lexSymbol(Lexer *lexer) {
    Token ret = {};
    ret.tokenType = TOK_SYMBOL;
    ret.str.val = lexer->buf;
    for (;isSymbolChar(*lexer->buf); lexer->buf++) {
        ret.str.length++;
    }
    return ret;
}
            
Token lexNumber(Lexer *lexer) {
    Token ret = {};
    ret.tokenType = TOK_INT;
    int sign = 1;
    if (*lexer->buf == '+') {
        lexer->buf++;
    } else if (*lexer->buf == '-') {
        sign = -1;
        lexer->buf++;
    }
    for (; isdigit(*lexer->buf); ++lexer->buf) {
        ret.intVal = ret.intVal*10 + (*lexer->buf - '0');
    }
    ret.intVal *= sign;
    return ret;
}
            
Token lexToken(Lexer *lexer) {
    eatWhitespace(lexer);
    Token ret{};
    switch (*lexer->buf) {
        case '.': {
            lexer->buf++;
            ret.tokenType = TOK_DOT;
            return ret;
        } break;
        case '\'': {
            lexer->buf++;
            ret.tokenType = TOK_QUOTE_ABR;
            return ret;
        } break;
        case '(': {
            lexer->buf++;
            ret.tokenType = TOK_LPAREN;
            ret.line = lexer->currLine;
            return ret;
        } break;
        case ')': {
            lexer->buf++;
            ret.tokenType = TOK_RPAREN;
            ret.line = lexer->currLine;
            return ret;
        } break;
        case '"': {
            return lexString(lexer);
        }
        case 0: {
            ret.tokenType = TOK_EOF;
            ret.line = lexer->currLine;
            return ret;
        } break;
        default: {
            if (isdigit(*lexer->buf) ||
                ((*lexer->buf == '-' ||
                  *lexer->buf == '+') &&
                 isdigit(lexer->buf[1]))) {
                return lexNumber(lexer);
            }
            if (startOfSymbol(*lexer->buf)) {
                return lexSymbol(lexer);
            }
        } break;
    }
    ret.tokenType = TOK_UNKNOWN;
    return ret;
}

Token eatToken(Lexer *lexer) {
    Token ret = lexer->currTok;
    lexer->currTok = lexToken(lexer);
    return ret;
}

Token peekToken(Lexer *lexer) {
    return lexer->currTok;
}

