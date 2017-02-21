#ifndef PARSER_H
#define PARSER_H
#include "common.h"

enum ExprType {
    EXPR_STRING,
    EXPR_INT,
    EXPR_FLOAT,
    EXPR_SYMBOL,
    EXPR_CALL,
    EXPR_QUOTE,
    EXPR_LIST
};

struct Expr;

struct ExprList {
    Expr *val;
    ExprList *next;
};

struct Function {
    ExprList *params;
    ExprList *body;
};

struct Expr {
    union {
        struct {
            Expr *quoted;
        };
        struct {
            String str;
        };
        int intVal;
        float floatVal;
        ExprList *list;
        struct {
            ExprList *params;
            Expr *callee;
        };
    };
    ExprType exprType;
    int line;
};

struct Lexer;
Expr *parseExpression(Lexer *lexer);

extern Expr quoteSym;

#endif