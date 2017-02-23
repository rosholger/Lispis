#ifndef PARSER_H
#define PARSER_H
#include "common.h"

enum ExprType {
    EXPR_UNDEF,
    EXPR_STRING,
    EXPR_INT,
    EXPR_FLOAT,
    EXPR_SYMBOL,
    EXPR_CALL,
    EXPR_QUOTE,
    EXPR_LAMBDA,
    EXPR_PARAMS,
    EXPR_BODY,
    EXPR_LIST,
    EXPR_LET,
    EXPR_IF,
};

struct Expr;

struct ExprList {
    Expr *val;
    ExprList *next;
};

struct Expr {
    union {
        struct { // QUOTE
            Expr *quoted;
        };
        String str; // SYMBOL, STRING
        int intVal; // INT
        float floatVal; // FLOAT
        ExprList *list; // QUOTED LIST
        struct { // CALL (a b)
            Expr *callee;
            ExprList *arguments;
        };
        struct { // LET (let! a 1)
            Expr *variable;
            Expr *value;
        };
        struct { // lambda
            ExprList *params;
            ExprList *body;
            int32 paramsCount;
            bool varargs;
        };
        struct {
            Expr *predicate;
            Expr *trueBranch;
            Expr *falseBranch; // may be null
        };
    };
    ExprType exprType;
    int line;
};

struct Lexer;
Expr *parseExpression(Lexer *lexer);

extern Expr quoteSym;

#endif