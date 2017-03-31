#ifndef PARSER_H
#define PARSER_H
#include "common.h"

enum ExprType {
    EXPR_UNDEF,
    EXPR_STRING,
    EXPR_INT,
    EXPR_DOUBLE,
    EXPR_SYMBOL,
    EXPR_CALL,
    EXPR_QUOTE,
    EXPR_QUASIQUOTE,
    EXPR_LAMBDA,
    EXPR_MACRO,
    EXPR_LIST,
    EXPR_LET,
    EXPR_SET,
    EXPR_DEFINE,
    EXPR_IF,
    EXPR_SYMBOL_ID,
    EXPR_VARIABLE,
    EXPR_VECTOR,
    EXPR_OBJECT,
    EXPR_KEY_VALUE_PAIR,
    EXPR_FOR,
    EXPR_DO,
    EXPR_REF,
    EXPR_REF_SET,
    EXPR_BOOLEAN,
};

enum VariableKind {
    VAR_LOCAL,
    VAR_UPVAL,
    VAR_GLOBAL,
};

struct Expr;

struct ExprList {
    Expr *val;
    ExprList *next;
};

struct QuasiquoteList {
    Expr *val;
    QuasiquoteList *next;
    bool unquoted;
    bool unquoteSpliced;
};

struct LexicalVariable {
    uint32 variableID;
    uint32 index; // if upval
    uint32 symbolID;
    uint32 depth;
    VariableKind kind;
};

struct Expr {
    union {
        bool boolean;
        Expr *quoted; // QUOTE
        uint32 symbolID;
        String str; // SYMBOL, STRING
        int intVal; // INT
        double doubleVal; // FLOAT
        ExprList *list; // QUOTED LIST, DO
        struct {
            Expr *obj;
            Expr *ref;
        } ref;
        struct {
            Expr *obj;
            Expr *ref;
            Expr *val;
        } refSet;
        struct {
            Expr *key;
            Expr *val;
            bool unquotedKey;
        } keyValPair;
        struct {
            uint32 numElems;
            ExprList *elems;
        } obj;
        struct {
            uint32 numElems;
            ExprList *elems; // VECTOR
        } vec;
        QuasiquoteList *quasiquoteList; // QUASIQUOTED LIST
        struct { // FOR (for (init pred upd) body)
            Expr *init;
            Expr *pred;
            Expr *upd;
            ExprList *body;
            Expr *it;
        };
        struct { // CALL (a b)
            Expr *callee;
            ExprList *arguments;
        };
        struct { // LET (let! a 1), SET (set! a 1) or DEFINE (define! a 1)
            Expr *variable;
            Expr *value;
        };
        struct {
            ExprList *params;
            ExprList *body;
            int32 paramsCount;
            uint32 numLocals; // includes params
            uint32 numUpvals;
            bool varargs;
            bool closedOver;
        } lambda;
        struct {
            uint32 name;
            ExprList *params;
            ExprList *body;
            int32 paramsCount;
            uint32 numLocals; // includes params
            uint32 numUpvals; // always 0
            bool varargs;
        } macro;
        struct {
            Expr *predicate;
            Expr *trueBranch;
            Expr *falseBranch; // may be null
        };
        LexicalVariable var;
    };
    bool dotted;
    ExprType exprType;
    int line;
};

struct Lexer;
Expr *parseExpression(Lexer *lexer);

extern Expr quoteSym;
extern Expr quasiquoteSym;
extern Expr unquoteSym;
extern Expr unquoteSpliceSym;

#endif