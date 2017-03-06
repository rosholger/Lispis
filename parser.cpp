#include "parser.h"
#include "lexer.h"
#include <cstdio>
#include <cstdlib>
#include <cassert>
#include <cctype>
#include <cstring>
#include <cmath>

Expr quoteSym;

Expr *parseExpression(Lexer *lexer) {
    Expr *ret = (Expr *)calloc(1, sizeof(Expr));
    Token tok = eatToken(lexer);
    switch(tok.tokenType) {
        case TOK_QUOTE_ABR: {
            ret->exprType = EXPR_CALL;
            ret->callee = (Expr *)malloc(sizeof(Expr));
            *ret->callee = quoteSym;
            ret->callee->line = tok.line;
            ret->arguments = (ExprList *)malloc(sizeof(ExprList));
            ret->arguments->next = 0;
            ret->arguments->val = parseExpression(lexer);
        } break;
        case TOK_SYMBOL: {
            ret->exprType = EXPR_SYMBOL;
            ret->line = tok.line;
            ret->str = tok.str;
        } break;
        case TOK_INT: {
            ret->exprType = EXPR_INT;
            ret->line = tok.line;
            ret->intVal = tok.intVal;
        } break;
        case TOK_LPAREN: {
            ret->exprType = EXPR_CALL;
            if (peekToken(lexer).tokenType != TOK_RPAREN) {
                ret->callee = parseExpression(lexer);
            } else {
                ret->callee = 0;
            }
            ret->arguments = 0;
            ExprList *tail = 0;
            while (peekToken(lexer).tokenType != TOK_RPAREN) {
                ExprList *paramNode =
                    (ExprList *)malloc(sizeof(ExprList));
                if (peekToken(lexer).tokenType == TOK_DOT) {
                    eatToken(lexer);
                    ret->dotted = true;
                    paramNode->val = parseExpression(lexer);
                    paramNode->next = 0;
                    assert(peekToken(lexer).tokenType == TOK_RPAREN);
                    if (tail) {
                        tail->next = paramNode;
                        tail = paramNode;
                    } else {
                        ret->arguments = paramNode;
                        tail = paramNode;
                    }
                } else {
                    paramNode->val = parseExpression(lexer);
                    paramNode->next = 0;
                    if (tail) {
                        tail->next = paramNode;
                        tail = paramNode;
                    } else {
                        ret->arguments = paramNode;
                        tail = paramNode;
                    }
                }
            }
            eatToken(lexer); //TOK_RPAREN
        } break;
        default:assert(false);
    }
    return ret;
}