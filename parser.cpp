#include "parser.h"
#include "lexer.h"
#include <cstdio>
#include <cstdlib>
#include <cassert>
#include <cctype>
#include <cstring>
#include <cmath>

Expr quoteSym;
Expr quasiquoteSym;
Expr unquoteSym;
Expr unquoteSpliceSym;

Expr *parseKeyValPair(Lexer *lexer) {
    Token lparen = eatToken(lexer);
    assert(lparen.tokenType == TOK_LPAREN);
    Expr *ret = (Expr *)calloc(1, sizeof(Expr));
    ret->exprType = EXPR_KEY_VALUE_PAIR;
    ret->line = lparen.line;
    ret->keyValPair.key = parseExpression(lexer);
    ret->keyValPair.val = parseExpression(lexer);
    assert(eatToken(lexer).tokenType == TOK_RPAREN);
    return ret;
}

Expr *parseExpression(Lexer *lexer) {
    Expr *ret = (Expr *)calloc(1, sizeof(Expr));
    Token tok = eatToken(lexer);
    ret->line = tok.line;
    switch(tok.tokenType) {
        case TOK_LBRACKET: {
            ret->exprType = EXPR_OBJECT;
            ret->obj.elems = 0;
            if (peekToken(lexer).tokenType != TOK_RBRACKET) {
                assert(peekToken(lexer).tokenType != TOK_EOF);
                ret->obj.elems = (ExprList *)malloc(sizeof(ExprList));
                ret->obj.elems->val = parseKeyValPair(lexer);
                ret->obj.elems->next = 0;
                ExprList *prev = ret->obj.elems;
                ret->obj.numElems++;
                while(peekToken(lexer).tokenType != TOK_RBRACKET) {
                    assert(peekToken(lexer).tokenType != TOK_EOF);
                    prev->next = (ExprList *)malloc(sizeof(ExprList));
                    prev = prev->next;
                    prev->val = parseKeyValPair(lexer);
                    prev->next = 0;
                    ret->obj.numElems++;
                }
            }
            assert(eatToken(lexer).tokenType == TOK_RBRACKET);
        } break;
        case TOK_LSQUARE: {
            ret->exprType = EXPR_VECTOR;
            ret->vec.elems = 0;
            ret->vec.numElems = 0;
            if (peekToken(lexer).tokenType != TOK_RSQUARE) {
                assert(peekToken(lexer).tokenType != TOK_EOF);
                ret->vec.elems = (ExprList *)malloc(sizeof(ExprList));
                ret->vec.numElems++;
                ExprList *prev = ret->vec.elems;
                prev->val = parseExpression(lexer);
                prev->next = 0;
                while(peekToken(lexer).tokenType != TOK_RSQUARE) {
                    assert(peekToken(lexer).tokenType != TOK_EOF);
                    ret->vec.numElems++;
                    prev->next = (ExprList *)malloc(sizeof(ExprList));
                    prev = prev->next;
                    prev->val = parseExpression(lexer);
                    prev->next = 0;
                }
                eatToken(lexer); //TOK_RSQUARE
            }
        } break;
        case TOK_QUASIQUOTE_ABR: {
            ret->exprType = EXPR_CALL;
            ret->callee = (Expr *)malloc(sizeof(Expr));
            *ret->callee = quasiquoteSym;
            ret->callee->line = tok.line;
            ret->arguments = (ExprList *)malloc(sizeof(ExprList));
            ret->arguments->next = 0;
            ret->arguments->val = parseExpression(lexer);
        } break;
        case TOK_UNQUOTE_ABR: {
            ret->exprType = EXPR_CALL;
            ret->callee = (Expr *)malloc(sizeof(Expr));
            *ret->callee = unquoteSym;
            ret->callee->line = tok.line;
            ret->arguments = (ExprList *)malloc(sizeof(ExprList));
            ret->arguments->next = 0;
            ret->arguments->val = parseExpression(lexer);
        } break;
        case TOK_UNQUOTE_SPLICE_ABR: {
            ret->exprType = EXPR_CALL;
            ret->callee = (Expr *)malloc(sizeof(Expr));
            *ret->callee = unquoteSpliceSym;
            ret->callee->line = tok.line;
            ret->arguments = (ExprList *)malloc(sizeof(ExprList));
            ret->arguments->next = 0;
            ret->arguments->val = parseExpression(lexer);
        } break;
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
            ret->str = tok.str;
        } break;
        case TOK_INT: {
            ret->exprType = EXPR_INT;
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