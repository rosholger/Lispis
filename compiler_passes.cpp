#include "compiler_passes.h"
#include "parser.h"
#include <cstdlib>
#include <cassert>

String quote;

Expr *createQuotedList(Expr *expr);

Expr *createQuotedElement(Expr *expr) {
    Expr *ret = 0;
    switch (expr->exprType) {
        case EXPR_STRING:
        case EXPR_INT:
        case EXPR_FLOAT:
        case EXPR_SYMBOL: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
        } break;
        case EXPR_CALL: {
            ret = createQuotedList(expr);
        } break;
        default:assert(false);
    }
    return ret;
}

Expr *createQuotedList(Expr *expr) {
    Expr *ret = (Expr *)malloc(sizeof(Expr));
    *ret = *expr;
    ret->exprType = EXPR_LIST;
    ExprList *list = 0;
    if (expr->callee) {
        list = (ExprList *)malloc(sizeof(ExprList));
        list->val = createQuotedElement(expr->callee);
        list->next = 0;
        ExprList *prev = list;
        for (ExprList *param = expr->params;
             param; param = param->next) {
            prev->next = (ExprList *)malloc(sizeof(ExprList));
            prev = prev->next;
            prev->val = createQuotedElement(param->val);
            prev->next = 0;
        }
    } else {
        assert(expr->params == 0);
    }
    ret->list = list;
    return ret;
}

Expr *firstPass(Expr *expr) {
    Expr *ret = (Expr *)malloc(sizeof(Expr));
    switch (expr->exprType) {
        case EXPR_STRING:
        case EXPR_INT:
        case EXPR_FLOAT:
        case EXPR_SYMBOL: {
            *ret = *expr;
        } break;
        case EXPR_CALL: {
            if (expr->callee->exprType == EXPR_SYMBOL) {
                if (symCmp(quote,
                           hashFunc(quote),
                           expr->callee->str,
                           hashFunc(expr->callee->str))) {
                    ret->exprType = EXPR_QUOTE;
                    ret->line = expr->line;
                    assert(expr->params);
                    assert(!expr->params->next);
                    if (expr->params->val->exprType == EXPR_CALL) {
                        ret->quoted = createQuotedList(expr->params->val);
                    } else {
                        ret->quoted = firstPass(expr->params->val);
                    }
                    return ret;
                }
            }
            *ret = *expr;
            ret->callee = firstPass(expr->callee);
            ret->params = 0;
            if (expr->params) {
                ret->params = (ExprList *)malloc(sizeof(ExprList));
                ret->params->val = firstPass(expr->params->val);
                ExprList *prev = ret->params;
                for (ExprList *param = expr->params->next;
                     param; param = param->next) {
                    prev->next = (ExprList *)malloc(sizeof(ExprList));
                    prev = prev->next;
                    prev->val = firstPass(param->val);
                }
                prev->next = 0;
            }
        } break;
        default:assert(false);
    }
    return ret;
}