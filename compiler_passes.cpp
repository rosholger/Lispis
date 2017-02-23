#include "compiler_passes.h"
#include "parser.h"
#include <cstdlib>
#include <cassert>

String quote;
String lambda;
String let;
String ifSym;

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
        for (ExprList *argument = expr->arguments;
             argument; argument = argument->next) {
            prev->next = (ExprList *)malloc(sizeof(ExprList));
            prev = prev->next;
            prev->val = createQuotedElement(argument->val);
            prev->next = 0;
        }
    } else {
        assert(expr->arguments == 0);
    }
    ret->list = list;
    return ret;
}

Expr *quotePass(Expr *expr) {
    Expr *ret = (Expr *)malloc(sizeof(Expr));
    switch (expr->exprType) {
        case EXPR_STRING:
        case EXPR_INT:
        case EXPR_FLOAT:
        case EXPR_SYMBOL: {
            *ret = *expr;
        } break;
        case EXPR_CALL: {
            if (expr->callee && expr->callee->exprType == EXPR_SYMBOL) {
                if (symCmp(quote,
                           hashFunc(quote),
                           expr->callee->str,
                           hashFunc(expr->callee->str))) {
                    ret->exprType = EXPR_QUOTE;
                    ret->line = expr->line;
                    assert(expr->arguments);
                    assert(!expr->arguments->next);
                    if (expr->arguments->val->exprType == EXPR_CALL) {
                        ret->quoted = createQuotedList(expr->arguments->val);
                    } else {
                        ret->quoted = quotePass(expr->arguments->val);
                    }
                    return ret;
                }
            }
            *ret = *expr;
            if (expr->callee) {
                ret->callee = quotePass(expr->callee);
                ret->arguments = 0;
                if (expr->arguments) {
                    ret->arguments = (ExprList *)malloc(sizeof(ExprList));
                    ret->arguments->val = quotePass(expr->arguments->val);
                    ExprList *prev = ret->arguments;
                    for (ExprList *param = expr->arguments->next;
                         param; param = param->next) {
                        prev->next = (ExprList *)malloc(sizeof(ExprList));
                        prev = prev->next;
                        prev->val = quotePass(param->val);
                    }
                    prev->next = 0;
                }
            }
        } break;
        default:assert(false);
    }
    return ret;
}

Expr *copyQuoted(Expr *expr) {
    Expr *ret = 0;
    switch (expr->exprType) {
        case EXPR_STRING:
        case EXPR_INT:
        case EXPR_FLOAT:
        case EXPR_SYMBOL: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
        } break;
        case EXPR_LIST: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            if (expr->list) {
                ret->list = (ExprList *)malloc(sizeof(ExprList));
                ret->list->next = 0;
                ExprList *prev = ret->list;
                for (ExprList *elem = expr->list;
                     elem; elem = elem->next) {
                    prev->next = (ExprList *)malloc(sizeof(ExprList));
                    prev = prev->next;
                    prev->val = copyQuoted(elem->val);
                    prev->next = 0;
                }
            }
        } break;
        default:assert(false);
    }
    return ret;
}

Expr *lambdaPass(Expr *expr) {
    Expr *ret = (Expr *)malloc(sizeof(Expr));
    switch (expr->exprType) {
        case EXPR_STRING:
        case EXPR_INT:
        case EXPR_FLOAT:
        case EXPR_SYMBOL: {
            *ret = *expr;
        } break;
        case EXPR_QUOTE: {
            *ret = *expr;
            ret->quoted = copyQuoted(expr->quoted);
        } break;
        case EXPR_CALL: {
            if (expr->callee && expr->callee->exprType == EXPR_SYMBOL) {
                if (symCmp(lambda,
                           hashFunc(lambda),
                           expr->callee->str,
                           hashFunc(expr->callee->str))) {
                    ret->exprType = EXPR_LAMBDA;
                    ret->line = expr->line;
                    ret->params = 0;
                    assert(expr->arguments);
                    assert(expr->arguments->val);
                    assert(expr->arguments->val->exprType == EXPR_CALL);
                    if (expr->arguments->val->callee) {
                        assert(expr->arguments->val->callee->exprType ==
                               EXPR_SYMBOL);
                        ret->params =
                            (ExprList *)malloc(sizeof(ExprList));
                        ret->params->val = (Expr *)malloc(sizeof(Expr));
                        *ret->params->val = *expr->arguments->val->callee;
                        ret->params->next = 0;
                        // FIXME: We cant allow special symbols as params
                        ret->paramsCount = 1;
                        ExprList *prev = ret->params;
                        for (ExprList *param =
                                 expr->arguments->val->arguments;
                             param; param = param->next) {
                            assert(param->val->exprType = EXPR_SYMBOL);
                            prev->next =
                                (ExprList *)malloc(sizeof(ExprList));
                            prev = prev->next;
                            prev->val = (Expr *)malloc(sizeof(Expr));
                            *prev->val = *param->val;
                            prev->next = 0;
                            ret->paramsCount++;
                        }
                    }                    assert(expr->arguments->next);
                    ret->body = (ExprList *)malloc(sizeof(ExprList));
                    ret->body->val =
                        lambdaPass(expr->arguments->next->val);
                    ret->body->next = 0;
                    ExprList *prev = ret->body;
                    for (ExprList *param =
                             expr->arguments->next->next;
                         param; param = param->next) {
                        prev->next = (ExprList *)malloc(sizeof(ExprList));
                        prev = prev->next;
                        prev->val = lambdaPass(param->val);
                        prev->next = 0;
                    }
                    return ret;
                }
            }
            *ret = *expr;
            if (expr->callee) {
                ret->callee = lambdaPass(expr->callee);
                ret->arguments = 0;
                if (expr->arguments) {
                    ret->arguments = (ExprList *)malloc(sizeof(ExprList));
                    ret->arguments->val = lambdaPass(expr->arguments->val);
                    ExprList *prev = ret->arguments;
                    for (ExprList *param = expr->arguments->next;
                         param; param = param->next) {
                        prev->next = (ExprList *)malloc(sizeof(ExprList));
                        prev = prev->next;
                        prev->val = lambdaPass(param->val);
                    }
                    prev->next = 0;
                }
            }
        } break;
        default:assert(false);
    }
    return ret;
}

ExprList *copyLambdaParams(ExprList *params) {
    if (params) {
        assert(params->val->exprType == EXPR_SYMBOL);
        ExprList *ret = (ExprList *)malloc(sizeof(ExprList));
        ret->val = (Expr *)malloc(sizeof(Expr));
        *ret->val = *params->val;
        ret->next = 0;
        ExprList *prev = ret;
        for (ExprList *param = params->next; param; param = param->next) {
            assert(param->val->exprType == EXPR_SYMBOL);
            prev->next = (ExprList *)malloc(sizeof(ExprList));
            prev = prev->next;
            prev->val = (Expr *)malloc(sizeof(Expr));
            *prev->val = *param->val;
            prev->next = 0;
        }
        return ret;
    } else {
        return params;
    }
}

ExprList *copyLambdaBody(ExprList *body,
                         Expr *(*recursiveFunc)(Expr *e)) {
    assert(body);
    ExprList *ret = (ExprList *)malloc(sizeof(ExprList));
    ret->val = recursiveFunc(body->val);
    ret->next = 0;
    ExprList *prev = ret;
    for (ExprList *expr = body->next; expr; expr = expr->next) {
        prev->next = (ExprList *)malloc(sizeof(ExprList));
        prev = prev->next;
        prev->next = 0;
        prev->val = recursiveFunc(expr->val);
    }
    return ret;
}

Expr *letPass(Expr *expr) {
    Expr *ret = (Expr *)malloc(sizeof(Expr));
    switch (expr->exprType) {
        case EXPR_STRING:
        case EXPR_INT:
        case EXPR_FLOAT:
        case EXPR_SYMBOL: {
            *ret = *expr;
        } break;
        case EXPR_QUOTE: {
            *ret = *expr;
            ret->quoted = copyQuoted(expr->quoted);
        } break;
        case EXPR_LAMBDA: {
            *ret = *expr;
            ret->params = copyLambdaParams(expr->params);
            ret->body = copyLambdaBody(expr->body, letPass);
        } break;
        case EXPR_CALL: {
            if (expr->callee && expr->callee->exprType == EXPR_SYMBOL) {
                if (symCmp(let,
                           hashFunc(let),
                           expr->callee->str,
                           hashFunc(expr->callee->str))) {
                    ret->exprType = EXPR_LET;
                    ret->line = expr->line;
                    assert(expr->arguments);
                    assert(expr->arguments->val);
                    assert(expr->arguments->val->exprType == EXPR_SYMBOL);
                    assert(expr->arguments->next);
                    assert(! expr->arguments->next->next);
                    ret->variable = (Expr *)malloc(sizeof(Expr));
                    *ret->variable = *expr->arguments->val;
                    ret->value = letPass(expr->arguments->next->val);
                    return ret;
                }
            }
            *ret = *expr;
            if (expr->callee) {
                ret->callee = letPass(expr->callee);
                ret->arguments = 0;
                if (expr->arguments) {
                    ret->arguments = (ExprList *)malloc(sizeof(ExprList));
                    ret->arguments->val = letPass(expr->arguments->val);
                    ExprList *prev = ret->arguments;
                    for (ExprList *param = expr->arguments->next;
                         param; param = param->next) {
                        prev->next = (ExprList *)malloc(sizeof(ExprList));
                        prev = prev->next;
                        prev->val = letPass(param->val);
                    }
                    prev->next = 0;
                }
            }
        } break;
        default:assert(false);
    }
    return ret;
}

Expr *ifPass(Expr *expr) {
    Expr *ret = (Expr *)malloc(sizeof(Expr));
    switch (expr->exprType) {
        case EXPR_STRING:
        case EXPR_INT:
        case EXPR_FLOAT:
        case EXPR_SYMBOL: {
            *ret = *expr;
        } break;
        case EXPR_QUOTE: {
            *ret = *expr;
            ret->quoted = copyQuoted(expr->quoted);
        } break;
        case EXPR_LAMBDA: {
            *ret = *expr;
            ret->params = copyLambdaParams(expr->params);
            ret->body = copyLambdaBody(expr->body, ifPass);
        } break;
        case EXPR_LET: {
            *ret = *expr;
            ret->variable = (Expr *)malloc(sizeof(Expr));
            *ret->variable = *expr->variable;
            ret->value = ifPass(expr->value);
        } break;
        case EXPR_CALL: {
            if (expr->callee && expr->callee->exprType == EXPR_SYMBOL) {
                if (symCmp(ifSym,
                           hashFunc(ifSym),
                           expr->callee->str,
                           hashFunc(expr->callee->str))) {
                    ret->exprType = EXPR_IF;
                    ret->line = expr->line;
                    assert(expr->arguments); // predicate
                    ret->predicate = ifPass(expr->arguments->val);
                    assert(expr->arguments->val);
                    assert(expr->arguments->next); // trueBranch
                    ret->trueBranch = ifPass(expr->arguments->next->val);
                    if (expr->arguments->next->next) { // falseBranch
                        ret->falseBranch =
                            ifPass(expr->arguments->next->next->val);
                    }
                    return ret;
                }
            }
            *ret = *expr;
            if (expr->callee) {
                ret->callee = ifPass(expr->callee);
                ret->arguments = 0;
                if (expr->arguments) {
                    ret->arguments = (ExprList *)malloc(sizeof(ExprList));
                    ret->arguments->val = ifPass(expr->arguments->val);
                    ExprList *prev = ret->arguments;
                    for (ExprList *param = expr->arguments->next;
                         param; param = param->next) {
                        prev->next = (ExprList *)malloc(sizeof(ExprList));
                        prev = prev->next;
                        prev->val = ifPass(param->val);
                    }
                    prev->next = 0;
                }
            }
        } break;
        default:assert(false);
    }
    return ret;
}