#include "compiler_passes.h"
#include "parser.h"
#include "vm.h"
#include <cstdlib>
#include <cassert>

uint32 quote;
uint32 lambda;
uint32 let;
uint32 define;
uint32 ifSym;

Expr *symbolIdPass(LispisState *state, Expr *expr) {
    Expr *ret = (Expr *)malloc(sizeof(Expr));
    switch (expr->exprType) {
        case EXPR_STRING:
        case EXPR_INT:
        case EXPR_FLOAT: {
            *ret = *expr;
        } break;
        case EXPR_SYMBOL: {
            *ret = *expr;
            ret->exprType = EXPR_SYMBOL_ID;
            ret->symbolID = internSymbol(state, expr->str,
                                         hashFunc(expr->str));
        } break;
        case EXPR_CALL: {
            *ret = *expr;
            if (expr->callee) {
                ret->callee = symbolIdPass(state, expr->callee);
                ret->arguments = 0;
                if (expr->arguments) {
                    ret->arguments = (ExprList *)malloc(sizeof(ExprList));
                    ret->arguments->val =
                        symbolIdPass(state,
                                     expr->arguments->val);
                    ExprList *prev = ret->arguments;
                    for (ExprList *param = expr->arguments->next;
                         param; param = param->next) {
                        prev->next = (ExprList *)malloc(sizeof(ExprList));
                        prev = prev->next;
                        prev->val = symbolIdPass(state, param->val);
                    }
                    prev->next = 0;
                }
            }
        } break;
        default:assert(false);
    }
    return ret;
}

Expr *createQuotedList(LispisState *state, Expr *expr);

Expr *createQuotedElement(LispisState *state, Expr *expr) {
    Expr *ret = 0;
    switch (expr->exprType) {
        case EXPR_STRING:
        case EXPR_INT:
        case EXPR_FLOAT:
        case EXPR_SYMBOL_ID: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
        } break;
        case EXPR_CALL: {
            ret = createQuotedList(state, expr);
        } break;
        default:assert(false);
    }
    return ret;
}

Expr *createQuotedList(LispisState *state, Expr *expr) {
    Expr *ret = (Expr *)malloc(sizeof(Expr));
    *ret = *expr;
    ret->exprType = EXPR_LIST;
    ExprList *list = 0;
    if (expr->callee) {
        list = (ExprList *)malloc(sizeof(ExprList));
        list->val = createQuotedElement(state, expr->callee);
        list->next = 0;
        ExprList *prev = list;
        for (ExprList *argument = expr->arguments;
             argument; argument = argument->next) {
            prev->next = (ExprList *)malloc(sizeof(ExprList));
            prev = prev->next;
            prev->val = createQuotedElement(state, argument->val);
            prev->next = 0;
        }
    } else {
        assert(expr->arguments == 0);
    }
    ret->list = list;
    return ret;
}

Expr *quotePass(LispisState *state, Expr *expr) {
    Expr *ret = (Expr *)malloc(sizeof(Expr));
    switch (expr->exprType) {
        case EXPR_STRING:
        case EXPR_INT:
        case EXPR_FLOAT:
        case EXPR_SYMBOL_ID: {
            *ret = *expr;
        } break;
        case EXPR_CALL: {
            if (expr->callee &&
                expr->callee->exprType == EXPR_SYMBOL_ID) {
                if (expr->callee->symbolID == quote) {
                    ret->exprType = EXPR_QUOTE;
                    ret->line = expr->line;
                    assert(expr->arguments);
                    assert(!expr->arguments->next);
                    if (expr->arguments->val->exprType == EXPR_CALL) {
                        ret->quoted = createQuotedList(state,
                                                       expr->arguments->val);
                    } else {
                        ret->quoted = quotePass(state,
                                                expr->arguments->val);
                    }
                    return ret;
                }
            }
            *ret = *expr;
            if (expr->callee) {
                ret->callee = quotePass(state, expr->callee);
                ret->arguments = 0;
                if (expr->arguments) {
                    ret->arguments = (ExprList *)malloc(sizeof(ExprList));
                    ret->arguments->val = quotePass(state,
                                                    expr->arguments->val);
                    ExprList *prev = ret->arguments;
                    for (ExprList *param = expr->arguments->next;
                         param; param = param->next) {
                        prev->next = (ExprList *)malloc(sizeof(ExprList));
                        prev = prev->next;
                        prev->val = quotePass(state, param->val);
                    }
                    prev->next = 0;
                }
            }
        } break;
        default:assert(false);
    }
    return ret;
}

Expr *copyQuoted(LispisState *state, Expr *expr) {
    Expr *ret = 0;
    switch (expr->exprType) {
        case EXPR_STRING:
        case EXPR_INT:
        case EXPR_FLOAT:
        case EXPR_SYMBOL_ID: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
        } break;
        case EXPR_LIST: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            if (expr->list) {
                ret->list = (ExprList *)malloc(sizeof(ExprList));
                ret->list->val = copyQuoted(state, expr->list->val);
                ret->list->next = 0;
                ExprList *prev = ret->list;
                for (ExprList *elem = expr->list->next;
                     elem; elem = elem->next) {
                    prev->next = (ExprList *)malloc(sizeof(ExprList));
                    prev = prev->next;
                    prev->val = copyQuoted(state, elem->val);
                    prev->next = 0;
                }
            }
        } break;
        default:assert(false);
    }
    return ret;
}

Expr *lambdaPass(LispisState *state, Expr *expr) {
    Expr *ret = (Expr *)calloc(1, sizeof(Expr));
    switch (expr->exprType) {
        case EXPR_STRING:
        case EXPR_INT:
        case EXPR_FLOAT:
        case EXPR_SYMBOL_ID: {
            *ret = *expr;
        } break;
        case EXPR_QUOTE: {
            *ret = *expr;
            ret->quoted = copyQuoted(state, expr->quoted);
        } break;
        case EXPR_CALL: {
            if (expr->callee && expr->callee->exprType == EXPR_SYMBOL_ID) {
                if (expr->callee->symbolID == lambda) {
                    ret->exprType = EXPR_LAMBDA;
                    ret->line = expr->line;
                    ret->params = 0;
                    assert(expr->arguments);
                    assert(expr->arguments->val);
                    assert(expr->arguments->val->exprType == EXPR_CALL);
                    if (expr->arguments->val->callee) {
                        assert(expr->arguments->val->callee->exprType ==
                               EXPR_SYMBOL_ID);
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
                            assert(param->val->exprType = EXPR_SYMBOL_ID);
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
                        lambdaPass(state, expr->arguments->next->val);
                    ret->body->next = 0;
                    ExprList *prev = ret->body;
                    for (ExprList *param =
                             expr->arguments->next->next;
                         param; param = param->next) {
                        prev->next = (ExprList *)malloc(sizeof(ExprList));
                        prev = prev->next;
                        prev->val = lambdaPass(state, param->val);
                        prev->next = 0;
                    }
                    return ret;
                }
            }
            *ret = *expr;
            if (expr->callee) {
                ret->callee = lambdaPass(state, expr->callee);
                ret->arguments = 0;
                if (expr->arguments) {
                    ret->arguments = (ExprList *)malloc(sizeof(ExprList));
                    ret->arguments->val = lambdaPass(state,
                                                     expr->arguments->val);
                    ExprList *prev = ret->arguments;
                    for (ExprList *param = expr->arguments->next;
                         param; param = param->next) {
                        prev->next = (ExprList *)malloc(sizeof(ExprList));
                        prev = prev->next;
                        prev->val = lambdaPass(state, param->val);
                    }
                    prev->next = 0;
                }
            }
        } break;
        default:assert(false);
    }
    return ret;
}

ExprList *copyLambdaParams(LispisState *state, ExprList *params) {
    if (params) {
        assert(params->val->exprType == EXPR_SYMBOL_ID);
        ExprList *ret = (ExprList *)malloc(sizeof(ExprList));
        ret->val = (Expr *)malloc(sizeof(Expr));
        *ret->val = *params->val;
        ret->next = 0;
        ExprList *prev = ret;
        for (ExprList *param = params->next; param; param = param->next) {
            assert(param->val->exprType == EXPR_SYMBOL_ID);
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

ExprList *copyLambdaBody(LispisState *state, ExprList *body,
                         Expr *(*recursiveFunc)(LispisState *state,
                                                Expr *e)) {
    assert(body);
    ExprList *ret = (ExprList *)malloc(sizeof(ExprList));
    ret->val = recursiveFunc(state, body->val);
    ret->next = 0;
    ExprList *prev = ret;
    for (ExprList *expr = body->next; expr; expr = expr->next) {
        prev->next = (ExprList *)malloc(sizeof(ExprList));
        prev = prev->next;
        prev->next = 0;
        prev->val = recursiveFunc(state, expr->val);
    }
    return ret;
}

Expr *letPass(LispisState *state, Expr *expr) {
    Expr *ret = (Expr *)malloc(sizeof(Expr));
    switch (expr->exprType) {
        case EXPR_STRING:
        case EXPR_INT:
        case EXPR_FLOAT:
        case EXPR_SYMBOL_ID: {
            *ret = *expr;
        } break;
        case EXPR_QUOTE: {
            *ret = *expr;
            ret->quoted = copyQuoted(state, expr->quoted);
        } break;
        case EXPR_LAMBDA: {
            *ret = *expr;
            ret->params = copyLambdaParams(state, expr->params);
            ret->body = copyLambdaBody(state, expr->body, letPass);
        } break;
        case EXPR_CALL: {
            if (expr->callee && expr->callee->exprType == EXPR_SYMBOL_ID) {
                if (expr->callee->symbolID == let) {
                    ret->exprType = EXPR_LET;
                    ret->line = expr->line;
                    assert(expr->arguments);
                    assert(expr->arguments->val);
                    assert(expr->arguments->val->exprType == EXPR_SYMBOL_ID);
                    assert(expr->arguments->next);
                    assert(! expr->arguments->next->next);
                    ret->variable = (Expr *)malloc(sizeof(Expr));
                    *ret->variable = *expr->arguments->val;
                    ret->value = letPass(state,
                                         expr->arguments->next->val);
                    return ret;
                }
            }
            *ret = *expr;
            if (expr->callee) {
                ret->callee = letPass(state, expr->callee);
                ret->arguments = 0;
                if (expr->arguments) {
                    ret->arguments = (ExprList *)malloc(sizeof(ExprList));
                    ret->arguments->val = letPass(state,
                                                  expr->arguments->val);
                    ExprList *prev = ret->arguments;
                    for (ExprList *param = expr->arguments->next;
                         param; param = param->next) {
                        prev->next = (ExprList *)malloc(sizeof(ExprList));
                        prev = prev->next;
                        prev->val = letPass(state, param->val);
                    }
                    prev->next = 0;
                }
            }
        } break;
        default:assert(false);
    }
    return ret;
}

Expr *definePass(LispisState *state, Expr *expr) {
    Expr *ret = (Expr *)malloc(sizeof(Expr));
    switch (expr->exprType) {
        case EXPR_STRING:
        case EXPR_INT:
        case EXPR_FLOAT:
        case EXPR_SYMBOL_ID: {
            *ret = *expr;
        } break;
        case EXPR_QUOTE: {
            *ret = *expr;
            ret->quoted = copyQuoted(state, expr->quoted);
        } break;
        case EXPR_LAMBDA: {
            *ret = *expr;
            ret->params = copyLambdaParams(state, expr->params);
            ret->body = copyLambdaBody(state, expr->body, definePass);
        } break;
        case EXPR_LET: {
            *ret = *expr;
            ret->variable = (Expr *)malloc(sizeof(Expr));
            *ret->variable = *expr->variable;
            ret->value = definePass(state, expr->value);
        } break;
        case EXPR_CALL: {
            if (expr->callee && expr->callee->exprType == EXPR_SYMBOL_ID) {
                if (expr->callee->symbolID == define) {
                    ret->exprType = EXPR_DEFINE;
                    ret->line = expr->line;
                    assert(expr->arguments);
                    assert(expr->arguments->val);
                    assert(expr->arguments->val->exprType == EXPR_SYMBOL_ID);
                    assert(expr->arguments->next);
                    assert(! expr->arguments->next->next);
                    ret->variable = (Expr *)malloc(sizeof(Expr));
                    *ret->variable = *expr->arguments->val;
                    ret->value = definePass(state,
                                            expr->arguments->next->val);
                    return ret;
                }
            }
            *ret = *expr;
            if (expr->callee) {
                ret->callee = definePass(state, expr->callee);
                ret->arguments = 0;
                if (expr->arguments) {
                    ret->arguments = (ExprList *)malloc(sizeof(ExprList));
                    ret->arguments->val =
                        definePass(state, expr->arguments->val);
                    ExprList *prev = ret->arguments;
                    for (ExprList *param = expr->arguments->next;
                         param; param = param->next) {
                        prev->next = (ExprList *)malloc(sizeof(ExprList));
                        prev = prev->next;
                        prev->val = definePass(state, param->val);
                    }
                    prev->next = 0;
                }
            }
        } break;
        default:assert(false);
    }
    return ret;
}

Expr *ifPass(LispisState *state, Expr *expr) {
    Expr *ret = (Expr *)malloc(sizeof(Expr));
    switch (expr->exprType) {
        case EXPR_STRING:
        case EXPR_INT:
        case EXPR_FLOAT:
        case EXPR_SYMBOL_ID: {
            *ret = *expr;
        } break;
        case EXPR_QUOTE: {
            *ret = *expr;
            ret->quoted = copyQuoted(state, expr->quoted);
        } break;
        case EXPR_LAMBDA: {
            *ret = *expr;
            ret->params = copyLambdaParams(state, expr->params);
            ret->body = copyLambdaBody(state, expr->body, ifPass);
        } break;
        case EXPR_DEFINE:
        case EXPR_LET: {
            *ret = *expr;
            ret->variable = (Expr *)malloc(sizeof(Expr));
            *ret->variable = *expr->variable;
            ret->value = ifPass(state, expr->value);
        } break;
        case EXPR_CALL: {
            if (expr->callee && expr->callee->exprType == EXPR_SYMBOL_ID) {
                if (expr->callee->symbolID == ifSym) {
                    ret->exprType = EXPR_IF;
                    ret->line = expr->line;
                    assert(expr->arguments); // predicate
                    ret->predicate = ifPass(state, expr->arguments->val);
                    assert(expr->arguments->val);
                    assert(expr->arguments->next); // trueBranch
                    ret->trueBranch = ifPass(state,
                                             expr->arguments->next->val);
                    if (expr->arguments->next->next) { // falseBranch
                        ret->falseBranch =
                            ifPass(state,
                                   expr->arguments->next->next->val);
                    }
                    return ret;
                }
            }
            *ret = *expr;
            if (expr->callee) {
                ret->callee = ifPass(state, expr->callee);
                ret->arguments = 0;
                if (expr->arguments) {
                    ret->arguments = (ExprList *)malloc(sizeof(ExprList));
                    ret->arguments->val = ifPass(state,
                                                 expr->arguments->val);
                    ExprList *prev = ret->arguments;
                    for (ExprList *param = expr->arguments->next;
                         param; param = param->next) {
                        prev->next = (ExprList *)malloc(sizeof(ExprList));
                        prev = prev->next;
                        prev->val = ifPass(state, param->val);
                    }
                    prev->next = 0;
                }
            }
        } break;
        default:assert(false);
    }
    return ret;
}