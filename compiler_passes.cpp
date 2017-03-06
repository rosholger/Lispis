#include "compiler_passes.h"
#include "parser.h"
#include "vm.h"
#include <cstdlib>
#include <cstdio>
#include <cassert>

uint32 quote;
uint32 quasiquote;
uint32 unquote;
uint32 unquoteSplice;
uint32 defmacro;
uint32 lambda;
uint32 let;
uint32 define;
uint32 ifSym;

Expr *symbolIdPass(LispisState *state, Expr *expr) {
    Expr *ret = (Expr *)malloc(sizeof(Expr));
    switch (expr->exprType) {
        case EXPR_STRING:
        case EXPR_INT:
        case EXPR_DOUBLE: {
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

bool isMacro(LispisState *state, uint32 symbolID) {
    printf("Is Macro? %u\n", symbolID);
    Env *env = &state->globalEnviroment;
    uint32 variableID = symbolID % env->variablesSize;
    uint32 startID = variableID;
    while (env->variables[variableID].symbolID != symbolID) {
        variableID++;
        variableID = variableID % env->variablesSize;
        if (variableID == startID) {
            printf("Nop\n");
            return false;
        }
    }
    Value v = env->variables[variableID].val;
    if (getType(v) != LISPIS_LFUNC) {
        printf("Nopsi\n");
        return false;
    } else {
        printf("Yup\n");
        return unpackLFunc(v)->function->macro;
    }
}

Expr *evalMacro(LispisState *state, Expr *expr) {
    printf("Eval macro!\n");
    int32 numArgs = 0;
    for (ExprList *arg = expr->arguments;
         arg; arg = arg->next) {
        numArgs++;
        push(state, exprToConsList(state, arg->val));
    }
    Value ret = runFunction(state,
                            evalGlobalSymbol(&state->globalEnviroment,
                                             expr->callee->symbolID),
                            numArgs);
    printValue(&state->globalSymbolTable, ret);
    return consListToExpr(state, ret, expr->line);
}

Expr *evalMacroPass(LispisState *state, Expr *expr) {
    Expr *ret = (Expr *)malloc(sizeof(Expr));
    switch (expr->exprType) {
        case EXPR_STRING:
        case EXPR_INT:
        case EXPR_DOUBLE:
        case EXPR_SYMBOL_ID: {
            *ret = *expr;
        } break;
        case EXPR_CALL: {
            if (expr->callee &&
                expr->callee->exprType == EXPR_SYMBOL_ID &&
                isMacro(state, expr->callee->symbolID)) {
                ret = evalMacro(state, expr);
            } else {
                *ret = *expr;
                if (expr->callee) {
                    ret->callee = evalMacroPass(state, expr->callee);
                    ret->arguments = 0;
                    if (expr->arguments) {
                        ret->arguments =
                            (ExprList *)malloc(sizeof(ExprList));
                        ret->arguments->val =
                            evalMacroPass(state, expr->arguments->val);
                        ExprList *prev = ret->arguments;
                        for (ExprList *param = expr->arguments->next;
                             param; param = param->next) {
                            prev->next =
                                (ExprList *)malloc(sizeof(ExprList));
                            prev = prev->next;
                            prev->val = evalMacroPass(state, param->val);
                        }
                        prev->next = 0;
                    }
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
        case EXPR_DOUBLE:
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

QuasiquoteList *createQuasiquotedList(LispisState *state, Expr *expr);

Expr *createQuasiquotedElement(LispisState *state, Expr *expr) {
    Expr *ret = 0;
    switch (expr->exprType) {
        case EXPR_STRING:
        case EXPR_INT:
        case EXPR_DOUBLE:
        case EXPR_SYMBOL_ID: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
        } break;
        case EXPR_CALL: {
            ret = (Expr *)malloc(sizeof(Expr));

            ret->exprType = EXPR_QUASIQUOTE;
            ret->quasiquoteList = createQuasiquotedList(state, expr);
        } break;
        default:assert(false);
    }
    return ret;
}

QuasiquoteList *createQuasiquotedList(LispisState *state, Expr *expr) {
    QuasiquoteList *ret = (QuasiquoteList *)calloc(1,
                                                   sizeof(QuasiquoteList));
    if (expr->callee) {
        if (expr->callee->exprType == EXPR_CALL &&
            expr->callee->callee->exprType == EXPR_SYMBOL_ID &&
            (expr->callee->callee->symbolID == unquote ||
             expr->callee->callee->symbolID == unquoteSplice)) {
            if (expr->callee->callee->symbolID == unquote) {
                assert(expr->callee->arguments);
                assert(!expr->callee->arguments->next);
                ret->unquoted = true;
                ret->val = quotePass(state,
                                          expr->callee->arguments->val);
            }
            if (expr->callee->callee->symbolID == unquoteSplice) {
                assert(expr->callee->arguments);
                assert(!expr->callee->arguments->next);
                ret->unquoteSpliced = true;
                ret->val = quotePass(state,
                                          expr->callee->arguments->val);
            }
        } else {
            ret->val = createQuasiquotedElement(state, expr->callee);
        }
        ret->next = 0;
        QuasiquoteList *prev = ret;
        for (ExprList *argument = expr->arguments;
             argument; argument = argument->next) {
            prev->next = (QuasiquoteList *)malloc(sizeof(QuasiquoteList));
            prev = prev->next;
            Expr *e = argument->val;
            if (e->exprType == EXPR_CALL && e->callee &&
                e->callee->exprType == EXPR_SYMBOL_ID) {
                if (e->callee->symbolID == unquote) {
                    assert(e->arguments);
                    assert(!e->arguments->next);
                    prev->unquoted = true;
                    prev->val = quotePass(state, e->arguments->val);
                    prev->next = 0;
                    continue;
                }
                if (e->callee->symbolID == unquoteSplice) {
                    assert(e->arguments);
                    assert(!e->arguments->next);
                    prev->unquoteSpliced = true;
                    prev->val = quotePass(state, e->arguments->val);
                    prev->next = 0;
                    continue;
                }
            }
            prev->val = createQuasiquotedElement(state, argument->val);
            prev->next = 0;
        }
    } else {
        assert(expr->arguments == 0);
    }
    return ret;
}

Expr *quotePass(LispisState *state, Expr *expr) {
    Expr *ret = (Expr *)malloc(sizeof(Expr));
    switch (expr->exprType) {
        case EXPR_STRING:
        case EXPR_INT:
        case EXPR_DOUBLE:
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
                if (expr->callee->symbolID == quasiquote) {
                    ret->line = expr->line;
                    assert(expr->arguments);
                    assert(!expr->arguments->next);
                    if (expr->arguments->val->exprType == EXPR_CALL) {
                        ret->exprType = EXPR_QUASIQUOTE;
                        ret->quasiquoteList =
                            createQuasiquotedList(state,
                                                  expr->arguments->val);
                    } else {
                        ret->exprType = EXPR_QUOTE;
                        ret->quoted =
                            quotePass(state,
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

Expr *copyQuasiquoted(LispisState *state, Expr *expr);
Expr *copyQuoted(LispisState *state, Expr *expr) {
    Expr *ret = 0;
    switch (expr->exprType) {
        case EXPR_QUASIQUOTE: {
            ret = copyQuasiquoted(state, expr);
        } break;
        case EXPR_STRING:
        case EXPR_INT:
        case EXPR_DOUBLE:
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

Expr *copyQuasiquoted(LispisState *state, Expr *expr) {
    Expr *ret = 0;
    switch (expr->exprType) {
        case EXPR_STRING:
        case EXPR_INT:
        case EXPR_DOUBLE:
        case EXPR_SYMBOL_ID: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
        } break;
        case EXPR_QUASIQUOTE: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            assert(expr->quasiquoteList);
            ret->quasiquoteList =
                (QuasiquoteList *)calloc(1, sizeof(QuasiquoteList));
            if (expr->quasiquoteList->val) {
                *ret->quasiquoteList = *expr->quasiquoteList;
                ret->quasiquoteList->val =
                    copyQuasiquoted(state, expr->quasiquoteList->val);
                ret->quasiquoteList->next = 0;
                QuasiquoteList *prev = ret->quasiquoteList;
                for (QuasiquoteList *elem = expr->quasiquoteList->next;
                     elem; elem = elem->next) {
                    prev->next =
                        (QuasiquoteList *)malloc(sizeof(QuasiquoteList));
                    prev = prev->next;
                    *prev = *elem;
                    prev->val = copyQuasiquoted(state, elem->val);
                    prev->next = 0;
                }
            }
        } break;
        default:assert(false);
    }
    return ret;
}

Expr *macroPass(LispisState *state, Expr *expr) {
    Expr *ret = 0;
    switch (expr->exprType) {
        case EXPR_QUASIQUOTE: {
            ret = copyQuasiquoted(state, expr);
        } break;
        case EXPR_STRING:
        case EXPR_INT:
        case EXPR_DOUBLE:
        case EXPR_SYMBOL_ID: {
            ret = (Expr *)calloc(1, sizeof(Expr));
            *ret = *expr;
        } break;
        case EXPR_QUOTE: {
            ret = (Expr *)calloc(1, sizeof(Expr));
            *ret = *expr;
            ret->quoted = copyQuoted(state, expr->quoted);
        } break;
        case EXPR_CALL: {
            ret = (Expr *)calloc(1, sizeof(Expr));
            if (expr->callee && expr->callee->exprType == EXPR_SYMBOL_ID) {
                if (expr->callee->symbolID == defmacro) {
                    ret->exprType = EXPR_MACRO;
                    ret->line = expr->line;
                    ret->macro.params = 0;
                    assert(expr->arguments);
                    assert(expr->arguments->val);
                    Expr *name = expr->arguments->val;
                    assert(name->exprType == EXPR_SYMBOL_ID);
                    ret->macro.name = name->symbolID;
                    assert(expr->arguments->next);
                    assert(expr->arguments->next->val);
                    Expr *params = expr->arguments->next->val;
                    //assert(params->exprType = EXPR_SYMBOL_ID);
                    assert(expr->arguments->next->next);
                    ExprList *body = expr->arguments->next->next;
                    ret->macro.varargs = params->dotted;
                    if (params->exprType == EXPR_CALL) {
                        if (params->callee) {
                            assert(params->callee->exprType ==
                                   EXPR_SYMBOL_ID);
                            ret->macro.params =
                                (ExprList *)malloc(sizeof(ExprList));
                            ret->macro.params->val = (Expr *)malloc(sizeof(Expr));
                            *ret->macro.params->val = *params->callee;
                            ret->macro.params->next = 0;
                            // FIXME: We cant allow special symbols as params
                            ret->macro.paramsCount = 1;
                            ExprList *prev = ret->macro.params;
                            for (ExprList *param =
                                     params->arguments;
                                 param; param = param->next) {
                                assert(param->val->exprType = EXPR_SYMBOL_ID);
                                prev->next =
                                    (ExprList *)malloc(sizeof(ExprList));
                                prev = prev->next;
                                prev->val = (Expr *)malloc(sizeof(Expr));
                                *prev->val = *param->val;
                                prev->next = 0;
                                ret->macro.paramsCount++;
                            }
                        }
                    } else {
                        assert(params->exprType == EXPR_SYMBOL_ID);
                        ret->macro.varargs = true;
                        ret->macro.params =
                            (ExprList *)malloc(sizeof(ExprList));
                        ret->macro.params->val =
                            (Expr *)malloc(sizeof(Expr));
                        *ret->macro.params->val =
                            *params;
                        ret->macro.params->next = 0;
                        ret->macro.paramsCount = 1;

                    }
                    ret->macro.body = (ExprList *)malloc(sizeof(ExprList));
                    ret->macro.body->val =
                        macroPass(state, body->val);
                    ret->macro.body->next = 0;
                    ExprList *prev = ret->macro.body;
                    for (ExprList *param =
                             body->next;
                         param; param = param->next) {
                        prev->next = (ExprList *)malloc(sizeof(ExprList));
                        prev = prev->next;
                        prev->val = macroPass(state, param->val);
                        prev->next = 0;
                    }
                    return ret;
                }
            }
            *ret = *expr;
            if (expr->callee) {
                ret->callee = macroPass(state, expr->callee);
                ret->arguments = 0;
                if (expr->arguments) {
                    ret->arguments = (ExprList *)malloc(sizeof(ExprList));
                    ret->arguments->val = macroPass(state,
                                                    expr->arguments->val);
                    ExprList *prev = ret->arguments;
                    for (ExprList *param = expr->arguments->next;
                         param; param = param->next) {
                        prev->next = (ExprList *)malloc(sizeof(ExprList));
                        prev = prev->next;
                        prev->val = macroPass(state, param->val);
                    }
                    prev->next = 0;
                }
            }
        } break;
        default:assert(false);
    }
    return ret;
}

ExprList *copyMacroParams(LispisState *state, ExprList *params) {
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

ExprList *copyMacroBody(LispisState *state, ExprList *body,
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

Expr *lambdaPass(LispisState *state, Expr *expr) {
    Expr *ret = 0;
    switch (expr->exprType) {
        case EXPR_QUASIQUOTE: {
            ret = copyQuasiquoted(state, expr);
        } break;
        case EXPR_STRING:
        case EXPR_INT:
        case EXPR_DOUBLE:
        case EXPR_SYMBOL_ID: {
            ret = (Expr *)calloc(1, sizeof(Expr));
            *ret = *expr;
        } break;
        case EXPR_QUOTE: {
            ret = (Expr *)calloc(1, sizeof(Expr));
            *ret = *expr;
            ret->quoted = copyQuoted(state, expr->quoted);
        } break;
        case EXPR_MACRO: {
            ret = (Expr *)calloc(1, sizeof(Expr));
            *ret = *expr;
            ret->macro.params =
                copyMacroParams(state, expr->macro.params);
            ret->macro.body =
                copyMacroBody(state, expr->macro.body, lambdaPass);
        } break;
        case EXPR_CALL: {
            ret = (Expr *)calloc(1, sizeof(Expr));
            if (expr->callee && expr->callee->exprType == EXPR_SYMBOL_ID) {
                if (expr->callee->symbolID == lambda) {
                    ret->exprType = EXPR_LAMBDA;
                    ret->line = expr->line;
                    ret->lambda.params = 0;
                    assert(expr->arguments);
                    assert(expr->arguments->val);
                    //assert(expr->arguments->val->exprType == EXPR_CALL);
                    if (expr->arguments->val->exprType == EXPR_CALL) {
                        ret->lambda.varargs =
                            expr->arguments->val->dotted;
                        if (expr->arguments->val->callee) {
                            assert(expr->arguments->val->callee->exprType ==
                                   EXPR_SYMBOL_ID);
                            ret->lambda.params =
                                (ExprList *)malloc(sizeof(ExprList));
                            ret->lambda.params->val =
                                (Expr *)malloc(sizeof(Expr));
                            *ret->lambda.params->val =
                                *expr->arguments->val->callee;
                            ret->lambda.params->next = 0;
                            // FIXME: We cant allow special symbols as params
                            ret->lambda.paramsCount = 1;
                            ExprList *prev = ret->lambda.params;
                            for (ExprList *param =
                                     expr->arguments->val->arguments;
                                 param; param = param->next) {
                                assert(param->val->exprType =
                                       EXPR_SYMBOL_ID);
                                prev->next =
                                    (ExprList *)malloc(sizeof(ExprList));
                                prev = prev->next;
                                prev->val = (Expr *)malloc(sizeof(Expr));
                                *prev->val = *param->val;
                                prev->next = 0;
                                ret->lambda.paramsCount++;
                            }
                        }
                    } else {
                        printf("VARARG\n");
                        assert(expr->arguments->val->exprType ==
                               EXPR_SYMBOL_ID);
                        ret->lambda.varargs = true;
                        ret->lambda.params =
                            (ExprList *)malloc(sizeof(ExprList));
                        ret->lambda.params->val =
                            (Expr *)malloc(sizeof(Expr));
                        *ret->lambda.params->val =
                            *expr->arguments->val;
                        ret->lambda.params->next = 0;
                        ret->lambda.paramsCount = 1;
                    }
                    assert(expr->arguments->next);
                    ret->lambda.body =
                        (ExprList *)malloc(sizeof(ExprList));
                    ret->lambda.body->val =
                        lambdaPass(state, expr->arguments->next->val);
                    ret->lambda.body->next = 0;
                    ExprList *prev = ret->lambda.body;
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
    Expr *ret = 0;
    switch (expr->exprType) {
        case EXPR_QUASIQUOTE: {
            ret = copyQuasiquoted(state, expr);
        } break;
        case EXPR_STRING:
        case EXPR_INT:
        case EXPR_DOUBLE:
        case EXPR_SYMBOL_ID: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
        } break;
        case EXPR_QUOTE: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->quoted = copyQuoted(state, expr->quoted);
        } break;
        case EXPR_MACRO: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->macro.params = copyMacroParams(state,
                                                expr->macro.params);
            ret->macro.body = copyMacroBody(state, expr->macro.body,
                                            letPass);
        } break;
        case EXPR_LAMBDA: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->lambda.params = copyLambdaParams(state,
                                                  expr->lambda.params);
            ret->lambda.body = copyLambdaBody(state, expr->lambda.body,
                                              letPass);
        } break;
        case EXPR_CALL: {
            ret = (Expr *)malloc(sizeof(Expr));
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
    Expr *ret = 0;
    switch (expr->exprType) {
        case EXPR_QUASIQUOTE: {
            ret = copyQuasiquoted(state, expr);
        } break;
        case EXPR_STRING:
        case EXPR_INT:
        case EXPR_DOUBLE:
        case EXPR_SYMBOL_ID: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
        } break;
        case EXPR_QUOTE: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->quoted = copyQuoted(state, expr->quoted);
        } break;
        case EXPR_MACRO: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->macro.params = copyMacroParams(state, expr->macro.params);
            ret->macro.body = copyMacroBody(state, expr->macro.body, definePass);
        } break;
        case EXPR_LAMBDA: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->lambda.params = copyLambdaParams(state, expr->lambda.params);
            ret->lambda.body = copyLambdaBody(state, expr->lambda.body, definePass);
        } break;
        case EXPR_LET: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->variable = (Expr *)malloc(sizeof(Expr));
            *ret->variable = *expr->variable;
            ret->value = definePass(state, expr->value);
        } break;
        case EXPR_CALL: {
            ret = (Expr *)malloc(sizeof(Expr));
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
    Expr *ret = 0;
    switch (expr->exprType) {
        case EXPR_QUASIQUOTE: {
            ret = copyQuasiquoted(state, expr);
        } break;
        case EXPR_STRING:
        case EXPR_INT:
        case EXPR_DOUBLE:
        case EXPR_SYMBOL_ID: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
        } break;
        case EXPR_QUOTE: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->quoted = copyQuoted(state, expr->quoted);
        } break;
        case EXPR_MACRO: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->macro.params = copyMacroParams(state, expr->macro.params);
            ret->macro.body = copyMacroBody(state, expr->macro.body, ifPass);
        } break;
        case EXPR_LAMBDA: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->lambda.params = copyLambdaParams(state, expr->lambda.params);
            ret->lambda.body = copyLambdaBody(state, expr->lambda.body, ifPass);
        } break;
        case EXPR_DEFINE:
        case EXPR_LET: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->variable = (Expr *)malloc(sizeof(Expr));
            *ret->variable = *expr->variable;
            ret->value = ifPass(state, expr->value);
        } break;
        case EXPR_CALL: {
            ret = (Expr *)malloc(sizeof(Expr));
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