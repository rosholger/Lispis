#include "compiler_passes.h"
#include "parser.h"
#include "vm.h"
#include <cstdlib>
#include <cstdio>
#include <cassert>
#include <cstring>

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
    Env *env = &state->globalEnviroment;
    uint32 variableID = symbolID % env->variablesSize;
    uint32 startID = variableID;
    while (env->variables[variableID].symbolID != symbolID) {
        variableID++;
        variableID = variableID % env->variablesSize;
        if (variableID == startID) {
            return false;
        }
    }
    Value v = env->variables[variableID].val;
    if (getType(v) != LISPIS_LFUNC) {
        return false;
    } else {
        return unpackLFunc(v)->function->macro;
    }
}

Expr *evalMacro(LispisState *state, Expr *expr) {
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
#if LOG_ENABLED
    printValue(&state->globalSymbolTable, ret);
#endif
    return consListToExpr(state, ret, expr->line);
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

Expr *evalMacroPassInternal(LispisState *state, Expr *expr,
                            bool *expandedMacro);

QuasiquoteList *createQuasiquotedList(LispisState *state,
                                      Expr *expr, bool *expandedMacro);

Expr *createQuasiquotedElement(LispisState *state,
                               Expr *expr, bool *expandedMacro) {
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
            *ret = *expr;

            ret->exprType = EXPR_QUASIQUOTE;
            ret->quasiquoteList = createQuasiquotedList(state,
                                                        expr,
                                                        expandedMacro);
        } break;
        default:assert(false);
    }
    return ret;
}

QuasiquoteList *createQuasiquotedList(LispisState *state,
                                      Expr *expr, bool *expandedMacro) {
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
                ret->val =
                    evalMacroPassInternal(state,
                                          expr->callee->arguments->val,
                                          expandedMacro);
            }
            if (expr->callee->callee->symbolID == unquoteSplice) {
                assert(expr->callee->arguments);
                assert(!expr->callee->arguments->next);
                ret->unquoteSpliced = true;
                ret->val =
                    evalMacroPassInternal(state,
                                          expr->callee->arguments->val,
                                          expandedMacro);
            }
        } else {
            ret->val = createQuasiquotedElement(state, expr->callee,
                                                expandedMacro);
        }
        ret->next = 0;
        QuasiquoteList *prev = ret;
        for (ExprList *argument = expr->arguments;
             argument; argument = argument->next) {
            prev->next = (QuasiquoteList *)malloc(sizeof(QuasiquoteList));
            prev = prev->next;
            prev->unquoted = false;
            prev->unquoteSpliced = false;
            Expr *e = argument->val;
            if (e->exprType == EXPR_CALL && e->callee &&
                e->callee->exprType == EXPR_SYMBOL_ID) {
                if (e->callee->symbolID == unquote) {
                    assert(e->arguments);
                    assert(!e->arguments->next);
                    prev->unquoted = true;
                    prev->val = evalMacroPassInternal(state,
                                                      e->arguments->val,
                                                      expandedMacro);
                    prev->next = 0;
                    continue;
                }
                if (e->callee->symbolID == unquoteSplice) {
                    assert(e->arguments);
                    assert(!e->arguments->next);
                    prev->unquoteSpliced = true;
                    prev->val = evalMacroPassInternal(state,
                                                      e->arguments->val,
                                                      expandedMacro);
                    prev->next = 0;
                    continue;
                }
            }
            prev->val = createQuasiquotedElement(state,
                                                 argument->val,
                                                 expandedMacro);
            prev->next = 0;
        }
    } else {
        assert(expr->arguments == 0);
    }
    return ret;
}

Expr *evalMacroPassInternal(LispisState *state, Expr *expr,
                            bool *expandedMacro) {
                   
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
            if (expr->callee &&
                expr->callee->exprType == EXPR_SYMBOL_ID) {
                if (isMacro(state, expr->callee->symbolID)) {
                    *expandedMacro = true;
                    ret = evalMacro(state, expr);
                    return ret;
                }
                if (expr->callee->symbolID == quote) {
                    ret = (Expr *)malloc(sizeof(Expr));
                    ret->exprType = EXPR_QUOTE;
                    ret->line = expr->line;
                    assert(expr->arguments);
                    assert(!expr->arguments->next);
                    ret->quoted =
                        createQuotedElement(state, expr->arguments->val);
                    return ret;
                }
                if (expr->callee->symbolID == quasiquote) {
                    ret = (Expr *)malloc(sizeof(Expr));
                    ret->line = expr->line;
                    assert(expr->arguments);
                    assert(!expr->arguments->next);
                    if (expr->arguments->val->exprType == EXPR_CALL) {
                        ret->dotted = expr->arguments->val->dotted;
                        ret->exprType = EXPR_QUASIQUOTE;
                        ret->quasiquoteList =
                            createQuasiquotedList(state,
                                                  expr->arguments->val,
                                                  expandedMacro);
                    } else {
                        ret->exprType = EXPR_QUOTE;
                        ret->quoted =
                            createQuotedElement(state,
                                                expr->arguments->val);
                    }
                    return ret;
                }
            }
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            if (expr->callee) {
                ret->callee = evalMacroPassInternal(state, expr->callee,
                                                    expandedMacro);
                ret->arguments = 0;
                if (expr->arguments) {
                    ret->arguments =
                        (ExprList *)malloc(sizeof(ExprList));
                    ret->arguments->val =
                        evalMacroPassInternal(state,
                                              expr->arguments->val,
                                              expandedMacro);
                    ExprList *prev = ret->arguments;
                    for (ExprList *param = expr->arguments->next;
                         param; param = param->next) {
                        prev->next =
                            (ExprList *)malloc(sizeof(ExprList));
                        prev = prev->next;
                        prev->val = evalMacroPassInternal(state,
                                                          param->val,
                                                          expandedMacro);
                    }
                    prev->next = 0;
                }
            }
        } break;
        default:assert(false);
    }
    return ret;
}

Expr *evalMacroPass(LispisState *state, Expr *expr) {
    bool expandedMacro = false;
    Expr *prev = expr;
    Expr *next = 0;
    int numExpansions = 0;
    do {
        expandedMacro = false;
        next = evalMacroPassInternal(state, prev, &expandedMacro);
        if (numExpansions) {
            dealloc(prev);
        }
        prev = next;
        next = 0;
        numExpansions++;
    } while(expandedMacro);
    return prev;
}

Expr *copyQuoted(LispisState *state, Expr *expr) {
    Expr *ret = 0;
    switch (expr->exprType) {
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

Expr *copyQuasiquoted(LispisState *state, Expr *expr,
                      CompilerPass pass1, VariablePass pass2,
                      LexicalScope *scope) {
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
                    copyQuasiquoted(state, expr->quasiquoteList->val,
                                    pass1, pass2, scope);
                ret->quasiquoteList->next = 0;
                QuasiquoteList *prev = ret->quasiquoteList;
                for (QuasiquoteList *elem = expr->quasiquoteList->next;
                     elem; elem = elem->next) {
                    prev->next =
                        (QuasiquoteList *)malloc(sizeof(QuasiquoteList));
                    prev = prev->next;
                    *prev = *elem;
                    if (elem->unquoted || elem->unquoteSpliced) {
                        if (pass1) {
                            assert(!pass2);
                            assert(!scope);
                            prev->val = pass1(state, elem->val);
                        } else {
                            assert(pass2);
                            assert(scope);
                            assert(!pass1);
                            prev->val = pass2(state, elem->val, scope);
                        }
                    } else {
                        prev->val = copyQuasiquoted(state, elem->val,
                                                    pass1, pass2, scope);
                    }
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
            ret = copyQuasiquoted(state, expr, macroPass, 0, 0);
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
            *ret = *expr;
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
                        CompilerPass cpass,
                        VariablePass vpass, LexicalScope *scope) {
    assert(body);
    ExprList *ret = (ExprList *)malloc(sizeof(ExprList));
    if (cpass) {
        assert(!vpass);
        assert(!scope);
        ret->val = cpass(state, body->val);
    } else {
        assert(vpass);
        assert(scope);
        assert(!cpass);
        ret->val = vpass(state, body->val, scope);
    }
    ret->next = 0;
    ExprList *prev = ret;
    for (ExprList *expr = body->next; expr; expr = expr->next) {
        prev->next = (ExprList *)malloc(sizeof(ExprList));
        prev = prev->next;
        prev->next = 0;
        if (cpass) {
            assert(!vpass);
            assert(!scope);
            prev->val = cpass(state, expr->val);
        } else {
            assert(vpass);
            assert(scope);
            assert(!cpass);
            prev->val = vpass(state, expr->val, scope);
        }
    }
    return ret;
}

Expr *lambdaPass(LispisState *state, Expr *expr) {
    Expr *ret = 0;
    switch (expr->exprType) {
        case EXPR_QUASIQUOTE: {
            ret = copyQuasiquoted(state, expr, lambdaPass, 0, 0);
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
                copyMacroBody(state, expr->macro.body, lambdaPass, 0, 0);
        } break;
        case EXPR_CALL: {
            ret = (Expr *)calloc(1, sizeof(Expr));
            *ret = *expr;
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
                         CompilerPass cpass,
                         VariablePass vpass, LexicalScope *scope) {
    assert(body);
    ExprList *ret = (ExprList *)malloc(sizeof(ExprList));
    if (cpass) {
        assert(!vpass);
        assert(!scope);
        ret->val = cpass(state, body->val);
    } else {
        assert(vpass);
        assert(scope);
        assert(!cpass);
        ret->val = vpass(state, body->val, scope);
    }
    ret->next = 0;
    ExprList *prev = ret;
    for (ExprList *expr = body->next; expr; expr = expr->next) {
        prev->next = (ExprList *)malloc(sizeof(ExprList));
        prev = prev->next;
        prev->next = 0;
        if (cpass) {
            assert(!vpass);
            assert(!scope);
            prev->val = cpass(state, expr->val);
        } else {
            assert(vpass);
            assert(scope);
            assert(!cpass);
            prev->val = vpass(state, expr->val, scope);
        }
    }
    return ret;
}

Expr *letPass(LispisState *state, Expr *expr) {
    Expr *ret = 0;
    switch (expr->exprType) {
        case EXPR_QUASIQUOTE: {
            ret = copyQuasiquoted(state, expr, letPass, 0, 0);
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
                                            letPass, 0, 0);
        } break;
        case EXPR_LAMBDA: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->lambda.params = copyLambdaParams(state,
                                                  expr->lambda.params);
            ret->lambda.body = copyLambdaBody(state, expr->lambda.body,
                                              letPass, 0, 0);
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
            ret = copyQuasiquoted(state, expr, definePass, 0, 0);
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
            ret->macro.body = copyMacroBody(state, expr->macro.body,
                                            definePass, 0, 0);
        } break;
        case EXPR_LAMBDA: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->lambda.params = copyLambdaParams(state, expr->lambda.params);
            ret->lambda.body = copyLambdaBody(state, expr->lambda.body,
                                              definePass, 0, 0);
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
            ret = copyQuasiquoted(state, expr, ifPass, 0, 0);
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
            ret->macro.body = copyMacroBody(state, expr->macro.body,
                                            ifPass, 0, 0);
        } break;
        case EXPR_LAMBDA: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->lambda.params = copyLambdaParams(state,
                                                  expr->lambda.params);
            ret->lambda.body = copyLambdaBody(state, expr->lambda.body,
                                              ifPass, 0, 0);
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

Expr *variablePass(LispisState *state, Expr *expr, LexicalScope *scope) {
    Expr *ret = 0;
    switch (expr->exprType) {
        case EXPR_QUASIQUOTE: {
            ret = copyQuasiquoted(state, expr, 0, variablePass, scope);
        } break;
        case EXPR_STRING:
        case EXPR_INT:
        case EXPR_DOUBLE: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
        } break;
        case EXPR_SYMBOL_ID: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->exprType = EXPR_VARIABLE;
            ret->var.symbolID = expr->symbolID;
            ret->var.kind = VAR_GLOBAL;
            bool found = false;
            for (uint32 i = 0; i < scope->variableIDsTop; ++i) {
                if (scope->variableIDs[i] == ret->var.symbolID) {
                    found = true;
                    ret->var.kind = VAR_LOCAL;
                    ret->var.variableID = i;
                    break;
                }
            }
            if (!found) {
                for (LexicalScope *searchedScope = scope->parentScope;
                     searchedScope;
                     searchedScope = searchedScope->parentScope) {
                    for (uint32 i = 0;
                         i < searchedScope->variableIDsTop;
                         ++i) {
                        if (searchedScope->variableIDs[i] ==
                            ret->var.symbolID) {
                            ret->var.kind = VAR_UPVAL;
                            ret->var.variableID = i;
                            break;
                        }
                    }
                }
            }
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
            LexicalScope macroScope = {};
            macroScope.parentScope = scope;
            for (ExprList *params = ret->macro.params; params;
                 params = params->next) {
                macroScope.variableIDs[macroScope.variableIDsTop] =
                    params->val->symbolID;
                macroScope.variableIDsTop++;
            }
            ret->macro.body = copyMacroBody(state, expr->macro.body,
                                            0, variablePass, &macroScope);
        } break;
        case EXPR_LAMBDA: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->lambda.params = copyLambdaParams(state,
                                                  expr->lambda.params);
            LexicalScope lambdaScope = {};
            lambdaScope.parentScope = scope;
            for (ExprList *params = ret->lambda.params; params;
                 params = params->next) {
                lambdaScope.variableIDs[lambdaScope.variableIDsTop] =
                    params->val->symbolID;
                lambdaScope.variableIDsTop++;
            }
            ret->lambda.body = copyLambdaBody(state, expr->lambda.body,
                                              0, variablePass,
                                              &lambdaScope);
        } break;
        case EXPR_DEFINE:
        case EXPR_LET: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->variable = (Expr *)malloc(sizeof(Expr));
            *ret->variable = *expr->variable;
            scope->variableIDs[scope->variableIDsTop] =
                ret->variable->symbolID;
            scope->variableIDsTop++;
            ret->value = variablePass(state, expr->value, scope);
        } break;
        case EXPR_CALL: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            if (expr->callee) {
                ret->callee = variablePass(state, expr->callee, scope);
                ret->arguments = 0;
                if (expr->arguments) {
                    ret->arguments = (ExprList *)malloc(sizeof(ExprList));
                    ret->arguments->val =
                        variablePass(state, expr->arguments->val, scope);
                    ExprList *prev = ret->arguments;
                    for (ExprList *param = expr->arguments->next;
                         param; param = param->next) {
                        prev->next = (ExprList *)malloc(sizeof(ExprList));
                        prev = prev->next;
                        prev->val = variablePass(state, param->val,
                                                 scope);
                    }
                    prev->next = 0;
                }
            }
        } break;
        case EXPR_IF: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->predicate = variablePass(state,
                                          expr->predicate,
                                          scope);
            ret->trueBranch = variablePass(state,
                                           expr->trueBranch,
                                           scope);
            if (expr->falseBranch) {
                ret->falseBranch = variablePass(state,
                                                expr->falseBranch,
                                                scope);
            }
        } break;
        default:assert(false);
    }
    return ret;
}

void setupSpecialFormSymbols(LispisState *state) {
    String quoteStr;
    quoteStr.val = (char *)"quote";
    quoteStr.length = strlen(quoteStr.val);
    quote = internSymbol(state, quoteStr, hashFunc(quoteStr));
    quoteSym.exprType = EXPR_SYMBOL;
    quoteSym.str = quoteStr;

    String quasiquoteStr;
    quasiquoteStr.val = (char *)"quasiquote";
    quasiquoteStr.length = strlen(quasiquoteStr.val);
    quasiquote = internSymbol(state, quasiquoteStr,
                              hashFunc(quasiquoteStr));
    String unquoteStr;
    unquoteStr.val = (char *)"unquote";
    unquoteStr.length = strlen(unquoteStr.val);
    unquote = internSymbol(state, unquoteStr,
                           hashFunc(unquoteStr));
    String unquoteSpliceStr;
    unquoteSpliceStr.val = (char *)"unquote-splice";
    unquoteSpliceStr.length = strlen(unquoteSpliceStr.val);
    unquoteSplice = internSymbol(state, unquoteSpliceStr,
                                 hashFunc(unquoteSpliceStr));

    String lambdaStr;
    lambdaStr.val = (char *)"lambda";
    lambdaStr.length = strlen(lambdaStr.val);
    lambda = internSymbol(state, lambdaStr, hashFunc(lambdaStr));

    String letStr;
    letStr.val = (char *)"let!";
    letStr.length = strlen(letStr.val);
    let = internSymbol(state, letStr, hashFunc(letStr));

    String defineStr;
    defineStr.val = (char *)"define!";
    defineStr.length = strlen(defineStr.val);
    define = internSymbol(state, defineStr, hashFunc(defineStr));

    String ifStr;
    ifStr.val = (char *)"if";
    ifStr.length = strlen(ifStr.val);
    ifSym = internSymbol(state, ifStr, hashFunc(ifStr));

    String defmacroStr;
    defmacroStr.val = (char *)"defmacro!";
    defmacroStr.length = strlen(defmacroStr.val);
    defmacro = internSymbol(state, defmacroStr, hashFunc(defmacroStr));
}