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
uint32 set;
uint32 define;
uint32 ifSym;
uint32 forSym;
uint32 doSym;
uint32 it;
uint32 ref;
uint32 refSet;

Expr *CompilerPass::startTransforming(LispisState *state, Expr *expr) {
    return this->transform(state, expr);
}

Expr *copyVector(LispisState *state, Expr *expr, CompilerPass *pass) {
    assert(expr);
    Expr *ret = (Expr *)malloc(sizeof(Expr));
    *ret = *expr;
    if (expr->vec.elems) {
        ret->vec.elems = (ExprList *)malloc(sizeof(ExprList));
        ExprList *prev = ret->vec.elems;
        prev->next = 0;
        prev->val = pass->transform(state, expr->vec.elems->val);
        for (ExprList *e = expr->vec.elems->next; e; e = e->next) {
            prev->next = (ExprList *)malloc(sizeof(ExprList));
            prev = prev->next;
            prev->next = 0;
            prev->val = pass->transform(state, e->val);
        }
    }
    return ret;
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
        case EXPR_VECTOR: {
            assert(expr);
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            if (expr->vec.elems) {
                ret->vec.elems = (ExprList *)malloc(sizeof(ExprList));
                ExprList *prev = ret->vec.elems;
                prev->next = 0;
                prev->val = copyQuoted(state, expr->vec.elems->val);
                for (ExprList *e = expr->vec.elems->next;
                     e; e = e->next) {
                    prev->next = (ExprList *)malloc(sizeof(ExprList));
                    prev = prev->next;
                    prev->next = 0;
                    prev->val = copyQuoted(state, e->val);
                }
            }
            assert(ret);
        } break;
        case EXPR_OBJECT: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            if (expr->obj.elems) {
                ret->obj.elems = (ExprList *)malloc(sizeof(ExprList));
                ExprList *prev = ret->obj.elems;
                prev->val = (Expr *)malloc(sizeof(Expr));
                *prev->val = *expr->obj.elems->val;
                prev->val->keyValPair.key =
                    copyQuoted(state,
                               expr->vec.elems->val->keyValPair.key);
                prev->val->keyValPair.val =
                    copyQuoted(state,
                               expr->vec.elems->val->keyValPair.val);
                prev->next = 0;
                for (ExprList *e = expr->vec.elems->next;
                     e; e = e->next) {
                    prev->next = (ExprList *)malloc(sizeof(ExprList));
                    prev = prev->next;
                    prev->val = (Expr *)malloc(sizeof(Expr));
                    *prev->val = *expr->obj.elems->val;
                    prev->val->keyValPair.key =
                        copyQuoted(state, e->val->keyValPair.key);
                    prev->val->keyValPair.val =
                        copyQuoted(state, e->val->keyValPair.val);
                    prev->next = 0;
                }
            }
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

Expr *copyKeyValPair(LispisState *state, Expr *expr, CompilerPass *pass) {
    assert(expr);
    Expr *ret = (Expr *)malloc(sizeof(Expr));
    *ret = *expr;
    if (expr->keyValPair.unquotedKey) {
        ret->keyValPair.key = pass->transform(state,
                                              expr->keyValPair.key);
    } else {
        ret->keyValPair.key = copyQuoted(state, expr->keyValPair.key);
    }
    ret->keyValPair.val = pass->transform(state, expr->keyValPair.val);
    return ret;
}

Expr *copyObject(LispisState *state, Expr *expr, CompilerPass *pass) {
    assert(expr);
    Expr *ret = (Expr *)malloc(sizeof(Expr));
    *ret = *expr;
    if (expr->obj.elems) {
        ret->obj.elems = (ExprList *)malloc(sizeof(ExprList));
        ExprList *prev = ret->obj.elems;
        prev->next = 0;
        prev->val = copyKeyValPair(state, expr->obj.elems->val, pass);
        for (ExprList *e = expr->obj.elems->next; e; e = e->next) {
            prev->next = (ExprList *)malloc(sizeof(ExprList));
            prev = prev->next;
            prev->next = 0;
            prev->val = copyKeyValPair(state, e->val, pass);
        }
    }
    return ret;
}

Expr *SymbolIdPass::transform(LispisState *state, Expr *expr) {
    Expr *ret = 0;
    switch (expr->exprType) {
        case EXPR_OBJECT: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            if (expr->obj.elems) {
                ret->obj.elems = (ExprList *)malloc(sizeof(ExprList));
                ExprList *prev = ret->obj.elems;
                prev->next = 0;
                prev->val = (Expr *)malloc(sizeof(Expr));
                *prev->val = *expr->obj.elems->val;
                prev->val->keyValPair.key =
                    this->transform(state,
                                    expr->obj.elems->val->keyValPair.key);
                prev->val->keyValPair.val =
                    this->transform(state,
                                    expr->obj.elems->val->keyValPair.val);
                                                            
                for (ExprList *e = expr->obj.elems->next;
                     e; e = e->next) {
                    prev->next = (ExprList *)malloc(sizeof(ExprList));
                    prev = prev->next;
                    prev->next = 0;
                    prev->val = (Expr *)malloc(sizeof(Expr));
                    *prev->val = *e->val;
                    prev->val->keyValPair.key =
                        this->transform(state, e->val->keyValPair.key);
                    prev->val->keyValPair.val =
                        this->transform(state, e->val->keyValPair.val);
                }
            }
        } break;
        case EXPR_VECTOR: {
            ret = copyVector(state, expr, this);
        } break;
        case EXPR_STRING:
        case EXPR_INT:
        case EXPR_DOUBLE: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
        } break;
        case EXPR_SYMBOL: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->exprType = EXPR_SYMBOL_ID;
            ret->symbolID = internSymbol(state, expr->str,
                                         hashFunc(expr->str));
        } break;
        case EXPR_CALL: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            if (expr->callee) {
                ret->callee = this->transform(state, expr->callee);
                ret->arguments = 0;
                if (expr->arguments) {
                    ret->arguments = (ExprList *)malloc(sizeof(ExprList));
                    ret->arguments->val =
                        this->transform(state, expr->arguments->val);
                    ExprList *prev = ret->arguments;
                    for (ExprList *param = expr->arguments->next;
                         param; param = param->next) {
                        prev->next = (ExprList *)malloc(sizeof(ExprList));
                        prev = prev->next;
                        prev->val = this->transform(state, param->val);
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
    GlobalEnv *env = &state->globalEnviroment;
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
        pushInternal(state, exprToConsList(state, arg->val));
    }
    // Dont catch exceptions here
    runFunctionInternal(state,
                        getGlobal(state, expr->callee->symbolID),
                        numArgs);
    Value ret = popInternal(state);

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
        case EXPR_OBJECT: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            if (expr->obj.elems) {
                ret->obj.elems = (ExprList *)malloc(sizeof(ExprList));
                ExprList *prev = ret->obj.elems;
                prev->val = (Expr *)malloc(sizeof(Expr));
                *prev->val = *expr->obj.elems->val;
                prev->val->keyValPair.key =
                    createQuotedElement(state,
                                        expr->vec.elems->val->keyValPair.key);
                prev->val->keyValPair.val =
                    createQuotedElement(state,
                                        expr->vec.elems->val->keyValPair.val);
                prev->next = 0;
                for (ExprList *e = expr->vec.elems->next;
                     e; e = e->next) {
                    prev->next = (ExprList *)malloc(sizeof(ExprList));
                    prev = prev->next;
                    prev->val = (Expr *)malloc(sizeof(Expr));
                    *prev->val = *expr->obj.elems->val;
                    prev->val->keyValPair.key =
                        createQuotedElement(state,
                                            e->val->keyValPair.key);
                    prev->val->keyValPair.val =
                        createQuotedElement(state,
                                            e->val->keyValPair.val);
                    prev->next = 0;
                }
            }
        } break;
        case EXPR_VECTOR: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            if (expr->vec.elems) {
                ret->vec.elems = (ExprList *)malloc(sizeof(ExprList));
                ExprList *prev = ret->vec.elems;
                prev->val = createQuotedElement(state,
                                                expr->vec.elems->val);
                prev->next = 0;
                for (ExprList *e = expr->vec.elems->next;
                     e; e = e->next) {
                    prev->next = (ExprList *)malloc(sizeof(ExprList));
                    prev = prev->next;
                    prev->val = createQuotedElement(state,
                                                    e->val);
                    prev->next = 0;
                }
            }
        } break;
        case EXPR_CALL: {
            ret = createQuotedList(state, expr);
        } break;
        default:assert(false);
    }
    assert(ret);
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

bool isUnquote(Expr *expr) {
    return (expr && expr->exprType == EXPR_CALL && expr->callee
            && expr->callee->exprType == EXPR_SYMBOL_ID &&
            expr->callee->symbolID == unquote);
}

bool isUnquoteSplice(Expr *expr) {
    return (expr && expr->exprType == EXPR_CALL && expr->callee
            && expr->callee->exprType == EXPR_SYMBOL_ID &&
            expr->callee->symbolID == unquoteSplice);
}

bool isQuasiquote(Expr *expr) {
    return (expr && expr->exprType == EXPR_CALL && expr->callee
            && expr->callee->exprType == EXPR_SYMBOL_ID &&
            expr->callee->symbolID == quasiquote);
}

#define doUnquote(s, e, r, p, d, b)                                     \
    do{                                                                 \
        d--;                                                            \
        if (d == 0) {                                                   \
            d = 1;                                                      \
            b = true;                                                   \
            pushError((s), ((e)->arguments ||                           \
                            !(e)->arguments->next),                     \
                      "unquote-has-arity-1");                           \
            (r)->unquoted = true;                                       \
            (r)->val = (p)->transform((s), (e)->arguments->val);        \
        }                                                               \
    }while(0)

#define doUnquoteSplice(s, e, r, p, d, b)                               \
    do{                                                                 \
        d--;                                                            \
        if (d == 0) {                                                   \
            d = 1;                                                      \
            b = true;                                                   \
            pushError((s), ((e)->arguments ||                           \
                            !(e)->arguments->next),                     \
                      "unquote-splice-has-arity-1");                    \
            (r)->unquoteSpliced = true;                                 \
            (r)->val = (p)->transform((s), (e)->arguments->val);        \
        }                                                               \
    }while(0)

QuasiquoteList *createQuasiquotedList(LispisState *state,
                                      Expr *expr,
                                      CompilerPass *pass, int depth);

Expr *createQuasiquotedElement(LispisState *state,
                               Expr *expr, CompilerPass *pass,
                               int depth) {
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

            if (isQuasiquote(expr)) {
                depth++;
            }

            ret->exprType = EXPR_QUASIQUOTE;
            ret->quasiquoteList = createQuasiquotedList(state, expr,
                                                        pass, depth);
                                                        
        } break;
        default:assert(false);
    }
    return ret;
}

QuasiquoteList *createQuasiquotedList(LispisState *state, Expr *expr,
                                      CompilerPass *pass, int depth) {
    QuasiquoteList *ret =
        (QuasiquoteList *)calloc(1, sizeof(QuasiquoteList));
    if (expr->callee) {
        if (isUnquote(expr->callee)) {
            bool wasUnquoted = false;
            doUnquote(state, expr->callee, ret, pass, depth, wasUnquoted);
            if (!wasUnquoted) {
                ret->val = createQuasiquotedElement(state, expr->callee,
                                                    pass, depth);
            }
        } else if (isUnquoteSplice(expr->callee)) {
            bool wasUnquoted = false;
            doUnquoteSplice(state, expr->callee, ret, pass,
                            depth, wasUnquoted);
            if (!wasUnquoted) {
                ret->val = createQuasiquotedElement(state, expr->callee,
                                                    pass, depth);
            }
        } else {
            ret->val = createQuasiquotedElement(state, expr->callee,
                                                pass, depth);
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
            bool wasUnquoted = false;
            if (isUnquote(e)) {
                doUnquote(state, e, prev, pass, depth, wasUnquoted);
                prev->next = 0;
                if (wasUnquoted) {
                    continue;
                }
            }
            if (isUnquoteSplice(e)) {
                doUnquoteSplice(state, e, prev, pass, depth, wasUnquoted);
                prev->next = 0;
                if (wasUnquoted) {
                    continue;
                }
            }
            prev->val = createQuasiquotedElement(state, argument->val,
                                                 pass, depth);
            prev->next = 0;
        }
    } else {
        assert(expr->arguments == 0);
    }
    return ret;
}

Expr *MacroExpansionPass::transform(LispisState *state, Expr *expr) {
                   
    Expr *ret = 0;
    switch (expr->exprType) {
        case EXPR_VECTOR: {
            ret = copyVector(state, expr, this);
        } break;
        case EXPR_OBJECT: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            if (expr->obj.elems) {
                ret->obj.elems = (ExprList *)malloc(sizeof(ExprList));
                ExprList *prev = ret->obj.elems;
                prev->next = 0;
                prev->val = (Expr *)malloc(sizeof(Expr));
                *prev->val = *expr->obj.elems->val;
                Expr *key = expr->obj.elems->val->keyValPair.key;
                if (isUnquote(key)) {
                    pushError(state, (key->arguments &&
                                      !key->arguments->next),
                              "unquote-has-arity-1");
                    prev->val->keyValPair.key =
                        this->transform(state, key->arguments->val);
                    prev->val->keyValPair.unquotedKey = true;
                } else {
                    pushError(state, key->exprType == EXPR_SYMBOL_ID,
                              "object-key-not-symbol");
                    prev->val->keyValPair.key = 
                        copyQuoted(state,
                                   expr->obj.elems->val->keyValPair.key);
                }
                prev->val->keyValPair.val =
                    this->transform(state,
                                    expr->obj.elems->val->keyValPair.val);
                                                            
                for (ExprList *e = expr->obj.elems->next;
                     e; e = e->next) {
                    prev->next = (ExprList *)malloc(sizeof(ExprList));
                    prev = prev->next;
                    prev->next = 0;
                    prev->val = (Expr *)malloc(sizeof(Expr));
                    *prev->val = *e->val;
                    Expr *key = e->val->keyValPair.key;
                    if (isUnquote(key)) {
                        pushError(state, (key->arguments &&
                                          !key->arguments->next),
                                  "unquote-has-arity-1");
                        prev->val->keyValPair.key =
                            this->transform(state,
                                            key->arguments->val);
                        prev->val->keyValPair.unquotedKey = true;
                    } else {
                        pushError(state,
                                  key->exprType == EXPR_SYMBOL_ID,
                                  "object-key-not-symbol");
                        prev->val->keyValPair.key = 
                            copyQuoted(state,
                                       key);
                    }
                    prev->val->keyValPair.val =
                        this->transform(state, e->val->keyValPair.val);
                }
            }
        } break;
        case EXPR_QUOTE: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->quoted = copyQuoted(state, expr->quoted);
        } break;
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
                    *this->expandedAMacro = true;
                    ret = evalMacro(state, expr);
                    return ret;
                }
                if (expr->callee->symbolID == quote) {
                    ret = (Expr *)malloc(sizeof(Expr));
                    ret->exprType = EXPR_QUOTE;
                    ret->line = expr->line;
                    pushError(state, (expr->arguments &&
                                      !expr->arguments->next),
                              "quote-has-arity-1");
                    ret->quoted =
                        createQuotedElement(state, expr->arguments->val);
                    return ret;
                }
                if (isQuasiquote(expr)) {
                    pushError(state, (expr->arguments &&
                                      !expr->arguments->next),
                              "quasiquote-has-arity-1");
                    if (expr->arguments->val->exprType == EXPR_CALL) {
                        if (isUnquote(expr->arguments->val)) {
                            ret =
                                this->transform(state,
                                                expr->arguments->val->arguments->val);
                            return ret;
                        }
                        ret = (Expr *)malloc(sizeof(Expr));
                        ret->line = expr->line;
                        ret->dotted = expr->arguments->val->dotted;
                        ret->exprType = EXPR_QUASIQUOTE;
                        int depth = 1;
                        if (isQuasiquote(expr->arguments->val)) {
                            depth++;
                        }
                        ret->quasiquoteList =
                            createQuasiquotedList(state,
                                                  expr->arguments->val,
                                                  this, depth);
                    } else {
                        ret = (Expr *)malloc(sizeof(Expr));
                        ret->line = expr->line;
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
                ret->callee = this->transform(state, expr->callee);
                ret->arguments = 0;
                if (expr->arguments) {
                    ret->arguments =
                        (ExprList *)malloc(sizeof(ExprList));
                    ret->arguments->val =
                        this->transform(state, expr->arguments->val);
                    ExprList *prev = ret->arguments;
                    for (ExprList *param = expr->arguments->next;
                         param; param = param->next) {
                        prev->next =
                            (ExprList *)malloc(sizeof(ExprList));
                        prev = prev->next;
                        prev->val = this->transform(state, param->val);
                    }
                    prev->next = 0;
                }
            }
        } break;
        default:assert(false);
    }
    return ret;
}

Expr *MacroExpansionPass::startTransforming(LispisState *state,
                                            Expr *expr) {
    bool expandedMacro = false;
    Expr *prev = expr;
    Expr *next = 0;
    int numExpansions = 0;
    do {
        expandedMacro = false;
        this->expandedAMacro = &expandedMacro;
        next = this->transform(state, prev);
        if (numExpansions) {
            dealloc(prev);
        }
        prev = next;
        next = 0;
        numExpansions++;
    } while(expandedMacro);
    return prev;
}

Expr *copyQuasiquoted(LispisState *state, Expr *expr, CompilerPass *pass) {
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
                if (expr->quasiquoteList->unquoted ||
                    expr->quasiquoteList->unquoteSpliced) {
                    *ret->quasiquoteList = *expr->quasiquoteList;
                    ret->quasiquoteList->val =
                        pass->transform(state, expr->quasiquoteList->val);
                } else {
                    ret->quasiquoteList->val =
                        copyQuasiquoted(state, expr->quasiquoteList->val,
                                        pass);
                }
                ret->quasiquoteList->next = 0;
                QuasiquoteList *prev = ret->quasiquoteList;
                for (QuasiquoteList *elem = expr->quasiquoteList->next;
                     elem; elem = elem->next) {
                    prev->next =
                        (QuasiquoteList *)malloc(sizeof(QuasiquoteList));
                    prev = prev->next;
                    *prev = *elem;
                    if (elem->unquoted || elem->unquoteSpliced) {
                        prev->val = pass->transform(state, elem->val);
                    } else {
                        prev->val = copyQuasiquoted(state, elem->val,
                                                    pass);
                    }
                    prev->next = 0;
                }
            }
        } break;
        default:assert(false);
    }
    return ret;
}

Expr *DefmacroPass::transform(LispisState *state, Expr *expr) {
    Expr *ret = 0;
    switch (expr->exprType) {
        case EXPR_OBJECT: {
            ret = copyObject(state, expr, this);
        } break;
        case EXPR_VECTOR: {
            ret = copyVector(state, expr, this);
        } break;
        case EXPR_QUASIQUOTE: {
            ret = copyQuasiquoted(state, expr, this);
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
                    ret->macro.numLocals = 0;
                    ret->macro.numUpvals = 0;
                    ret->exprType = EXPR_MACRO;
                    ret->line = expr->line;
                    ret->macro.params = 0;

                    pushError(state, expr->arguments,
                              "cant-define-a-macro-without-a-name");
                    pushError(state, expr->arguments->next,
                              "cant-define-a-macro-without-a-"
                              "parameter-list");
                    Expr *name = expr->arguments->val;
                    pushError(state, name->exprType == EXPR_SYMBOL_ID,
                              "macro-name-has-to-be-a-symbol");
                    ret->macro.name = name->symbolID;
                    Expr *params = expr->arguments->next->val;
                    //assert(params->exprType = EXPR_SYMBOL_ID);
                    pushError(state, expr->arguments->next->next,
                              "macro-needs-a-body");
                    ExprList *body = expr->arguments->next->next;
                    ret->macro.varargs = params->dotted;
                    if (params->exprType == EXPR_CALL) {
                        if (params->callee) {
                            pushError(state, (params->callee->exprType ==
                                              EXPR_SYMBOL_ID),
                                      "macro-parameters-has-"
                                      "to-be-symbol");
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
                                pushError(state,
                                          (param->val->exprType ==
                                           EXPR_SYMBOL_ID),
                                          "macro-parameters-has-"
                                          "to-be-symbol");
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
                        pushError(state,
                                  (params->exprType ==
                                   EXPR_SYMBOL_ID),
                                  "macro-parameters-has-"
                                  "to-be-symbol");
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
                    ret->macro.body =
                        (ExprList *)malloc(sizeof(ExprList));
                    ret->macro.body->val =
                        this->transform(state, body->val);
                    ret->macro.body->next = 0;
                    ExprList *prev = ret->macro.body;
                    for (ExprList *param =
                             body->next;
                         param; param = param->next) {
                        prev->next = (ExprList *)malloc(sizeof(ExprList));
                        prev = prev->next;
                        prev->val = this->transform(state, param->val);
                        prev->next = 0;
                    }
                    return ret;
                }
            }
            if (expr->callee) {
                ret->callee = this->transform(state, expr->callee);
                ret->arguments = 0;
                if (expr->arguments) {
                    ret->arguments = (ExprList *)malloc(sizeof(ExprList));
                    ret->arguments->val =
                        this->transform(state, expr->arguments->val);
                    ExprList *prev = ret->arguments;
                    for (ExprList *param = expr->arguments->next;
                         param; param = param->next) {
                        prev->next = (ExprList *)malloc(sizeof(ExprList));
                        prev = prev->next;
                        prev->val = this->transform(state, param->val);
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
                        CompilerPass *pass) {
    assert(body);
    ExprList *ret = (ExprList *)malloc(sizeof(ExprList));
    ret->val = pass->transform(state, body->val);
    ret->next = 0;
    ExprList *prev = ret;
    for (ExprList *expr = body->next; expr; expr = expr->next) {
        prev->next = (ExprList *)malloc(sizeof(ExprList));
        prev = prev->next;
        prev->next = 0;
        prev->val = pass->transform(state, expr->val);
    }
    return ret;
}

Expr *LambdaPass::transform(LispisState *state, Expr *expr) {
    Expr *ret = 0;
    switch (expr->exprType) {
        case EXPR_OBJECT: {
            ret = copyObject(state, expr, this);
        } break;
        case EXPR_VECTOR: {
            ret = copyVector(state, expr, this);
        } break;
        case EXPR_QUASIQUOTE: {
            ret = copyQuasiquoted(state, expr, this);
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
                copyMacroBody(state, expr->macro.body, this);
        } break;
        case EXPR_CALL: {
            ret = (Expr *)calloc(1, sizeof(Expr));
            *ret = *expr;
            if (expr->callee && expr->callee->exprType == EXPR_SYMBOL_ID) {
                if (expr->callee->symbolID == lambda) {
                    ret->lambda.numLocals = 0;
                    ret->lambda.numUpvals = 0;
                    ret->exprType = EXPR_LAMBDA;
                    ret->line = expr->line;
                    ret->lambda.params = 0;
                    pushError(state, (expr->arguments &&
                                      expr->arguments->val),
                              "lambda-needs-a-parameter-list");
                    //assert(expr->arguments->val->exprType == EXPR_CALL);
                    if (expr->arguments->val->exprType == EXPR_CALL) {
                        ret->lambda.varargs =
                            expr->arguments->val->dotted;
                        if (expr->arguments->val->callee) {
                            pushError(state,
                                      expr->arguments->val->callee->exprType ==
                                      EXPR_SYMBOL_ID,
                                      "lambda-parameter-needs-"
                                      "to-be-symbol");
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
                                pushError(state, (param->val->exprType =
                                                  EXPR_SYMBOL_ID),
                                          "lambda-parameter-needs-"
                                          "to-be-symbol");
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
                        pushError(state, (expr->arguments->val->exprType ==
                                          EXPR_SYMBOL_ID),
                                  "lambda-parameter-needs-"
                                  "to-be-symbol");
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
                    pushError(state, expr->arguments->next,
                              "missing-lambda-body");
                    ret->lambda.body =
                        (ExprList *)malloc(sizeof(ExprList));
                    ret->lambda.body->val =
                        this->transform(state,
                                        expr->arguments->next->val);
                    ret->lambda.body->next = 0;
                    ExprList *prev = ret->lambda.body;
                    for (ExprList *param =
                             expr->arguments->next->next;
                         param; param = param->next) {
                        prev->next = (ExprList *)malloc(sizeof(ExprList));
                        prev = prev->next;
                        prev->val = this->transform(state, param->val);
                        prev->next = 0;
                    }
                    return ret;
                }
            }
            if (expr->callee) {
                ret->callee = this->transform(state, expr->callee);
                ret->arguments = 0;
                if (expr->arguments) {
                    ret->arguments = (ExprList *)malloc(sizeof(ExprList));
                    ret->arguments->val =
                        this->transform(state, expr->arguments->val);
                    ExprList *prev = ret->arguments;
                    for (ExprList *param = expr->arguments->next;
                         param; param = param->next) {
                        prev->next = (ExprList *)malloc(sizeof(ExprList));
                        prev = prev->next;
                        prev->val = this->transform(state, param->val);
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
                         CompilerPass *pass) {
    assert(body);
    ExprList *ret = (ExprList *)malloc(sizeof(ExprList));
    ret->val = pass->transform(state, body->val);
    ret->next = 0;
    ExprList *prev = ret;
    for (ExprList *expr = body->next; expr; expr = expr->next) {
        prev->next = (ExprList *)malloc(sizeof(ExprList));
        prev = prev->next;
        prev->next = 0;
        prev->val = pass->transform(state, expr->val);
    }
    return ret;
}

Expr *LetPass::transform(LispisState *state, Expr *expr) {
    Expr *ret = 0;
    switch (expr->exprType) {
        case EXPR_OBJECT: {
            ret = copyObject(state, expr, this);
        } break;
        case EXPR_VECTOR: {
            ret = copyVector(state, expr, this);
        } break;
        case EXPR_QUASIQUOTE: {
            ret = copyQuasiquoted(state, expr, this);
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
                                            this);
        } break;
        case EXPR_LAMBDA: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->lambda.params = copyLambdaParams(state,
                                                  expr->lambda.params);
            ret->lambda.body = copyLambdaBody(state, expr->lambda.body,
                                              this);
        } break;
        case EXPR_CALL: {
            ret = (Expr *)malloc(sizeof(Expr));
            if (expr->callee && expr->callee->exprType == EXPR_SYMBOL_ID) {
                if (expr->callee->symbolID == let) {
                    ret->exprType = EXPR_LET;
                    ret->line = expr->line;
                    pushError(state, (expr->arguments &&
                                      expr->arguments->val),
                              "let!-missing-variable");
                    pushError(state, (expr->arguments->val->exprType ==
                                      EXPR_SYMBOL_ID),
                              "let!-variable-not-symbol");
                    pushError(state, expr->arguments->next,
                              "let!-missing-value");
                    pushError(state, !expr->arguments->next->next,
                              "let!-has-to-be-on-form-(let! a 1)");
                    ret->variable = (Expr *)malloc(sizeof(Expr));
                    *ret->variable = *expr->arguments->val;
                    ret->value =
                        this->transform(state,
                                        expr->arguments->next->val);
                    return ret;
                }
            }
            *ret = *expr;
            if (expr->callee) {
                ret->callee = this->transform(state, expr->callee);
                ret->arguments = 0;
                if (expr->arguments) {
                    ret->arguments = (ExprList *)malloc(sizeof(ExprList));
                    ret->arguments->val =
                        this->transform(state, expr->arguments->val);
                    ExprList *prev = ret->arguments;
                    for (ExprList *param = expr->arguments->next;
                         param; param = param->next) {
                        prev->next = (ExprList *)malloc(sizeof(ExprList));
                        prev = prev->next;
                        prev->val = this->transform(state, param->val);
                    }
                    prev->next = 0;
                }
            }
        } break;
        default:assert(false);
    }
    return ret;
}

Expr *SetPass::transform(LispisState *state, Expr *expr) {
    Expr *ret = 0;
    switch (expr->exprType) {
        case EXPR_OBJECT: {
            ret = copyObject(state, expr, this);
        } break;
        case EXPR_VECTOR: {
            ret = copyVector(state, expr, this);
        } break;
        case EXPR_QUASIQUOTE: {
            ret = copyQuasiquoted(state, expr, this);
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
                                            this);
        } break;
        case EXPR_LAMBDA: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->lambda.params = copyLambdaParams(state, expr->lambda.params);
            ret->lambda.body = copyLambdaBody(state, expr->lambda.body,
                                              this);
        } break;
        case EXPR_LET: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->variable = (Expr *)malloc(sizeof(Expr));
            *ret->variable = *expr->variable;
            ret->value = this->transform(state, expr->value);
        } break;
        case EXPR_CALL: {
            ret = (Expr *)malloc(sizeof(Expr));
            if (expr->callee && expr->callee->exprType == EXPR_SYMBOL_ID) {
                if (expr->callee->symbolID == set) {
                    ret->exprType = EXPR_SET;
                    ret->line = expr->line;
                    pushError(state, (expr->arguments &&
                                      expr->arguments->val),
                              "set!-missing-variable");
                    pushError(state, (expr->arguments->val->exprType ==
                                      EXPR_SYMBOL_ID),
                              "set!-variable-not-symbol");
                    pushError(state, expr->arguments->next,
                              "set!-missing-value");
                    pushError(state, !expr->arguments->next->next,
                              "set! has to be on the form "
                              "(set! var val)");
                    ret->variable = (Expr *)malloc(sizeof(Expr));
                    *ret->variable = *expr->arguments->val;
                    ret->value =
                        this->transform(state,
                                        expr->arguments->next->val);
                    return ret;
                }
            }
            *ret = *expr;
            if (expr->callee) {
                ret->callee = this->transform(state, expr->callee);
                ret->arguments = 0;
                if (expr->arguments) {
                    ret->arguments = (ExprList *)malloc(sizeof(ExprList));
                    ret->arguments->val =
                        this->transform(state, expr->arguments->val);
                    ExprList *prev = ret->arguments;
                    for (ExprList *param = expr->arguments->next;
                         param; param = param->next) {
                        prev->next = (ExprList *)malloc(sizeof(ExprList));
                        prev = prev->next;
                        prev->val = this->transform(state, param->val);
                    }
                    prev->next = 0;
                }
            }
        } break;
        default:assert(false);
    }
    return ret;
}

Expr *DefinePass::transform(LispisState *state, Expr *expr) {
    Expr *ret = 0;
    switch (expr->exprType) {
        case EXPR_OBJECT: {
            ret = copyObject(state, expr, this);
        } break;
        case EXPR_VECTOR: {
            ret = copyVector(state, expr, this);
        } break;
        case EXPR_QUASIQUOTE: {
            ret = copyQuasiquoted(state, expr, this);
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
                                            this);
        } break;
        case EXPR_LAMBDA: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->lambda.params = copyLambdaParams(state, expr->lambda.params);
            ret->lambda.body = copyLambdaBody(state, expr->lambda.body,
                                              this);
        } break;
        case EXPR_SET:
        case EXPR_LET: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->variable = (Expr *)malloc(sizeof(Expr));
            *ret->variable = *expr->variable;
            ret->value = this->transform(state, expr->value);
        } break;
        case EXPR_CALL: {
            ret = (Expr *)malloc(sizeof(Expr));
            if (expr->callee && expr->callee->exprType == EXPR_SYMBOL_ID) {
                if (expr->callee->symbolID == define) {
                    ret->exprType = EXPR_DEFINE;
                    ret->line = expr->line;
                    pushError(state, expr->arguments,
                              "define!-missing-variable");
                    pushError(state, (expr->arguments->val->exprType ==
                                      EXPR_SYMBOL_ID),
                              "define!-variable-not-symbol");
                    pushError(state, expr->arguments->next,
                              "define!-missing-value");
                    pushError(state, !expr->arguments->next->next,
                              "define! not on the form "
                              "(define! var val)");
                    ret->variable = (Expr *)malloc(sizeof(Expr));
                    *ret->variable = *expr->arguments->val;
                    ret->value =
                        this->transform(state,
                                        expr->arguments->next->val);
                    return ret;
                }
            }
            *ret = *expr;
            if (expr->callee) {
                ret->callee = this->transform(state, expr->callee);
                ret->arguments = 0;
                if (expr->arguments) {
                    ret->arguments = (ExprList *)malloc(sizeof(ExprList));
                    ret->arguments->val =
                        this->transform(state, expr->arguments->val);
                    ExprList *prev = ret->arguments;
                    for (ExprList *param = expr->arguments->next;
                         param; param = param->next) {
                        prev->next = (ExprList *)malloc(sizeof(ExprList));
                        prev = prev->next;
                        prev->val = this->transform(state, param->val);
                    }
                    prev->next = 0;
                }
            }
        } break;
        default:assert(false);
    }
    return ret;
}

Expr *IfPass::transform(LispisState *state, Expr *expr) {
    Expr *ret = 0;
    switch (expr->exprType) {
        case EXPR_OBJECT: {
            ret = copyObject(state, expr, this);
        } break;
        case EXPR_VECTOR: {
            ret = copyVector(state, expr, this);
        } break;
        case EXPR_QUASIQUOTE: {
            ret = copyQuasiquoted(state, expr, this);
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
                                            this);
        } break;
        case EXPR_LAMBDA: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->lambda.params = copyLambdaParams(state,
                                                  expr->lambda.params);
            ret->lambda.body = copyLambdaBody(state, expr->lambda.body,
                                              this);
        } break;
        case EXPR_SET:
        case EXPR_DEFINE:
        case EXPR_LET: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->variable = (Expr *)malloc(sizeof(Expr));
            *ret->variable = *expr->variable;
            ret->value = this->transform(state, expr->value);
        } break;
        case EXPR_CALL: {
            ret = (Expr *)malloc(sizeof(Expr));
            if (expr->callee && expr->callee->exprType == EXPR_SYMBOL_ID) {
                if (expr->callee->symbolID == ifSym) {
                    ret->exprType = EXPR_IF;
                    ret->line = expr->line;
                    pushError(state, expr->arguments,
                              "if-missing-predicate");
                    ret->predicate =
                        this->transform(state, expr->arguments->val);
                    pushError(state, expr->arguments->next,
                              "if-missing-true-branch");
                    ret->trueBranch =
                        this->transform(state,
                                        expr->arguments->next->val);
                    if (expr->arguments->next->next) { // falseBranch
                        ret->falseBranch =
                            this->transform(state,
                                            expr->arguments->next->next->val);
                    }
                    return ret;
                }
            }
            *ret = *expr;
            if (expr->callee) {
                ret->callee = this->transform(state, expr->callee);
                ret->arguments = 0;
                if (expr->arguments) {
                    ret->arguments = (ExprList *)malloc(sizeof(ExprList));
                    ret->arguments->val =
                        this->transform(state, expr->arguments->val);
                    ExprList *prev = ret->arguments;
                    for (ExprList *param = expr->arguments->next;
                         param; param = param->next) {
                        prev->next = (ExprList *)malloc(sizeof(ExprList));
                        prev = prev->next;
                        prev->val = this->transform(state, param->val);
                    }
                    prev->next = 0;
                }
            }
        } break;
        default:assert(false);
    }
    return ret;
}

ExprList *createForBody(LispisState *state, ExprList *lst,
                        CompilerPass *pass) {
    ExprList *ret = (ExprList *)calloc(1, sizeof(ExprList));
    ret->val = pass->transform(state, lst->val);
    ret->next = 0;
    ExprList *prev = ret;
    for (ExprList *e = lst->next; e; e = e->next) {
        prev->next = (ExprList *)calloc(1, sizeof(ExprList));
        prev = prev->next;
        prev->val = pass->transform(state, e->val);
        prev->next = 0;
    }
    return ret;
}

Expr *ForPass::transform(LispisState *state, Expr *expr) {
    Expr *ret = 0;
    switch (expr->exprType) {
        case EXPR_OBJECT: {
            ret = copyObject(state, expr, this);
        } break;
        case EXPR_VECTOR: {
            ret = copyVector(state, expr, this);
        } break;
        case EXPR_QUASIQUOTE: {
            ret = copyQuasiquoted(state, expr, this);
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
                                            this);
        } break;
        case EXPR_LAMBDA: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->lambda.params = copyLambdaParams(state,
                                                  expr->lambda.params);
            ret->lambda.body = copyLambdaBody(state, expr->lambda.body,
                                              this);
        } break;
        case EXPR_SET:
        case EXPR_DEFINE:
        case EXPR_LET: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->variable = (Expr *)malloc(sizeof(Expr));
            *ret->variable = *expr->variable;
            ret->value = this->transform(state, expr->value);
        } break;
        case EXPR_CALL: {
            ret = (Expr *)malloc(sizeof(Expr));
            if (expr->callee && expr->callee->exprType == EXPR_SYMBOL_ID) {
                if (expr->callee->symbolID == forSym) {
                    ret->exprType = EXPR_FOR;
                    ret->line = expr->line;
                    pushError(state, expr->arguments,
                              "for-missing-header");
                    Expr *header = expr->arguments->val;
                    pushError(state, header->exprType == EXPR_CALL,
                              "for not on form "
                              "(for (init pred upd) body)");
                    // we start with just full header
                    pushError(state, header->callee,
                              "for-missing-init");
                    if (header->callee->exprType == EXPR_CALL) {
                        pushError(state, header->callee->callee,
                                  "for-missing-init?");
                        pushError(state,
                                  header->callee->callee->exprType ==
                                  EXPR_SYMBOL_ID,
                                  "for-explicit-variable-not-a-symbol");
                        ret->it = this->transform(state,
                                                  header->callee->callee);
                        pushError(state, header->callee->arguments,
                                  "for-explicit-variable-missing-value");
                        pushError(state, !header->callee->arguments->next,
                                  "for-explicit-variable-form-incorrect");
                        ret->init =
                            this->transform(state,
                                            header->callee->arguments->val);
                    } else {
                        ret->init = this->transform(state,
                                                    header->callee);
                        ret->it = (Expr *)malloc(sizeof(Expr));
                        *ret->it = *expr;
                        ret->it->exprType = EXPR_SYMBOL_ID;
                        ret->it->symbolID = it;
                    }
                    pushError(state, header->arguments,
                              "for-missing-predicate");
                    ret->pred = this->transform(state,
                                                header->arguments->val);
                    pushError(state, header->arguments->next,
                              "for-missing-update-expression");
                    ret->upd =
                        this->transform(state,
                                        header->arguments->next->val);
                    pushError(state, expr->arguments->next,
                              "for-missing-body");
                    assert(expr->arguments->next->val);
                    ret->body = createForBody(state,
                                              expr->arguments->next,
                                              this);
                    return ret;
                }
            }
            *ret = *expr;
            if (expr->callee) {
                ret->callee = this->transform(state, expr->callee);
                ret->arguments = 0;
                if (expr->arguments) {
                    ret->arguments = (ExprList *)malloc(sizeof(ExprList));
                    ret->arguments->val =
                        this->transform(state, expr->arguments->val);
                    ExprList *prev = ret->arguments;
                    for (ExprList *param = expr->arguments->next;
                         param; param = param->next) {
                        prev->next = (ExprList *)malloc(sizeof(ExprList));
                        prev = prev->next;
                        prev->val = this->transform(state, param->val);
                    }
                    prev->next = 0;
                }
            }
        } break;
        case EXPR_IF: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->predicate = this->transform(state,
                                             expr->predicate);
            ret->trueBranch = this->transform(state,
                                              expr->trueBranch);
            if (expr->falseBranch) {
                ret->falseBranch = this->transform(state,
                                                   expr->falseBranch);
            }
        } break;
        default:assert(false);
    }
    return ret;
}

ExprList *copyForBody(LispisState *state, ExprList *body,
                      CompilerPass *pass) {
    assert(body);
    ExprList *ret = (ExprList *)malloc(sizeof(ExprList));
    ret->val = pass->transform(state, body->val);
    ret->next = 0;
    ExprList *prev = ret;
    for (ExprList *expr = body->next; expr; expr = expr->next) {
        prev->next = (ExprList *)malloc(sizeof(ExprList));
        prev = prev->next;
        prev->next = 0;
        prev->val = pass->transform(state, expr->val);
    }
    return ret;
}

ExprList *copyDoBody(LispisState *state, ExprList *body,
                      CompilerPass *pass) {
    assert(body);
    ExprList *ret = (ExprList *)malloc(sizeof(ExprList));
    ret->val = pass->transform(state, body->val);
    ret->next = 0;
    ExprList *prev = ret;
    for (ExprList *expr = body->next; expr; expr = expr->next) {
        prev->next = (ExprList *)malloc(sizeof(ExprList));
        prev = prev->next;
        prev->next = 0;
        prev->val = pass->transform(state, expr->val);
    }
    return ret;
}

Expr *DoPass::transform(LispisState *state, Expr *expr) {
    Expr *ret = 0;
    switch (expr->exprType) {
        case EXPR_OBJECT: {
            ret = copyObject(state, expr, this);
        } break;
        case EXPR_FOR: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->it = this->transform(state, expr->it);
            ret->init = this->transform(state, expr->init);
            ret->pred = this->transform(state, expr->pred);
            ret->upd = this->transform(state, expr->upd);
            ret->body = copyForBody(state, expr->body, this);
        } break;
        case EXPR_VECTOR: {
            ret = copyVector(state, expr, this);
        } break;
        case EXPR_QUASIQUOTE: {
            ret = copyQuasiquoted(state, expr, this);
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
                                            this);
        } break;
        case EXPR_LAMBDA: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->lambda.params = copyLambdaParams(state,
                                                  expr->lambda.params);
            ret->lambda.body = copyLambdaBody(state, expr->lambda.body,
                                              this);
        } break;
        case EXPR_SET:
        case EXPR_DEFINE:
        case EXPR_LET: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->variable = (Expr *)malloc(sizeof(Expr));
            *ret->variable = *expr->variable;
            ret->value = this->transform(state, expr->value);
        } break;
        case EXPR_CALL: {
            ret = (Expr *)malloc(sizeof(Expr));
            if (expr->callee && expr->callee->exprType == EXPR_SYMBOL_ID) {
                if (expr->callee->symbolID == doSym) {
                    *ret = *expr;
                    ret->exprType = EXPR_DO;
                    ret->list = copyDoBody(state, expr->arguments, this);
                    return ret;
                }
            }
            *ret = *expr;
            if (expr->callee) {
                ret->callee = this->transform(state, expr->callee);
                ret->arguments = 0;
                if (expr->arguments) {
                    ret->arguments = (ExprList *)malloc(sizeof(ExprList));
                    ret->arguments->val =
                        this->transform(state, expr->arguments->val);
                    ExprList *prev = ret->arguments;
                    for (ExprList *param = expr->arguments->next;
                         param; param = param->next) {
                        prev->next = (ExprList *)malloc(sizeof(ExprList));
                        prev = prev->next;
                        prev->val = this->transform(state, param->val);
                    }
                    prev->next = 0;
                }
            }
        } break;
        case EXPR_IF: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->predicate = this->transform(state,
                                             expr->predicate);
            ret->trueBranch = this->transform(state,
                                              expr->trueBranch);
            if (expr->falseBranch) {
                ret->falseBranch = this->transform(state,
                                                   expr->falseBranch);
            }
        } break;
        default:assert(false);
    }
    return ret;
}

Expr *RefPass::transform(LispisState *state, Expr *expr) {
    Expr *ret = 0;
    switch (expr->exprType) {
        case EXPR_OBJECT: {
            ret = copyObject(state, expr, this);
        } break;
        case EXPR_FOR: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->it = this->transform(state, expr->it);
            ret->init = this->transform(state, expr->init);
            ret->pred = this->transform(state, expr->pred);
            ret->upd = this->transform(state, expr->upd);
            ret->body = copyForBody(state, expr->body, this);
        } break;
        case EXPR_DO: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->list = copyDoBody(state, expr->list, this);
        } break;
        case EXPR_VECTOR: {
            ret = copyVector(state, expr, this);
        } break;
        case EXPR_QUASIQUOTE: {
            ret = copyQuasiquoted(state, expr, this);
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
                                            this);
        } break;
        case EXPR_LAMBDA: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->lambda.params = copyLambdaParams(state,
                                                  expr->lambda.params);
            ret->lambda.body = copyLambdaBody(state, expr->lambda.body,
                                              this);
        } break;
        case EXPR_SET:
        case EXPR_DEFINE:
        case EXPR_LET: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->variable = (Expr *)malloc(sizeof(Expr));
            *ret->variable = *expr->variable;
            ret->value = this->transform(state, expr->value);
        } break;
        case EXPR_CALL: {
            ret = (Expr *)malloc(sizeof(Expr));
            if (expr->callee && expr->callee->exprType == EXPR_SYMBOL_ID) {
                if (expr->callee->symbolID == ref) {
                    *ret = *expr;
                    ret->exprType = EXPR_REF;
                    pushError(state, (expr->arguments &&
                                      expr->arguments->next &&
                                      !expr->arguments->next->next),
                              ":-has-arity-2");
                    ret->ref.obj = this->transform(state,
                                               expr->arguments->val);
                    ret->ref.ref =
                        this->transform(state,
                                        expr->arguments->next->val);
                    return ret;
                }
            }
            if (expr->callee && expr->callee->exprType == EXPR_SYMBOL_ID) {
                if (expr->callee->symbolID == refSet) {
                    *ret = *expr;
                    ret->exprType = EXPR_REF_SET;
                    pushError(state, (expr->arguments &&
                                      expr->arguments->next &&
                                      expr->arguments->next->next &&
                                      !expr->arguments->next->next->next),
                              ":!-has-arity-3");
                    ret->refSet.obj = this->transform(state,
                                               expr->arguments->val);
                    ret->refSet.ref =
                        this->transform(state,
                                        expr->arguments->next->val);
                    ret->refSet.val =
                        this->transform(state,
                                        expr->arguments->next->next->val);
                    return ret;
                }
            }
            *ret = *expr;
            if (expr->callee) {
                ret->callee = this->transform(state, expr->callee);
                ret->arguments = 0;
                if (expr->arguments) {
                    ret->arguments = (ExprList *)malloc(sizeof(ExprList));
                    ret->arguments->val =
                        this->transform(state, expr->arguments->val);
                    ExprList *prev = ret->arguments;
                    for (ExprList *param = expr->arguments->next;
                         param; param = param->next) {
                        prev->next = (ExprList *)malloc(sizeof(ExprList));
                        prev = prev->next;
                        prev->val = this->transform(state, param->val);
                    }
                    prev->next = 0;
                }
            }
        } break;
        case EXPR_IF: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->predicate = this->transform(state,
                                             expr->predicate);
            ret->trueBranch = this->transform(state,
                                              expr->trueBranch);
            if (expr->falseBranch) {
                ret->falseBranch = this->transform(state,
                                                   expr->falseBranch);
            }
        } break;
        default:assert(false);
    }
    return ret;
}

void addVariable(LexicalScope *scope, uint32 symbolID) {
    uint32 variablesBottom = 0;
    if (scope->type == QUASI_SCOPE) {
        variablesBottom = scope->quasi.variableIDsBottom;
    }
    while (scope->type == QUASI_SCOPE) {
        scope = scope->parentScope;
    }
    for (uint32 i = variablesBottom;
         i < scope->real.variableIDsTop; ++i) {
        if (scope->real.variableIDs[i] == symbolID) {
            return;
        }
    }
    scope->real.variableIDs[scope->real.variableIDsTop] = symbolID;
    scope->real.variableIDsTop++;
    scope->real.totalVariablesSize++;
}

void closeQuasiScope(LexicalScope *scope) {
    assert(scope->type == QUASI_SCOPE);
    if (!scope->closedOver) {
        LexicalScope *realScope = scope->parentScope;
        while(realScope->type == QUASI_SCOPE) {
            realScope = realScope->parentScope;
        }
        realScope->real.variableIDsTop = scope->quasi.variableIDsBottom;
    }
}

LexicalVariable getVariable(LexicalScope *origScope, uint32 symbolID) {
    LexicalVariable ret = {};
    ret.symbolID = symbolID;
    ret.kind = VAR_GLOBAL;
    LexicalScope *scope = origScope;
    while (scope->type == QUASI_SCOPE) {
        scope = scope->parentScope;
    }
    if (origScope->type == QUASI_SCOPE) {
        for (uint32 i = origScope->quasi.variableIDsBottom;
             i < scope->real.variableIDsTop; ++i) {
            if (scope->real.variableIDs[i] == ret.symbolID) {
                ret.kind = VAR_LOCAL;
                ret.variableID = i;
                return ret;
            }
        }
    }
    for (uint32 i = 0;
         i < scope->real.variableIDsTop; ++i) {
        if (scope->real.variableIDs[i] == ret.symbolID) {
            ret.kind = VAR_LOCAL;
            ret.variableID = i;
            return ret;
        }
    }
    uint32 depth = 0;
    for (LexicalScope *searchedScope = scope->parentScope;
         searchedScope;
         searchedScope = searchedScope->parentScope) {
        depth++;
        while (searchedScope->type == QUASI_SCOPE) {
            searchedScope = searchedScope->parentScope;
        }
        for (uint32 i = 0;
             i < searchedScope->real.variableIDsTop;
             ++i) {
            if (searchedScope->real.variableIDs[i] ==
                ret.symbolID) {
                ret.kind = VAR_UPVAL;
                ret.variableID = scope->real.upvalsTop;
                ret.depth = depth;
                ret.index = i;
                scope->real.upvalsTop++;
                LexicalScope *s = origScope->parentScope;
                for (uint32 i = 0; i < depth; ++i) {
                    while (s->type == QUASI_SCOPE) {
                        s->closedOver=true;
                        s = s->parentScope;
                    }
                    s->closedOver=true;
                    s = s->parentScope;
                }
                return ret;
            }
        }
    }
    return ret;
}

LexicalScope makeQuasiScope(LexicalScope *parent) {
    LexicalScope ret = {};
    ret.type = QUASI_SCOPE;
    ret.parentScope = parent;
    if (parent) {
        while (parent->type == QUASI_SCOPE) {
            parent = parent->parentScope;
        }
        ret.quasi.variableIDsBottom = parent->real.variableIDsTop;
    }
    return ret;
}

LexicalScope makeRealScope(LexicalScope *parent) {
    LexicalScope ret = {};
    ret.type = REAL_SCOPE;
    ret.parentScope = parent;
    return ret;
}

Expr *VariablePass::transform(LispisState *state, Expr *expr) {
    Expr *ret = 0;
    switch (expr->exprType) {
        case EXPR_OBJECT: {
            ret = copyObject(state, expr, this);
        } break;
        case EXPR_VECTOR: {
            ret = copyVector(state, expr, this);
        } break;
        case EXPR_REF: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->ref.obj = this->transform(state, expr->ref.obj);
            ret->ref.ref = this->transform(state, expr->ref.ref);
        } break;
        case EXPR_REF_SET: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->refSet.obj = this->transform(state, expr->refSet.obj);
            ret->refSet.ref = this->transform(state, expr->refSet.ref);
            ret->refSet.val = this->transform(state, expr->refSet.val);
        } break;
        case EXPR_DO: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            LexicalScope doScope = makeQuasiScope(this->currentScope);
            this->currentScope = &doScope;
            ret->list = copyDoBody(state, expr->list, this);
            this->currentScope = this->currentScope->parentScope;
            closeQuasiScope(&doScope);
        } break;
        case EXPR_FOR: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            LexicalScope forScope = makeQuasiScope(this->currentScope);
            this->currentScope = &forScope;
            addVariable(&forScope, ret->it->symbolID);
            ret->it = this->transform(state, expr->it);
            ret->init = this->transform(state, expr->init);
            ret->pred = this->transform(state, expr->pred);
            ret->upd = this->transform(state, expr->upd);
            ret->body = copyForBody(state, expr->body, this);
            this->currentScope = this->currentScope->parentScope;
            closeQuasiScope(&forScope);
        } break;
        case EXPR_QUASIQUOTE: {
            ret = copyQuasiquoted(state, expr, this);
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
            ret->var = getVariable(this->currentScope, expr->symbolID);
        } break;
        case EXPR_QUOTE: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->quoted = copyQuoted(state, expr->quoted);
        } break;
        case EXPR_MACRO: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            //ret->macro.params = copyMacroParams(state,
            //expr->macro.params);
            ret->macro.params = 0;
            LexicalScope macroScope = makeRealScope(this->currentScope);
            this->currentScope = &macroScope;
            if (expr->macro.params) {
                ExprList *retParams =
                    (ExprList *)malloc(sizeof(ExprList));
                ExprList *prev = retParams;
                addVariable(&macroScope,
                            expr->macro.params->val->symbolID);
                prev->val = this->transform(state,
                                            expr->macro.params->val);
                prev->next = 0;
                for (ExprList *params = expr->macro.params->next;
                     params;
                     params = params->next) {
                    addVariable(&macroScope, params->val->symbolID);
                    prev->next = (ExprList *)malloc(sizeof(ExprList));
                    prev = prev->next;
                    prev->val = this->transform(state,
                                                params->val);
                    prev->next = 0;
                }
                ret->macro.params = retParams;
            } else {
                ret->macro.params = 0;
            }
            ret->macro.body = copyMacroBody(state, expr->macro.body,
                                            this);
            this->currentScope = this->currentScope->parentScope;
            ret->macro.numLocals = macroScope.real.totalVariablesSize;
            ret->macro.numUpvals = 0;
        } break;
        case EXPR_LAMBDA: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            //ret->lambda.params = copyLambdaParams(state,
            //expr->lambda.params);
            ret->lambda.params = 0;
            LexicalScope lambdaScope = makeRealScope(this->currentScope);
            this->currentScope = &lambdaScope;
            if (expr->lambda.params) {
                ExprList *retParams =
                    (ExprList *)malloc(sizeof(ExprList));
                ExprList *prev = retParams;
                addVariable(&lambdaScope,
                            expr->lambda.params->val->symbolID);
                prev->val = this->transform(state,
                                            expr->lambda.params->val);
                assert(prev->val->exprType == EXPR_VARIABLE);
                prev->next = 0;
                for (ExprList *params = expr->lambda.params->next;
                     params;
                     params = params->next) {
                    addVariable(&lambdaScope, params->val->symbolID);
                    prev->next = (ExprList *)malloc(sizeof(ExprList));
                    prev = prev->next;

                    prev->val = this->transform(state,
                                                params->val);
                    prev->next = 0;
                }
                ret->lambda.params = retParams;
            } else {
                ret->lambda.params = 0;
            }
            ret->lambda.body = copyLambdaBody(state, expr->lambda.body,
                                              this);
            this->currentScope = this->currentScope->parentScope;
            ret->lambda.numLocals = lambdaScope.real.totalVariablesSize;
            ret->lambda.numUpvals = lambdaScope.real.upvalsTop;
            ret->lambda.closedOver = lambdaScope.closedOver;
        } break;
        case EXPR_DEFINE: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->variable = this->transform(state, expr->variable);
            //*ret->variable = *expr->variable;
            ret->value = this->transform(state, expr->value);
        } break;
        case EXPR_LET: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            addVariable(this->currentScope, expr->variable->symbolID);
            ret->variable = this->transform(state, expr->variable);
            ret->value = this->transform(state, expr->value);
        } break;
        case EXPR_SET: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->variable = this->transform(state, expr->variable);
            ret->value = this->transform(state, expr->value);
        } break;
        case EXPR_CALL: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            if (expr->callee) {
                ret->callee = this->transform(state, expr->callee);
                ret->arguments = 0;
                if (expr->arguments) {
                    ret->arguments = (ExprList *)malloc(sizeof(ExprList));
                    ret->arguments->val =
                        this->transform(state, expr->arguments->val);
                    ExprList *prev = ret->arguments;
                    for (ExprList *param = expr->arguments->next;
                         param; param = param->next) {
                        prev->next = (ExprList *)malloc(sizeof(ExprList));
                        prev = prev->next;
                        prev->val = this->transform(state, param->val);
                    }
                    prev->next = 0;
                }
            }
        } break;
        case EXPR_IF: {
            ret = (Expr *)malloc(sizeof(Expr));
            *ret = *expr;
            ret->predicate = this->transform(state,
                                             expr->predicate);
            ret->trueBranch = this->transform(state,
                                              expr->trueBranch);
            if (expr->falseBranch) {
                ret->falseBranch = this->transform(state,
                                                   expr->falseBranch);
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
    quasiquoteSym.exprType = EXPR_SYMBOL;
    quasiquoteSym.str = quasiquoteStr;
    unquoteSym.exprType = EXPR_SYMBOL;
    unquoteSym.str = unquoteStr;
    unquoteSpliceSym.exprType = EXPR_SYMBOL;
    unquoteSpliceSym.str = unquoteSpliceStr;

    String lambdaStr;
    lambdaStr.val = (char *)"lambda";
    lambdaStr.length = strlen(lambdaStr.val);
    lambda = internSymbol(state, lambdaStr, hashFunc(lambdaStr));

    String letStr;
    letStr.val = (char *)"let!";
    letStr.length = strlen(letStr.val);
    let = internSymbol(state, letStr, hashFunc(letStr));

    String setStr;
    setStr.val = (char *)"set!";
    setStr.length = strlen(setStr.val);
    set = internSymbol(state, setStr, hashFunc(setStr));

    String defineStr;
    defineStr.val = (char *)"define!";
    defineStr.length = strlen(defineStr.val);
    define = internSymbol(state, defineStr, hashFunc(defineStr));

    String ifStr;
    ifStr.val = (char *)"if";
    ifStr.length = strlen(ifStr.val);
    ifSym = internSymbol(state, ifStr, hashFunc(ifStr));

    String forStr;
    forStr.val = (char *)"for";
    forStr.length = strlen(forStr.val);
    forSym = internSymbol(state, forStr, hashFunc(forStr));

    doSym = internCStr(state, "do");

    ref = internCStr(state, "ref");
    refSet = internCStr(state, "ref-set");

    String itStr;
    itStr.val = (char *)"it";
    itStr.length = strlen(itStr.val);
    it = internSymbol(state, itStr, hashFunc(itStr));

    String defmacroStr;
    defmacroStr.val = (char *)"defmacro!";
    defmacroStr.length = strlen(defmacroStr.val);
    defmacro = internSymbol(state, defmacroStr, hashFunc(defmacroStr));
}