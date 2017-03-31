#include "codegen.h"
#include "parser.h"
#include <cmath>
#include <cstdlib>
#include <cassert>
#include <cstdio>
#include <cstring>

void allocBytecode(LispisState *state, LispisFunction *func) {
    uint64 prevBytecodeSize = func->bytecodeSize;
    if (func->bytecodeTop == func->bytecodeSize) {
        func->bytecodeSize = func->bytecodeSize * 1.5f;
        func->bytecode = (Bytecode *)realloc(func->bytecode,
                                             sizeof(Bytecode) *
                                             func->bytecodeSize);
        func->instructionToLine =
            (int32 *)realloc(func->instructionToLine,
                             sizeof(int32) * func->bytecodeSize);
        memset(func->instructionToLine + prevBytecodeSize,
               -1, func->bytecodeSize-prevBytecodeSize);
    }
}

void pushOp(LispisState *state, LispisFunction *func,
            OpCodes op, int32 line) {
    allocBytecode(state, func);
    func->bytecode[func->bytecodeTop] = nanPack(op, LISPIS_OP);
    func->instructionToLine[func->bytecodeTop] = line;
    func->bytecodeTop++;
}

uint64 pushDummy(LispisState *state, LispisFunction *func) {
    allocBytecode(state, func);
    func->bytecode[func->bytecodeTop] = nanPack(0, LISPIS_UNDEF);
    uint64 ret = func->bytecodeTop;
    func->bytecodeTop++;
    return ret;
}

void pushUndef(LispisState *state, LispisFunction *func) {
    pushDummy(state, func);
}

void pushUint64(LispisState *state, LispisFunction *func, uint64 u) {
    allocBytecode(state, func);
    func->bytecode[func->bytecodeTop].ui64 = u;
    func->bytecodeTop++;
}

void pushInt64(LispisState *state, LispisFunction *func, int64 a) {
    allocBytecode(state, func);
    func->bytecode[func->bytecodeTop].i64 = a;
    func->bytecodeTop++;
}

void pushDouble(LispisState *state, LispisFunction *func, double d) {
    allocBytecode(state, func);
    func->bytecode[func->bytecodeTop].f64 = d;
    func->bytecodeTop++;
}

void pushInt32(LispisState *state, LispisFunction *func, int32 a) {
    pushDouble(state, func, nanPackInt32(a).f64);
}

void set(LispisState *state, LispisFunction *func, Value v, uint64 pos) {
    assert(pos < func->bytecodeTop);
    func->bytecode[pos] = v;
}

void pushSymbol(LispisState *state, LispisFunction *func, uint32 str) {
    uint32 symbolIdx = str;
    pushDouble(state, func, nanPackSymbolIdx(symbolIdx).f64);
}

void pushBoolean(LispisState *state, LispisFunction *func, bool boolean) {
    pushDouble(state, func, nanPackBoolean(boolean).f64);
}

void compileQuotedList(LispisState *state, LispisFunction *func,
                       ExprList *exprList, bool dotted, int32 line);

void compileQuotedExpr(LispisState *state, LispisFunction *func, Expr *expr) {
    switch (expr->exprType) {
        case EXPR_LIST: {
            compileQuotedList(state, func, expr->list,
                              expr->dotted, expr->line);
        } break;
        case EXPR_SYMBOL_ID: {
            pushOp(state, func, OP_PUSH, expr->line);
            pushSymbol(state, func, expr->symbolID);
        } break;
        case EXPR_VECTOR: {
            printf("Compile quoted vector\n");
            pushOp(state, func, OP_ALLOC_VECTOR, expr->line);
            pushUint64(state, func, expr->vec.numElems);
            int currElem = 0;
            for (ExprList *e = expr->vec.elems; e; e = e->next) {
                compileQuotedExpr(state, func, e->val);
                pushOp(state, func, OP_PUSH, e->val->line);
                pushInt32(state, func, currElem);
                pushOp(state, func, OP_SET_ELEM, e->val->line);
                currElem++;
            }
            assert((uint32)currElem == expr->vec.numElems);
        } break;
        case EXPR_OBJECT: {
            pushOp(state, func, OP_ALLOC_OBJECT, expr->line);
            pushUint64(state, func, expr->obj.numElems);
            int currElem = 0;
            for (ExprList *e = expr->obj.elems; e; e = e->next) {
                compileQuotedExpr(state, func, e->val->keyValPair.val);
                if (e->val->keyValPair.unquotedKey) {
                    compileExpression(state, func,
                                      e->val->keyValPair.key);
                } else {
                    assert(e->val->keyValPair.key->exprType ==
                           EXPR_SYMBOL_ID);
                    pushOp(state, func, OP_PUSH,
                           e->val->keyValPair.key->line);
                    pushSymbol(state, func,
                               e->val->keyValPair.key->symbolID);
                }
                pushOp(state, func, OP_SET_ELEM,
                       e->val->keyValPair.key->line);
                currElem++;
            }
            assert((uint32)currElem == expr->obj.numElems);
        } break;
        default: {
            compileExpression(state, func, expr);
        } break;
    }
}

void compileQuotedList(LispisState *state, LispisFunction *func,
                       ExprList *exprList, bool dotted, int32 line) {
    int32 numElems = 0;
    for (ExprList *head = exprList; head; head = head->next) {
        numElems++;
        compileQuotedExpr(state, func, head->val);
    }
    if (!dotted) {
        pushOp(state, func, OP_PUSH_NULL, line);
        numElems++;
    }
    pushOp(state, func, OP_PUSH, line);
    pushInt32(state, func, numElems);
    pushOp(state, func, OP_LIST, line);
}

LispisFunction *startNewLambda(LispisState *state, LispisFunction *func) {
    func->subFunctionsLength++;
    func->subFunctions =
        (LispisFunction **)realloc(func->subFunctions,
                                   func->subFunctionsLength *
                                   sizeof(LispisFunction *));
    LispisFunction *ret =
        (LispisFunction *)callocGcObject(state, sizeof(LispisFunction));
    ret->header.type = GC_LISPIS_FUNCTION;
    //putInZeroCountTable(state, (GcObjectHeader *)ret);
    //ret->localToGlobalTableSize = 64;
    //ret->localToGlobalTable =
    //(uint32 *)calloc(ret->localToGlobalTableSize, sizeof(uint32));
    ret->bytecodeSize = 16;
    ret->bytecode =
        (Bytecode *)calloc(1, ret->bytecodeSize * sizeof(Bytecode));
    ret->instructionToLine =
        (int32 *)malloc(ret->bytecodeSize * sizeof(int32));
    memset(ret->instructionToLine, -1, ret->bytecodeSize * sizeof(int32));
    func->subFunctions[func->subFunctionsLength-1] = ret;
    return ret;
}

LispisFunction *startNewMacro(LispisState *state) {
    //func->subFunctionsLength++;
    //func->subFunctions =
    //(LispisFunction **)realloc(func->subFunctions,
    //func->subFunctionsLength *
    //sizeof(LispisFunction *));
    LispisFunction *ret =
        (LispisFunction *)callocGcObject(state, sizeof(LispisFunction));
    ret->header.type = GC_LISPIS_FUNCTION;
    //putInZeroCountTable(state, (GcObjectHeader *)ret);
    //ret->localToGlobalTableSize = 64;
    //ret->localToGlobalTable =
    //(uint32 *)calloc(ret->localToGlobalTableSize, sizeof(uint32));
    ret->bytecodeSize = 16;
    ret->bytecode =
        (Bytecode *)calloc(1, ret->bytecodeSize * sizeof(Bytecode));
    ret->instructionToLine =
        (int32 *)malloc(ret->bytecodeSize * sizeof(int32));
    memset(ret->instructionToLine, -1, ret->bytecodeSize * sizeof(int32));
    ret->macro = true;
    //func->subFunctions[func->subFunctionsLength-1] = ret;
    return ret;
}

void compileLambdaParamsRec(LispisState *state, LispisFunction *func,
                            ExprList *params, bool varargs,
                            int32 numFormal) {
    if ((!varargs && params->next) ||
        (params->next && params->next->next)) {
        compileLambdaParamsRec(state, func, params->next, varargs,
                               numFormal);
    } else if (varargs && params->next) {
        pushOp(state, func, OP_COLLECT_VARARGS,
               params->next->val->line);
        pushInt32(state, func, numFormal);
        assert(params->next->val->exprType == EXPR_VARIABLE);
        pushOp(state, func, OP_SET_LOCAL,
               params->next->val->line);
        pushUint64(state, func, params->next->val->var.variableID);
    }
    assert(params->val->exprType == EXPR_VARIABLE);
    pushOp(state, func, OP_SET_LOCAL,
               params->val->line);
    pushUint64(state, func, params->val->var.variableID);
}

void compileLambdaParams(LispisState *state, LispisFunction *func,
                         ExprList *params, int32 paramsCount,
                         bool varargs, int32 line) {
    if (varargs) {
        if (params) {
            if (params->next) {
                pushOp(state, func, OP_PUSH, line);
                pushInt32(state, func, paramsCount-1); // last is rest
                pushOp(state, func, OP_POP_ASSERT_LESS_OR_EQUAL, line);
                if (params) {
                    compileLambdaParamsRec(state, func, params,
                                           varargs, paramsCount-1);
                }
            } else {
                pushOp(state, func, OP_PUSH, params->val->line);
                pushInt32(state, func, 0); // last is rest
                pushOp(state, func, OP_POP_ASSERT_LESS_OR_EQUAL,
                       params->val->line);
                pushOp(state, func, OP_COLLECT_VARARGS,
                       params->val->line);
                pushInt32(state, func, 0);
                assert(params->val->exprType == EXPR_VARIABLE);
                pushOp(state, func, OP_SET_LOCAL, params->val->line);
                pushUint64(state, func, params->val->var.variableID);
            }
        }
    } else {
        pushOp(state, func, OP_PUSH, line);
        pushInt32(state, func, paramsCount);
        pushOp(state, func, OP_POP_ASSERT_EQUAL, line);
        if (params) {
            compileLambdaParamsRec(state, func, params,
                                   varargs, paramsCount);
        }
    }
}

void compileLambdaBody(LispisState *state, LispisFunction *func,
                       ExprList *body) {
    int32 lastLine = -1;
    for (ExprList *exprElem = body; exprElem; exprElem = exprElem->next) {
        compileExpression(state, func, exprElem->val);
        if (exprElem->next) {
            pushOp(state, func, OP_CLEAR_STACK, exprElem->val->line);
            lastLine = exprElem->val->line;
        }
    }
    pushOp(state, func, OP_RETURN, lastLine);
    pushOp(state, func, OP_EXIT, lastLine);
}

int64 calcRelativeJumpTo(LispisState *state, LispisFunction *func,
                         uint64 jumpTo) {
    assert((int64)jumpTo);
    return (((int64)jumpTo)-1) - ((int64)func->bytecodeTop);
}

int64 calcRelativeJumpToTop(LispisState *state, LispisFunction *func, uint64 jumpFrom) {
    assert(((int64)func->bytecodeTop));
    return (((int64)func->bytecodeTop)-1) - ((int64)jumpFrom);
    // -1 since we advance the pc to
}

void compileQuasiquote(LispisState *state, LispisFunction *func,
                       QuasiquoteList *exprList, bool dotted, int32 line);

void compileQuasiquotedExpr(LispisState *state, LispisFunction *func,
                            Expr *expr, int32 line) {
    if (expr) {
        switch (expr->exprType) {
            case EXPR_QUASIQUOTE: {
                compileQuasiquote(state, func, expr->quasiquoteList,
                                  expr->dotted, expr->line);
            } break;
            case EXPR_SYMBOL_ID: {
                pushOp(state, func, OP_PUSH, expr->line);
                pushSymbol(state, func, expr->symbolID);
            } break;
            default: {
                compileExpression(state, func, expr);
            } break;
        }
    } else {
        pushOp(state, func, OP_PUSH_NULL, line);
    }
}

void compileQuasiquote(LispisState *state, LispisFunction *func,
                       QuasiquoteList *lst, bool dotted, int32 line) {
    if (!lst->next && !lst->val) {
        pushOp(state, func, OP_PUSH_NULL, line);
        return;
    }
    int32 tmpElems = 0;
    int32 numElems = 0;
    bool prevUnquoted = true;
    for (QuasiquoteList *head = lst; head; head = head->next) {
        if (head->unquoted) {
            prevUnquoted = true;
            numElems++;
            if (tmpElems) {
                pushOp(state, func, OP_PUSH_NULL, head->val->line);
                pushOp(state, func, OP_PUSH, head->val->line);
                pushInt32(state, func, tmpElems+1);
                pushOp(state, func, OP_LIST, head->val->line);
                tmpElems = 0;
            }
            compileExpression(state, func, head->val);
            pushOp(state, func, OP_PUSH_NULL, head->val->line);
            pushOp(state, func, OP_PUSH, head->val->line);
            pushInt32(state, func, 2);
            pushOp(state, func, OP_LIST, head->val->line);
        } else if (head->unquoteSpliced) {
            prevUnquoted = true;
            numElems++;
            if (tmpElems) {
                pushOp(state, func, OP_PUSH_NULL, head->val->line);
                pushOp(state, func, OP_PUSH, head->val->line);
                pushInt32(state, func, tmpElems+1);
                pushOp(state, func, OP_LIST, head->val->line);
                tmpElems = 0;
            }
            compileExpression(state, func, head->val);
        } else {
            if (prevUnquoted) {
                numElems++;
                prevUnquoted = false;
            }
            tmpElems++;
            compileQuasiquotedExpr(state, func, head->val, line);
        }
    }
    if (tmpElems) {
        if (!dotted) {
            pushOp(state, func, OP_PUSH_NULL, line);
            tmpElems++;
        }
        pushOp(state, func, OP_PUSH, line);
        pushInt32(state, func, tmpElems);
        pushOp(state, func, OP_LIST, line);
    }
    pushOp(state, func, OP_PUSH, line);
    pushInt32(state, func, numElems);
    pushOp(state, func, OP_APPEND, line);
}

uint64 getTarget(LispisState *state, LispisFunction *func) {
    return func->bytecodeTop;
}

void compileExpression(LispisState *state, LispisFunction *func, Expr *expr) {
    assert(expr);
    switch (expr->exprType) {
        case EXPR_OBJECT: {
            pushOp(state, func, OP_ALLOC_OBJECT, expr->line);
            pushUint64(state, func, expr->obj.numElems);
            int currElem = 0;
            for (ExprList *e = expr->obj.elems; e; e = e->next) {
                compileExpression(state, func, e->val->keyValPair.val);
                if (e->val->keyValPair.unquotedKey) {
                    compileExpression(state, func,
                                      e->val->keyValPair.key);
                } else {
                    assert(e->val->keyValPair.key->exprType ==
                           EXPR_SYMBOL_ID);
                    pushOp(state, func, OP_PUSH, expr->line);
                    pushSymbol(state, func,
                               e->val->keyValPair.key->symbolID);
                }
                pushOp(state, func, OP_SET_ELEM, expr->line);
                currElem++;
            }
            assert((uint32)currElem == expr->obj.numElems);
        } break;
        case EXPR_VECTOR: {
            pushOp(state, func, OP_ALLOC_VECTOR, expr->line);
            pushUint64(state, func, expr->vec.numElems);
            int currElem = 0;
            for (ExprList *e = expr->vec.elems; e; e = e->next) {
                compileExpression(state, func, e->val);
                pushOp(state, func, OP_PUSH, expr->line);
                pushInt32(state, func, currElem);
                pushOp(state, func, OP_SET_ELEM, expr->line);
                currElem++;
            }
            assert((uint32)currElem == expr->vec.numElems);
        } break;
        case EXPR_FOR: {

            compileExpression(state, func, expr->init);
            pushOp(state, func, OP_SET_LOCAL, expr->line);
            pushUint64(state, func, expr->it->var.variableID);
            uint64 loopCheckTarget = getTarget(state, func);
            compileExpression(state, func, expr->pred);
            pushOp(state, func, OP_JUMP_IF_TRUE, expr->line);
            uint64 loopCheckSuccededLoc = pushDummy(state, func);
            pushOp(state, func, OP_JUMP, expr->line);
            uint64 loopCheckFailedLoc = pushDummy(state, func);
            Value loopCheckSuccededRel;
            loopCheckSuccededRel.i64 =
                calcRelativeJumpToTop(state, func,
                                      loopCheckSuccededLoc);
            set(state, func, loopCheckSuccededRel, loopCheckSuccededLoc);
            for (ExprList *b = expr->body; b; b = b->next) {
                compileExpression(state, func, b->val);
                pushOp(state, func, OP_CLEAR_STACK, expr->line);
            }
            compileExpression(state, func, expr->upd);
            pushOp(state, func, OP_JUMP, expr->line);

            pushInt64(state, func, calcRelativeJumpTo(state, func,
                                                      loopCheckTarget));
            Value loopCheckFailedRel;
            loopCheckFailedRel.i64 =
                calcRelativeJumpToTop(state, func,
                                      loopCheckFailedLoc);
            set(state, func, loopCheckFailedRel, loopCheckFailedLoc);

        } break;
        case EXPR_QUOTE: {
            compileQuotedExpr(state, func, expr->quoted);
        } break;
        //case EXPR_STRING: {
        //printf("\"%.*s\"", node->strLen, node->str);
        //} break;
        case EXPR_INT: {
            pushOp(state, func, OP_PUSH, expr->line);
            pushInt32(state, func, expr->intVal);
        } break;
        case EXPR_DOUBLE: {
            pushOp(state, func, OP_PUSH, expr->line);
            pushDouble(state, func, expr->doubleVal);
        } break;
        case EXPR_VARIABLE: {
            switch (expr->var.kind) {
                case VAR_GLOBAL: {
                    pushOp(state, func, OP_PUSH, expr->line);
                    pushSymbol(state, func, expr->var.symbolID);
                    pushOp(state, func, OP_PUSH_GLOBAL, expr->line);
                } break;
                case VAR_LOCAL: {
                    pushOp(state, func, OP_PUSH_LOCAL, expr->line);
                    pushUint64(state, func, expr->var.variableID);
                } break;
                case VAR_UPVAL: {
                    pushOp(state, func, OP_PUSH_UPVAL, expr->line);
                    pushUint64(state, func, expr->var.variableID);
                    func->upvalProtos[expr->var.variableID].depth =
                        expr->var.depth;
                    func->upvalProtos[expr->var.variableID].index =
                        expr->var.index;
                } break;
                default: assert(false);
            }
        } break;
        case EXPR_BOOLEAN: {
            pushOp(state, func, OP_PUSH, expr->line);
            pushBoolean(state, func, expr->boolean);
        } break;
        case EXPR_SYMBOL_ID: {
            pushOp(state, func, OP_PUSH, expr->line);
            pushSymbol(state, func, expr->symbolID);
            pushOp(state, func, OP_PUSH_GLOBAL, expr->line);
        } break;
        case EXPR_CALL: {
            uint64 numArgs = 0;
            for (ExprList *param = expr->arguments;
                 param; param = param->next) {
                compileExpression(state, func, param->val);
                numArgs++;
            }
            compileExpression(state, func, expr->callee);
            pushOp(state, func, OP_CALL, expr->line);
            pushUint64(state, func, numArgs);
        } break;
        case EXPR_MACRO: {
            LispisFunction *newMacro = startNewMacro(state);
            compileLambdaParams(state, newMacro, expr->macro.params,
                                expr->macro.paramsCount, expr->macro.varargs, expr->line);
            compileLambdaBody(state, newMacro, expr->macro.body);
#if LOG_ENABLED
            printf("Macro %u\n", expr->macro.name);
            dumpBytecode(state, newMacro);
#endif
            newMacro->upvalProtos = 0;
            newMacro->upvalProtosSize = 0;
            newMacro->numLocals = expr->macro.numLocals;

            LispisFunctionObject *macroObj =
                (LispisFunctionObject *)callocGcObject(state,
                                                       sizeof(LispisFunctionObject));
            macroObj->header.type = GC_LISPIS_FUNCTION_OBJECT;
            macroObj->function = newMacro;
            macroObj->parentEnv = 0;
            setGlobal(state, nanPackPointer(macroObj, LISPIS_LFUNC),
                      expr->macro.name);
                           
        } break;
        case EXPR_LAMBDA: {
            pushOp(state, func, OP_PUSH_LAMBDA_ID, expr->line);
            // FIX
            pushUint64(state, func, func->subFunctionsLength);
            LispisFunction *newFunc = startNewLambda(state, func);
            newFunc->closedOver = expr->lambda.closedOver;
            newFunc->macro = false;
            newFunc->upvalProtos = 0;
            newFunc->upvalProtosSize = 0;
            if (expr->lambda.numUpvals) {
                newFunc->upvalProtosSize = expr->lambda.numUpvals;
                newFunc->upvalProtos =
                    (UpvalProto *)calloc(newFunc->upvalProtosSize,
                                         sizeof(UpvalProto));
            }
            compileLambdaParams(state, newFunc, expr->lambda.params,
                                expr->lambda.paramsCount,
                                expr->lambda.varargs, expr->line);
            compileLambdaBody(state, newFunc, expr->lambda.body);
            newFunc->numLocals = expr->lambda.numLocals;
            //encodeSymbolSection(state, newFunc);
        } break;
        case EXPR_DEFINE: {
            compileExpression(state, func, expr->value);
            pushOp(state, func, OP_PUSH, expr->line);
            pushSymbol(state, func, expr->variable->var.symbolID);
            pushOp(state, func, OP_SET_GLOBAL, expr->line);
            // makes let! return the value, prob. pretty slow...
            compileExpression(state, func, expr->variable);
        } break;
        case EXPR_SET: {
            compileExpression(state, func, expr->value);
            switch (expr->variable->var.kind) {
                case VAR_GLOBAL: {
                    pushOp(state, func, OP_PUSH, expr->line);
                    pushSymbol(state, func, expr->variable->var.symbolID);
                    pushOp(state, func, OP_SET_GLOBAL, expr->line);
                } break;
                case VAR_LOCAL: {
                    pushOp(state, func, OP_SET_LOCAL, expr->line);
                    pushUint64(state, func,
                               expr->variable->var.variableID);
                } break;
                case VAR_UPVAL: {
                    pushOp(state, func, OP_SET_UPVAL, expr->line);
                    pushUint64(state,
                               func, expr->variable->var.variableID);
                    func->upvalProtos[expr->variable->var.variableID].depth =
                        expr->variable->var.depth;
                    func->upvalProtos[expr->variable->var.variableID].index =
                        expr->variable->var.index;
                } break;
                default: assert(false);
            }
            //pushOp(state, func, OP_SET_LOCAL);
            // makes let! return the value, prob. pretty slow...
            compileExpression(state, func, expr->variable);
        } break;
        case EXPR_LET: {
            compileExpression(state, func, expr->value);
            pushOp(state, func, OP_SET_LOCAL, expr->line);
            pushUint64(state, func, expr->variable->var.variableID);
            //pushOp(state, func, OP_SET_LOCAL);
            // makes let! return the value, prob. pretty slow...
            compileExpression(state, func, expr->variable);
        } break;
        case EXPR_IF: {
            compileExpression(state, func, expr->predicate);
            pushOp(state, func, OP_JUMP_IF_TRUE, expr->line);
            uint64 trueTargetIdLoc = pushDummy(state, func);
            if (expr->falseBranch) {
                compileExpression(state, func, expr->falseBranch);
            } else {
                pushOp(state, func, OP_PUSH, expr->line);
                pushUndef(state, func);
            }
            pushOp(state, func, OP_JUMP, expr->line);
            uint64 afterTrueTargetIdLoc = pushDummy(state, func);
            Value trueRelTarget;
            trueRelTarget.i64 =
                calcRelativeJumpToTop(state, func, trueTargetIdLoc);
            set(state, func, trueRelTarget, trueTargetIdLoc);
            compileExpression(state, func, expr->trueBranch);
            Value afterTrueRelTarget;
            afterTrueRelTarget.i64 =
                calcRelativeJumpToTop(state, func, afterTrueTargetIdLoc);
            set(state, func, afterTrueRelTarget, afterTrueTargetIdLoc);

        } break;
        case EXPR_QUASIQUOTE: {
            compileQuasiquote(state, func, expr->quasiquoteList,
                              expr->dotted, expr->line);
        } break;
        case EXPR_DO: {
            for (ExprList *e = expr->list; e; e = e->next) {
                compileExpression(state, func, e->val);
            }
        } break;
        case EXPR_REF: {
            compileExpression(state, func, expr->ref.obj);
            compileExpression(state, func, expr->ref.ref);
            pushOp(state, func, OP_PUSH_ELEM, expr->line);
        } break;
        case EXPR_REF_SET: {
            compileExpression(state, func, expr->refSet.obj);
            compileExpression(state, func, expr->refSet.val);
            compileExpression(state, func, expr->refSet.ref);
            pushOp(state, func, OP_SET_ELEM, expr->line);
        } break;
        default:assert(false);
    }
}

#if 0
void allocSymbolSection(CompilerState *state) {
    if (state->symbolSectionTop == state->symbolSectionSize) {
        state->symbolSectionSize = state->symbolSectionSize * 1.5f;
        state->symbolSection = (Bytecode *)realloc(state->symbolSection,
                                                   sizeof(Bytecode) *
                                                   state->symbolSectionSize);
    }
}

// TODO replace with better stuff
void encodeSymbolSection(LispisState *state, LispisFunction *func) {
    SymbolIndexMap *map = &compiler->symbolIndexMap;
    for (uint64 i = 0; i < map->symbolsSize; ++i) {
        if (map->symbolMap[i].filled) {
            allocSymbolSection(compiler);
            compiler->symbolSection[compiler->symbolSectionTop] = 
                nanPackInt32(map->symbolMap[i].symbol.length);
            compiler->symbolSectionTop++;

            allocSymbolSection(compiler);
            compiler->symbolSection[compiler->symbolSectionTop] =
                nanPackSymbolIdx(map->symbolMap[i].symbolId);
            compiler->symbolSectionTop++;
            bool done = true;
            for (int32 c = 0; c < map->symbolMap[i].symbol.length; c++) {
                compiler->symbolSection[compiler->symbolSectionTop].c[c%8] = map->symbolMap[i].symbol.val[c];
                done = false;
                if (c % 8 == 7) {
                    allocSymbolSection(compiler);
                    compiler->symbolSectionTop++;
                    done = true;
                }
            }
            if (!done) {
                allocSymbolSection(compiler);
                compiler->symbolSectionTop++;
            }
        }
    }
}

uint64 totalBytecodeSize(CompilerState *state) {
    uint64 ret = 0;
    for (int i = 0; i < state->childStatesLength; ++i) {
        ret += totalBytecodeSize(state->childStates + i);
    }
    ret += state->symbolSectionTop;
    ret += func->bytecodeTop;
    ret += 3;
    return ret;
}

void compactBytecodeInternal(CompilerState *state,
                                  Bytecode *bytecode) {
    uint64 numSubFuncs = state->childStatesLength;
    memcpy(bytecode, &numSubFuncs, sizeof(uint64));
    bytecode++;
    for (int i = 0; i < state->childStatesLength; ++i) {
        compactBytecodeInternal(state->childStates + i,
                                bytecode);
        bytecode += totalBytecodeSize(state->childStates + i);
    }
    uint64 numSymbols = state->symbolIndexMap.symbolsFilled;
    printf("num symbols %llu\n", numSymbols);
    memcpy(bytecode,
           &numSymbols,
           sizeof(numSymbols));
    bytecode++;
    memcpy(bytecode,
           state->symbolSection,
           state->symbolSectionTop * sizeof(Bytecode));
    bytecode += state->symbolSectionTop;
    uint64 bytecodeSize = func->bytecodeTop;
    printf("bytecodeSize %llu\n", bytecodeSize);
    memcpy(bytecode,
           &bytecodeSize,
           sizeof(bytecodeSize));
    bytecode++;
    memcpy(bytecode,
           func->bytecode,
           func->bytecodeTop * sizeof(Bytecode));
}

Bytecode *compactBytecode(CompilerState *state) {
    Bytecode *ret = (Bytecode *)malloc(sizeof(Bytecode) *
                                       totalBytecodeSize(state));
    compactBytecodeInternal(state, ret);
    return ret;
}
#endif
