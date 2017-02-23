#include "codegen.h"
#include "parser.h"
#include <cmath>
#include <cstdlib>
#include <cassert>
#include <cstdio>
#include <cstring>

Value nanPackPointer(void *p, uint32 typeID) {
    double nan = NAN;
    uint64 retI = (0xFFFFFFFFFFFF & ((uint64)p));
    uint64 nanVal = ((*(uint64 *)&nan) |
                     retI |
                     ((uint64)typeID << 47));
    Value ret;
    ret.ui64 = nanVal;
    return ret;
}

Value nanPack(uint32 val, uint32 typeID) {
    Value ret;
    ret.f64 = NAN;
    ret.ui64 |= (uint64)val | ((uint64)typeID << 47);
    return ret;
}

Value nanPackInt32(int32 a) {
    uint32 u = *(uint32 *)&a;
    return nanPack(u, LISPIS_INT32);
}

Value nanPackSymbolIdx(uint32 s) {
    return nanPack(s, LISPIS_SYM_IDX);
}

Value nanPackDouble(double d) {
    Value ret;
    ret.f64 = d;
    return ret;
}

Value nanPackBoolean(bool b) {
    return nanPack(b, LISPIS_BOOLEAN);
}

void allocBytecode(LispisState *state, LispisFunction *func) {
    if (func->bytecodeTop == func->bytecodeSize) {
        func->bytecodeSize = func->bytecodeSize * 1.5f;
        func->bytecode = (Bytecode *)realloc(func->bytecode,
                                             sizeof(Bytecode) *
                                             func->bytecodeSize);
    }
}

void pushOp(LispisState *state, LispisFunction *func, OpCodes op) {
    allocBytecode(state, func);
    func->bytecode[func->bytecodeTop] = nanPack(op, LISPIS_OP);
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

void compileQuotedList(LispisState *state, LispisFunction *func, ExprList *exprList);

void compileQuotedExpr(LispisState *state, LispisFunction *func, Expr *expr) {
    switch (expr->exprType) {
        case EXPR_LIST: {
            compileQuotedList(state, func, expr->list);
        } break;
        case EXPR_SYMBOL_ID: {
            pushOp(state, func, OP_PUSH);
            pushSymbol(state, func, expr->symbolID);
        } break;
        default: {
            compileExpression(state, func, expr);
        } break;
    }
}

void compileQuotedList(LispisState *state, LispisFunction *func, ExprList *exprList) {
    int32 numElems = 0;
    for (ExprList *head = exprList; head; head = head->next) {
        numElems++;
        compileQuotedExpr(state, func, head->val);
    }
    pushOp(state, func, OP_PUSH);
    pushInt32(state, func, numElems);
    pushOp(state, func, OP_LIST);
}

LispisFunction *startNewLambda(LispisState *state, LispisFunction *func) {
// Created a new CompilerState... what to do
    func->subFunctionsLength++;
    func->subFunctions =
        (LispisFunction **)realloc(func->subFunctions,
                                   func->subFunctionsLength);
    LispisFunction *ret =
        (LispisFunction *)calloc(1, sizeof(LispisFunction));
    //ret->localToGlobalTableSize = 64;
    //ret->localToGlobalTable =
    //(uint32 *)calloc(ret->localToGlobalTableSize, sizeof(uint32));
    ret->bytecodeSize = 16;
    ret->bytecode =
        (Bytecode *)calloc(1, ret->bytecodeSize * sizeof(Bytecode));
    func->subFunctions[func->subFunctionsLength-1] = ret;
    return ret;
#if 0
    state->childStatesLength++;
    if (state->childStates) {
        state->childStates =
            (CompilerState *)realloc(state->childStates,
                                     sizeof(CompilerState) *
                                     state->childStatesLength);
    } else {
        state->childStates =
            (CompilerState *)malloc(sizeof(CompilerState) *
                                    state->childStatesLength);
    } 
    CompilerState *ret = state->childStates + state->childStatesLength-1;
    ret->symbolSectionTop = 0;
    ret->symbolSectionSize = 8;
    ret->bytecodeTop = 0;
    ret->bytecodeSize = 16;
    ret->symbolSection = (Bytecode *)malloc(sizeof(Bytecode) *
                                            ret->symbolSectionSize);
    ret->bytecode = (Bytecode *)malloc(sizeof(Bytecode) *
                                       ret->bytecodeSize);
    ret->symbolIndexMap.symbolsSize = 16;
    ret->symbolIndexMap.symbolsFilled = 0;
    ret->symbolIndexMap.nextSymbolId = 0;
    ret->symbolIndexMap.symbolMap =
        (SymIdBucket *)calloc(ret->symbolIndexMap.symbolsSize,
                              sizeof(SymIdBucket));
    ret->childStates = 0;
    ret->childStatesLength = 0;
    return ret;
#endif
    return 0;
}

void compileLambdaParamsRec(LispisState *state, LispisFunction *func, ExprList *params) {
    if (params->next) {
        compileLambdaParamsRec(state, func, params->next);
    }
    assert(params->val->exprType == EXPR_SYMBOL_ID);
    pushOp(state, func, OP_PUSH);
    pushSymbol(state, func, params->val->symbolID);
    pushOp(state, func, OP_SET_LOCAL_VARIABLE);
}

void compileLambdaParams(LispisState *state, LispisFunction *func, ExprList *params,
                         int32 paramsCount) {
    pushOp(state, func, OP_PUSH);
    pushInt32(state, func, paramsCount);
    pushOp(state, func, OP_POP_ASSERT_EQUAL);
    if (params) {
        compileLambdaParamsRec(state, func, params);
    }
}

void compileLambdaBody(LispisState *state, LispisFunction *func, ExprList *body) {
    for (ExprList *exprElem = body; exprElem; exprElem = exprElem->next) {
        compileExpression(state, func, exprElem->val);
        if (exprElem->next) {
            pushOp(state, func, OP_CLEAR_STACK);
        }
    }
    pushOp(state, func, OP_RETURN);
    pushOp(state, func, OP_EXIT);
}

int64 calcRelativeJumpToTop(LispisState *state, LispisFunction *func, uint64 jumpFrom) {
    assert(((int64)func->bytecodeTop));
    return (((int64)func->bytecodeTop)-1) - ((int64)jumpFrom);
    // -1 since we advance the pc to
}

void compileExpression(LispisState *state, LispisFunction *func, Expr *expr) {
    assert(expr);
    switch (expr->exprType) {
        case EXPR_QUOTE: {
            compileQuotedExpr(state, func, expr->quoted);
        } break;
        //case EXPR_STRING: {
        //printf("\"%.*s\"", node->strLen, node->str);
        //} break;
        case EXPR_INT: {
            pushOp(state, func, OP_PUSH);
            pushInt32(state, func, expr->intVal);
        } break;
        case EXPR_FLOAT: {
            pushOp(state, func, OP_PUSH);
            pushDouble(state, func, expr->floatVal);
        } break;
        case EXPR_SYMBOL_ID: {
            pushOp(state, func, OP_PUSH);
            pushSymbol(state, func, expr->symbolID);
            pushOp(state, func, OP_EVAL_SYMBOL);
        } break;
        case EXPR_CALL: {
            uint64 numArgs = 0;
            for (ExprList *param = expr->arguments;
                 param; param = param->next) {
                compileExpression(state, func, param->val);
                numArgs++;
            }
            compileExpression(state, func, expr->callee);
            pushOp(state, func, OP_CALL);
            pushUint64(state, func, numArgs);
        } break;
        case EXPR_LAMBDA: {
            //TODO varargs
            pushOp(state, func, OP_PUSH_LAMBDA_ID);
            // FIX
            pushUint64(state, func, func->subFunctionsLength);
            LispisFunction *newFunc = startNewLambda(state, func);
            compileLambdaParams(state, newFunc, expr->params,
                                expr->paramsCount);
            compileLambdaBody(state, newFunc, expr->body);
            //encodeSymbolSection(state, newFunc);
        } break;
        case EXPR_LET: {
            compileExpression(state, func, expr->value);
            pushOp(state, func, OP_PUSH);
            pushSymbol(state, func, expr->variable->symbolID);
            pushOp(state, func, OP_SET_LOCAL_VARIABLE);
            // makes let! return the value, prob. pretty slow...
            compileExpression(state, func, expr->variable);
        } break;
        case EXPR_IF: {
            compileExpression(state, func, expr->predicate);
            pushOp(state, func, OP_JUMP_IF_TRUE);
            uint64 trueTargetIdLoc = pushDummy(state, func);
            if (expr->falseBranch) {
                compileExpression(state, func, expr->falseBranch);
            } else {
                pushOp(state, func, OP_PUSH);
                pushUndef(state, func);
            }
            pushOp(state, func, OP_JUMP);
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
