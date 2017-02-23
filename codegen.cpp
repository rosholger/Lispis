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

void allocBytecode(CompilerState *state) {
    if (state->bytecodeTop == state->bytecodeSize) {
        state->bytecodeSize = state->bytecodeSize * 1.5f;
        state->bytecode = (Bytecode *)realloc(state->bytecode,
                                              sizeof(Bytecode) *
                                              state->bytecodeSize);
    }
}

void pushOp(CompilerState *state, OpCodes op) {
    allocBytecode(state);
    state->bytecode[state->bytecodeTop] = nanPack(op, LISPIS_OP);
    state->bytecodeTop++;
}

uint64 pushDummy(CompilerState *state) {
    allocBytecode(state);
    state->bytecode[state->bytecodeTop] = nanPack(0, LISPIS_UNDEF);
    uint64 ret = state->bytecodeTop;
    state->bytecodeTop++;
    return ret;
}

void pushUndef(CompilerState *state) {
    pushDummy(state);
}

void pushUint64(CompilerState *state, uint64 u) {
    allocBytecode(state);
    state->bytecode[state->bytecodeTop].ui64 = u;
    state->bytecodeTop++;
}

void pushDouble(CompilerState *state, double d) {
    allocBytecode(state);
    state->bytecode[state->bytecodeTop].f64 = d;
    state->bytecodeTop++;
}

void pushInt32(CompilerState *state, int32 a) {
    pushDouble(state, nanPackInt32(a).f64);
}

void set(CompilerState *state, Value v, uint64 pos) {
    assert(pos < state->bytecodeTop);
    state->bytecode[pos] = v;
}

uint32 getSymbolIndex(CompilerState *state, String str) {
    SymbolIndexMap *map = &state->symbolIndexMap;
    if (map->symbolsFilled >
        map->symbolsSize*0.7) {
        uint64 oldSize = map->symbolsSize;
        SymIdBucket *oldBuckets = map->symbolMap;
        map->symbolsSize *= 1.5f;
        map->symbolMap = (SymIdBucket *)calloc(map->symbolsSize,
                                               sizeof(SymIdBucket));
        for (uint64 i = 0; i < oldSize; ++i) {
            if (oldBuckets[i].filled) {
                uint64 hash = hashFunc(oldBuckets[i].symbol);
                uint64 id = hash % map->symbolsSize;
                while (map->symbolMap[id].filled) {
                    id++;
                    id = id % map->symbolsSize;
                }
                map->symbolMap[id] = oldBuckets[i];
            }
        }
        free(oldBuckets);
    }
    uint64 symHash = hashFunc(str);
    uint64 symMapIndex = symHash % map->symbolsSize;
    while (map->symbolMap[symMapIndex].filled) {
        if (symCmp(str, symHash,
                   map->symbolMap[symMapIndex].symbol,
                   hashFunc(map->symbolMap[symMapIndex].symbol))) {
            break;
        }
        symMapIndex++;
        symMapIndex = symMapIndex % map->symbolsSize;
    }
    uint32 id = 0;
    if (map->symbolMap[symMapIndex].filled) {
        id = map->symbolMap[symMapIndex].symbolId;
    } else {
        map->symbolMap[symMapIndex].symbol = str;
        map->symbolMap[symMapIndex].filled = true;
        id = map->nextSymbolId;
        map->symbolMap[symMapIndex].symbolId = id;
        map->nextSymbolId++;
        map->symbolsFilled++;
    }
    return id;
}

void pushSymbol(CompilerState *state, String str) {
    uint32 symbolIdx = getSymbolIndex(state, str);
    pushDouble(state, nanPackSymbolIdx(symbolIdx).f64);
}

void compileQuotedList(CompilerState *state, ExprList *exprList);

void compileQuotedExpr(CompilerState *state, Expr *expr) {
    switch (expr->exprType) {
        case EXPR_LIST: {
            compileQuotedList(state, expr->list);
        } break;
        case EXPR_SYMBOL: {
            pushOp(state, OP_PUSH_TRANSLATE_SYMBOL);
            pushSymbol(state, expr->str);
        } break;
        default: {
            compileExpression(state, expr);
        } break;
    }
}

void compileQuotedList(CompilerState *state, ExprList *exprList) {
    int32 numElems = 0;
    for (ExprList *head = exprList; head; head = head->next) {
        numElems++;
        compileQuotedExpr(state, head->val);
    }
    pushOp(state, OP_PUSH);
    pushInt32(state, numElems);
    pushOp(state, OP_LIST);
}

CompilerState *startNewLambda(CompilerState *state) {
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
}

void compileLambdaParamsRec(CompilerState *state, ExprList *params) {
    if (params->next) {
        compileLambdaParamsRec(state, params->next);
    }
    assert(params->val->exprType == EXPR_SYMBOL);
    pushOp(state, OP_PUSH_TRANSLATE_SYMBOL);
    pushSymbol(state, params->val->str);
    pushOp(state, OP_SET_LOCAL_VARIABLE);
}

void compileLambdaParams(CompilerState *state, ExprList *params,
                         int32 paramsCount) {
    pushOp(state, OP_PUSH);
    pushInt32(state, paramsCount);
    pushOp(state, OP_POP_ASSERT_EQUAL);
    if (params) {
        compileLambdaParamsRec(state, params);
    }
}

void compileLambdaBody(CompilerState *state, ExprList *body) {
    for (ExprList *exprElem = body; exprElem; exprElem = exprElem->next) {
        compileExpression(state, exprElem->val);
        if (exprElem->next) {
            pushOp(state, OP_CLEAR_STACK);
        }
    }
    pushOp(state, OP_RETURN);
    pushOp(state, OP_EXIT);
}

int64 calcRelativeJumpToTop(CompilerState *state, uint64 jumpFrom) {
    assert(((int64)state->bytecodeTop));
    return (((int64)state->bytecodeTop)-1) - ((int64)jumpFrom);
    // -1 since we advance the pc to
}

void compileExpression(CompilerState *state, Expr *expr) {
    assert(expr);
    switch (expr->exprType) {
        case EXPR_QUOTE: {
            compileQuotedExpr(state, expr->quoted);
        } break;
        //case EXPR_STRING: {
        //printf("\"%.*s\"", node->strLen, node->str);
        //} break;
        case EXPR_INT: {
            pushOp(state, OP_PUSH);
            pushInt32(state, expr->intVal);
        } break;
        case EXPR_FLOAT: {
            pushOp(state, OP_PUSH);
            pushDouble(state, expr->floatVal);
        } break;
        case EXPR_SYMBOL: {
            pushOp(state, OP_PUSH_TRANSLATE_SYMBOL);
            pushSymbol(state, expr->str);
            pushOp(state, OP_EVAL_SYMBOL);
        } break;
        case EXPR_CALL: {
            uint64 numArgs = 0;
            for (ExprList *param = expr->arguments;
                 param; param = param->next) {
                compileExpression(state, param->val);
                numArgs++;
            }
            compileExpression(state, expr->callee);
            pushOp(state, OP_CALL);
            pushUint64(state, numArgs);
        } break;
        case EXPR_LAMBDA: {
            //TODO varargs
            pushOp(state, OP_PUSH_LAMBDA_ID);
            pushUint64(state, state->childStatesLength);
            CompilerState *newFuncState = startNewLambda(state);
            compileLambdaParams(newFuncState, expr->params,
                                expr->paramsCount);
            compileLambdaBody(newFuncState, expr->body);
            encodeSymbolSection(newFuncState);
        } break;
        case EXPR_LET: {
            compileExpression(state, expr->value);
            pushOp(state, OP_PUSH_TRANSLATE_SYMBOL);
            pushSymbol(state, expr->variable->str);
            pushOp(state, OP_SET_LOCAL_VARIABLE);
            // make let! return the value, prob. pretty slow...
            compileExpression(state, expr->variable);
        } break;
        case EXPR_IF: {
            compileExpression(state, expr->predicate);
            pushOp(state, OP_JUMP_IF_TRUE);
            uint64 trueTargetIdLoc = pushDummy(state);
            if (expr->falseBranch) {
                compileExpression(state, expr->falseBranch);
            } else {
                pushOp(state, OP_PUSH);
                pushUndef(state);
            }
            pushOp(state, OP_JUMP);
            uint64 afterTrueTargetIdLoc = pushDummy(state);
            Value trueRelTarget;
            trueRelTarget.i64 =
                calcRelativeJumpToTop(state, trueTargetIdLoc);
            set(state, trueRelTarget, trueTargetIdLoc);
            compileExpression(state, expr->trueBranch);
            Value afterTrueRelTarget;
            afterTrueRelTarget.i64 =
                calcRelativeJumpToTop(state, afterTrueTargetIdLoc);
            set(state, afterTrueRelTarget, afterTrueTargetIdLoc);

        } break;
        default:assert(false);
    }
}

void compileFunctionBody(CompilerState *state, ExprList *body) {
    for (ExprList *expr = body; expr; expr = expr->next) {
        compileExpression(state, expr->val);
    }
}

void allocSymbolSection(CompilerState *state) {
    if (state->symbolSectionTop == state->symbolSectionSize) {
        state->symbolSectionSize = state->symbolSectionSize * 1.5f;
        state->symbolSection = (Bytecode *)realloc(state->symbolSection,
                                                   sizeof(Bytecode) *
                                                   state->symbolSectionSize);
    }
}

void encodeSymbolSection(CompilerState *compiler) {
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
    ret += state->bytecodeTop;
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
    uint64 bytecodeSize = state->bytecodeTop;
    printf("bytecodeSize %llu\n", bytecodeSize);
    memcpy(bytecode,
           &bytecodeSize,
           sizeof(bytecodeSize));
    bytecode++;
    memcpy(bytecode,
           state->bytecode,
           state->bytecodeTop * sizeof(Bytecode));
}

Bytecode *compactBytecode(CompilerState *state) {
    Bytecode *ret = (Bytecode *)malloc(sizeof(Bytecode) *
                                       totalBytecodeSize(state));
    compactBytecodeInternal(state, ret);
    return ret;
}