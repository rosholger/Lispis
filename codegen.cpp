#include "codegen.h"
#include "parser.h"
#include <cmath>
#include <cstdlib>
#include <cassert>
#include <cstdio>

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
    double nan = NAN;
    uint64 nanVal = (*(uint64 *)&nan) | (uint64)val | ((uint64)typeID << 47);
    Value ret;
    ret.ui64 = nanVal;
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
    state->bytecode[state->bytecodeTop].opCode = op;
    state->bytecodeTop++;
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
            pushOp(state, OP_PUSH);
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

void compileExpression(CompilerState *state, Expr *expr) {
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
            for (ExprList *param = expr->params;
                 param; param = param->next) {
                compileExpression(state, param->val);
                numArgs++;
            }
            compileExpression(state, expr->callee);
            pushOp(state, OP_CALL);
            pushUint64(state, numArgs);
        } break;
        default:assert(false);
    }
}

void compileFunctionBody(CompilerState *state, ExprList *body) {
    for (ExprList *expr = body; expr; expr = expr->next) {
        compileExpression(state, expr->val);
    }
}

void compileFunction(CompilerState *state, Function *func) {
    for (ExprList *param = func->params;
         param; param = param->next) {
        pushOp(state, OP_PUSH);
        pushSymbol(state, param->val->str);
        pushOp(state, OP_SET_LOCAL_SYMBOL);
    }
    compileFunctionBody(state, func->body);
    pushOp(state, OP_RETURN);
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
    allocSymbolSection(compiler);
    compiler->symbolSection[compiler->symbolSectionTop] = nanPackInt32(0);
    compiler->symbolSectionTop++;
}