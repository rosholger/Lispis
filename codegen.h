#ifndef CODEGEN_H
#define CODEGEN_H
#include "common.h"
struct SymIdBucket {
    String symbol;
    uint32 symbolId;
    bool filled;
};

struct SymbolIndexMap {
    SymIdBucket *symbolMap;
    uint64 symbolsSize;
    uint64 symbolsFilled;
    uint32 nextSymbolId;
};

struct CompilerState {
    Bytecode *symbolSection;
    uint64 symbolSectionTop;
    uint64 symbolSectionSize;
    Bytecode *bytecode;
    uint64 bytecodeTop;
    uint64 bytecodeSize;
    SymbolIndexMap symbolIndexMap;
    CompilerState *childStates;
    int64 childStatesLength;
};

void compileExpression(CompilerState *state, Expr *expr);
void encodeSymbolSection(CompilerState *compiler);
void pushOp(CompilerState *state, OpCodes op);
Bytecode *compactBytecode(CompilerState *state);
#endif