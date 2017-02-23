#ifndef VM_H
#define VM_H
#include "common.h"

struct GcObjectHeader {
    // Top 2 bits are gray-white markers for the incremental mark-sweep gc
    // The rest are actual refCount
    uint32 refCount;
};

struct Symbol {
    String symbol;
    uint64 hash; // Do i need this?
    uint32 globalSymbolID;
    bool filled;
};

struct SymbolTable {
    Symbol *symbols;
    uint64 symbolsSize;
    uint64 symbolsInterned;
    uint32 nextGlobalSymbolID;
};

struct Var {
    Value val;
    uint32 symbolID;
    bool filled;
};

struct Env {
    GcObjectHeader header;
    Var *variables;
    uint64 variablesSize;
    uint64 variablesFilled;
    Env *parentEnv; // lexical parents enviroment, 0 if the global env
};

struct LispisFunction;

struct ActivationRecord {
    GcObjectHeader header;
    uint32 dataStackBottom;
    uint64 pc;
    ActivationRecord *caller; // 0 if called from C
    LispisFunction *function;
    Env *enviroment;
};

struct LispisFunction {
    GcObjectHeader header;
    Bytecode *bytecode;
    uint64 bytecodeSize;
    uint64 bytecodeTop; // used while compiling only
    LispisFunction **subFunctions;
    uint64 subFunctionsLength;
};

struct LispisState;

typedef bool (*LispisCFunction)(LispisState *state,
                                uint64 numVariables);

struct CFunction {
    GcObjectHeader header;
    LispisCFunction func;
};

struct Pair {
    Value car;
    Value cdr;
};

struct LispisState {
    Env globalEnviroment;
    SymbolTable globalSymbolTable;
    ActivationRecord *currRecord;
    Value *dataStack;
    uint32 dataStackSize;
    uint32 dataStackTop;
};

uint32 internSymbol(LispisState *state, String symbol, uint64 symHash);

#endif