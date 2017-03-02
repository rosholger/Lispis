#ifndef VM_H
#define VM_H
#include "common.h"

enum GcObjectType {
    GC_UNDEF, // ERROR to use
    GC_ENV,
    GC_ACTIVATION_RECORD,
    GC_LISPIS_FUNCTION,
    GC_LISPIS_FUNCTION_OBJECT,
    GC_C_FUNCTION,
    GC_PAIR
};

//TODO circular references, closures always close on them self

enum GcColor {
    GC_black = 1 << 0,
    GC_gray = 1 << 1,
    GC_white = 1 << 2,
    GC_frozen = 1 << 3,
};

struct GcObjectHeader {
    uint8 color;
    uint32 refCount;
    GcObjectHeader *next; // allocated GcObject
    GcObjectHeader *prev; // allocated GcObject
    GcObjectHeader *nextZCT;
    GcObjectHeader *prevZCT;
    GcObjectType type;
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
    Env *parentEnv; // lexical parents enviroment
};

struct LispisFunction;

struct LispisFunctionObject {
    GcObjectHeader header;
    LispisFunction *function;
    Env *parentEnv;
};

struct ActivationRecord {
    GcObjectHeader header;
    uint32 dataStackBottom;
    uint64 pc;
    ActivationRecord *caller;
    LispisFunction *function;
    Env *enviroment;
    bool calledFromC;
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
    GcObjectHeader header;
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
    GcObjectHeader *firstZeroCountGcObject;
    GcObjectHeader *lastZeroCountGcObject;
    GcObjectHeader *firstGcObject;
    GcObjectHeader *toSweep;
    GcObjectHeader *grayStack;
    bool currentWhite;
    bool sweepPhase;
};

void *callocGcObject(LispisState *state, uint64 size);

uint32 internSymbol(LispisState *state, String symbol, uint64 symHash);

void decRef(LispisState *state, GcObjectHeader *obj);
void incRef(LispisState *state, GcObjectHeader *obj);
void decRef(LispisState *state, Value v);
void incRef(LispisState *state, Value v);
void putInZeroCountTable(LispisState *state, GcObjectHeader *obj);
#endif