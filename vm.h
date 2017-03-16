#ifndef VM_H
#define VM_H
#include "common.h"
#include <cassert>

enum GcObjectType {
    GC_UNDEF, // ERROR to use
    GC_GLOBAL_ENV,
    GC_ENV,
    GC_ACTIVATION_RECORD,
    GC_LISPIS_FUNCTION,
    GC_LISPIS_FUNCTION_OBJECT,
    GC_C_FUNCTION,
    GC_PAIR
};

enum GcColor {
    GC_black = 1 << 0,
    GC_gray = 1 << 1,
    GC_white = 1 << 2,
    GC_frozen = 1 << 3,
};

struct GcObjectHeader {
    uint8 color;
    //uint32 refCount;
    GcObjectHeader *next; // allocated GcObject
    GcObjectHeader *prev; // allocated GcObject
    //GcObjectHeader *nextZCT;
    //GcObjectHeader *prevZCT;
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
    bool filled;
};

struct Env {
    GcObjectHeader header;
    Var *variables;
    uint64 variablesSize;
    Env *parentEnv; // lexical parents enviroment
};

struct GlobalVar {
    Value val;
    uint32 symbolID;
    bool filled;
};

struct GlobalEnv {
    GcObjectHeader header;
    GlobalVar *variables;
    uint64 variablesSize;
    uint64 variablesFilled;
};

struct LispisFunction;

struct Upval {
    Env *env;
    uint64 index;
};

struct LispisFunctionObject {
    GcObjectHeader header;
    Upval *upvals;
    uint32 upvalsSize;
    LispisFunction *function;
    Env *parentEnv;
};

struct ActivationRecord {
    GcObjectHeader header;
    Upval *upvals;
    uint32 upvalsSize;
    uint32 dataStackBottom;
    uint64 pc;
    ActivationRecord *caller;
    LispisFunction *function;
    Env *enviroment;
    bool calledFromC;
};

struct UpvalProto {
    uint64 depth;
    uint64 index;
};

// Used for macros to
struct LispisFunction {
    GcObjectHeader header;
    UpvalProto *upvalProtos;
    uint32 upvalProtosSize;
    uint32 numLocals;
    bool macro;
    Bytecode *bytecode;
    uint64 bytecodeSize;
    uint64 bytecodeTop; // used while compiling only
    LispisFunction **subFunctions;
    uint64 subFunctionsLength;
    bool closedOver;
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

struct Nursery {
    void *mem;
    uint32 size;
    uint32 top;
};

struct ForwardingList {
    void **oldAddr;
    void **newAddr;
    uint32 size;
    uint32 top;
};

struct LispisState {
    GlobalEnv globalEnviroment;
    SymbolTable globalSymbolTable;
    ActivationRecord *currRecord;
    Value *dataStack;
    uint32 dataStackSize;
    uint32 dataStackTop;
    //GcObjectHeader *firstZeroCountGcObject;
    //GcObjectHeader *lastZeroCountGcObject;
    GcObjectHeader *firstGcObject;
    GcObjectHeader *toSweep;
    GcObjectHeader *grayStack;
    bool currentWhite;
    bool sweepPhase;
    uint32 numAllocated;
};

Value cons(LispisState *state, Value car, Value cdr);
Value emptyList();
bool isNill(Value v);

void *callocGcObject(LispisState *state, uint64 size);

uint32 internSymbol(LispisState *state, String symbol, uint64 symHash);

#if 0
void decRef(LispisState *state, GcObjectHeader *obj);
void incRef(LispisState *state, GcObjectHeader *obj);
void decRef(LispisState *state, Value v);
void incRef(LispisState *state, Value v);
void putInZeroCountTable(LispisState *state, GcObjectHeader *obj);
#endif
//Value evalGlobalSymbol(Env *env, uint32 globalSymbolID);
void writeBarrier(LispisState *state, GcObjectHeader *header);
//Value peek(LispisState *state);
//Value pop(LispisState *state);
//void push(LispisState *state, Value v);
//Value indexStack(LispisState *state, int64 i);
Pair *unpackCons(Value v);
CFunction *unpackCFunc(Value v);
LispisFunctionObject *unpackLFunc(Value v);
void freeze(LispisState *state, GcObjectHeader *header);
void unfreeze(LispisState *state, GcObjectHeader *header);
ActivationRecord *allocActivationRecord(LispisState *state,
                                        LispisFunction *func,
                                        Env *parentEnv,
                                        bool topRecord);
void bindFunction(LispisState *state, String symbol,
                  LispisCFunction func);
Value runFunction(LispisState *state, Value funcObjValue, uint64 numArgs);
void clearStack(LispisState *state);
void markAndSweep(LispisState *state);
void destroy(LispisState *state);
//void setVariableRaw(LispisState *state, Env *env,
//Value v, uint32 symbolID);
Value compileNullTerminatedString(LispisState *state, char *str);
void initState(LispisState *state);
Value runNullTerminatedString(LispisState *state, char *str);
Value lookupGlobal(LispisState *state, Value symbol);
Value getGlobal(LispisState *state, uint32 globalSymbolID);
void setGlobal(LispisState *state, Value v, uint32 symbolID);
uint32 internCStr(LispisState *state, const char *cstr);

#define assertStackInBounds(state, value)                               \
    assert((value) >= (state)->currRecord->dataStackBottom)

inline
Value indexStack(LispisState *state, int64 i) {
    if (i >= 0) {
        assertStackInBounds(state,
                            state->currRecord->dataStackBottom + i);
        return state->dataStack[state->currRecord->dataStackBottom+i];
    } else {
        assertStackInBounds(state, state->dataStackTop - i);
        return state->dataStack[state->dataStackTop-i];
    }
}

inline
Value peek(LispisState *state) {
    assert(state->dataStackTop);
    assertStackInBounds(state, state->dataStackTop - 1);
    return state->dataStack[state->dataStackTop-1];
}

inline
Value pop(LispisState *state) {
    assert(state->dataStackTop);
    assertStackInBounds(state, state->dataStackTop - 1);
    state->dataStackTop--;
    Value v = state->dataStack[state->dataStackTop];
    return v;
}

inline
void push(LispisState *state, Value v) {
    assertStackInBounds(state, state->dataStackTop);
    state->dataStack[state->dataStackTop] = v;
    state->dataStackTop++;
}

#undef assertStackInBounds

#endif