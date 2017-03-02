#include "lexer.h"
#include "parser.h"
#include "codegen.h"
#include "compiler_passes.h"
#include "vm.h"

#include <cstdio>
#include <cstdlib>
#include <cassert>
#include <cctype>
#include <cstring>
#include <cmath>


int32 unpackInt(Value v) {
    assert(getType(v) == LISPIS_INT32);
    return v.i32;
}

bool unpackBoolean(Value v) {
    assert(getType(v) == LISPIS_BOOLEAN);
    return (bool)v.ui32;
}

uint32 unpackSymbolID(Value v) {
    assert(getType(v) == LISPIS_SYM_IDX);
    return v.ui32;
}

void *unpackPointer(Value v, NanPackingTypes typeID) {
    uint64 unpacked = v.ui64 & 0xFFFFFFFFFFF;
    assert(getType(v) == typeID);
    // TODO: Do i need this?
    //if (unpacked > 0x00008FFFFFFFFFFF) {
    //unpacked |= 0xFFFF000000000000;
    //}
    return (void *)unpacked;
}


CFunction *unpackCFunc(Value v) {
    return (CFunction *)unpackPointer(v, LISPIS_CFUNC);
}

LispisFunctionObject *unpackLFunc(Value v) {
    return (LispisFunctionObject *)unpackPointer(v, LISPIS_LFUNC);
}

Value emptyList() {
    return nanPackPointer(0, LISPIS_CONS);
}

void setVariableRaw(LispisState *state, Env *env,
                    Value v, uint32 symbolID) {
    if (env->variablesFilled > env->variablesSize*0.7) {
        uint64 oldSize = env->variablesSize;
        Var *oldVariables = env->variables;
        env->variablesSize *= 1.5f;
        env->variables = (Var *)calloc(env->variablesSize,
                                            sizeof(Var));
        for (uint64 i = 0; i < oldSize; ++i) {
            if (oldVariables[i].filled) {
                uint64 variableID = (oldVariables[i].symbolID %
                                     env->variablesSize);
                while (env->variables[variableID].filled) {
                    variableID++;
                    variableID = variableID % env->variablesSize;
                }
                env->variables[variableID] = oldVariables[i];
            }
        }
        free(oldVariables);
    }
    uint32 variableID = symbolID % env->variablesSize;
    while(env->variables[variableID].filled &&
          env->variables[variableID].symbolID != symbolID) {
        variableID++;
        variableID = variableID % env->variablesSize;
    }
    //FIXME ugly hack
    if (env->variables[variableID].filled) {
        decRef(state, env->variables[variableID].val);
    } else {
        env->variablesFilled++;
    }
    incRef(state, v);
    env->variables[variableID].symbolID = symbolID;
    env->variables[variableID].filled = true;
    env->variables[variableID].val = v;
}

uint32 internSymbol(LispisState *state, String symbol, uint64 symHash) {
    SymbolTable *st = &state->globalSymbolTable;
    if (st->symbolsInterned > st->symbolsSize*0.7) {
        uint64 oldSize = st->symbolsSize;
        Symbol *oldSymbols = st->symbols;
        st->symbolsSize *= 1.5f;
        st->symbols = (Symbol *)calloc(st->symbolsSize,
                                            sizeof(Symbol));
        for (uint64 i = 0; i < oldSize; ++i) {
            if (oldSymbols[i].filled) {
                uint64 id = oldSymbols[i].hash % st->symbolsSize;
                while (st->symbols[id].filled) {
                    id++;
                    id = id % st->symbolsSize;
                }
                st->symbols[id] = oldSymbols[i];
            }
        }
        free(oldSymbols);
    }
    uint64 stIndex = symHash % st->symbolsSize;
    while (st->symbols[stIndex].filled) {
        if (symCmp(symbol, symHash,
                   st->symbols[stIndex].symbol,
                   st->symbols[stIndex].hash)) {
            return st->symbols[stIndex].globalSymbolID;
        }
        stIndex++;
        stIndex = stIndex % st->symbolsSize;
    }
    st->symbols[stIndex].symbol = symbol;
    st->symbols[stIndex].globalSymbolID = st->nextGlobalSymbolID;
    st->nextGlobalSymbolID++;
    st->symbols[stIndex].hash = symHash;
    st->symbols[stIndex].filled = true;
    st->symbolsInterned++;
    printf("Interned %.*s as %u\n",
           st->symbols[stIndex].symbol.length,
           st->symbols[stIndex].symbol.val,
           st->symbols[stIndex].globalSymbolID);

    return st->symbols[stIndex].globalSymbolID;
}

void loadLocalSymbolRaw(LispisState *state,
                        LispisFunction *func,
                        String symbol,
                        uint32 symbolID) {
#if 0
    if (func->localToGlobalTableFilled + 1 >=
        func->localToGlobalTableSize) { 
        func->localToGlobalTableSize *= 1.5f;
        func->localToGlobalTable =
            (uint32 *)realloc(func->localToGlobalTable,
                              sizeof(uint32)*
                              func->localToGlobalTableSize);
    }
    assert(func->localToGlobalTableSize > symbolID);
    func->localToGlobalTable[symbolID] = internSymbol(state, symbol,
                                                      hashFunc(symbol));
    func->localToGlobalTableFilled++;
#endif
}

Bytecode *loadSymbolSection(LispisState *state,
                            LispisFunction *func,
                            Bytecode *startBytecode) {
    Bytecode *bytecode = startBytecode;
    uint64 size = bytecode->ui64;
    bytecode++;
    for (uint64 i = 0; i < size; ++i) {
        int32 length = unpackInt(*bytecode);
        bytecode++;
        if (length) {
            uint32 id = bytecode->ui32;
            bytecode++;
            String sym;
            sym.val = bytecode->c;
            sym.length = length;
            loadLocalSymbolRaw(state, func, sym, id);
            bytecode = bytecode + length / 8 + (length % 8 == 0 ? 0 : 1);
        } else {
            assert(false);
        }
    }
    return bytecode;
}

#define assertStackInBounds(state, value)                               \
    assert((value) >= (state)->currRecord->dataStackBottom)

Value peek(LispisState *state) {
    assert(state->dataStackTop);
    assertStackInBounds(state, state->dataStackTop - 1);
    return state->dataStack[state->dataStackTop-1];
}

Value pop(LispisState *state) {
    assert(state->dataStackTop);
    assertStackInBounds(state, state->dataStackTop - 1);
    state->dataStackTop--;
    Value v = state->dataStack[state->dataStackTop];
    return v;
}

void push(LispisState *state, Value v) {
    assertStackInBounds(state, state->dataStackTop);
    state->dataStack[state->dataStackTop] = v;
    state->dataStackTop++;
}

Pair *unpackCons(Value v) {
    return (Pair *)unpackPointer(v, LISPIS_CONS);
}

void decRef(LispisState *state, Value v) {
    switch (getType(v)) {
        case LISPIS_CFUNC: {
            decRef(state, (GcObjectHeader *)unpackCFunc(v));
        } break;
        case LISPIS_LFUNC: {
            decRef(state, (GcObjectHeader *)unpackLFunc(v));
        } break;
        case LISPIS_CONS: {
            GcObjectHeader *header = (GcObjectHeader *)unpackCons(v);
            if (header) {
                decRef(state, header);
            }
        } break;
        default:break;
    }
}
void incRef(LispisState *state, Value v) {
    switch (getType(v)) {
        case LISPIS_CFUNC: {
            incRef(state, (GcObjectHeader *)unpackCFunc(v));
        } break;
        case LISPIS_LFUNC: {
            incRef(state, (GcObjectHeader *)unpackLFunc(v));
        } break;
        case LISPIS_CONS: {
            GcObjectHeader *header = (GcObjectHeader *)unpackCons(v);
            if (header) {
                incRef(state, header);
            }
        } break;
        default:break;
    }
}

Value cons(LispisState *state, Value ncar, Value ncdr) {
    Pair *pair = (Pair *)callocGcObject(state, sizeof(Pair));
    pair->header.refCount = 0;
    pair->header.type = GC_PAIR;
    putInZeroCountTable(state, (GcObjectHeader *)pair);
    incRef(state, ncar);
    incRef(state, ncdr);
    pair->car = ncar;
    pair->cdr = ncdr;
    return nanPackPointer(pair, LISPIS_CONS);
}

typedef uint64 SymbolID;

Value evalGlobalSymbol(Env *env, uint32 globalSymbolID) {
    assert(env && "Variable undefined");
    uint32 variableID = globalSymbolID % env->variablesSize;
    uint32 startID = variableID;
    while (env->variables[variableID].symbolID != globalSymbolID) {
        variableID++;
        variableID = variableID % env->variablesSize;
        if (variableID == startID) {
            break;
        }
    }
    if (!env->variables[variableID].filled) {
        return evalGlobalSymbol(env->parentEnv, globalSymbolID);
    }
    return env->variables[variableID].val;
}

#if 0 // maybe later
LispisFunction *loadFunction(LispisState *state,
                             Bytecode *startBytecode) {
    Bytecode *bytecode = startBytecode;
    LispisFunction *ret =
        (LispisFunction *)calloc(1, sizeof(LispisFunction));
    //ret->localToGlobalTableSize = 64;
    //ret->localToGlobalTable =
    //(uint32 *)calloc(ret->localToGlobalTableSize, sizeof(uint32));
    uint64 numSubFunctions = bytecode->ui64;
    bytecode++;
    if (numSubFunctions) {
        ret->subFunctions =
            (LispisFunction **)malloc(sizeof(LispisFunction *) *
                                      numSubFunctions);
    }
    ret->subFunctionsLength = numSubFunctions;
    for (uint64 i = 0; i < numSubFunctions; ++i) {
        ret->subFunctions[i] = loadFunction(state,
                                            bytecode);
        bytecode =
            ret->subFunctions[i]->bytecode +
            ret->subFunctions[i]->bytecodeSize;
    }
    bytecode = loadSymbolSection(state, ret, bytecode);
    ret->bytecodeSize = bytecode->ui64;
    bytecode++;
    ret->bytecode = bytecode;
    return ret;
}
#endif

String globalSymbolIdToSymbol(SymbolTable *globalSymbolTable,
                              uint32 globalSymbolId) {
    //FIXME!!!
    for (uint32 i = 0; i < globalSymbolTable->symbolsSize; ++i) {
        if (globalSymbolTable->symbols[i].filled &&
            globalSymbolTable->symbols[i].globalSymbolID ==
            globalSymbolId) {
            return globalSymbolTable->symbols[i].symbol;
        }
    }
    // Not used in local symbol table, maybe in global?
    assert(false);
    return String{0, 0};
}

void printValue(SymbolTable *globalSymbolTable,
                LispisFunction *func,
                Value v) {
    switch (getType(v)) {
        case LISPIS_INT32: {
            printf("%d\n", unpackInt(v));
        } break;
        case LISPIS_SYM_IDX: {
            String sym = globalSymbolIdToSymbol(globalSymbolTable,
                                                v.ui32);
            printf("%.*s\n", sym.length, sym.val);
        } break;
        case LISPIS_DOUBLE: {
            printf("%f\n", v.f64);
        } break;
        case LISPIS_CONS: {
            Pair *p = unpackCons(v);
            if (p) {
                printf("(\n");
                printValue(globalSymbolTable, func, p->car);
                printf(".\n");
                printValue(globalSymbolTable, func, p->cdr);
                printf(")\n");
            } else {
                printf("()\n");
            }
        } break;
            //case NAN_PACKINT_USERP: {
            //} break;
        case LISPIS_CFUNC: {
            CFunction *cfunc = unpackCFunc(v);
            printf("<#CFunction %p>\n", cfunc);
        } break;
        case LISPIS_LFUNC: {
            LispisFunctionObject *lfunc = unpackLFunc(v);
            printf("<#Function %p>\n", lfunc);
        } break;
            //case NAN_PACKING_LFUNC: {
            //} break;
        case LISPIS_UNDEF:
        default:assert(false);
    }
}

LispisFunctionObject *allocFunctionObject(LispisState *state,
                                          LispisFunction *func,
                                          Env *parentEnv) {
    LispisFunctionObject *funcObj =
        (LispisFunctionObject *)callocGcObject(state,
                                               sizeof(LispisFunctionObject));
    funcObj->header.type = GC_LISPIS_FUNCTION_OBJECT;
    putInZeroCountTable(state, &funcObj->header);
    incRef(state, &func->header);
    funcObj->function = func;
    incRef(state, &parentEnv->header);
    funcObj->parentEnv = parentEnv;
    return funcObj;
}


ActivationRecord *allocActivationRecord(LispisState *state,
                                        LispisFunction *func,
                                        Env *parentEnv) {
    ActivationRecord *record =
        (ActivationRecord *)callocGcObject(state,
                                           sizeof(ActivationRecord));
    record->header.type = GC_ACTIVATION_RECORD;
    putInZeroCountTable(state, (GcObjectHeader *)record);
    if (func) {
        incRef(state, (GcObjectHeader *)func);
    }
    if (parentEnv) {
        incRef(state, (GcObjectHeader *)parentEnv);
    }
    record->function = func;
    if (parentEnv) {
        record->enviroment = (Env *)callocGcObject(state, sizeof(Env));
        record->enviroment->header.type = GC_ENV;
        incRef(state, (GcObjectHeader *)record->enviroment);
        record->enviroment->parentEnv = parentEnv;
        record->enviroment->variablesSize = 256;
        record->enviroment->variables =
            (Var *)calloc(1,
                          sizeof(Var) *
                          record->enviroment->variablesSize);
    }
    return record;
}

ActivationRecord *allocActivationRecord(LispisState *state,
                                        LispisFunctionObject *funcObj) {
    return allocActivationRecord(state, funcObj->function,
                                 funcObj->parentEnv);
}

void putInZeroCountTable(LispisState *state, GcObjectHeader *obj) {
    assert(obj->type != GC_UNDEF);
    assert(!obj->refCount);
    obj->nextZCT = 0;
    obj->prevZCT = state->lastZeroCountGcObject;
    if (!state->firstZeroCountGcObject) {
        state->firstZeroCountGcObject = obj;
    }
    if (obj->prevZCT) {
        obj->prevZCT->nextZCT = obj;
    }
    state->lastZeroCountGcObject = obj;
}

ActivationRecord *copyActivationRecord(LispisState *state,
                                       ActivationRecord *record) {
    ActivationRecord *ret =
        (ActivationRecord *)callocGcObject(state,
                                           sizeof(ActivationRecord));
    ret->header.type = GC_ACTIVATION_RECORD;
    ret->function = record->function;
    ret->enviroment = record->enviroment;
    incRef(state, (GcObjectHeader *)ret->enviroment);
    putInZeroCountTable(state, (GcObjectHeader *)ret);
    return ret;
}

void decRef(LispisState *state, GcObjectHeader *obj) {
    if (!(obj->color & GC_frozen)) {
        obj->refCount--;
        if (!obj->refCount) {
            putInZeroCountTable(state, obj);
        }
    }
}

void incRef(LispisState *state, GcObjectHeader *obj) {
    assert(obj->type);
    if (!(obj->color & GC_frozen)) {
        if (obj->refCount == 0) {
            if (state->lastZeroCountGcObject == obj) {
                if (obj->prevZCT) {
                    obj->prevZCT->nextZCT = obj->nextZCT;
                } else {
                    assert(state->firstZeroCountGcObject ==
                           state->lastZeroCountGcObject);
                }
                state->lastZeroCountGcObject = obj->prevZCT;
            } else if (obj->nextZCT) {
                obj->nextZCT->prevZCT = obj->prevZCT;
            }
            if (state->firstZeroCountGcObject == obj) {
                state->firstZeroCountGcObject = obj->nextZCT;
            } else if (obj->prevZCT) {
                obj->prevZCT->nextZCT = obj->nextZCT;
            }
            obj->prevZCT = 0;
            obj->nextZCT = 0;
        }
        obj->refCount++;
    }
}

void popActivationRecord(LispisState *state) {
    state->currRecord = state->currRecord->caller;
}

Value lookupGlobal(LispisState *state, Value symbol) {
    SymbolID id = unpackSymbolID(symbol);
    Value result =
        evalGlobalSymbol(&state->globalEnviroment, id);
    return result;
}

Value symbolFromString(LispisState *state, String str) {
    return nanPackSymbolIdx(internSymbol(state, str, hashFunc(str)));
}

Value runFunction(LispisState *state,
                  Value funcObjValue, uint64 numArgs) {
    assert(numArgs <= state->dataStackTop);
    LispisFunctionObject *funcObj = unpackLFunc(funcObjValue);
    {
        ActivationRecord *record =
            allocActivationRecord(state, funcObj);
        decRef(state, &funcObj->header);
        record->caller = state->currRecord;
        record->calledFromC = true;
        record->dataStackBottom = state->dataStackTop - numArgs;
        state->currRecord = record;
    }
    push(state, nanPackInt32(numArgs));
    LispisFunction *func = 0;
 CALL:
    func = state->currRecord->function;
    while(func->bytecode[state->currRecord->pc].opCode != OP_EXIT) {
        assert(state->currRecord->pc < func->bytecodeSize);
        OpCodes op =
            func->bytecode[state->currRecord->pc].opCode;
        state->currRecord->pc++;
        switch (op) {
            case OP_EVAL_SYMBOL: {
                SymbolID id = unpackSymbolID(pop(state));
                // FIXME: global variables
                Value result =
                    evalGlobalSymbol(state->currRecord->enviroment, id);
                push(state, result);
            } break;
            case OP_RETURN: {
                Value ret = pop(state);
                state->dataStackTop =
                    state->currRecord->dataStackBottom;
                if (!state->currRecord->calledFromC) {
                    push(state, ret);
                    popActivationRecord(state);
                    func = state->currRecord->function;
                } else {
                    popActivationRecord(state);
                    return ret;
                }
            } break;
            case OP_PUSH: {
                Value p;
                p = func->bytecode[state->currRecord->pc];
                state->currRecord->pc++;
                push(state, p);
            } break;
            case OP_LIST: {
                int32 numElems = unpackInt(pop(state));
                Value head = emptyList();
                for (int32 i = 0; i < numElems; ++i) {
                    head = cons(state, pop(state), head);
                }
                push(state, head);
            } break;
            case OP_CALL: {
                uint64 numArgs =
                    func->bytecode[state->currRecord->pc].ui64;
                state->currRecord->pc++;
                Value callee = pop(state);
                if (getType(callee) == LISPIS_CFUNC) {
                    ActivationRecord *record =
                        allocActivationRecord(state,
                                              0,
                                              &state->globalEnviroment);
                    record->caller = state->currRecord;
                    record->dataStackBottom =
                        state->dataStackTop-numArgs;
                    record->function = 0;
                    state->currRecord = record;
                    CFunction *f = unpackCFunc(callee);
                    bool returned = f->func(state, numArgs);
                    if (returned) {
                        Value retVal = pop(state);
                        state->dataStackTop =
                            state->currRecord->dataStackBottom;
                        popActivationRecord(state);
                        push(state, retVal);
                    } else {
                        state->dataStackTop =
                            state->currRecord->dataStackBottom;
                        popActivationRecord(state);
                    }
                } else if (getType(callee) == LISPIS_LFUNC) {
                    LispisFunctionObject *lfuncObj = unpackLFunc(callee);
                    ActivationRecord *record =
                        allocActivationRecord(state, lfuncObj);
                    record->dataStackBottom = state->dataStackTop-numArgs;
                    record->caller = state->currRecord;
                    state->currRecord = record;
                    push(state, nanPackInt32(numArgs));
                    goto CALL;
                } else {
                    assert(!"Can't call this type\n");
                }
            } break;
            case OP_PUSH_LAMBDA_ID: {
                uint64 id = func->bytecode[state->currRecord->pc].ui64;
                state->currRecord->pc++;
                LispisFunctionObject *funcObj =
                    allocFunctionObject(state, func->subFunctions[id],
                                        state->currRecord->enviroment);
                push(state, nanPackPointer(funcObj,
                                           LISPIS_LFUNC));
            } break;
            case OP_POP_ASSERT_EQUAL: {
                Value a = pop(state);
                Value b = pop(state);
                assert(a.ui64 == b.ui64);
            } break;
            case OP_SET_LOCAL_VARIABLE: {
                uint32 symId = unpackSymbolID(pop(state));
                Value v = pop(state);
                setVariableRaw(state, state->currRecord->enviroment,
                               v, symId);
            } break;
            case OP_SET_GLOBAL_VARIABLE: {
                uint32 symId = unpackSymbolID(pop(state));
                Value v = pop(state);
                setVariableRaw(state, &state->globalEnviroment,
                               v, symId);
            } break;
            case OP_CLEAR_STACK: {
                state->dataStackTop = state->currRecord->dataStackBottom;
            } break;
            case OP_JUMP_IF_TRUE: {
                bool pred = unpackBoolean(pop(state));
                int64 target = func->bytecode[state->currRecord->pc].i64;
                state->currRecord->pc++;
                if (pred) {
                    state->currRecord->pc += target;
                }
            } break;
            case OP_JUMP: {
                int64 target = func->bytecode[state->currRecord->pc].i64;
                state->currRecord->pc++;
                state->currRecord->pc += target;
            } break;
            default:assert(false);
        }
    }
    assert(false);
}

void bindFunction(LispisState *state,
                  String symbol,
                  LispisCFunction func) {
    CFunction *binding = (CFunction *)callocGcObject(state,
                                                     sizeof(CFunction));
    binding->header.refCount = 0;
    // We could skip putting it in zct but that would trigger asserts
    binding->header.type = GC_C_FUNCTION;
    putInZeroCountTable(state, (GcObjectHeader *)binding);
    binding->func = func;
    Value funcVal;
    funcVal = nanPackPointer(binding, LISPIS_CFUNC);
    uint32 symbolID = internSymbol(state,
                                   symbol,
                                   hashFunc(symbol));
    setVariableRaw(state, &state->globalEnviroment, funcVal, symbolID);
}

double lispisAddDouble(LispisState *state, uint64 argsLeft) {
    double ret = 0;
    for (uint64 i = 0; i < argsLeft; ++i) {
        Value operand = pop(state);
        if (getType(operand) == LISPIS_DOUBLE) {
            ret += operand.f64;
        } else {
            ret += unpackInt(operand);
        }
    }
    return ret;
}

bool lispisAdd(LispisState *state, uint64 numArgs) {
    assert(numArgs >= 1);
    int ret = 0;
    uint64 i;
    for (i = 0; i < numArgs; ++i) {
        if (getType(peek(state)) != LISPIS_INT32) {
            break;
        }
        Value operand = pop(state);
        ret += unpackInt(operand);
    }
    Value retValue;
    if (i < numArgs) {

        retValue.f64 = ret + lispisAddDouble(state, numArgs-i);
    } else {
        retValue = nanPackInt32(ret);
    }
    push(state, retValue);
    return true;
}

bool lispisToFloat(LispisState *state, uint64 numArgs) {
    Value ret;
    assert(numArgs == 1);
    if (getType(peek(state))) {
        ret.f64 = unpackInt(pop(state));
    } else {
        assert(getType(peek(state)) == LISPIS_DOUBLE);
        ret = pop(state);
    }
    push(state, ret);
    return true;
}

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

bool lispisSub(LispisState *state, uint64 numArgs) {
    assert(numArgs >= 1);
    Value retValue;
    retValue.f64 = 0;

    if (numArgs == 1) {
        Value val = pop(state);
        retValue.f64 = (getType(val) == LISPIS_DOUBLE ?
                        -val.f64 :
                        nanPackInt32(-unpackInt(val)).f64);
    } else {
        bool allInt = true;
        for (uint64 i = 0; i < numArgs; ++i) {
            if (getType(indexStack(state, i)) == LISPIS_DOUBLE) {
                allInt = false;
                break;
            } else {
                assert(getType(indexStack(state, i)) == LISPIS_INT32);
            }
        }
        if (allInt) {
            int pos = 0;
            int neg = 0;
            for (uint32 i = 1; i < numArgs; ++i) {
                neg += unpackInt(pop(state));
            }
            pos = unpackInt(pop(state));
            retValue = nanPackInt32(pos - neg);
        } else {
            double pos = 0;
            double neg = 0;
            for (uint32 i = 1; i < numArgs; ++i) {
                if (getType(peek(state)) == LISPIS_INT32) {
                    neg += unpackInt(pop(state));
                } else {
                    neg += pop(state).f64;
                }
            }
            if (getType(peek(state)) == LISPIS_INT32) {
                pos = unpackInt(pop(state));
            } else {
                pos = pop(state).f64;
            }
            retValue.f64 = pos - neg;
        }
    }
    push(state, retValue);
    return true;
}

bool lispisMul(LispisState *state, uint64 numArgs) {
    assert(numArgs >= 1);
    bool allInt = true;
    for (uint64 i = 0; i < numArgs; ++i) {
        if (getType(indexStack(state, i)) == LISPIS_DOUBLE) {
            allInt = false;
            break;
        } else {
            assert(getType(indexStack(state, i)) == LISPIS_INT32);
        }
    }
    if (allInt) {
        int ret = 1;
        for (uint32 i = 0; i < numArgs; ++i) {
            ret *= unpackInt(pop(state));
        }
        Value retValue;
        retValue = nanPackInt32(ret);
        push(state, retValue);
    } else {
        double ret = 1;
        for (uint32 i = 0; i < numArgs; ++i) {
            ret *= pop(state).f64;
        }
        Value retValue;
        retValue.f64 = ret;
        push(state, retValue);
    }
    return true;
}

bool lispisIDiv(LispisState *state, uint64 numArgs) {
    assert(numArgs == 2);
    int denominator = unpackInt(pop(state));
    int nominator = unpackInt(pop(state));
    Value retValue;
    retValue = nanPackInt32(nominator/denominator);
    push(state, retValue);
    return true;
}

bool lispisDiv(LispisState *state, uint64 numArgs) {
    assert(numArgs == 2);
    Value denominator = pop(state);
    Value nominator = pop(state);
    Value retValue;
    if (getType(denominator) == LISPIS_INT32 &&
        getType(nominator) == LISPIS_INT32) {
        int denom = unpackInt(denominator);
        int nom = unpackInt(nominator);
        if ((nom/denom)*denom != nom) {
            double dDenom = (double)denom;
            double dNom = (double)nom;
            retValue.f64 = dNom/dDenom;
        } else {
            retValue = nanPackInt32(nom/denom);
        }
    } else if (getType(denominator) == LISPIS_DOUBLE &&
               getType(nominator) == LISPIS_DOUBLE) {
        retValue.f64 = nominator.f64/denominator.f64;
    } else {
        double denom = 0;
        double nom = 0;
        if (getType(denominator) == LISPIS_DOUBLE) {
            denom = denominator.f64;
        } else {
            denom = (double)unpackInt(denominator);
        }
        if (getType(nominator) == LISPIS_DOUBLE) {
            nom = nominator.f64;
        } else {
            nom = (double)unpackInt(nominator);
        }
        retValue.f64 = nom/denom;
    }
    push(state, retValue);
    return true;
}

bool lispisLess(LispisState *state, uint64 numArgs) {
    assert(numArgs == 2);
    Value rh = pop(state);
    Value lh = pop(state);
    bool ret = false;
    if (getType(rh) == LISPIS_DOUBLE && getType(lh) == LISPIS_DOUBLE) {
        ret = lh.f64 < rh.f64;
    } else if (getType(rh) == LISPIS_INT32 &&
               getType(lh) == LISPIS_INT32) {
        ret = unpackInt(lh) < unpackInt(rh);
    } else if (getType(rh) == LISPIS_DOUBLE &&
               getType(lh) == LISPIS_INT32) {
        ret = unpackInt(lh) < rh.f64;
    } else if (getType(rh) == LISPIS_INT32 &&
               getType(lh) == LISPIS_DOUBLE) {
        ret = lh.f64 < unpackInt(rh);
    } else {
        assert(false && "Comparison only works on ints and doubles");
    }
    push(state, nanPackBoolean(ret));
    return true;
}

void dumpEnviroment(SymbolTable *globalSymbolTable,
                    LispisFunction *func,
                    Env *env) {
    if (!env) {
        return;
    }
    for (uint32 i = 0; i < env->variablesSize; ++i) {
        if (env->variables[i].filled) {
            Value symId;
            symId = nanPackSymbolIdx(env->variables[i].symbolID);
            printValue(globalSymbolTable, func, symId);
            printf("==\n");
            printValue(globalSymbolTable, func,
                       env->variables[i].val);
        }
    }
    dumpEnviroment(globalSymbolTable, func, env->parentEnv);
}

#define arrayLength(array) (sizeof(array)/sizeof(*array))

void decRefAllInEnv(LispisState *state, Env *env) {
    for (uint64 i = 0, f = 0; f < env->variablesFilled; i++) {
        if (env->variables[i].filled) {
            f++;
            decRef(state, env->variables[i].val);
        }
    }
}

void destroy(LispisState *state, GcObjectHeader *header) {
    //printf("deallocating ");
    switch (header->type) {
        case GC_ENV: {
            //printf("Env ");
            Env *e = (Env *)header;
            free(e->variables);
        } break;
        case GC_ACTIVATION_RECORD: {
            //printf("ActivationRecord ");
        } break;
        case GC_LISPIS_FUNCTION: {
            //printf("LispisFunction ");
            LispisFunction *f = (LispisFunction *)header;
            free(f->subFunctions);
            free(f->bytecode);
        } break;
        case GC_PAIR: {
            //printf("Pair ");
        } break;
        case GC_C_FUNCTION: {
            //printf("CFunction ");
        } break;
        case GC_LISPIS_FUNCTION_OBJECT: {
            //printf("LispisFunctionObject ");
        } break;
        default:assert(false);
    }
    //printf("%p\n", header);
    if (header == state->firstGcObject) {
        state->firstGcObject = header->next;
    }
    if (header->prev) {
        header->prev->next = header->next;
    }
    if (header->next) {
        header->next->prev = header->prev;
    }
    if (state->lastZeroCountGcObject == header) {
        state->lastZeroCountGcObject = header->prevZCT;
    }
    if (state->firstZeroCountGcObject == header) {
        state->firstZeroCountGcObject = header->nextZCT;
    }
    if (header->prevZCT) {
        header->prevZCT->nextZCT = header->nextZCT;
    }
    if (header->nextZCT) {
        header->nextZCT->prevZCT = header->prevZCT;
    }
    if (header == state->toSweep) {
        state->toSweep = state->toSweep->next;
    }
    free(header);
}

void decRefAllReferedToBy(LispisState *state, GcObjectHeader *header) {
    switch (header->type) {
        case GC_LISPIS_FUNCTION_OBJECT: {
            LispisFunctionObject *lfo
                = (LispisFunctionObject *)header;
            decRef(state, &lfo->function->header);
            decRef(state, &lfo->parentEnv->header);
        } break;
        case GC_ENV: {
            Env *e = (Env *)header;
            decRefAllInEnv(state, e);
            decRef(state, (GcObjectHeader *)e->parentEnv);
        } break;
        case GC_ACTIVATION_RECORD: {
            ActivationRecord *ar = (ActivationRecord *)header;
            if (ar->function) {
                decRef(state, (GcObjectHeader *)ar->function);
            }
            decRef(state, (GcObjectHeader *)ar->enviroment);
        } break;
        case GC_LISPIS_FUNCTION: {
            LispisFunction *f = (LispisFunction *)header;
            for (uint64 i = 0; i < f->subFunctionsLength; i++) {
                decRef(state, (GcObjectHeader *)f->subFunctions[i]);
            }
        } break;
        case GC_PAIR: {
            Pair *p = (Pair *)header;
            decRef(state, p->car);
            decRef(state, p->cdr);
        } break;
        case GC_C_FUNCTION: {
        } break;
        default:assert(false);
    }
}

// Enviroments leak...
void gc(LispisState *state) {
    printf("Gc start\n");
    if (state->currRecord == 0) {
        for (uint64 i = 0, f = 0;
             f < state->globalEnviroment.variablesFilled; i++) {
            if (state->globalEnviroment.variables[i].filled) {
                f++;
                incRef(state, state->globalEnviroment.variables[i].val);
            }
        }
        int numCollected = 0;
        for (GcObjectHeader *header = state->firstZeroCountGcObject;
             header && numCollected < 25;
             header = state->firstZeroCountGcObject) {
            numCollected++;
            state->firstZeroCountGcObject =
                state->firstZeroCountGcObject->nextZCT;
            decRefAllReferedToBy(state, header);
            destroy(state, header);
        }
        decRefAllInEnv(state, &state->globalEnviroment);
    }
    printf("Gc end\n");
}

void freeze(LispisState *state, GcObjectHeader *header) {
    if (!(header->color & GC_frozen)) {
        if (header != state->grayStack) {
            if (header == state->toSweep) {
                state->toSweep = state->toSweep->next;
            }
            if (header == state->firstGcObject) {
                state->firstGcObject = state->firstGcObject->next;
            }
            if (header->next) {
                header->next->prev = header->prev;
            }
            if (header->prev) {
                header->prev->next = header->next;
            }
            header->next = state->grayStack;
            if (header->next) {
                header->next->prev = header;
            }
            header->prev = 0;
            state->grayStack = header;
        }
    } else {
        //WARNING
    }
    header->color |= GC_frozen | GC_gray;
}

void unfreeze(LispisState *state, GcObjectHeader *header) {
    if (!(header->color & GC_frozen)) {
        //WARNING
    }
    header->color &= ~GC_frozen;
}

// does not free state, only the pointers inside
void destroy(LispisState *state) {
    printf("destroying state\n");
    if (state->globalEnviroment.header.next) {
        state->globalEnviroment.header.next->prev =
            state->globalEnviroment.header.prev;
    }
    if (state->globalEnviroment.header.prev) {
        state->globalEnviroment.header.prev->next =
            state->globalEnviroment.header.next;
    }
    GcObjectHeader *next = state->firstGcObject;
    while (next) {
        assert(next != next->next);
        GcObjectHeader *header = next;
        next = next->next;
        if (header != &state->globalEnviroment.header) {
            destroy(state, header);
        }
    };
    next = state->grayStack;
    while (next) {
        assert(next != next->next);
        GcObjectHeader *header = next;
        next = next->next;
        if (header != &state->globalEnviroment.header) {
            destroy(state, header);
        }
    };
    //decRefAllInEnv(state, &state->globalEnviroment);
    //for (GcObjectHeader *header = state->firstZeroCountGcObject;
    //header; header = state->firstZeroCountGcObject) {
    //state->firstZeroCountGcObject =
    //state->firstZeroCountGcObject->nextZCT;
    //dealloc(state, header);
    //}
    free(state->globalSymbolTable.symbols);
    // they are malloced as one block
    //free(state->globalEnviroment.variables);
    free(state->dataStack);
}

void *callocGcObject(LispisState *state, uint64 size) {
    GcObjectHeader *ret = (GcObjectHeader *)calloc(1, size);
    ret->next = state->firstGcObject;
    if (ret->next) {
        ret->next->prev = ret;
    }
    assert(!ret->next || ret->next->prev == ret);
    state->firstGcObject = ret;
    return ret;
}

GcObjectHeader *headerFromValue(Value v) {
    GcObjectHeader *ret = 0;
    switch (getType(v)) {
        case LISPIS_CFUNC: {
            ret = (GcObjectHeader *)unpackCFunc(v);
        } break;
        case LISPIS_LFUNC: {
            ret = (GcObjectHeader *)unpackLFunc(v);
        } break;
        case LISPIS_CONS: {
            ret = (GcObjectHeader *)unpackCons(v);
        } break;
        default:break;
    }
    if (ret) {
        assert(!ret->next || ret->next->prev == ret);
        assert(!ret->prev || ret->prev->next == ret);
    }
    return ret;
}

void markGray(LispisState *state, GcObjectHeader *obj) {
    if (obj && !((obj->color & GC_gray) || (obj->color & GC_black))) {
        assert(!obj->next || obj->next->prev == obj);
        assert(!obj->prev || obj->prev->next == obj);
        obj->color |= GC_gray;
        if (obj == state->firstGcObject) {
            state->firstGcObject = obj->next;
        }
        if (obj->next) {
            obj->next->prev = obj->prev;
        }
        if (obj->prev) {
            obj->prev->next = obj->next;
        }
        obj->next = state->grayStack;
        obj->prev = 0;
        if (obj->next) {
            obj->next->prev = obj;
        }
        state->grayStack = obj;
    }
}

bool markStep(LispisState *state) {
    GcObjectHeader *obj = state->grayStack;
    if (obj) {
        assert(!obj->prev);
        assert(!obj->next || obj->next->prev == obj);
        assert(!obj->prev || obj->prev->next == obj);
        state->grayStack = state->grayStack->next;
        if (state->grayStack) {
            state->grayStack->prev = 0;
        }
        obj->prev = 0;
        obj->next = state->firstGcObject;
        state->firstGcObject = obj;
        if (obj->next) {
            obj->next->prev = obj;
        }
        obj->color |= GC_black;
        switch (obj->type) {
            case GC_ENV: {
                Env *e = (Env *)obj;
                if (e->parentEnv) {
                    markGray(state,
                             (GcObjectHeader *)e->parentEnv);
                }
                for (uint32 i = 0, f = 0;
                     f < e->variablesFilled; ++i) {
                    if (e->variables[i].filled) {
                        f++;
                        GcObjectHeader *v =
                            headerFromValue(e->variables[i].val);
                        markGray(state, v);
                    }
                }
            } break;
            case GC_ACTIVATION_RECORD: {
                ActivationRecord *ar = (ActivationRecord *)obj;
                markGray(state, (GcObjectHeader *)ar->enviroment);
                markGray(state, (GcObjectHeader *)ar->function);
                markGray(state, (GcObjectHeader *)ar->caller);
            } break;
            case GC_LISPIS_FUNCTION_OBJECT: {
                LispisFunctionObject *lfo =
                    (LispisFunctionObject *)obj;
                markGray(state, (GcObjectHeader *)lfo->parentEnv);
                markGray(state, (GcObjectHeader *)lfo->function);
            } break;
            case GC_LISPIS_FUNCTION: {
                LispisFunction *f = (LispisFunction *)obj;
                for (uint64 i = 0; i < f->subFunctionsLength; ++i) {
                    markGray(state,
                             (GcObjectHeader *)f->subFunctions[i]);
                }
            } break;
            case GC_PAIR: {
                Pair *p = (Pair *)obj;
                markGray(state, headerFromValue(p->car));
                markGray(state, headerFromValue(p->cdr));
            } break;
            case GC_C_FUNCTION: {
            } break;
            default:assert(false);
        }
    }
    return state->grayStack != 0;
}

bool sweepStep(LispisState *state) {
    GcObjectHeader *curr = state->toSweep;
    if (curr) {
        state->toSweep = state->toSweep->next;
        if ((curr->color & (GC_white * (!state->currentWhite))) &&
            !(curr->color & GC_black) && !(curr->color & GC_frozen)) {
            destroy(state, curr);
        } else {
            if (curr->color & GC_frozen) {
                curr->color = GC_frozen;
                curr->color |= GC_gray;
                if (curr->next) {
                    curr->next->prev = curr->prev;
                }
                if (curr->prev) {
                    curr->prev->next = curr->next;
                }
                curr->next = state->grayStack;
                if (curr->next) {
                    curr->next->prev = curr;
                }
                curr->prev = 0;
                state->grayStack = curr;
            } else {
                curr->color = GC_white * state->currentWhite;
                if (curr != state->firstGcObject) {
                    if (curr->next) {
                        curr->next->prev = curr->prev;
                    }
                    if (curr->prev) {
                        curr->prev->next = curr->next;
                    }
                    curr->next = state->firstGcObject;
                    if (curr->next) {
                        curr->next->prev = curr;
                    }
                    state->firstGcObject = curr;
                    curr->prev = 0;
                }
            } 
        }
    }
    return curr != 0;
}

void markAndSweep(LispisState *state) {
    if (state->sweepPhase) {
        for (int i = 0; i < 200; ++i) {
            state->sweepPhase = sweepStep(state);
            if (!state->sweepPhase) {
                //printf("sweep end\n");
                assert(state->grayStack);
                break;
            }
        }
    } else {
        for (int i = 0; i < 200; ++i) {
            state->sweepPhase = !markStep(state);
            if (state->sweepPhase) {
                //printf("mark end\n");
                state->currentWhite = !state->currentWhite;
                state->toSweep = state->firstGcObject;
                break;
            }
        }
    }
}

void clearStack(LispisState *state) {
    state->dataStackTop = state->currRecord->dataStackBottom;
}

int main(int argsc, char **args) {
    assert(sizeof(Bytecode) == sizeof(uint64));

    // CompileTime
    LispisState state = {};
    state.globalEnviroment.header.type = GC_ENV;
    state.globalEnviroment.parentEnv = 0;
    state.globalSymbolTable.symbolsSize = 256;
    state.globalEnviroment.variablesSize = 256;
    state.globalSymbolTable.symbols =
        (Symbol *)calloc(1,
                         sizeof(Symbol) *
                         state.globalSymbolTable.symbolsSize +
                         sizeof(Var) *
                         state.globalEnviroment.variablesSize);
    state.globalEnviroment.variables =
        (Var *)(state.globalSymbolTable.symbols +
                state.globalSymbolTable.symbolsSize);
    state.dataStackSize = 1024;
    state.dataStack = (Value *)malloc(state.dataStackSize*sizeof(uint64));
    freeze(&state, &state.globalEnviroment.header);
    state.currRecord = allocActivationRecord(&state, 0, 0);
    // TODO: make the currRecord and the dataStack up to
    // dataStackTop roots
    // then we dont have to freeze it
    freeze(&state, &state.currRecord->header);



    //Bytecode *bytecode = compactBytecode(&compiler);






    // standard library

    String quoteStr;
    quoteStr.val = (char *)"quote";
    quoteStr.length = strlen(quoteStr.val);
    quote = internSymbol(&state, quoteStr, hashFunc(quoteStr));
    quoteSym.exprType = EXPR_SYMBOL;
    quoteSym.str = quoteStr;
    String lambdaStr;
    lambdaStr.val = (char *)"lambda";
    lambdaStr.length = strlen(lambdaStr.val);
    lambda = internSymbol(&state, lambdaStr, hashFunc(lambdaStr));
    String letStr;
    letStr.val = (char *)"let!";
    letStr.length = strlen(letStr.val);
    let = internSymbol(&state, letStr, hashFunc(letStr));
    String defineStr;
    defineStr.val = (char *)"define!";
    defineStr.length = strlen(defineStr.val);
    define = internSymbol(&state, defineStr, hashFunc(defineStr));
    String ifStr;
    ifStr.val = (char *)"if";
    ifStr.length = strlen(ifStr.val);
    ifSym = internSymbol(&state, ifStr, hashFunc(ifStr));


    String addSymbol;
    addSymbol.val = (char *)"+";
    addSymbol.length = 1;
    bindFunction(&state, addSymbol, lispisAdd);
    
    String toFloatSymbol;
    toFloatSymbol.val = (char *)"to-float";
    toFloatSymbol.length = strlen(toFloatSymbol.val);
    bindFunction(&state, toFloatSymbol, lispisToFloat);
    
    String subSymbol;
    subSymbol.val = (char *)"-";
    subSymbol.length = 1;
    bindFunction(&state, subSymbol, lispisSub);
    
    String mulSymbol;
    mulSymbol.val = (char *)"*";
    mulSymbol.length = 1;
    bindFunction(&state, mulSymbol, lispisMul);
    
    String idivSymbol;
    idivSymbol.val = (char *)"//";
    idivSymbol.length = 2;
    bindFunction(&state, idivSymbol, lispisIDiv);
    
    String divSymbol;
    divSymbol.val = (char *)"/";
    divSymbol.length = 1;
    bindFunction(&state, divSymbol, lispisDiv);
    
    String lessSymbol;
    lessSymbol.val = (char *)"<";
    lessSymbol.length = 1;
    bindFunction(&state, lessSymbol, lispisLess);


    // usage code!


    // put in utility function
    LispisFunction *entry =
        (LispisFunction *)callocGcObject(&state, sizeof(LispisFunction));
    entry->header.type = GC_LISPIS_FUNCTION;
    putInZeroCountTable(&state, (GcObjectHeader *)entry);
    //entry->localToGlobalTableSize = 64;
    //entry->localToGlobalTable =
    //(uint32 *)calloc(entry->localToGlobalTableSize, sizeof(uint32));
    entry->bytecodeSize = 256;
    entry->bytecode =
        (Bytecode *)calloc(1, entry->bytecodeSize * sizeof(Bytecode));

    char *fileContent = readEntireFile((char *)"./test.lsp");
    Lexer lexer = {fileContent};
    eatToken(&lexer);

    CompilerPass compilerPasses[] = {
        symbolIdPass,
        quotePass,
        lambdaPass,
        letPass,
        definePass,
        ifPass
    };
    pushOp(&state, entry, OP_PUSH);
    pushInt32(&state, entry, 0);
    pushOp(&state, entry, OP_POP_ASSERT_EQUAL);
    while (peekToken(&lexer).tokenType != TOK_EOF) {
        Expr *parseTreeExpr = parseExpression(&lexer);
        printf("ParseTree:\n");
        dumpTree(&state, parseTreeExpr, 0);
        Expr *prev = parseTreeExpr;
        Expr *next = 0;
        for (uint32 i = 0; i < arrayLength(compilerPasses);
             ++i) {
            next = compilerPasses[i](&state, prev);
            dealloc(prev);
            prev = next;
            next = 0;
        }
        Expr *astDone = prev;
        printf("AST:\n");
        dumpTree(&state, astDone, 0);
        compileExpression(&state, entry, astDone);
        dealloc(astDone);
        //dealloc(parseTreeExpr);
        if (peekToken(&lexer).tokenType != TOK_EOF) {
            pushOp(&state, entry, OP_CLEAR_STACK);
        }
    }
    pushOp(&state, entry, OP_RETURN);
    dumpBytecode(&state, entry);


    //LispisFunction *entry = loadFunction(&state,
    //bytecode);
    //printf("enviroment:\n");
    //dumpEnviroment(&state.globalSymbolTable,
    //entry,
    //entry->enviroment);
    LispisFunctionObject *entryObj =
        (LispisFunctionObject *)callocGcObject(&state,
                                               sizeof(LispisFunctionObject));
    entryObj->header.type = GC_LISPIS_FUNCTION_OBJECT;
    putInZeroCountTable(&state, &entryObj->header);
    incRef(&state, &entry->header);
    entryObj->function = entry;
    entryObj->parentEnv = &state.globalEnviroment;

    freeze(&state, &entryObj->header);
    Value entryObjVal = nanPackPointer(entryObj, LISPIS_LFUNC);
    Value retVal = runFunction(&state, entryObjVal, 0);
    freeze(&state, &unpackLFunc(retVal)->header);
    //clearStack(&state);
    printf("ret val:\n");
    printValue(&state.globalSymbolTable,
               entry,
               retVal);
    //gc(&state);

    for (int i = 0; i < 1000; ++i) {
        push(&state, nanPackInt32(6));
        Value ret = runFunction(&state, retVal, 1);
        clearStack(&state);
        printf("ret val:\n");
        printValue(&state.globalSymbolTable,
                   entry,
                   ret);
        markAndSweep(&state);
    }
    //gc(&state);

    
    // Will fail, with weird errors...
    // until freezing adds to the root set
                                          
    
    destroy(&state);
    free(fileContent);
    
    return 0;
}