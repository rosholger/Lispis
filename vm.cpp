#include "vm.h"
#include "compiler_passes.h"
#include "codegen.h"
#include "parser.h"
#include "lexer.h"
#include <cassert>
#include <cstdlib>
#include <cstdio>
#include <cstring>


bool indexStack(LispisState *state, int64 i, Value *ret) {
    try {
        *ret = indexStackInternal(state, i);
    } catch (int e) {
        return false;
    }
    return true;
}

bool pop(LispisState *state, Value *ret) {
    try {
        *ret = popInternal(state);
    } catch (int e) {
        return false;
    }
    return true;
}

bool peek(LispisState *state, Value *ret) {
    try {
        *ret = peekInternal(state);
    } catch (int e) {
        return false;
    }
    return true;
}

bool push(LispisState *state, Value v) {
    try {
        pushInternal(state, v);
    } catch (int e) {
        return false;
    }
    return true;
}

void pushError(LispisState *state, bool onlyIfFalse, const char *str) {
    if (!onlyIfFalse) {
        pushInternal((state), nanPackSymbolIdx(internCStr((state), (str))));
        throw 0;
    }
}

void unlinkGcObject(LispisState *state, GcObjectHeader *header) {
    if (state->firstGcObject == header) {
        state->firstGcObject = header->next;
    }
    if (state->toSweep == header) {
        state->toSweep = header->next;
    }
    if (state->grayStack == header) {
        state->grayStack = header->next;
    }
    if (header->next) {
        header->next->prev = header->prev;
    }
    if (header->prev) {
        header->prev->next = header->next;
    }
}

void writeBarrier(LispisState *state, GcObjectHeader *header) {
    if (!state->sweepPhase && (header->color & GC_black)) {
        unlinkGcObject(state, header);
        header->color &= ~GC_black;
        header->color |= GC_gray;
        header->next = state->grayStack;
        header->prev = 0;
        if (header->next) {
            header->next->prev = header;
        }
        state->grayStack = header;
    }
}

Value emptyList() {
    return nanPackPointer(0, LISPIS_CONS);
}

bool isNill(Value v) {
    return getType(v) == LISPIS_CONS && !unpackCons(v);
}

void setLocal(LispisState *state,
              Env *env,
              Value v, uint32 index) {
    assert(index < env->variablesSize && "ICE-illegal-local-index");
    assert(env->variables && "ICE-env->variables=0");
    env->variables[index].filled = true;
    env->variables[index].val = v;
}

void setUpval(LispisState *state, Value v, Upval upval) {
    setLocal(state, upval.env, v, upval.index);
}

#if 0
void setVariableRaw(LispisState *state, Env *env,
                    Value v, uint32 symbolID) {
    writeBarrier(state, &env->header);
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
        //decRef(state, env->variables[variableID].val);
    } else {
        env->variablesFilled++;
    }
    //incRef(state, v);
    env->variables[variableID].symbolID = symbolID;
    env->variables[variableID].filled = true;
    env->variables[variableID].val = v;
}
#endif

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
    char *symbolStrCopy = (char *)malloc(symbol.length *
                                         sizeof(char));
    memcpy(symbolStrCopy, symbol.val, symbol.length);
    st->symbols[stIndex].symbol.length = symbol.length;
    st->symbols[stIndex].symbol.val = symbolStrCopy;
    st->symbols[stIndex].globalSymbolID = st->nextGlobalSymbolID;
    st->nextGlobalSymbolID++;
    st->symbols[stIndex].hash = symHash;
    st->symbols[stIndex].filled = true;
    st->symbolsInterned++;
#if LOG_ENABLED
    printf("Interned %.*s as %u\n",
           st->symbols[stIndex].symbol.length,
           st->symbols[stIndex].symbol.val,
           st->symbols[stIndex].globalSymbolID);
#endif

    return st->symbols[stIndex].globalSymbolID;
}


Value cons(LispisState *state, Value ncar, Value ncdr) {
    Pair *pair = (Pair *)callocGcObject(state, sizeof(Pair));
    //pair->header.refCount = 0;
    pair->header.type = GC_PAIR;
    //putInZeroCountTable(state, (GcObjectHeader *)pair);
    //incRef(state, ncar);
    //incRef(state, ncdr);
    pair->car = ncar;
    pair->cdr = ncdr;
    return nanPackPointer(pair, LISPIS_CONS);
}

Value getLocal(LispisState *state, Env *env, uint32 index) {
    assert(index < env->variablesSize && "ICE-illegal-local-index");
    assert(env->variables[index].filled &&
           "ICE-local-variable-"
           "accessed-before-assigned");
    return env->variables[index].val;
}

Value getUpval(LispisState *state, Upval upval) {
    return getLocal(state, upval.env, upval.index);
}

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

LispisFunctionObject *allocFunctionObject(LispisState *state,
                                          LispisFunction *func,
                                          Env *parentEnv) {
    LispisFunctionObject *funcObj =
        (LispisFunctionObject *)callocGcObject(state,
                                               sizeof(LispisFunctionObject));
    funcObj->header.type = GC_LISPIS_FUNCTION_OBJECT;
    //putInZeroCountTable(state, &funcObj->header);
    //incRef(state, &func->header);
    funcObj->function = func;
    //incRef(state, &parentEnv->header);
    funcObj->parentEnv = parentEnv;
    funcObj->upvalsSize = func->upvalProtosSize;
    if (funcObj->upvalsSize) {
        funcObj->upvals = (Upval *)calloc(funcObj->upvalsSize,
                                          sizeof(Upval));
    }
    for (uint32 i = 0; i < func->upvalProtosSize; ++i) {
        funcObj->upvals[i].index = func->upvalProtos[i].index;
        Env *targetEnv = parentEnv;
        for (uint32 j = 1; j < func->upvalProtos[i].depth; ++j) {
            targetEnv = targetEnv->parentEnv;
        }
        funcObj->upvals[i].env = targetEnv;
    }
    return funcObj;
}


ActivationRecord *allocActivationRecord(LispisState *state,
                                        LispisFunction *func,
                                        Env *parentEnv,
                                        bool topRecord) {
    ActivationRecord *record =
        (ActivationRecord *)callocGcObject(state,
                                           sizeof(ActivationRecord));
    record->header.type = GC_ACTIVATION_RECORD;
    record->function = func;
    if (!topRecord) {
        record->enviroment = (Env *)callocGcObject(state, sizeof(Env));
        record->enviroment->header.type = GC_ENV;
        //incRef(state, (GcObjectHeader *)record->enviroment);
        record->enviroment->parentEnv = parentEnv;
        if (func) {
            record->enviroment->variablesSize = func->numLocals;
        }
        if (record->enviroment->variablesSize) {
            record->enviroment->variables =
                (Var *)calloc(record->enviroment->variablesSize,
                              sizeof(Var));
        }
    }
    return record;
}

ActivationRecord *allocActivationRecord(LispisState *state,
                                        LispisFunctionObject *funcObj) {
    ActivationRecord *ret = allocActivationRecord(state,
                                                  funcObj->function,
                                                  funcObj->parentEnv,
                                                  false);
    ret->upvalsSize = funcObj->upvalsSize;
    ret->upvals = (Upval *)malloc(ret->upvalsSize*sizeof(Upval));
    memcpy(ret->upvals, funcObj->upvals, ret->upvalsSize*sizeof(Upval));
    return ret;
}

void popActivationRecord(LispisState *state) {
    state->dataStackTop = state->currRecord->dataStackBottom;
    state->currRecord = state->currRecord->caller;
}

Value lookupGlobal(LispisState *state, Value symbol) {
    uint32 id = unpackSymbolID(symbol);
    Value result = getGlobal(state, id);
    return result;
}

Value symbolFromString(LispisState *state, String str) {
    return nanPackSymbolIdx(internSymbol(state, str, hashFunc(str)));
}

Value internalAppend(LispisState *state, Value r, Value l) {
    pushError(state,
              getType(r) == LISPIS_CONS && getType(l) == LISPIS_CONS,
              "ICE?-internalAppend-used-on-non-cons");
    Pair *p = unpackCons(r);
    if (!p) {
        return l;
    }
    pushError(state, getType(p->cdr) == LISPIS_CONS,
              "ICE?-right-hands-cdr-in-internalAppend-not-a-cons");
    return cons(state, p->car, internalAppend(state, p->cdr, l));
}

inline
void pushActivationRecord(LispisState *state,
                          LispisFunctionObject *funcObj,
                          bool calledFromC,
                          uint64 numArgs) {
    ActivationRecord *record = allocActivationRecord(state, funcObj);
    record->caller = state->currRecord;
    record->calledFromC = calledFromC;
    record->dataStackBottom = state->dataStackTop - numArgs;
    state->currRecord = record;
}

inline
void pushCActivationRecord(LispisState *state, uint64 numArgs) {
    ActivationRecord *record =
        allocActivationRecord(state,
                              0,
                              0,
                              false);
    record->caller = state->currRecord;
    record->dataStackBottom =
        state->dataStackTop-numArgs;
    record->function = 0;
    state->currRecord = record;
}

uint32 protoSym;

void extendObject(LispisState *state, Object *object);

inline
void setObjectElem(LispisState *state,
                   Object *object, uint32 key, Value value) {
    writeBarrier(state, &object->header);
    if (object->numFilled * 1.5 > object->size) {
        extendObject(state, object);
    }
    if (key == protoSym) {
        pushError(state, getType(value) == LISPIS_OBJECT,
                  "object-*prototype*-has-to-be-an-object");
        Object *proto = unpackObject(value);
        object->proto = proto;
    }
    uint32 id = key % object->size;
    // HASH
    uint32 perturb = key;
    while (object->elems[id].filled &&
           object->elems[id].key != key) {
        id = (5*id)+1+perturb;
        perturb >>= 5;
        id = id % object->size;
    }
    if (!object->elems[id].filled) {
        object->numFilled++;
    }
    object->elems[id].val = value;
    object->elems[id].key = key;
    object->elems[id].filled = true;
}


void extendObject(LispisState *state, Object *object) {
    Object tmp = *object;
    object->size *= 2;
    // TODO: scratch space
    object->elems = (KeyValPair *)calloc(object->size,
                                         sizeof(KeyValPair));
    object->numFilled = 0;
    writeBarrier(state, &object->header);
    for (int i = 0, j = 0; i < tmp.size && j < tmp.numFilled; i++) {
        if (tmp.elems[i].filled) {
            setObjectElem(state, object,
                          tmp.elems[i].key, tmp.elems[i].val);
        }
    }
}

// returns LISPIS_UNDEF if no value is found
Value getObjectElem(Object *object, uint32 key) {
    if (key == protoSym) {
        if (object->proto) {
            return nanPackPointer(object->proto, LISPIS_OBJECT);
        } else {
            return nanPack(0, LISPIS_UNDEF);
        }
    }
    uint32 id = key % object->size;
    // HASH
    uint32 startID = id;
    uint32 perturb = key;
    bool found = true;
    while (!object->elems[id].filled ||
           object->elems[id].key != key) {
        id = (5*id)+1+perturb;
        perturb >>= 5;
        id = id % object->size;
        if (id == startID) {
            found = false;
            break;
        }
    }
    if (!found || !object->elems[id].filled) {
        if (!object->proto) {
            return nanPack(0, LISPIS_UNDEF);
        } else {
            return getObjectElem(object->proto, key);
        }
    } else {
        return object->elems[id].val;
    }
}

// OPTIMIZE
void extendVector(Vector *vector, int32 newNumberFilled) {
    if (newNumberFilled > vector->size) {

        int32 numberOfAddedElements = newNumberFilled - vector->size;
        int32 numberOfAddedBuckets =
            numberOfAddedElements/VECTOR_BUCKET_SIZE;
        if (numberOfAddedElements % VECTOR_BUCKET_SIZE) {
            numberOfAddedBuckets++;
        }
        if (!vector->size) {
            vector->firstBucket =
                (VectorBucket *)calloc(1, sizeof(VectorBucket));
            vector->size += VECTOR_BUCKET_SIZE;
            numberOfAddedBuckets--;
        }
        VectorBucket *lastBucket = vector->firstBucket;
        while (lastBucket->next) {
            lastBucket = lastBucket->next;
        }
        for (int i = 0; i < numberOfAddedBuckets; ++i) {
            lastBucket->next =
                (VectorBucket *)calloc(1, sizeof(VectorBucket));
            lastBucket = lastBucket->next;
        }
        vector->size += numberOfAddedBuckets*VECTOR_BUCKET_SIZE;
    }
}

Vector *allocVector(LispisState *state, int32 numToBeFilled) {
    uint32 numBuckets = numToBeFilled/VECTOR_BUCKET_SIZE;
    if (numToBeFilled % VECTOR_BUCKET_SIZE && numToBeFilled) {
        numBuckets++;
    }
    uint32 size = numBuckets * VECTOR_BUCKET_SIZE;
    Vector *v = (Vector *)callocGcObject(state,
                                         sizeof(Vector));
    v->header.type = GC_VECTOR;
    v->size = size;
    v->numFilled = 0;
    v->firstBucket = 0;
    if (numBuckets) {
        v->firstBucket =
            (VectorBucket *)calloc(1, sizeof(VectorBucket));
        VectorBucket *prevBucket = v->firstBucket;
        for (uint32 i = 1; i < numBuckets; ++i) {
            prevBucket->next =
                (VectorBucket *)calloc(1,
                                       sizeof(VectorBucket));
            prevBucket = prevBucket->next;
        }
    }
    return v;
}

void setGlobal(LispisState *state, Value v, uint32 symbolID) {
#if 0
    GlobalEnv *env = &state->globalEnviroment;
    writeBarrier(state, &env->header);
    if (env->variablesFilled > env->variablesSize*0.7) {
        uint64 oldSize = env->variablesSize;
        GlobalVar *oldVariables = env->variables;
        env->variablesSize *= 1.5f;
        env->variables = (GlobalVar *)calloc(env->variablesSize,
                                             sizeof(GlobalVar));
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
        //decRef(state, env->variables[variableID].val);
    } else {
        env->variablesFilled++;
    }
    //incRef(state, v);
    env->variables[variableID].symbolID = symbolID;
    env->variables[variableID].filled = true;
    env->variables[variableID].val = v;
    #endif
    setObjectElem(state, &state->globalEnviroment, symbolID, v);
}

void appendVectorDestructive(LispisState *state,
                             Vector *appendedTo,
                             Vector *appended) {
    writeBarrier(state, &appendedTo->header);
    extendVector(appendedTo, appended->numFilled + appendedTo->numFilled);
    assert(appendedTo->size >=
           appended->numFilled + appendedTo->numFilled);
    writeBarrier(state, &appendedTo->header); // gc might have happend
    VectorBucket *toBucket = appendedTo->firstBucket;
    for (int i = 0; i < appendedTo->numFilled/VECTOR_BUCKET_SIZE; ++i) {
        toBucket = toBucket->next;
    }
    VectorBucket *fromBucket = appended->firstBucket;
    for (int from = 0, to = appendedTo->numFilled;
         from < appended->numFilled; ++from, ++to) {
        int indexInToBucket = to % VECTOR_BUCKET_SIZE;
        int indexInFromBucket = from % VECTOR_BUCKET_SIZE;
        if (to && !indexInToBucket) {
            toBucket = toBucket->next;
            assert(toBucket);
        }
        if (from && !indexInFromBucket) {
            fromBucket = fromBucket->next;
            assert(fromBucket);
        }
        toBucket->elems[indexInToBucket] =
            fromBucket->elems[indexInFromBucket];
    }
    appendedTo->numFilled += appended->numFilled;
}
                             
#define NEXT_POW2(v)                            \
    do {                                        \
    v--;                                     \
    v |= v >> 1;                          \
    v |= v >> 2;                          \
    v |= v >> 4;                          \
    v |= v >> 8;                          \
    v |= v >> 16;                         \
    v++;                                     \
    } while(0)

void runFunctionInternal(LispisState *state, Value funcObjValue,
                         uint64 numArgs) {
    pushError(state, numArgs <= state->dataStackTop,
              "fewer-values-in-data-"
              "stack-then-arguments");
    LispisFunctionObject *funcObj = unpackLFunc(funcObjValue);
    pushActivationRecord(state, funcObj, true, numArgs);
    pushInternal(state, nanPackInt32(numArgs));
    LispisFunction *func = 0;
 CALL:
    func = state->currRecord->function;
    while(func->bytecode[state->currRecord->pc].opCode != OP_EXIT) {
        uint64 pc = state->currRecord->pc;
        assert(state->currRecord->pc < func->bytecodeSize);
        OpCodes op =
            func->bytecode[pc].opCode;
        state->currRecord->pc++;
        switch (op) {
            case OP_PUSH_GLOBAL: {
                uint32 id = unpackSymbolID(popInternal(state));
                Value result = getGlobal(state, id);
                pushInternal(state, result);
            } break;
            case OP_PUSH_LOCAL: {
                uint64 index = func->bytecode[state->currRecord->pc].ui64;
                state->currRecord->pc++;
                Value result = getLocal(state,
                                        state->currRecord->enviroment,
                                        index);
                pushInternal(state, result);
            } break;
            case OP_PUSH_UPVAL: {
                uint64 index = func->bytecode[state->currRecord->pc].ui64;
                state->currRecord->pc++;
                assert(index < state->currRecord->upvalsSize);
                Upval upval = state->currRecord->upvals[index];
                Value result = getUpval(state, upval);
                pushInternal(state, result);
            } break;
            case OP_RETURN: {
                Value ret = popInternal(state);
                bool returnToC = state->currRecord->calledFromC;
                popActivationRecord(state);
                pushInternal(state, ret);
                if (!returnToC) {
                    func = state->currRecord->function;
                } else {
                    return;
                }
            } break;
            case OP_PUSH: {
                Value p;
                p = func->bytecode[state->currRecord->pc];
                state->currRecord->pc++;
                pushInternal(state, p);
            } break;
            case OP_PUSH_NULL: {
                pushInternal(state, emptyList());
            } break;
            case OP_LIST: {
                int32 numElems = unpackInt(popInternal(state));
                Value head = popInternal(state);
                assert(numElems > 0);
                for (int32 i = 0; i < numElems-1; ++i) {
                    head = cons(state, popInternal(state), head);
                }
                pushInternal(state, head);
            } break;
            case OP_CALL: {
                uint64 numArgs =
                    func->bytecode[state->currRecord->pc].ui64;
                state->currRecord->pc++;
                Value callee = popInternal(state);
                if (getType(callee) == LISPIS_CFUNC) {
                    pushCActivationRecord(state, numArgs);
                    CFunction *f = unpackCFunc(callee);
                    bool returned = f->func(state, numArgs);
                    Value retVal;
                    if (returned) {
                        retVal = popInternal(state);
                    } else {
                        retVal = nanPack(0, LISPIS_UNDEF);
                    }
                    popActivationRecord(state);
                    pushInternal(state, retVal);
                } else if (getType(callee) == LISPIS_LFUNC) {
                    LispisFunctionObject *lfuncObj = unpackLFunc(callee);
                    assert(!lfuncObj->function->macro);
                    pushActivationRecord(state, lfuncObj, false, numArgs);
                    pushInternal(state, nanPackInt32(numArgs));
                    goto CALL;
                } else {
                    pushError(state, false, "cant-call-this-type");
                }
            } break;
            case OP_PUSH_LAMBDA_ID: {
                uint64 id = func->bytecode[state->currRecord->pc].ui64;
                state->currRecord->pc++;
                LispisFunctionObject *funcObj =
                    allocFunctionObject(state, func->subFunctions[id],
                                        state->currRecord->enviroment);
                pushInternal(state, nanPackPointer(funcObj,
                                           LISPIS_LFUNC));
            } break;
            case OP_POP_ASSERT_EQUAL: {
                int32 a = unpackInt(popInternal(state));
                int32 b = unpackInt(popInternal(state));
                pushError(state, a == b, "arrity-missmatch");
            } break;
            case OP_SET_LOCAL: {
                uint32 index = func->bytecode[state->currRecord->pc].ui64;
                state->currRecord->pc++;
                Value v = popInternal(state);
                setLocal(state, state->currRecord->enviroment,
                         v, index);
            } break;
            case OP_SET_UPVAL: {
                uint32 index = func->bytecode[state->currRecord->pc].ui64;
                state->currRecord->pc++;
                Value v = popInternal(state);
                assert(index < state->currRecord->upvalsSize);
                Upval upval = state->currRecord->upvals[index];
                setUpval(state, v, upval);
            } break;
            case OP_SET_GLOBAL: {
                uint32 symId = unpackSymbolID(popInternal(state));
                Value v = popInternal(state);
                setGlobal(state, v, symId);
            } break;
            case OP_CLEAR_STACK: {
                state->dataStackTop = state->currRecord->dataStackBottom;
            } break;
            case OP_JUMP_IF_TRUE: {
                bool pred = unpackBoolean(popInternal(state));
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
            case OP_COLLECT_VARARGS: {
                int numFormals =
                    unpackInt(func->bytecode[state->currRecord->pc]);
                state->currRecord->pc++;
                int numVarargs = (state->dataStackTop -
                                  state->currRecord->dataStackBottom -
                                  numFormals);
                //assert(numVarargs > 0);
                
                Value head = emptyList();
                for (int32 i = 0; i < numVarargs; ++i) {
                    head = cons(state, popInternal(state), head);
                }
                pushInternal(state, head);
            } break;
            case OP_POP_ASSERT_LESS_OR_EQUAL: {
                int32 smaller = unpackInt(popInternal(state));
                int32 larger = unpackInt(popInternal(state));

                pushError(state, smaller <= larger, "arrity-missmatch");
            } break;
            case OP_APPEND: {
                int32 numElems = unpackInt(popInternal(state));
                assert(numElems > 0);
                Value head = popInternal(state);
                for (int32 i = 0; i < numElems-1; ++i) {
                    head = internalAppend(state, popInternal(state),
                                          head);
                }
                pushInternal(state, head);
            } break;
            case OP_ALLOC_OBJECT: {
                // CHANGE TO int32!!!!
                uint64 size = func->bytecode[state->currRecord->pc].ui64;
                state->currRecord->pc++;
                Object *obj = (Object *)callocGcObject(state,
                                                     sizeof(Object));
                obj->numFilled = 0;
                obj->header.type = GC_OBJECT;
                if (size < MIN_OBJECT_SIZE) {
                    obj->size = MIN_OBJECT_SIZE;
                } else {
                    uint32 objSize = size;
                    NEXT_POW2(objSize);
                    if (objSize * 0.75 > size) {
                        objSize *= 2;
                    }
                    obj->size = objSize;
                }
                obj->elems = (KeyValPair *)calloc(obj->size,
                                                  sizeof(KeyValPair));
                pushInternal(state, nanPackPointer(obj, LISPIS_OBJECT));
            } break;
            case OP_ALLOC_VECTOR: {
                uint64 numFilled =
                    func->bytecode[state->currRecord->pc].ui64;
                state->currRecord->pc++;
                Vector *res = allocVector(state, numFilled);
                res->numFilled = numFilled;
                pushInternal(state,
                             nanPackPointer(res, LISPIS_VECTOR));
            } break;
            case OP_SET_ELEM: {
                Value ref = popInternal(state);
                Value value = popInternal(state);
                Value obj = popInternal(state);
                if (getType(obj) == LISPIS_VECTOR) {
                    pushError(state, getType(ref) == LISPIS_INT32,
                              "vectors-can-only-be-referenced-with-"
                              "integers");
                    Vector *vec = unpackVector(obj);
                    writeBarrier(state, &vec->header);
                    int32 index = unpackInt(ref);
                    pushError(state, index >= 0 && index < vec->numFilled,
                              "index-out-of-bounds");
                    int32 indexInBucket = index % VECTOR_BUCKET_SIZE;
                    int32 indexOfBucket = index / VECTOR_BUCKET_SIZE;
                    VectorBucket *bucket = vec->firstBucket;
                    for (int i = 0; i < indexOfBucket; ++i) {
                        bucket = bucket->next;
                    }
                    bucket->elems[indexInBucket] = value;
                    pushInternal(state, obj);
                } else if (getType(obj) == LISPIS_OBJECT) {
                    pushError(state, getType(ref) == LISPIS_SYM_IDX,
                              "objects-can-only-be-referenced-with-"
                              "symbols");
                    Object *object = unpackObject(obj);
                    uint32 key = unpackSymbolID(ref);
                    setObjectElem(state, object, key, value);
                    pushInternal(state, obj);
                } else {
                    pushError(state, false,
                              "you-can-only-set-elements-of-vectors");
                }
            } break;
            case OP_PUSH_ELEM: {
                Value ref = popInternal(state);
                Value obj = popInternal(state);
                if (getType(obj) == LISPIS_VECTOR) {
                    pushError(state, getType(ref) == LISPIS_INT32,
                              "vectors-can-only-be-referenced-with-"
                              "integers");
                    Vector *vec = unpackVector(obj);
                    int32 index = unpackInt(ref);
                    pushError(state, index >= 0 && index < vec->numFilled,
                              "index-out-of-bounds");
                    int32 indexInBucket = index % VECTOR_BUCKET_SIZE;
                    int32 indexOfBucket = index / VECTOR_BUCKET_SIZE;
                    VectorBucket *bucket = vec->firstBucket;
                    for (int i = 0; i < indexOfBucket; ++i) {
                        bucket = bucket->next;
                    }
                    pushInternal(state, bucket->elems[indexInBucket]);
                } else if (getType(obj) == LISPIS_OBJECT) {
                    pushError(state, getType(ref) == LISPIS_SYM_IDX,
                              "objects-can-only-be-referenced-with-"
                              "integers");
                    Object *object = unpackObject(obj);
                    uint32 key = unpackSymbolID(ref);
                    pushInternal(state, getObjectElem(object, key));
                } else {
                    pushError(state, false,
                              "you-can-only-get-elements-of-vectors");
                }
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
    //binding->header.refCount = 0;
    // We could skip putting it in zct but that would trigger asserts
    binding->header.type = GC_C_FUNCTION;
    //putInZeroCountTable(state, (GcObjectHeader *)binding);
    binding->func = func;
    Value funcVal;
    funcVal = nanPackPointer(binding, LISPIS_CFUNC);
    uint32 symbolID = internSymbol(state,
                                   symbol,
                                   hashFunc(symbol));
    setGlobal(state, funcVal, symbolID);
}

Value getGlobal(LispisState *state, uint32 globalSymbolID) {
#if 0
    GlobalEnv *env = &state->globalEnviroment;
    uint32 variableID = globalSymbolID % env->variablesSize;
    uint32 startID = variableID;
    while (env->variables[variableID].symbolID != globalSymbolID &&
           env->variables[variableID].filled) {
        variableID++;
        variableID = variableID % env->variablesSize;
        if (variableID == startID) {
            break;
        }
    }
        pushError(state,
                  env->variables[variableID].filled,
                  "variable-undefined");
    return env->variables[variableID].val;
#endif
    return getObjectElem(&state->globalEnviroment, globalSymbolID);
}

void destroy(LispisState *state, GcObjectHeader *header) {
    state->numAllocated--;
    //printf("deallocating ");
    switch (header->type) {
        case GC_ENV: {
            //printf("Env ");
            Env *e = (Env *)header;
            free(e->variables);
        } break;
        case GC_ACTIVATION_RECORD: {
            //printf("ActivationRecord ");
            ActivationRecord *ar = (ActivationRecord *)header;
            free(ar->upvals);
        } break;
        case GC_LISPIS_FUNCTION: {
            //printf("LispisFunction ");
            LispisFunction *f = (LispisFunction *)header;
            free(f->subFunctions);
            free(f->bytecode);
            free(f->upvalProtos);
        } break;
        case GC_PAIR: {
            //printf("Pair ");
        } break;
        case GC_C_FUNCTION: {
            //printf("CFunction ");
        } break;
        case GC_LISPIS_FUNCTION_OBJECT: {
            LispisFunctionObject *lfo = (LispisFunctionObject *)header;
            free(lfo->upvals);
        } break;
        case GC_VECTOR: {
            Vector *v = (Vector *)header;
            VectorBucket *bucket = v->firstBucket;
            while(bucket) {
                VectorBucket *next = bucket->next;
                free(bucket);
                bucket = next;
            }
        } break;
        case GC_OBJECT: {
            Object *o = (Object *)header;
            free(o->elems);
        } break;
        default:assert(false);
    }
    //printf("%p\n", header);
    unlinkGcObject(state, header);
    free(header);
}

void freeze(LispisState *state, GcObjectHeader *header) {
    if (!(header->color & GC_frozen)) {
        if (header != state->grayStack) {
            unlinkGcObject(state, header);
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
#if LOG_ENABLED
    printf("destroying state\n");
#endif
    if (state->globalEnviroment.header.next) {
        state->globalEnviroment.header.next->prev =
            state->globalEnviroment.header.prev;
    }
    if (state->globalEnviroment.header.prev) {
        state->globalEnviroment.header.prev->next =
            state->globalEnviroment.header.next;
    }
    GcObjectHeader *next = state->firstGcObject;
    for (uint64 deleted = 0, i = 0;
         deleted < state->globalSymbolTable.symbolsInterned; i++) {
        if (state->globalSymbolTable.symbols[i].filled) {
            free(state->globalSymbolTable.symbols[i].symbol.val);
            deleted++;
        }
    }
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
    state->numAllocated++;
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
        case LISPIS_VECTOR: {
            ret = (GcObjectHeader *)unpackVector(v);
        } break;
        case LISPIS_OBJECT: {
            ret = (GcObjectHeader *)unpackObject(v);
        } break;
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
        unlinkGcObject(state, obj);
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
            case GC_VECTOR: {
                Vector *v = (Vector *)obj;
                VectorBucket *bucket = v->firstBucket;
                for (int i = 0; i < v->numFilled; ++i) {
                    int indexInBucket = i % VECTOR_BUCKET_SIZE;
                    if (i && !indexInBucket) {
                        bucket = bucket->next;
                    }
                    markGray(state,
                             headerFromValue(bucket->elems[indexInBucket]));
                }
            } break;
            case GC_OBJECT: {
                Object *o = (Object *)obj;
                for (int i = 0; i < o->size; ++i) {
                    if (o->elems[i].filled) {
                        markGray(state, headerFromValue(o->elems[i].val));
                    }
                }
            } break;
            case GC_ENV: {
                Env *e = (Env *)obj;
                if (e->parentEnv) {
                    markGray(state,
                             (GcObjectHeader *)e->parentEnv);
                }
                for (uint32 i = 0; i < e->variablesSize; ++i) {
                    if (e->variables[i].filled) {
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
                for (uint32 i = 0; i < ar->upvalsSize; ++i) {
                    markGray(state, &ar->upvals[i].env->header);
                }
            } break;
            case GC_LISPIS_FUNCTION_OBJECT: {
                LispisFunctionObject *lfo =
                    (LispisFunctionObject *)obj;
                markGray(state, (GcObjectHeader *)lfo->parentEnv);
                markGray(state, (GcObjectHeader *)lfo->function);
                for (uint32 i = 0; i < lfo->upvalsSize; ++i) {
                    markGray(state, &lfo->upvals[i].env->header);
                }
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
    // Slow as fuck, prob. need custom allocator to be snappy :(
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
                //assert(state->grayStack);
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

// Returns a LispisFunctionObject
bool compileNullTerminatedString(LispisState *state, char *str,
                                 Value *ret) {
    LispisFunction *entry =
        (LispisFunction *)callocGcObject(state, sizeof(LispisFunction));
    entry->header.type = GC_LISPIS_FUNCTION;
    entry->bytecodeSize = 256;
    entry->bytecode =
        (Bytecode *)calloc(1, entry->bytecodeSize * sizeof(Bytecode));
    entry->instructionToLine =
        (int32 *)malloc(entry->bytecodeSize * sizeof(int32));
    memset(entry->instructionToLine, -1,
           entry->bytecodeSize * sizeof(int32));

    Lexer lexer = {str};
    eatToken(&lexer);

    SymbolIdPass symbolIdPass;
    MacroExpansionPass macroExpansionPass;
    DefmacroPass defmacroPass;
    LambdaPass lambdaPass;
    LetPass letPass;
    SetPass setPass;
    DefinePass definePass;
    IfPass ifPass;
    ForPass forPass;
    DoPass doPass;
    RefPass refPass;
    VariablePass variablePass;
    LexicalScope entryScope = makeRealScope(0);
    variablePass.currentScope = &entryScope;

    CompilerPass *compilerPasses[] = {
        &symbolIdPass,
        &macroExpansionPass,
        &defmacroPass,
        &lambdaPass,
        &letPass,
        &setPass,
        &definePass,
        &ifPass,
        &forPass,
        &doPass,
        &refPass,
        &variablePass
    };
    pushOp(state, entry, OP_PUSH, 0);
    pushInt32(state, entry, 0);
    pushOp(state, entry, OP_POP_ASSERT_EQUAL, 0);
    ActivationRecord *r = state->currRecord;
    while (peekToken(&lexer).tokenType != TOK_EOF) {
        Expr *parseTreeExpr = parseExpression(&lexer);
#if LOG_ENABLED
        printf("ParseTree:\n");
        dumpTree(state, parseTreeExpr, 0);
#endif
        Expr *prev = parseTreeExpr;
        Expr *next = 0;
        for (uint32 i = 0; i < arrayLength(compilerPasses);
             ++i) {
            // Might currently leak...
            try {
                next = compilerPasses[i]->startTransforming(state, prev);
            } catch (int e) {
                dealloc(prev);
                state->currRecord = r;
                return false;
            }
            dealloc(prev);
            prev = next;
            next = 0;
        }
        Expr *astDone = prev;;
#if LOG_ENABLED
        printf("AST:\n");
        dumpTree(state, astDone, 0);
#endif
        try {
            compileExpression(state, entry, astDone);
        } catch (int e) {
            state->currRecord = r;
            return false;
        }
        // hack
        entry->upvalProtos = 0;
        entry->upvalProtosSize = 0;
        entry->numLocals = entryScope.real.totalVariablesSize;
        dealloc(astDone);
        //dealloc(parseTreeExpr);
        if (peekToken(&lexer).tokenType != TOK_EOF) {
            pushOp(state, entry, OP_CLEAR_STACK, peekToken(&lexer).line);
        }
    }
    pushOp(state, entry, OP_RETURN, peekToken(&lexer).line);
#if LOG_ENABLED
    dumpBytecode(state, entry);
#endif

    LispisFunctionObject *entryObj =
        (LispisFunctionObject *)callocGcObject(state,
                                               sizeof(LispisFunctionObject));
    entryObj->header.type = GC_LISPIS_FUNCTION_OBJECT;
    entryObj->function = entry;
    entryObj->parentEnv = 0;

    //TODO: should this really be frozen?
    freeze(state, &entryObj->header);
    *ret = nanPackPointer(entryObj, LISPIS_LFUNC);
    return true;
}

void initState(LispisState *state) {
    state->globalEnviroment.header.type = GC_GLOBAL_ENV;
    state->globalSymbolTable.symbolsSize = 256;
    state->globalEnviroment.size = 256;
    state->globalSymbolTable.symbols =
        (Symbol *)calloc(1,
                         sizeof(Symbol) *
                         state->globalSymbolTable.symbolsSize +
                         sizeof(Var) *
                         state->globalEnviroment.size);
    state->globalEnviroment.elems =
        (KeyValPair *)(state->globalSymbolTable.symbols +
                      state->globalSymbolTable.symbolsSize);
    state->dataStackSize = 2*1024;
    state->dataStack = (Value *)malloc(state->dataStackSize *
                                       sizeof(Value));
    freeze(state, &state->globalEnviroment.header);
    state->currRecord = allocActivationRecord(state, 0, 0, true);
    // TODO: make the currRecord and the dataStack up to
    // dataStackTop roots
    // then we dont have to freeze it
    freeze(state, &state->currRecord->header);

    protoSym = internCStr(state, "*proto*");
    setupSpecialFormSymbols(state);
}

bool runFunction(LispisState *state, Value funcObjValue, uint64 numArgs) {
    ActivationRecord *r = state->currRecord;
    try {
        runFunctionInternal(state, funcObjValue, numArgs);
    } catch (int e) {
        state->currRecord = r;
        return false;
    }
    return true;
}

bool runNullTerminatedString(LispisState *state, char *str) {
    Value entryObjVal;
    if (!compileNullTerminatedString(state, str, &entryObjVal)) {
        return false;
    }
    ActivationRecord *r = state->currRecord;
    try {
        runFunctionInternal(state, entryObjVal, 0);
    } catch (int e) {
        unfreeze(state, &unpackLFunc(entryObjVal)->header);
        state->currRecord = r;
        return false;
    }
    unfreeze(state, &unpackLFunc(entryObjVal)->header);
    return true;
}

uint32 internCStr(LispisState *state, const char *cstr) {
    String str;
    str.val = (char *)cstr;
    str.length = strlen(cstr);
    return internSymbol(state, str, hashFunc(str));
}