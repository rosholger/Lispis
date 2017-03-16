#include "vm.h"
#include "compiler_passes.h"
#include "codegen.h"
#include "parser.h"
#include "lexer.h"
#include <cassert>
#include <cstdlib>
#include <cstdio>
#include <cstring>

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

void setLocal(Env *env,
              Value v, uint32 index) {
    assert(index < env->variablesSize);
    assert(env->variables);
    env->variables[index].filled = true;
    env->variables[index].val = v;
}

void setUpval(Value v, Upval upval) {
    setLocal(upval.env, v, upval.index);
}

void setGlobal(LispisState *state, Value v, uint32 symbolID) {
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

#if 0
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
#endif

#define assertStackInBounds(state, value)                               \
    assert((value) >= (state)->currRecord->dataStackBottom)

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

Value getLocal(Env *env, uint32 index) {
    assert(index < env->variablesSize);
    assert(env->variables[index].filled);
    return env->variables[index].val;
}

Value getUpval(Upval upval) {
    return getLocal(upval.env, upval.index);
}

Value getGlobal(LispisState *state, uint32 globalSymbolID) {
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
    if (!env->variables[variableID].filled) {
        assert(!"Variable undefined");
    }
    return env->variables[variableID].val;
}

#if 0
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
    assert(getType(r) == LISPIS_CONS);
    assert(getType(l) == LISPIS_CONS);
    Pair *p = unpackCons(r);
    if (!p) {
        return l;
    }
    assert(getType(p->cdr) == LISPIS_CONS);
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

Value runFunction(LispisState *state, Value funcObjValue,
                  uint64 numArgs) {
    assert(numArgs <= state->dataStackTop);
    LispisFunctionObject *funcObj = unpackLFunc(funcObjValue);
    pushActivationRecord(state, funcObj, true, numArgs);
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
            case OP_PUSH_GLOBAL: {
                uint32 id = unpackSymbolID(pop(state));
                Value result = getGlobal(state, id);
                push(state, result);
            } break;
            case OP_PUSH_LOCAL: {
                uint64 index = func->bytecode[state->currRecord->pc].ui64;
                state->currRecord->pc++;
                Value result = getLocal(state->currRecord->enviroment,
                                        index);
                push(state, result);
            } break;
            case OP_PUSH_UPVAL: {
                uint64 index = func->bytecode[state->currRecord->pc].ui64;
                state->currRecord->pc++;
                assert(index < state->currRecord->upvalsSize);
                Upval upval = state->currRecord->upvals[index];
                Value result = getUpval(upval);
                push(state, result);
            } break;
            case OP_RETURN: {
                Value ret = pop(state);
                if (!state->currRecord->calledFromC) {
                    popActivationRecord(state);
                    push(state, ret);
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
            case OP_PUSH_NULL: {
                push(state, emptyList());
            } break;
            case OP_LIST: {
                int32 numElems = unpackInt(pop(state));
                Value head = pop(state);
                assert(numElems > 0);
                for (int32 i = 0; i < numElems-1; ++i) {
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
                    pushCActivationRecord(state, numArgs);
                    CFunction *f = unpackCFunc(callee);
                    bool returned = f->func(state, numArgs);
                    Value retVal;
                    if (returned) {
                        retVal = pop(state);
                    } else {
                        retVal = nanPack(0, LISPIS_UNDEF);
                    }
                    popActivationRecord(state);
                    push(state, retVal);
                } else if (getType(callee) == LISPIS_LFUNC) {
                    LispisFunctionObject *lfuncObj = unpackLFunc(callee);
                    assert(!lfuncObj->function->macro);
                    pushActivationRecord(state, lfuncObj, false, numArgs);
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
            case OP_SET_LOCAL: {
                uint32 index = func->bytecode[state->currRecord->pc].ui64;
                state->currRecord->pc++;
                Value v = pop(state);
                setLocal(state->currRecord->enviroment,
                         v, index);
            } break;
            case OP_SET_GLOBAL: {
                uint32 symId = unpackSymbolID(pop(state));
                Value v = pop(state);
                setGlobal(state, v, symId);
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
                    head = cons(state, pop(state), head);
                }
                push(state, head);
            } break;
            case OP_POP_ASSERT_LESS_OR_EQUAL: {
                int32 smaller = unpackInt(pop(state));
                int32 larger = unpackInt(pop(state));
                assert(smaller <= larger);
            } break;
            case OP_APPEND: {
                int32 numElems = unpackInt(pop(state));
                assert(numElems > 0);
                Value head = pop(state);
                for (int32 i = 0; i < numElems-1; ++i) {
                    head = internalAppend(state, pop(state),
                                          head);
                }
                push(state, head);
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

void destroy(LispisState *state, GcObjectHeader *header) {
    state->numAllocated--;
    //printf("deallocating ");
    switch (header->type) {
        case GC_GLOBAL_ENV: {
            //printf("Env ");
            GlobalEnv *e = (GlobalEnv *)header;
            free(e->variables);
        } break;
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
            case GC_GLOBAL_ENV: {
                GlobalEnv *e = (GlobalEnv *)obj;
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
    uint32 numToProcess = state->numAllocated/2;
    if (numToProcess < 8) {
        numToProcess = 8;
    }
    if (state->sweepPhase) {
        for (uint32 i = 0; i < numToProcess; ++i) {
            state->sweepPhase = sweepStep(state);
            if (!state->sweepPhase) {
                //printf("sweep end\n");
                //assert(state->grayStack);
                break;
            }
        }
    } else {
        numToProcess /= 2;
        for (uint32 i = 0; i < numToProcess; ++i) {
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
Value compileNullTerminatedString(LispisState *state, char *str) {
    LispisFunction *entry =
        (LispisFunction *)callocGcObject(state, sizeof(LispisFunction));
    entry->header.type = GC_LISPIS_FUNCTION;
    entry->bytecodeSize = 256;
    entry->bytecode =
        (Bytecode *)calloc(1, entry->bytecodeSize * sizeof(Bytecode));

    Lexer lexer = {str};
    eatToken(&lexer);

    CompilerPass compilerPasses[] = {
        symbolIdPass,
        // TODO: integrate quotePass into evalMacroPass and make it
        // expand recursive macros correctly
        evalMacroPass,
        macroPass,
        lambdaPass,
        letPass,
        definePass,
        ifPass,
        //variablePass
    };
    pushOp(state, entry, OP_PUSH);
    pushInt32(state, entry, 0);
    pushOp(state, entry, OP_POP_ASSERT_EQUAL);
    LexicalScope entryScope = {};
    // Where we left of, WHO THE FUCK SHOULD SET LispisFunction's
    // numLocals and upvalProtos
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
            next = compilerPasses[i](state, prev);
            dealloc(prev);
            prev = next;
            next = 0;
        }
        Expr *astDone = variablePass(state, prev, &entryScope);
        dealloc(prev);
#if LOG_ENABLED
        printf("AST:\n");
        dumpTree(state, astDone, 0);
#endif
        compileExpression(state, entry, astDone);
        // hack
        entry->upvalProtos = 0;
        entry->upvalProtosSize = 0;
        entry->numLocals = entryScope.variableIDsTop;
        dealloc(astDone);
        //dealloc(parseTreeExpr);
        if (peekToken(&lexer).tokenType != TOK_EOF) {
            pushOp(state, entry, OP_CLEAR_STACK);
        }
    }
    pushOp(state, entry, OP_RETURN);
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
    return nanPackPointer(entryObj, LISPIS_LFUNC);
}

void initState(LispisState *state) {
    state->globalEnviroment.header.type = GC_GLOBAL_ENV;
    state->globalSymbolTable.symbolsSize = 256;
    state->globalEnviroment.variablesSize = 256;
    state->globalSymbolTable.symbols =
        (Symbol *)calloc(1,
                         sizeof(Symbol) *
                         state->globalSymbolTable.symbolsSize +
                         sizeof(Var) *
                         state->globalEnviroment.variablesSize);
    state->globalEnviroment.variables =
        (GlobalVar *)(state->globalSymbolTable.symbols +
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

    setupSpecialFormSymbols(state);
}

Value runNullTerminatedString(LispisState *state, char *str) {
    Value entryObjVal = compileNullTerminatedString(state,
                                                    str);
    Value retVal = runFunction(state, entryObjVal, 0);
    clearStack(state);
    unfreeze(state, &unpackLFunc(entryObjVal)->header);
    return retVal;
}

uint32 internCStr(LispisState *state, const char *cstr) {
    String str;
    str.val = (char *)cstr;
    str.length = strlen(cstr);
    return internSymbol(state, str, hashFunc(str));
}