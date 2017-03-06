#include "vm.h"
#include <cassert>
#include <cstdlib>
#include <cstdio>

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

bool isNill(Value v) {
    return getType(v) == LISPIS_CONS && !unpackCons(v);
}

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
    return funcObj;
}


ActivationRecord *allocActivationRecord(LispisState *state,
                                        LispisFunction *func,
                                        Env *parentEnv) {
    ActivationRecord *record =
        (ActivationRecord *)callocGcObject(state,
                                           sizeof(ActivationRecord));
    record->header.type = GC_ACTIVATION_RECORD;
    //putInZeroCountTable(state, (GcObjectHeader *)record);
    if (func) {
        //incRef(state, (GcObjectHeader *)func);
    }
    if (parentEnv) {
        //incRef(state, (GcObjectHeader *)parentEnv);
    }
    record->function = func;
    if (parentEnv) {
        record->enviroment = (Env *)callocGcObject(state, sizeof(Env));
        record->enviroment->header.type = GC_ENV;
        //incRef(state, (GcObjectHeader *)record->enviroment);
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

void popActivationRecord(LispisState *state) {
    state->currRecord = state->currRecord->caller;
}

Value lookupGlobal(LispisState *state, Value symbol) {
    uint32 id = unpackSymbolID(symbol);
    Value result =
        evalGlobalSymbol(&state->globalEnviroment, id);
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

Value runFunction(LispisState *state, Value funcObjValue,
                  uint64 numArgs) {
    assert(numArgs <= state->dataStackTop);
    LispisFunctionObject *funcObj = unpackLFunc(funcObjValue);
    {
        ActivationRecord *record =
            allocActivationRecord(state, funcObj);
        //decRef(state, &funcObj->header);
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
                uint32 id = unpackSymbolID(pop(state));
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
                    assert(!lfuncObj->function->macro);
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
    setVariableRaw(state, &state->globalEnviroment, funcVal, symbolID);
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
