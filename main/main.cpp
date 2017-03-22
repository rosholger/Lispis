#include "../lexer.h"
#include "../parser.h"
#include "../codegen.h"
#include "../compiler_passes.h"
#include "../vm.h"
#include "../lispisStdLib.h"

#include <cstdio>
#include <cstdlib>
#include <cctype>
#include <cstring>
#include <cmath>
#include <cassert>

#if 0
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
#if 0
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
#endif

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
#endif

#if 0
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
            printValue(globalSymbolTable, symId);
            printf("==\n");
            printValue(globalSymbolTable,
                       env->variables[i].val);
        }
    }
    dumpEnviroment(globalSymbolTable, func, env->parentEnv);
}
#endif

#if 0
void decRefAllInEnv(LispisState *state, Env *env) {
    for (uint64 i = 0, f = 0; f < env->variablesFilled; i++) {
        if (env->variables[i].filled) {
            f++;
            decRef(state, env->variables[i].val);
        }
    }
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
#endif

int main(int argsc, char **args) {
    assert(sizeof(Bytecode) == sizeof(uint64));
    LispisState state = {};
    initState(&state);
    initStdLib(&state);
    char *fileContent = readEntireFile((char *)"./test.lsp");
    if (runNullTerminatedString(&state, fileContent)) {
        Value retVal;
        if (!pop(&state, &retVal)) {
            Value err;
            if (!pop(&state, &err)) {
                printf("Failed to pop return value of test.lsp with"
                       "error: ");
                printValue(&state.globalSymbolTable, err);
            }
        }
        printValue(&state.globalSymbolTable,
                   retVal);
        clearStack(&state);
        Value ret;
        for (int i = 0; i < 1000; ++i) {
            push(&state, nanPackInt32(6));
            if (!runFunction(&state, retVal, 1)) {
                Value err;
                pop(&state, &err);
                printf("Failed to run retval. Error: ");
                printValue(&state.globalSymbolTable, err);
                break;
            }
            if (!pop(&state, &ret)) {
                printf("Failed to pop return value of the"
                       "return value of test.lsp with"
                       "error: ");
                Value err;
                pop(&state, &err);
                printValue(&state.globalSymbolTable, err);
            }
            clearStack(&state);
            markAndSweep(&state);
        }
        printValue(&state.globalSymbolTable,
                   ret);
    } else {
        printf("Failed to run test.lsp. Error: ");
        Value err;
        pop(&state, &err);
        printValue(&state.globalSymbolTable, err);
    }
    
    destroy(&state);
    free(fileContent);
    
    return 0;
}