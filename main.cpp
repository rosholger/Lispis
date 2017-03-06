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

bool lispisCar(LispisState *state, uint64 numArgs) {
    assert(numArgs == 1);
    Pair *p = unpackCons(pop(state));
    push(state, p->car);
    return true;
}

bool lispisCdr(LispisState *state, uint64 numArgs) {
    assert(numArgs == 1);
    Pair *p = unpackCons(pop(state));
    push(state, p->cdr);
    return true;
}

bool lispisIsNull(LispisState *state, uint64 numArgs) {
    assert(numArgs == 1);
    push(state, nanPackBoolean(isNill(pop(state))));
    return true;
}

bool lispisCons(LispisState *state, uint64 numArgs) {
    assert(numArgs == 2);
    Value cdr = pop(state);
    Value car = pop(state);
    push(state, cons(state, car, cdr));
    return true;
}


void printValue(SymbolTable *globalSymbolTable,
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
                assert(v.ui64 != p->car.ui64);
                assert(v.ui64 != p->cdr.ui64);
                printf("(\n");
                printValue(globalSymbolTable, p->car);
                printf(".\n");
                printValue(globalSymbolTable, p->cdr);
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

#define arrayLength(array) (sizeof(array)/sizeof(*array))

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
    state.dataStackSize = 2*1024;
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


    String quasiquoteStr;
    quasiquoteStr.val = (char *)"quasiquote";
    quasiquoteStr.length = strlen(quasiquoteStr.val);
    quasiquote = internSymbol(&state, quasiquoteStr,
                              hashFunc(quasiquoteStr));
    String unquoteStr;
    unquoteStr.val = (char *)"unquote";
    unquoteStr.length = strlen(unquoteStr.val);
    unquote = internSymbol(&state, unquoteStr,
                           hashFunc(unquoteStr));
    String unquoteSpliceStr;
    unquoteSpliceStr.val = (char *)"unquote-splice";
    unquoteSpliceStr.length = strlen(unquoteSpliceStr.val);
    unquoteSplice = internSymbol(&state, unquoteSpliceStr,
                                 hashFunc(unquoteSpliceStr));



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
    String defmacroStr;
    defmacroStr.val = (char *)"defmacro!";
    defmacroStr.length = strlen(defmacroStr.val);
    defmacro = internSymbol(&state, defmacroStr, hashFunc(defmacroStr));


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

    String carSymbol;
    carSymbol.val = (char *)"car";
    carSymbol.length = 3;
    bindFunction(&state, carSymbol, lispisCar);

    String cdrSymbol;
    cdrSymbol.val = (char *)"cdr";
    cdrSymbol.length = 3;
    bindFunction(&state, cdrSymbol, lispisCdr);

    String consSymbol;
    consSymbol.val = (char *)"cons";
    consSymbol.length = 4;
    bindFunction(&state, consSymbol, lispisCons);

    String isNullSymbol;
    isNullSymbol.val = (char *)"null?";
    isNullSymbol.length = 5;
    bindFunction(&state, isNullSymbol, lispisIsNull);


    // usage code!


    // put in utility function
    LispisFunction *entry =
        (LispisFunction *)callocGcObject(&state, sizeof(LispisFunction));
    entry->header.type = GC_LISPIS_FUNCTION;
    //putInZeroCountTable(&state, (GcObjectHeader *)entry);
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
        evalMacroPass,
        quotePass,
        macroPass,
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
    //putInZeroCountTable(&state, &entryObj->header);
    //incRef(&state, &entry->header);
    entryObj->function = entry;
    entryObj->parentEnv = &state.globalEnviroment;

    freeze(&state, &entryObj->header);
    Value entryObjVal = nanPackPointer(entryObj, LISPIS_LFUNC);
    Value retVal = runFunction(&state, entryObjVal, 0);
    //freeze(&state, &unpackLFunc(retVal)->header);
    //clearStack(&state);
    printf("ret val:\n");
    printValue(&state.globalSymbolTable,
               retVal);
    //gc(&state);

    for (int i = 0; i < 1000; ++i) {
        push(&state, nanPackInt32(6));
        Value ret = runFunction(&state, retVal, 1);
        clearStack(&state);
        printf("ret val:\n");
        printValue(&state.globalSymbolTable,
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