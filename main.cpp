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

ActivationRecord *unpackLFunc(Value v) {
    return (ActivationRecord *)unpackPointer(v, LISPIS_LFUNC);
}

Value emptyList() {
    return nanPackPointer(0, LISPIS_CONS);
}

void setVariableRaw(Env *env, Value v, uint32 symbolID) {
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
    assertStackInBounds(state, state->dataStackTop - 1);
    return state->dataStack[state->dataStackTop-1];
}

Value pop(LispisState *state) {
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

Value cons(LispisState *state, Value ncar, Value ncdr) {
    Pair *pair = (Pair *)malloc(sizeof(Pair));
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

Pair *unpackCons(Value v) {
    return (Pair *)unpackPointer(v, LISPIS_CONS);
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
            ActivationRecord *lfunc = unpackLFunc(v);
            printf("<#Function %p>\n", lfunc);
        } break;
            //case NAN_PACKING_LFUNC: {
            //} break;
        case LISPIS_UNDEF:
        default:assert(false);
    }
}

ActivationRecord *allocActivationRecord(LispisState *state,
                                        LispisFunction *func,
                                        Env *parentEnv) {
    ActivationRecord *record =
        (ActivationRecord *)calloc(1, sizeof(ActivationRecord));
    record->function = func;
    record->enviroment =  (Env *)calloc(1, sizeof(Env));
    record->enviroment->parentEnv = parentEnv;
    record->enviroment->variablesSize = 256;
    record->enviroment->variables =
        (Var *)calloc(1,
                      sizeof(Var) *
                      record->enviroment->variablesSize);
    return record;
}

ActivationRecord *copyActivationRecord(LispisState *state,
                                       ActivationRecord *record) {
    ActivationRecord *ret =
        (ActivationRecord *)malloc(sizeof(ActivationRecord));
    memcpy(ret, record, sizeof(ActivationRecord));
    ret->header.refCount = 0;
    return ret;
}

Value runFunction(LispisState *state, LispisFunction *func) {
    {
        ActivationRecord *record =
            allocActivationRecord(state, func, &state->globalEnviroment);
        record->caller = 0;
        record->dataStackBottom = state->dataStackTop;
        state->currRecord = record;
    }
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
                if (state->currRecord->caller) {
                    push(state, ret);
                    state->currRecord = state->currRecord->caller;
                    func = state->currRecord->function;
                } else {
                    state->currRecord = 0;
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
                    {

                        ActivationRecord *record =
                            allocActivationRecord(state,
                                                  0,
                                                  &state->globalEnviroment);
                        record->caller = state->currRecord;
                        record->dataStackBottom =
                            state->dataStackTop-numArgs;
                        record->function = 0;
                        state->currRecord = record;
                    }
                    CFunction *f = unpackCFunc(callee);
                    bool returned = f->func(state, numArgs);
                    if (returned) {
                        Value retVal = pop(state);
                        state->dataStackTop =
                            state->currRecord->dataStackBottom;
                        state->currRecord = state->currRecord->caller;
                        push(state, retVal);
                    } else {
                        state->dataStackTop =
                            state->currRecord->dataStackBottom;
                        state->currRecord = state->currRecord->caller;
                    }
                } else if (getType(callee) == LISPIS_LFUNC) {
                    ActivationRecord *record = unpackLFunc(callee);
                    record = copyActivationRecord(state, record);
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
                ActivationRecord *record =
                    allocActivationRecord(state, func->subFunctions[id],
                                          state->currRecord->enviroment);
                push(state, nanPackPointer(record,
                                           LISPIS_LFUNC));
                push(state, nanPackPointer(unpackLFunc(pop(state)),
                                                           LISPIS_LFUNC));
                printf("PUSH LFUNC!\n");
            } break;
            case OP_POP_ASSERT_EQUAL: {
                Value a = pop(state);
                Value b = pop(state);
                assert(a.ui64 == b.ui64);
            } break;
            case OP_SET_LOCAL_VARIABLE: {
                uint32 symId = unpackSymbolID(pop(state));
                Value v = pop(state);
                setVariableRaw(state->currRecord->enviroment,
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
    CFunction *binding = (CFunction *)malloc(sizeof(CFunction));
    binding->header.refCount = 0;
    binding->func = func;
    Value funcVal;
    funcVal = nanPackPointer(binding, LISPIS_CFUNC);
    uint32 symbolID = internSymbol(state,
                                   symbol,
                                   hashFunc(symbol));
    setVariableRaw(&state->globalEnviroment, funcVal, symbolID);
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
        int pos = 0;
        int neg = 0;
        for (uint32 i = 1; i < numArgs; ++i) {
            if (getType(peek(state)) == LISPIS_DOUBLE) {
            } else {
                neg += unpackInt(pop(state));
            }
        }
        pos = unpackInt(pop(state));
        retValue = nanPackInt32(pos - neg);
    }
    push(state, retValue);
    return true;
}

bool lispisMul(LispisState *state, uint64 numArgs) {
    assert(numArgs >= 1);
    int ret = 1;
    for (uint32 i = 0; i < numArgs; ++i) {
        ret *= unpackInt(pop(state));
    }
    Value retValue;
    retValue = nanPackInt32(ret);
    push(state, retValue);
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

int main(int argsc, char **args) {
    assert(sizeof(Bytecode) == sizeof(uint64));

    // CompileTime
    LispisState state = {};
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
    String ifStr;
    ifStr.val = (char *)"if";
    ifStr.length = strlen(ifStr.val);
    ifSym = internSymbol(&state, ifStr, hashFunc(ifStr));


    String addSymbol;
    addSymbol.val = (char *)"+";
    addSymbol.length = 1;
    bindFunction(&state, addSymbol, lispisAdd);

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
        (LispisFunction *)calloc(1, sizeof(LispisFunction));
    //entry->localToGlobalTableSize = 64;
    //entry->localToGlobalTable =
    //(uint32 *)calloc(entry->localToGlobalTableSize, sizeof(uint32));
    entry->bytecodeSize = 256;
    entry->bytecode =
        (Bytecode *)calloc(1, entry->bytecodeSize * sizeof(Bytecode));

    char *fileContent = readEntireFile((char *)"./test.lsp");
    Lexer lexer = {fileContent};
    eatToken(&lexer);
    while (peekToken(&lexer).tokenType != TOK_EOF) {
        Expr *parseTreeExpr = parseExpression(&lexer);
        printf("ParseTree:\n");
        dumpTree(&state, parseTreeExpr, 0);
        Expr *astExpr2 = symbolIdPass(&state, parseTreeExpr);
        Expr *astExpr1 = quotePass(&state, astExpr2);
        dealloc(astExpr2);
        astExpr2 = 0;
        astExpr2 = lambdaPass(&state, astExpr1);
        dealloc(astExpr1);
        astExpr1 = 0;
        astExpr1 = letPass(&state, astExpr2);
        dealloc(astExpr2);
        astExpr2 = 0;
        astExpr2 = ifPass(&state, astExpr1);
        dealloc(astExpr1);
        astExpr1 = 0;
        Expr *astDone = astExpr2;
        printf("AST:\n");
        dumpTree(&state, astDone, 0);
        compileExpression(&state, entry, astDone);
        dealloc(astExpr2);
        dealloc(astExpr1);
        dealloc(parseTreeExpr);
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

    Value retVal = runFunction(&state, entry);
    printf("ret val:\n");
    printValue(&state.globalSymbolTable,
               entry,
               retVal);
    
    return 0;
}