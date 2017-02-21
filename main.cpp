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

uint32 unpackSymbolID(Value v) {
    assert(getType(v) == LISPIS_SYM_IDX);
    return v.ui32;
}

void *unpackPointer(Value v, NanPackingTypes typeID) {
    uint64 unpacked = v.ui64 & 0xFFFFFFFFFFFF;
    assert(getType(v) == typeID);
    if (unpacked > 0x00007FFFFFFFFFFF) {
        unpacked |= 0xFFFF000000000000;
    }
    return (void *)unpacked;
}


CFunction *unpackCFunc(Value v) {
    return (CFunction *)unpackPointer(v, LISPIS_CFUNC);
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
    st->symbols[stIndex].localSymbolID = state->nextGlobalSymbolID;
    st->symbols[stIndex].globalSymbolID = state->nextGlobalSymbolID;
    state->nextGlobalSymbolID++;
    st->symbols[stIndex].hash = symHash;
    st->symbols[stIndex].filled = true;
    st->symbolsInterned++;
    return st->symbols[stIndex].globalSymbolID;
}

void loadLocalSymbolRaw(LispisState *state,
                        LispisFunction *func,
                        String symbol,
                        uint32 symbolID) {
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
}

Bytecode *loadSymbolSection(LispisState *state,
                            LispisFunction *func,
                            Bytecode *startBytecode) {
    Bytecode *bytecode = startBytecode;
    while (bytecode->opCode != OP_EXIT) {
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
    assert(env);
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
                             Env *parentEnv,
                             Bytecode *bytecode) {
    LispisFunction *ret =
        (LispisFunction *)calloc(1, sizeof(LispisFunction));
    ret->enviroment =  (Env *)calloc(1, sizeof(Env));
    ret->enviroment->parentEnv = parentEnv;
    ret->enviroment->variablesSize = 256;
    ret->enviroment->variables =
        (Var *)calloc(1,
                      sizeof(Var) *
                      ret->enviroment->variablesSize);
    ret->localToGlobalTableSize = 64;
    ret->localToGlobalTable =
        (uint32 *)calloc(ret->localToGlobalTableSize, sizeof(uint32));
    bytecode = loadSymbolSection(state, ret, bytecode);
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
            //case NAN_PACKING_LFUNC: {
            //} break;
        case LISPIS_UNDEF:
        default:assert(false);
    }
}

Value runFunction(LispisState *state, LispisFunction *func) {
    {
        ActivationRecord *record =
            (ActivationRecord *)calloc(1, sizeof(ActivationRecord));
        record->caller = state->currRecord;
        record->dataStackBottom = state->dataStackTop;
        state->currRecord = record;
    }
    while(func->bytecode[state->currRecord->pc].opCode != OP_EXIT) {
        OpCodes op =
            func->bytecode[state->currRecord->pc].opCode;
        state->currRecord->pc++;
        switch (op) {
            case OP_EVAL_SYMBOL: {
                SymbolID id = unpackSymbolID(pop(state));
                // FIXME: global variables
                Value result = evalGlobalSymbol(func->enviroment, id);
                push(state, result);
            } break;
            case OP_RETURN: {
                Value ret = pop(state);
                assert(state->dataStackTop == 0);
                return ret;
            } break;
            case OP_PUSH: {
                Value p;
                p = func->bytecode[state->currRecord->pc];
                state->currRecord->pc++;
                push(state, p);
            } break;
            case OP_PUSH_TRANSLATE_SYMBOL: {
                Value idVal;
                idVal = func->bytecode[state->currRecord->pc];
                uint32 id = unpackSymbolID(idVal);
                assert(id < func->localToGlobalTableFilled);
                uint32 globalId = func->localToGlobalTable[id];
                idVal = nanPackSymbolIdx(globalId);
                state->currRecord->pc++;
                push(state, idVal);
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
                {
                    ActivationRecord *record =
                        (ActivationRecord *)calloc(1,
                                                   sizeof(ActivationRecord));
                    record->caller = state->currRecord;
                    record->dataStackBottom = state->dataStackTop-numArgs;
                    state->currRecord = record;
                }
                if (getType(callee) == LISPIS_CFUNC) {
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
                    //TODO
                    assert(false);
                } else {
                    assert(false);
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
    quote.val = (char *)"quote";
    quote.length = 5;
    quoteSym.exprType = EXPR_SYMBOL;
    quoteSym.str = quote;
    char *fileContent = readEntireFile((char *)"./test.lsp");
    Lexer lexer = {fileContent};
    eatToken(&lexer);
    Expr *parseTreeExpr = parseExpression(&lexer);
    printf("ParseTree:\n");
    dumpTree(parseTreeExpr, 0);
    Expr *astExpr = firstPass(parseTreeExpr);
    printf("AST:\n");
    dumpTree(astExpr, 0);
    CompilerState compiler = {};
    compiler.bytecodeSize = 256;
    compiler.symbolIndexMap.symbolsSize = 3;
    compiler.bytecode =
        (Bytecode *)calloc(1, compiler.bytecodeSize * sizeof(Bytecode) +
                           compiler.symbolIndexMap.symbolsSize *
                           sizeof(SymIdBucket));
    compiler.symbolIndexMap.symbolMap =
        (SymIdBucket *)(compiler.bytecode + compiler.bytecodeSize);
    compileExpression(&compiler, astExpr);
    compiler.symbolSectionSize = 256;
    compiler.symbolSection =
        (Bytecode *)calloc(compiler.symbolSectionSize,
                           sizeof(Bytecode));
    encodeSymbolSection(&compiler);
    pushOp(&compiler, OP_RETURN);
    printf("\nSymbol section:\n");
    dumpSymbolSection(compiler.symbolSection);
    printf("\nBytecode section:\n");
    dumpBytecode(compiler.bytecode);

    Bytecode *bytecode = (Bytecode *)malloc(sizeof(Bytecode) *
                                            compiler.bytecodeTop +
                                            sizeof(Bytecode) *
                                            compiler.symbolSectionTop);
    memcpy(bytecode, compiler.symbolSection, (sizeof(Bytecode) *
                                              compiler.symbolSectionTop));
    memcpy(bytecode + compiler.symbolSectionTop,
           compiler.bytecode, sizeof(uint64) * compiler.bytecodeTop);
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




    // standard library


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


    // usage code!


    LispisFunction *entry = loadFunction(&state,
                                         &state.globalEnviroment,
                                         bytecode);
    printf("enviroment:\n");
    dumpEnviroment(&state.globalSymbolTable,
                   entry,
                   entry->enviroment);

    Value retVal = runFunction(&state, entry);
    printf("ret val:\n");
    printValue(&state.globalSymbolTable,
               entry,
               retVal);
    
    dealloc(astExpr);
    dealloc(parseTreeExpr);
    return 0;
}