#include "lispisStdLib.h"
#include "vm.h"

#include <cassert>
#include <cstring>

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

void initStdLib(LispisState *state) {
    String addSymbol;
    addSymbol.val = (char *)"+";
    addSymbol.length = 1;
    bindFunction(state, addSymbol, lispisAdd);
    
    String toFloatSymbol;
    toFloatSymbol.val = (char *)"to-float";
    toFloatSymbol.length = strlen(toFloatSymbol.val);
    bindFunction(state, toFloatSymbol, lispisToFloat);
    
    String subSymbol;
    subSymbol.val = (char *)"-";
    subSymbol.length = 1;
    bindFunction(state, subSymbol, lispisSub);
    
    String mulSymbol;
    mulSymbol.val = (char *)"*";
    mulSymbol.length = 1;
    bindFunction(state, mulSymbol, lispisMul);
    
    String idivSymbol;
    idivSymbol.val = (char *)"//";
    idivSymbol.length = 2;
    bindFunction(state, idivSymbol, lispisIDiv);
    
    String divSymbol;
    divSymbol.val = (char *)"/";
    divSymbol.length = 1;
    bindFunction(state, divSymbol, lispisDiv);
    
    String lessSymbol;
    lessSymbol.val = (char *)"<";
    lessSymbol.length = 1;
    bindFunction(state, lessSymbol, lispisLess);

    String carSymbol;
    carSymbol.val = (char *)"car";
    carSymbol.length = 3;
    bindFunction(state, carSymbol, lispisCar);

    String cdrSymbol;
    cdrSymbol.val = (char *)"cdr";
    cdrSymbol.length = 3;
    bindFunction(state, cdrSymbol, lispisCdr);

    String consSymbol;
    consSymbol.val = (char *)"cons";
    consSymbol.length = 4;
    bindFunction(state, consSymbol, lispisCons);

    String isNullSymbol;
    isNullSymbol.val = (char *)"null?";
    isNullSymbol.length = 5;
    bindFunction(state, isNullSymbol, lispisIsNull);

    String undef;
    undef.val = (char *)"undefined";
    undef.length = strlen(undef.val);
    uint32 undefID = internSymbol(state, undef, hashFunc(undef));
    setVariableRaw(state, &state->globalEnviroment,
                   nanPack(0, LISPIS_UNDEF), undefID);

    runNullTerminatedString(state,
                            (char *)
                            "(defmacro! defun! (name params . body)"
                            "  (quasiquote (define! (unquote name)"
                            "                (lambda (unquote params)"
                            "                  (unquote-splice body)))))"
                            ""
                            "(defmacro! letfun! (name params . body)"
                            "  (quasiquote (let! (unquote name)"
                            "                (lambda (unquote params)"
                            "                  (unquote-splice body)))))"
                            ""
                            "(defun! list lst lst)"
                            "undefined");
}