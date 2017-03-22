#include "lispisStdLib.h"
#include "vm.h"

#include <cassert>
#include <cstring>

double lispisAddDouble(LispisState *state, uint64 argsLeft) {
    double ret = 0;
    for (uint64 i = 0; i < argsLeft; ++i) {
        Value operand = popInternal(state);
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
        if (getType(peekInternal(state)) != LISPIS_INT32) {
            break;
        }
        Value operand = popInternal(state);
        ret += unpackInt(operand);
    }
    Value retValue;
    if (i < numArgs) {

        retValue.f64 = ret + lispisAddDouble(state, numArgs-i);
    } else {
        retValue = nanPackInt32(ret);
    }
    pushInternal(state, retValue);
    return true;
}

bool lispisToFloat(LispisState *state, uint64 numArgs) {
    Value ret;
    assert(numArgs == 1);
    if (getType(peekInternal(state))) {
        ret.f64 = unpackInt(popInternal(state));
    } else {
        assert(getType(peekInternal(state)) == LISPIS_DOUBLE);
        ret = popInternal(state);
    }
    pushInternal(state, ret);
    return true;
}

bool lispisSub(LispisState *state, uint64 numArgs) {
    assert(numArgs >= 1);
    Value retValue;
    retValue.f64 = 0;

    if (numArgs == 1) {
        Value val = popInternal(state);
        retValue.f64 = (getType(val) == LISPIS_DOUBLE ?
                        -val.f64 :
                        nanPackInt32(-unpackInt(val)).f64);
    } else {
        bool allInt = true;
        for (uint64 i = 0; i < numArgs; ++i) {
            if (getType(indexStackInternal(state, i)) == LISPIS_DOUBLE) {
                allInt = false;
                break;
            } else {
                assert(getType(indexStackInternal(state, i)) == LISPIS_INT32);
            }
        }
        if (allInt) {
            int pos = 0;
            int neg = 0;
            for (uint32 i = 1; i < numArgs; ++i) {
                neg += unpackInt(popInternal(state));
            }
            pos = unpackInt(popInternal(state));
            retValue = nanPackInt32(pos - neg);
        } else {
            double pos = 0;
            double neg = 0;
            for (uint32 i = 1; i < numArgs; ++i) {
                if (getType(peekInternal(state)) == LISPIS_INT32) {
                    neg += unpackInt(popInternal(state));
                } else {
                    neg += popInternal(state).f64;
                }
            }
            if (getType(peekInternal(state)) == LISPIS_INT32) {
                pos = unpackInt(popInternal(state));
            } else {
                pos = popInternal(state).f64;
            }
            retValue.f64 = pos - neg;
        }
    }
    pushInternal(state, retValue);
    return true;
}

bool lispisMul(LispisState *state, uint64 numArgs) {
    assert(numArgs >= 1);
    bool allInt = true;
    for (uint64 i = 0; i < numArgs; ++i) {
        if (getType(indexStackInternal(state, i)) == LISPIS_DOUBLE) {
            allInt = false;
            break;
        } else {
            assert(getType(indexStackInternal(state, i)) == LISPIS_INT32);
        }
    }
    if (allInt) {
        int ret = 1;
        for (uint32 i = 0; i < numArgs; ++i) {
            ret *= unpackInt(popInternal(state));
        }
        Value retValue;
        retValue = nanPackInt32(ret);
        pushInternal(state, retValue);
    } else {
        double ret = 1;
        for (uint32 i = 0; i < numArgs; ++i) {
            ret *= popInternal(state).f64;
        }
        Value retValue;
        retValue.f64 = ret;
        pushInternal(state, retValue);
    }
    return true;
}

bool lispisIDiv(LispisState *state, uint64 numArgs) {
    assert(numArgs == 2);
    int denominator = unpackInt(popInternal(state));
    int nominator = unpackInt(popInternal(state));
    Value retValue;
    retValue = nanPackInt32(nominator/denominator);
    pushInternal(state, retValue);
    return true;
}

bool lispisDiv(LispisState *state, uint64 numArgs) {
    assert(numArgs == 2);
    Value denominator = popInternal(state);
    Value nominator = popInternal(state);
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
    pushInternal(state, retValue);
    return true;
}

bool lispisLess(LispisState *state, uint64 numArgs) {
    assert(numArgs == 2);
    Value rh = popInternal(state);
    Value lh = popInternal(state);
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
        assert(false && "< only works on ints and doubles");
    }
    pushInternal(state, nanPackBoolean(ret));
    return true;
}

bool lispisLessOrEqual(LispisState *state, uint64 numArgs) {
    assert(numArgs == 2);
    Value rh = popInternal(state);
    Value lh = popInternal(state);
    bool ret = false;
    if (getType(rh) == LISPIS_DOUBLE && getType(lh) == LISPIS_DOUBLE) {
        ret = lh.f64 <= rh.f64;
    } else if (getType(rh) == LISPIS_INT32 &&
               getType(lh) == LISPIS_INT32) {
        ret = unpackInt(lh) <= unpackInt(rh);
    } else if (getType(rh) == LISPIS_DOUBLE &&
               getType(lh) == LISPIS_INT32) {
        ret = unpackInt(lh) <= rh.f64;
    } else if (getType(rh) == LISPIS_INT32 &&
               getType(lh) == LISPIS_DOUBLE) {
        ret = lh.f64 <= unpackInt(rh);
    } else {
        assert(false && "<= only works on ints and doubles");
    }
    pushInternal(state, nanPackBoolean(ret));
    return true;
}

bool lispisCar(LispisState *state, uint64 numArgs) {
    assert(numArgs == 1);
    Pair *p = unpackCons(popInternal(state));
    pushInternal(state, p->car);
    return true;
}

bool lispisCdr(LispisState *state, uint64 numArgs) {
    assert(numArgs == 1);
    Pair *p = unpackCons(popInternal(state));
    pushInternal(state, p->cdr);
    return true;
}

bool lispisIsNull(LispisState *state, uint64 numArgs) {
    assert(numArgs == 1);
    pushInternal(state, nanPackBoolean(isNill(popInternal(state))));
    return true;
}

bool lispisCons(LispisState *state, uint64 numArgs) {
    assert(numArgs == 2);
    Value cdr = popInternal(state);
    Value car = popInternal(state);
    pushInternal(state, cons(state, car, cdr));
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

    String lessOrEqualSymbol;
    lessOrEqualSymbol.val = (char *)"<=";
    lessOrEqualSymbol.length = 2;
    bindFunction(state, lessOrEqualSymbol, lispisLessOrEqual);

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
    undef.val = (char *)"*undefined*";
    undef.length = strlen(undef.val);
    uint32 undefID = internSymbol(state, undef, hashFunc(undef));
    setGlobal(state, nanPack(0, LISPIS_UNDEF), undefID);

    runNullTerminatedString(state,
                            (char *)
                            "(defmacro! defun! (name params . body)"
                            "  `(define! ,name (lambda ,params ,@body)))"
                            ""
                            "(defmacro! letfun! (name params . body)"
                            "  `(let! ,name (lambda ,params ,@body)))"
                            ""
                            "(defun! list lst lst)"
                            ""
                            "(defmacro! : (obj ref)"
                            "  (let! qref (list 'quasiquote ref))"
                            "  `(ref ,obj ,qref))"
                            ""
                            "(defmacro! :! (obj ref val)"
                            "  (let! qref (list 'quasiquote ref))"
                            "  `(ref-set ,obj ,qref ,val))"
                            ""
                            "*undefined*"
                            );
}