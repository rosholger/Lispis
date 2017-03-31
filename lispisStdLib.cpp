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
    if (numArgs == 0) {
        pushInternal(state, nanPackInt32(0));
        return true;
    }
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
    if (numArgs == 0) {
        pushInternal(state, nanPackInt32(1));
        return true;
    }
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

bool lispisGreater(LispisState *state, uint64 numArgs) {
    assert(numArgs == 2);
    Value rh = popInternal(state);
    Value lh = popInternal(state);
    bool ret = false;
    if (getType(rh) == LISPIS_DOUBLE && getType(lh) == LISPIS_DOUBLE) {
        ret = lh.f64 > rh.f64;
    } else if (getType(rh) == LISPIS_INT32 &&
               getType(lh) == LISPIS_INT32) {
        ret = unpackInt(lh) > unpackInt(rh);
    } else if (getType(rh) == LISPIS_DOUBLE &&
               getType(lh) == LISPIS_INT32) {
        ret = unpackInt(lh) > rh.f64;
    } else if (getType(rh) == LISPIS_INT32 &&
               getType(lh) == LISPIS_DOUBLE) {
        ret = lh.f64 > unpackInt(rh);
    } else {
        assert(false && "> only works on ints and doubles");
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

bool lispisNumEqual(LispisState *state, uint64 numArgs) {
    pushError(state, numArgs == 2,
              "= is binary");
    Value rh = popInternal(state);
    Value lh = popInternal(state);
    bool ret = false;
    if (getType(rh) == LISPIS_DOUBLE && getType(lh) == LISPIS_DOUBLE) {
        ret = lh.f64 == rh.f64;
    } else if (getType(rh) == LISPIS_INT32 &&
               getType(lh) == LISPIS_INT32) {
        ret = unpackInt(lh) == unpackInt(rh);
    } else if (getType(rh) == LISPIS_DOUBLE &&
               getType(lh) == LISPIS_INT32) {
        ret = unpackInt(lh) == rh.f64;
    } else if (getType(rh) == LISPIS_INT32 &&
               getType(lh) == LISPIS_DOUBLE) {
        ret = lh.f64 == unpackInt(rh);
    } else {
        pushError(state, false, "= only works on ints and doubles");
    }
    pushInternal(state, nanPackBoolean(ret));
    return true;
}

bool lispisEqual(LispisState *state, uint64 numArgs) {
    pushError(state, numArgs == 2,
              "eq is binary");
    Value rh = popInternal(state);
    Value lh = popInternal(state);
    if (rh.ui64 == lh.ui64) {
        pushInternal(state, nanPackBoolean(true));
    } else if (getType(rh) != getType(lh)) {
        if (getType(rh) == LISPIS_DOUBLE) {
            Value tmp = rh;
            rh = lh;
            lh = tmp;
        }
        if (getType(rh) == LISPIS_INT32 && getType(lh) == LISPIS_DOUBLE) {
            int32 i = unpackInt(rh);
            double d = lh.f64;
            pushInternal(state, nanPackBoolean(i == d));
        } else {
            pushInternal(state, nanPackBoolean(false));
        }
    } else { // same type, different binary representation...
        switch (getType(rh)) {
            case LISPIS_SYM_IDX: {
                pushInternal(state,
                             nanPackBoolean(unpackSymbolID(rh) ==
                                            unpackSymbolID(lh)));
            } break;
            case LISPIS_INT32: {
                pushInternal(state,
                             nanPackBoolean(unpackInt(rh) ==
                                            unpackInt(lh)));
            } break;
            case LISPIS_UNDEF: {
                pushInternal(state, nanPackBoolean(false));
            } break;
            case LISPIS_BOOLEAN: {
                bool rhb = unpackBoolean(rh);
                bool lhb = unpackBoolean(lh);
                pushInternal(state, nanPackBoolean(rhb == lhb));
            } break;
            case LISPIS_DOUBLE: {
                pushInternal(state,
                             nanPackBoolean(rh.f64 == lh.f64));
            } break;
            default: pushError(state, false, "unknown type");
        }
    }
    return true;
}

bool lispisCar(LispisState *state, uint64 numArgs) {
    assert(numArgs == 1);
    Value v = popInternal(state);
    Pair *p = unpackCons(v);
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

#define intAbs(v) ((v) < 0 ? -v : v)

bool lispisAppendDestructive(LispisState *state, uint64 numArgs) {
    pushError(state, numArgs == 2,
              "append (currently) takes two arguments");
    Value appendedV = popInternal(state);
    Value appendedToV = popInternal(state);
    pushError(state,
              (getType(appendedV) == LISPIS_VECTOR &&
               getType(appendedToV) == LISPIS_VECTOR),
              "append currently only implemented on vectors");
    Vector *appended = unpackVector(appendedV);
    Vector *appendedTo = unpackVector(appendedToV);
    appendVectorDestructive(state, appendedTo, appended);
    pushInternal(state, appendedToV);
    return true;
}

bool lispisAppend(LispisState *state, uint64 numArgs) {
    pushError(state, numArgs == 2,
              "append (currently) takes two arguments");
    Value appendedV = popInternal(state);
    Value appendedToV = popInternal(state);
    pushError(state,
              (getType(appendedV) == LISPIS_VECTOR &&
               getType(appendedToV) == LISPIS_VECTOR),
              "append currently only implemented on vectors");
    Vector *appended = unpackVector(appendedV);
    Vector *appendedTo = unpackVector(appendedToV);
    Vector *result = allocVector(state,
                                 appended->numFilled +
                                 appendedTo->numFilled);
    appendVectorDestructive(state, result, appendedTo);
    appendVectorDestructive(state, result, appended);
    pushInternal(state, nanPackPointer(result, LISPIS_VECTOR));
    return true;
}

bool lispisIsUndef(LispisState *state, uint64 numArgs) {
    pushError(state, numArgs == 1,
              "undef? is unary");
    pushInternal(state,
                 nanPackBoolean(getType(popInternal(state)) ==
                                        LISPIS_UNDEF));
    return true;
}

bool lispisSize(LispisState *state, uint64 numArgs) {
    pushError(state, numArgs == 1,
              "size is unary");
    Value v = popInternal(state);
    switch (getType(v)) {
        case LISPIS_VECTOR: {
            Vector *vec = unpackVector(v);
            pushInternal(state, nanPackInt32(vec->numFilled));
        } break;
        case LISPIS_OBJECT: {
            pushError(state, false, "(size object) not-yet-implementer");
        } break;
        case LISPIS_CONS: {
            int32 len = 0;
            for (; !isNill(v) && getType(v) == LISPIS_CONS;
                 v = unpackCons(v)->cdr) {
                len++;
            }
            if (!isNill(v)) {
                len++;
            }
            pushInternal(state, nanPackInt32(len));
        } break;
        default: pushError(state, false,
                           "cant take size of this type");
    }
    return true;
}

bool lispisGetType(LispisState *state, uint64 numArgs) {
    pushError(state, numArgs == 1, "type proc is unary");
    switch (getType(popInternal(state))) {
        case LISPIS_UNDEF: {
            pushInternal(state,
                         nanPackSymbolIdx(internCStr(state,
                                                     "undefined")));
        } break;
        case LISPIS_INT32: {
            pushInternal(state,
                         nanPackSymbolIdx(internCStr(state,
                                                     "int")));
        } break;
        case LISPIS_SYM_IDX: {
            pushInternal(state,
                         nanPackSymbolIdx(internCStr(state,
                                                     "symbol")));
        } break;
        case LISPIS_USERP: {
            pushInternal(state,
                         nanPackSymbolIdx(internCStr(state,
                                                     "user-pointer")));
        } break;
        case LISPIS_CFUNC: {
            pushInternal(state,
                         nanPackSymbolIdx(internCStr(state,
                                                     "c-procedure")));
        } break;
        case LISPIS_LFUNC: {
            pushInternal(state,
                         nanPackSymbolIdx(internCStr(state,
                                                     "procedure")));
        } break;
        case LISPIS_CONS: {
            pushInternal(state,
                         nanPackSymbolIdx(internCStr(state,
                                                     "cons")));
        } break;
        case LISPIS_BOOLEAN: {
            pushInternal(state,
                         nanPackSymbolIdx(internCStr(state,
                                                     "boolean")));
        } break;
        case LISPIS_VECTOR: {
            pushInternal(state,
                         nanPackSymbolIdx(internCStr(state,
                                                     "vector")));
        } break;
        case LISPIS_OBJECT: {
            pushInternal(state,
                         nanPackSymbolIdx(internCStr(state,
                                                     "object")));
        } break;
        case LISPIS_DOUBLE: {
            pushInternal(state,
                         nanPackSymbolIdx(internCStr(state,
                                                     "double")));
        } break;
        default: pushError(state, false, "unknown type");
    }
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

    String greaterSymbol;
    greaterSymbol.val = (char *)">";
    greaterSymbol.length = 1;
    bindFunction(state, greaterSymbol, lispisGreater);

    String lessOrEqualSymbol;
    lessOrEqualSymbol.val = (char *)"<=";
    lessOrEqualSymbol.length = 2;
    bindFunction(state, lessOrEqualSymbol, lispisLessOrEqual);

    String numEqualSymbol;
    numEqualSymbol.val = (char *)"=";
    numEqualSymbol.length = 1;
    bindFunction(state, numEqualSymbol, lispisNumEqual);

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

    String appendDestructiveStr;
    appendDestructiveStr.val = (char *)"append!";
    appendDestructiveStr.length = 7;
    bindFunction(state, appendDestructiveStr, lispisAppendDestructive);

    String appendStr;
    appendStr.val = (char *)"append";
    appendStr.length = 6;
    bindFunction(state, appendStr, lispisAppend);

    String isUndef;
    isUndef.val = (char *)"undef?";
    isUndef.length = strlen(isUndef.val);
    bindFunction(state, isUndef, lispisIsUndef);

    String sizeStr;
    sizeStr.val = (char *)"size";
    sizeStr.length = strlen(sizeStr.val);
    bindFunction(state, sizeStr, lispisSize);

    String typeStr;
    typeStr.val = (char *)"type";
    typeStr.length = strlen(typeStr.val);
    bindFunction(state, typeStr, lispisGetType);

    String eqStr;
    eqStr.val = (char *)"eq?";
    eqStr.length = strlen(eqStr.val);
    bindFunction(state, eqStr, lispisEqual);


    String undef;
    undef.val = (char *)"*undefined*";
    undef.length = strlen(undef.val);
    uint32 undefID = internSymbol(state, undef, hashFunc(undef));
    setGlobal(state, nanPack(0, LISPIS_UNDEF), undefID);

    uint32 trueVal = internCStr(state, "true");
    setGlobal(state, nanPackBoolean(true), trueVal);

    uint32 falseVal = internCStr(state, "false");
    setGlobal(state, nanPackBoolean(false), falseVal);

    runNullTerminatedString(state,
                            (char *)
                            "(defmacro! defun! (name params . body)"
                            "  `(define! ,name (lambda ,params ,@body)))"
                            "*undefined*"
                            );
    runNullTerminatedString(state,
                            (char *)
                            "(defmacro! letfun! (name params . body)"
                            "  `(let! ,name (lambda ,params ,@body)))"
                            "*undefined*"
                            );
    runNullTerminatedString(state,
                            (char *)
                            "(defun! list lst lst)"
                            "*undefined*"
                            );
    runNullTerminatedString(state,
                            (char *)
                            "(defmacro! : (obj ref)"
                            "  (let! qref (list 'quasiquote ref))"
                            "  `(ref ,obj ,qref))"
                            "*undefined*"
                            );
    runNullTerminatedString(state,
                            (char *)
                            "(defmacro! :! (obj ref val)"
                            "  (let! qref (list 'quasiquote ref))"
                            "  `(ref-set ,obj ,qref ,val))"
                            "*undefined*"
                            );
    runNullTerminatedString(state,
                            (char *)
                            "(defmacro! caar (c)"
                            "  `(car (car ,c)))"
                            "*undefined*"
                            );
    runNullTerminatedString(state,
                            (char *)
                            "(defmacro! cdar (c)"
                            "  `(cdr (car ,c)))"
                            "*undefined*"
                            );
    runNullTerminatedString(state,
                            (char *)
                            "(defmacro! and elems"
                            "  (if (null? elems)"
                            "    true"
                            "    `(if ,(car elems)"
                            "       (and ,@(cdr elems))"
                            "       false)))"
                            "*undefined*"
                            );
    runNullTerminatedString(state,
                            (char *)
                            "(defmacro! not (elem)"
                            "  `(if ,elem"
                            "     false"
                            "     true))"
                            "*undefined*"
                            );
    runNullTerminatedString(state,
                            (char *)
                            "(defmacro! cond clauses"
                            "  (if (and (not (null? clauses))"
                            "           (not (null? (car clauses))))"
                            "    `(if ,(caar clauses)"
                            "       (do ,@(cdar clauses))"
                            "       (cond ,@(cdr clauses)))"
                            "    '*undefined*))"
                            "*undefined*"
                            );
    runNullTerminatedString(state,
                            (char *)
                            // Move foldl, foldr and map into C++ code
                            "(defun! foldr (proc init lst)"
                            "  (if (null? lst)"
                            "    init"
                            "    (proc (car lst)"
                            "          (foldr proc init (cdr lst)))))"
                            "*undefined*"
                            );
    runNullTerminatedString(state,
                            (char *)
                            "(defun! map (proc lst)"
                            "  (foldr (lambda (e r)"
                            "           (cons (proc e) r))"
                            "         '() lst))"
                            "*undefined*"
                            );
    runNullTerminatedString(state,
                            (char *)
                            "(defun! foldl (proc init lst)"
                            "  (let! res init)"
                            "  (for (lst (not (null? lst))"
                            "        (set! lst (cdr lst)))"
                            "    (set! res (proc (car lst) res)))"
                            "  res)"
                            "*undefined*"
                            );
    runNullTerminatedString(state,
                            (char *)
                            "(defmacro! reverse (lst)"
                            "  `(foldl cons '() ,lst))"
                            "*undefined*"
                            );
    runNullTerminatedString(state,
                            (char *)
                            "(defmacro! or elems"
                            "  `(cond ,@(map (lambda (e)"
                            "                  (list e 'true))"
                            "                elems)"
                            "         (true false)))"
                            "*undefined*"
                            );
}