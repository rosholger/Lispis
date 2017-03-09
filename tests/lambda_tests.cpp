#include "tests.h"

TEST(lambdaIsLambda) {
    TEST_SETUP;
    RUN_STR(res, "(lambda () 1)");
    t_assert("lambda is lambda...", unpackLFunc(res));
}

TEST(lambdaReturnInt) {
    TEST_SETUP;
    RUN_STR(res, "(lambda () 1)");
    t_assert("lambda returns int",
             unpackInt(runFunction(&state, res, 0)) == 1);
}

TEST(lambdaReturnSymbol) {
    TEST_SETUP;
    RUN_STR(res, "(lambda () 'a)");
    t_assert("lambda returns symbol",
             unpackSymbolID(runFunction(&state, res, 0)) ==
             internCStr(&state, "a"));
}

TEST(lambdaReturnList) {
    TEST_SETUP;
    RUN_STR(res, "(lambda () '(a b s ds p welkj))");
    RUN_STR(lst, "'(a b s ds p welkj)");
    t_assert("lambda return list",
             deepEqual(lst, runFunction(&state, res, 0)));
}

TEST(lambdaClosing) {
    TEST_SETUP;
    RUN_STR(res,
            "(let! a 1)"
            "(lambda () a)");
    t_assert("closure", unpackInt(runFunction(&state, res, 0)) == 1);
}

TEST(facTest) {
    TEST_SETUP;
    RUN_STR(res,
            "(let! fac (lambda (n) (if (< n 1) 1 (* n (fac (- n 1))))))"
            "fac");
    push(&state, nanPackInt32(12));
    t_assert("factorial",
             unpackInt(runFunction(&state, res, 1)) == 479001600);
}

TEST(listFuncTest) {
    TEST_SETUP;
    Value lstFunc =
        lookupGlobal(&state,
                     nanPackSymbolIdx(internCStr(&state, "list")));
    RUN_STR(lst, "'(a b asd 153)");
    push(&state, nanPackSymbolIdx(internCStr(&state, "a")));
    push(&state, nanPackSymbolIdx(internCStr(&state, "b")));
    push(&state, nanPackSymbolIdx(internCStr(&state, "asd")));
    push(&state, nanPackInt32(153));
    t_assert("list function",
             deepEqual(runFunction(&state, lstFunc, 4), lst));
}