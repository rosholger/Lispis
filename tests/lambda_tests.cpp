#include "tests.h"

TEST(lambdaIsLambda) {
    TEST_SETUP;
    RUN_STR(res, s, "(lambda () 1)");
    t_assert("lambda is lambda...", unpackLFunc(res) && s);
}

TEST(lambdaReturnInt) {
    TEST_SETUP;
    RUN_STR(res, s, "(lambda () 1)");
    bool s1 = runFunction(&state, res, 0);
    Value ret;
    pop(&state, &ret);
    t_assert("lambda returns int",
             unpackInt(ret) == 1 && s && s1);
}

TEST(lambdaReturnSymbol) {
    TEST_SETUP;
    RUN_STR(res, s, "(lambda () 'a)");
    bool s2 = runFunction(&state, res, 0);
    Value sym;
    pop(&state, &sym);
    t_assert("lambda returns symbol",
             unpackSymbolID(sym) ==
             internCStr(&state, "a") && s && s2);
}

TEST(lambdaReturnList) {
    TEST_SETUP;
    RUN_STR(res, s1, "(lambda () '(a b s ds p welkj))");
    RUN_STR(lst, s2, "'(a b s ds p welkj)");
    bool s3 = runFunction(&state, res, 0);
    Value ret;
    pop(&state, &ret);
    t_assert("lambda return list",
             deepEqual(lst, ret) && s1 && s2 && s3);
}

TEST(lambdaClosing) {
    TEST_SETUP;
    RUN_STR(res, s,
            "(let! a 1)"
            "(lambda () a)");
    bool s2 = runFunction(&state, res, 0);
    Value ret;
    pop(&state, &ret);
    t_assert("closure", unpackInt(ret) == 1 && s && s2);
}

TEST(facTest) {
    TEST_SETUP;
    RUN_STR(res, s,
            "(let! fac (lambda (n) (if (< n 1) 1 (* n (fac (- n 1))))))"
            "fac");
    push(&state, nanPackInt32(12));
    bool s2 = runFunction(&state, res, 1);
    Value ret;
    pop(&state, &ret);
    t_assert("factorial",
             unpackInt(ret) == 479001600 && s && s2);
}

TEST(listFuncTest) {
    TEST_SETUP;
    Value lstFunc =
        lookupGlobal(&state,
                     nanPackSymbolIdx(internCStr(&state, "list")));
    RUN_STR(lst, s, "'(a b asd 153)");
    push(&state, nanPackSymbolIdx(internCStr(&state, "a")));
    push(&state, nanPackSymbolIdx(internCStr(&state, "b")));
    push(&state, nanPackSymbolIdx(internCStr(&state, "asd")));
    push(&state, nanPackInt32(153));
    bool s2 = runFunction(&state, lstFunc, 4);
    Value ret;
    pop(&state, &ret);
    t_assert("list function",
             deepEqual(ret, lst) && s && s2);
}

TEST(multipleUpvals) {
    TEST_SETUP;
    RUN_STR(ret, s,
            "(letfun! test1 ()"
            "  (let! a 1)"
            "  (let! b 2)"
            "  (letfun! test2 ()"
            "    b)"
            "  (test2))"
            "(test1)");
    t_assert("multiple upvals", unpackInt(ret) == 2 && s);
}

TEST(innerRecursive) {
    TEST_SETUP;
    RUN_STR(ret, s,
            "(letfun! t1 ()"
            "  (let! a 1)"
            "  (letfun! t2 (n)"
            "    (if (< n 2)"
            "      1"
            "      (+ (t2 (- n 1)) 1)))"
            "  (t2 4))"
            "(t1)");
    t_assert("inner recursive", unpackInt(ret) == 4 && s);
}

TEST(multiLevelUpval) {
    TEST_SETUP;
    RUN_STR(ret, s,
            "(let! a 1)"
            "(letfun! t1 ()"
            "  (letfun! t2 ()"
            "    a)"
            "  (t2))"
            "(t1)");
    t_assert("multi-level upval", unpackInt(ret) == 1 && s);
}