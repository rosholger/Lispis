#include "tests.h"

TEST(letfunTest) {
    TEST_SETUP;
    RUN_STR(res, s,
            "(letfun! fac (n) (if (< n 1) 1 (* n (fac (- n 1))))) fac");

    push(&state, nanPackInt32(12));
    bool s2 = runFunction(&state, res, 1);
    Value ret;
    pop(&state, &ret);
    t_assert("letfun!",
             unpackInt(ret) == 479001600 && s && s2);
}

TEST(defunTest) {
    TEST_SETUP;
    RUN_STR(res, s,
            "(defun! fac (n) (if (< n 1) 1 (* n (fac (- n 1))))) 0");

    Value fac =
        lookupGlobal(&state,
                     nanPackSymbolIdx(internCStr(&state, "fac")));
    push(&state, nanPackInt32(12));
    bool s2 = runFunction(&state, fac, 1);
    Value ret;
    pop(&state, &ret);
    t_assert("letfun!",
             unpackInt(ret) == 479001600 && s && s2);
}

TEST(quotedMacro) {
    TEST_SETUP;
    RUN_STR(u, s1,
            "(defmacro! testMacro (a) 135)"
            "*undefined*");
    RUN_STR(res, s2, "'((testMacro c))");
    RUN_STR(oracle, s3, "(list (list 'testMacro 'c))");
    t_assert("quoted macro", deepEqual(res, oracle) && s1 && s2 && s3);
}

// tries to run inner as a function, wich the vm asserts is false!
TEST(multipleMacroLevels) {
    TEST_SETUP;
#if 1
    RUN_STR(u1, s1,
            "(defmacro! outer () '(inner))"
            "*undefined*");
    RUN_STR(u2, s2, "(defmacro! inner () 1)"
            "*undefined*");
    RUN_STR(res, s3, "(outer)");
    t_assert("multiple macro level",
             getType(res) == LISPIS_INT32 && unpackInt(res) == 1 && s1 &&
             s2 && s3);
#else
    t_assert("multiple macro level", false);
#endif
}