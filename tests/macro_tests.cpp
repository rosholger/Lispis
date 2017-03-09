#include "tests.h"

TEST(letfunTest) {
    TEST_SETUP;
    RUN_STR(res,
            "(letfun! fac (n) (if (< n 1) 1 (* n (fac (- n 1))))) fac");

    push(&state, nanPackInt32(12));
    t_assert("letfun!",
             unpackInt(runFunction(&state, res, 1)) == 479001600);
}

TEST(defunTest) {
    TEST_SETUP;
    RUN_STR(res,
            "(defun! fac (n) (if (< n 1) 1 (* n (fac (- n 1))))) 0");

    Value fac =
        lookupGlobal(&state,
                     nanPackSymbolIdx(internCStr(&state, "fac")));
    push(&state, nanPackInt32(12));
    t_assert("letfun!",
             unpackInt(runFunction(&state, fac, 1)) == 479001600);
}

TEST(quotedMacro) {
    TEST_SETUP;
    RUN_STR(u,
            "(defmacro! testMacro (a) 135)"
            "undefined");
    RUN_STR(res, "'((testMacro c))");
    RUN_STR(oracle, "(list (list 'testMacro 'c))");
    t_assert("quoted macro", deepEqual(res, oracle));
}

// tries to run inner as a function, wich the vm asserts is false!
TEST(multipleMacroLevels) {
    TEST_SETUP;
#if 1
    {
        RUN_STR(u,
                "(defmacro! outer () '(inner))"
                "undefined");
    }
    {
        RUN_STR(u, "(defmacro! inner () 1)"
                "undefined");
    }
    RUN_STR(res, "(outer)");
    t_assert("multiple macro level",
             getType(res) == LISPIS_INT32 && unpackInt(res) == 1);
#else
    t_assert("multiple macro level", false);
#endif
}