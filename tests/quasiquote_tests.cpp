#include "tests.h"

TEST(quoteEqualQuasiquote) {
    TEST_SETUP;
    RUN_STR(q, s1,
            "'(a b c 1 (23 + 8545 . a) (((aksf ag) asd) . (asd 923)))");
    RUN_STR(qq, s2,
            "(quasiquote (a b c 1 (23 + 8545 . a) (((aksf ag) asd) . (asd 923))))");
    t_assert("quote equals quasiquote", deepEqual(q, qq) && s1 && s2);
}

TEST(quoteEqualQuasiquoteUnquote) {
    TEST_SETUP;
    RUN_STR(q, s1,
            "'(a b c 1 (23 + 8545 . a) (((aksf ag) asd) . (asd 923)))");
    RUN_STR(qq, s2,
            "(let! t '(23 + 8545 . a))"
            "(quasiquote (a b c 1 (unquote t)"
            "(((aksf ag) asd) . (asd 923))))");
    t_assert("quote equals quasiquote", deepEqual(q, qq) && s1 && s2);
}

TEST(quoteEqualQuasiquoteUnquoteSplice) {
    TEST_SETUP;
    RUN_STR(q, s1,
            "'(asd sasd asd dass sasd ads asd "
            " a b c 1 (23 + 8545 . a) (((aksf ag) asd) . (asd 923)))");
    RUN_STR(qq, s2,
            "(let! t '(23 + 8545 . a))"
            "(let! t2 '((((aksf ag) asd) . (asd 923))))"
            "(quasiquote (asd sasd asd dass sasd ads asd "
            " a b c 1 (unquote t)"
            "(unquote-splice t2)))");
    t_assert("quote equals quasiquote", deepEqual(q, qq) && s1 && s2);
}

// Thinks that define! is a function, that is a LARGE bug
TEST(quasiquoteDefine) {
    TEST_SETUP;
#if 1
    RUN_STR(res, s,
            "(quasiquote (a (unquote (define! a 1))))"
            "a");
    t_assert("unquoted define", unpackInt(res) == 1 && s);
#else
    t_assert("unquoted define", false);
#endif
}

TEST(quasiquoteNestingBasic) {
    TEST_SETUP;
    RUN_STR(res1, s1,
            "(let! a 1) ``,a");
    RUN_STR(res2, s2,
            "`(quasiquote (unquote a))");
    t_assert("basic quasiquote nesting", (s1 && s2 &&
                                          deepEqual(res1, res2)));
}

TEST(quasiquoteNestingUnquote) {
    TEST_SETUP;
    RUN_STR(res1, s1,
            "(let! a 1) ``,,a");
    RUN_STR(res2, s2,
            "`(quasiquote (unquote 1))");
    t_assert("quasiquote unquote nesting", (s1 && s2 &&
                                            deepEqual(res1, res2)));
}