#include "tests.h"

TEST(quoteEqualQuasiquote) {
    TEST_SETUP;
    RUN_STR(q,
            "'(a b c 1 (23 + 8545 . a) (((aksf ag) asd) . (asd 923)))");
    RUN_STR(qq,
            "(quasiquote (a b c 1 (23 + 8545 . a) (((aksf ag) asd) . (asd 923))))");
    t_assert("quote equals quasiquote", deepEqual(q, qq));
}

TEST(quoteEqualQuasiquoteUnquote) {
    TEST_SETUP;
    RUN_STR(q,
            "'(a b c 1 (23 + 8545 . a) (((aksf ag) asd) . (asd 923)))");
    RUN_STR(qq,
            "(let! t '(23 + 8545 . a))"
            "(quasiquote (a b c 1 (unquote t)"
            "(((aksf ag) asd) . (asd 923))))");
    t_assert("quote equals quasiquote", deepEqual(q, qq));
}

TEST(quoteEqualQuasiquoteUnquoteSplice) {
    TEST_SETUP;
    RUN_STR(q,
            "'(asd sasd asd dass sasd ads asd "
            " a b c 1 (23 + 8545 . a) (((aksf ag) asd) . (asd 923)))");
    RUN_STR(qq,
            "(let! t '(23 + 8545 . a))"
            "(let! t2 '((((aksf ag) asd) . (asd 923))))"
            "(quasiquote (asd sasd asd dass sasd ads asd "
            " a b c 1 (unquote t)"
            "(unquote-splice t2)))");
    t_assert("quote equals quasiquote", deepEqual(q, qq));
}

// Thinks that define! is a function, that is a LARGE bug
TEST(quasiquoteDefine) {
    TEST_SETUP;
#if 1
    RUN_STR(res,
            "(quasiquote (a (unquote (define! a 1))))"
            "a");
    t_assert("unquoted define", unpackInt(res) == 1);
#else
    t_assert("unquoted define", false);
#endif
}