#include "tests.h"

TEST(setLocalT) {
    TEST_SETUP;
    RUN_STR(res, s,
            "(let! a 1)"
            "(set! a 2)"
            "a");
    t_assert("set local", unpackInt(res) == 2 && s);
}

TEST(setGlobalT) {
    TEST_SETUP;
    RUN_STR(res, s,
            "(define! a 1)"
            "(letfun! t () (set! a 2))"
            "(t)"
            "a");
    t_assert("set global", unpackInt(res) == 2 && s);
}

TEST(setUpvalT) {
    TEST_SETUP;
    RUN_STR(res, s,
            "(let! a 1)"
            "(letfun! t () (set! a 2))"
            "(t)"
            "a");
    t_assert("set upval", unpackInt(res) == 2 && s);
}