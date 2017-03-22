#include "tests.h"

TEST(basicVectorTest) {
    TEST_SETUP;
    RUN_STR(res, s, "(: [1] 0)");
    t_assert("basic vector test", s && unpackInt(res) == 1);
}

TEST(basicVectorSet) {
    TEST_SETUP;
    RUN_STR(res, s,
            "(let! a [1])"
            "(:! a 0 2)"
            "(: a 0)");
    t_assert("basic vector set", s && unpackInt(res) == 2);
}

TEST(nestedVector) {
    TEST_SETUP;
    RUN_STR(res, s,
            "(let! a [[1]])"
            "(: (: a 0) 0)");
    t_assert("nested vector test", s && unpackInt(res) == 1);
}

TEST(nestedVectorSet) {
    TEST_SETUP;
    RUN_STR(res, s,
            "(let! a [[1]])"
            "(:! (: a 0) 0 2)"
            "(: (: a 0) 0)");
    t_assert("nested vector set", s && unpackInt(res) == 2);
}