#include "tests.h"

TEST(forBasic) {
    TEST_SETUP;
    RUN_STR(res, s,
            "(let! res 0)"
            "(for (0 (< it 10) (set! it (+ it 1)))"
            "  (set! res (+ res 1)))"
            "res");
    t_assert("basic for loop", s && unpackInt(res) == 10);
}

TEST(forExplicitVar) {
    TEST_SETUP;
    RUN_STR(res, s,
            "(let! res 0)"
            "(for ((exp 0) (< exp 10) (set! exp (+ exp 1)))"
            "  (set! res (+ res 1)))"
            "res");
    t_assert("for loop, explicit variable", s && unpackInt(res) == 10);
}

TEST(forNestedVar) {
    TEST_SETUP;
    RUN_STR(res, s,
            "(let! res 0)"
            "(for ((out 0) (< out 10) (set! out (+ out 1)))"
            "  (for ((in 0) (< in 10) (set! in (+ in 1)))"
            "    (set! res (+ res 1))))"
            "res");
    t_assert("for loop, nested explicit variable",
             s && unpackInt(res) == 100);
}

TEST(forImpNestedVar) {
    TEST_SETUP;
    RUN_STR(res, s,
            "(let! res 0)"
            "(for (0 (< it 10) (set! it (+ it 1)))"
            "  (for (0 (< it 10) (set! it (+ it 1)))"
            "    (set! res (+ res 1))))"
            "res");
    t_assert("for loop, nested implicit variable",
             s && unpackInt(res) == 100);
}


TEST(forSameNestedVar) {
    TEST_SETUP;
    RUN_STR(res, s,
            "(let! res 0)"
            "(for ((it 0) (< it 10) (set! it (+ it 1)))"
            "  (for ((it 0) (< it 10) (set! it (+ it 1)))"
            "    (set! res (+ res 1))))"
            "res");
    t_assert("for loop, nested explicit same variable",
             s && unpackInt(res) == 100);
}