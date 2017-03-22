#include "tests.h"

TEST(intPassthroughTest) {
    TEST_SETUP;
    RUN_STR(res, success, "1");
    t_assert("int passthrough",
             unpackInt(res) == 1 && success);
}

TEST(quoteIntTest) {
    TEST_SETUP;
    RUN_STR(res, s, "'1");
    t_assert("quote int",
             unpackInt(res) == 1 && s);
}

TEST(negIntPassthroughTest) {
    TEST_SETUP;
    RUN_STR(res, s, "-1");
    t_assert("negative int passthrough",
             unpackInt(res) == -1 && s);
}

TEST(quoteNegIntTest) {
    TEST_SETUP;
    RUN_STR(res, s, "'-1");
    t_assert("quote negative int",
             unpackInt(res) == -1 && s);
}

TEST(quoteSymbolTest) {
    TEST_SETUP;
    RUN_STR(res, s, "'a");
    t_assert("quote symbol",
             unpackSymbolID(res) == internCStr(&state, "a") && s);
}

TEST(quoteNull) {
    TEST_SETUP;
    RUN_STR(res, s, "'()");
    t_assert("quote null",
             unpackCons(res) == 0 && s);
}

TEST(quasiquoteNull) {
    TEST_SETUP;
    RUN_STR(res, s, "(quasiquote ())");
    t_assert("quasiquote null",
             unpackCons(res) == 0 && s);
}

TEST(explicitQuoteNull) {
    TEST_SETUP;
    RUN_STR(res, s, "(quote ())");
    t_assert("explicit quote null",
             unpackCons(res) == 0 && s);
}

TEST(dotQuote) {
    TEST_SETUP;
    RUN_STR(res, s, "'(1 . a)");
    t_assert("dot quote",
             unpackInt(unpackCons(res)->car) == 1 &&
             unpackSymbolID(unpackCons(res)->cdr) == internCStr(&state,
                                                                "a") &&
             s);
}

TEST(simpleQuote) {
    TEST_SETUP;
    RUN_STR(res, s, "'(1 a)");
    t_assert("dot quote",
             unpackInt(unpackCons(res)->car) == 1 &&
             (unpackSymbolID(unpackCons(unpackCons(res)->cdr)->car) ==
              internCStr(&state,
                         "a")) &&
             (unpackCons(unpackCons(unpackCons(res)->cdr)->cdr) == 0) &&
             s);
}

TEST(dotQuasiquote) {
    TEST_SETUP;
    RUN_STR(res, s, "(quasiquote (1 . a))");
    t_assert("dot quasiquote",
             unpackInt(unpackCons(res)->car) == 1 &&
             unpackSymbolID(unpackCons(res)->cdr) == internCStr(&state,
                                                                "a") &&
             s);
}

TEST(simpleQuasiquote) {
    TEST_SETUP;
    RUN_STR(res, s, "(quasiquote (1 a))");
    t_assert("dot quote",
             unpackInt(unpackCons(res)->car) == 1 &&
             (unpackSymbolID(unpackCons(unpackCons(res)->cdr)->car) ==
              internCStr(&state,
                         "a")) &&
             (unpackCons(unpackCons(unpackCons(res)->cdr)->cdr) == 0) &&
             s);
}