#include "tests.h"

TEST(intPassthroughTest) {
    TEST_SETUP;
    RUN_STR(res, "1");
    t_assert("int passthrough",
             unpackInt(res) == 1);
}

TEST(quoteIntTest) {
    TEST_SETUP;
    RUN_STR(res, "'1");
    t_assert("quote int",
             unpackInt(res) == 1);
}

TEST(negIntPassthroughTest) {
    TEST_SETUP;
    RUN_STR(res, "-1");
    t_assert("negative int passthrough",
             unpackInt(res) == -1);
}

TEST(quoteNegIntTest) {
    TEST_SETUP;
    RUN_STR(res, "'-1");
    t_assert("quote negative int",
             unpackInt(res) == -1);
}

TEST(quoteSymbolTest) {
    TEST_SETUP;
    RUN_STR(res, "'a");
    t_assert("quote symbol",
             unpackSymbolID(res) == internCStr(&state, "a"));
}

TEST(quoteNull) {
    TEST_SETUP;
    RUN_STR(res, "'()");
    t_assert("quote null",
             unpackCons(res) == 0);
}

TEST(quasiquoteNull) {
    TEST_SETUP;
    RUN_STR(res, "(quasiquote ())");
    t_assert("quasiquote null",
             unpackCons(res) == 0);
}

TEST(explicitQuoteNull) {
    TEST_SETUP;
    RUN_STR(res, "(quote ())");
    t_assert("explicit quote null",
             unpackCons(res) == 0);
}

TEST(dotQuote) {
    TEST_SETUP;
    RUN_STR(res, "'(1 . a)");
    t_assert("dot quote",
             unpackInt(unpackCons(res)->car) == 1 &&
             unpackSymbolID(unpackCons(res)->cdr) == internCStr(&state,
                                                                "a"));
}

TEST(simpleQuote) {
    TEST_SETUP;
    RUN_STR(res, "'(1 a)");
    t_assert("dot quote",
             unpackInt(unpackCons(res)->car) == 1 &&
             (unpackSymbolID(unpackCons(unpackCons(res)->cdr)->car) ==
              internCStr(&state,
                         "a")) &&
             (unpackCons(unpackCons(unpackCons(res)->cdr)->cdr) == 0));
}

TEST(dotQuasiquote) {
    TEST_SETUP;
    RUN_STR(res, "(quasiquote (1 . a))");
    t_assert("dot quasiquote",
             unpackInt(unpackCons(res)->car) == 1 &&
             unpackSymbolID(unpackCons(res)->cdr) == internCStr(&state,
                                                                "a"));
}

TEST(simpleQuasiquote) {
    TEST_SETUP;
    RUN_STR(res, "(quasiquote (1 a))");
    t_assert("dot quote",
             unpackInt(unpackCons(res)->car) == 1 &&
             (unpackSymbolID(unpackCons(unpackCons(res)->cdr)->car) ==
              internCStr(&state,
                         "a")) &&
             (unpackCons(unpackCons(unpackCons(res)->cdr)->cdr) == 0));
}