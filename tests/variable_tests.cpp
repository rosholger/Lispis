#include "tests.h"

TEST(globalIntVar) {
    TEST_SETUP;
    RUN_STR(varSetVal, "(define! a 1)");
    RUN_STR(varVal, "a");
    t_assert("global int variable",
             unpackInt(varSetVal) == unpackInt(varVal) &&
             unpackInt(varSetVal) == 1);
}

TEST(globalSymVar) {
    TEST_SETUP;
    RUN_STR(varSetVal, "(define! a 'a)");
    RUN_STR(varVal, "a");
    t_assert("global symbol variable",
             unpackSymbolID(varSetVal) == unpackSymbolID(varVal) &&
             unpackSymbolID(varSetVal) == internCStr(&state, "a"));
}

TEST(localIntVar) {
    TEST_SETUP;
    RUN_STR(res, "(let! a 1) a");
    t_assert("local int variable", unpackInt(res) == 1);
}

TEST(localSymVar) {
    TEST_SETUP;
    RUN_STR(res, "(let! a 'a) a");
    t_assert("local symbol variable",
             unpackSymbolID(res) == internCStr(&state, "a"));
}

TEST(unquoteIntVar) {
    TEST_SETUP;
    RUN_STR(res, "(let! a 1) (quasiquote ((unquote a)))");
    t_assert("unquote int variable",
             unpackInt(unpackCons(res)->car) == 1 &&
             unpackCons(unpackCons(res)->cdr) == 0);
}

TEST(unquoteSymVar) {
    TEST_SETUP;
    RUN_STR(res, "(let! a 'a) (quasiquote ((unquote a)))");
    t_assert("unquote symbol variable",
             unpackSymbolID(unpackCons(res)->car) ==
             internCStr(&state, "a") &&
             unpackCons(unpackCons(res)->cdr) == 0);
}

TEST(unquoteSpliceIntVar) {
    TEST_SETUP;
    RUN_STR(res, "(let! a '(1)) (quasiquote ((unquote-splice a)))");
    t_assert("unquote-splice int variable",
             unpackInt(unpackCons(res)->car) == 1 &&
             unpackCons(unpackCons(res)->cdr) == 0);
}

TEST(unquoteSpliceSymVar) {
    TEST_SETUP;
    RUN_STR(res, "(let! a '(a)) (quasiquote ((unquote-splice a)))");
    t_assert("unquote-splice symbol variable",
             unpackSymbolID(unpackCons(res)->car) ==
             internCStr(&state, "a") &&
             unpackCons(unpackCons(res)->cdr) == 0);
}

TEST(unquoteSpliceGlobalVar) {
    TEST_SETUP;
    RUN_STR(res, "(define! a '(a)) (quasiquote ((unquote-splice a)))");
    t_assert("unquote-splice global variable",
             unpackSymbolID(unpackCons(res)->car) ==
             internCStr(&state, "a") &&
             unpackCons(unpackCons(res)->cdr) == 0);
}