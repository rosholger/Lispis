#include "tests.h"

TEST(basicObjectTest) {
    TEST_SETUP;
    RUN_STR(res, s, "(: {(a 1)} a)");
    t_assert("basic object test", s && unpackInt(res) == 1);
}

TEST(basicObjectSet) {
    TEST_SETUP;
    RUN_STR(res, s,
            "(let! a {(a 1)})"
            "(:! a a 2)"
            "(: a a)");
    t_assert("basic object set", s && unpackInt(res) == 2);
}

TEST(nestedObject) {
    TEST_SETUP;
    RUN_STR(res, s,
            "(let! a {(a {(a 1)})})"
            "(: (: a a) a)");
    t_assert("nested object test", s && unpackInt(res) == 1);
}

TEST(nestedObjectSet) {
    TEST_SETUP;
    RUN_STR(res, s,
            "(let! a {(a {(a 1)})})"
            "(:! (: a a) a 2)"
            "(: (: a a) a)");
    t_assert("nested object set", s && unpackInt(res) == 2);
}

TEST(unquotedObjectKey) {
    TEST_SETUP;
    RUN_STR(res, s,
            "(let! a 'z)"
            "(let! obj {(,a 10)})"
            "(: obj z)");
    t_assert("unquoted object key", s && unpackInt(res) == 10);
}

TEST(unquotedObjectKeyInMacro) {
    TEST_SETUP;
    RUN_STR(res, s,
            "(let! a 'z)"
            "(: {(,a 10)} z)");
    t_assert("unquoted object key in macro", s && unpackInt(res) == 10);
}

TEST(wrongObjectKey) {
    TEST_SETUP;
    RUN_STR(res, s,
            "(: {(a 10)} z)");
    t_assert("wrong object key", s && getType(res) == LISPIS_UNDEF);
}