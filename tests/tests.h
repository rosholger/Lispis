#ifndef TESTS_H
#define TESTS_H

#include "../lexer.h"
#include "../parser.h"
#include "../codegen.h"
#include "../compiler_passes.h"
#include "../vm.h"
#include "../lispisStdLib.h"

#include <cstdio>
#include <cstdlib>
#include <cctype>
#include <cstring>
#include <cmath>
#include <cassert>

#include <cstdio>

inline
bool deepEqual(Value a, Value b) {
    if (a.ui64 == b.ui64) {
        return true;
    } else {
        return (getType(a) == getType(b) &&
                getType(a) == LISPIS_CONS &&
                deepEqual(unpackCons(a)->car, unpackCons(b)->car) &&
                deepEqual(unpackCons(a)->cdr, unpackCons(b)->cdr));
    }
}


inline
void assertFailedFunc(const char *m1) {
    printf("failed %s\n", m1);
}

#define t_assert(message, test)                         \
    do {                                                \
        if (!(test)) {                                  \
            assertFailedFunc(message);                  \
            TEST_TEARDOWN;                              \
            return 0;                                   \
        } else {                                        \
            TEST_TEARDOWN;                              \
            return 1;                                   \
        }                                               \
    } while(0)
typedef int (*Test)();
#define TEST(name) int name ()
#define TEST_SETUP                              \
    LispisState state = {};                     \
    do {                                        \
    initState(&state);                          \
    initStdLib(&state);                         \
    } while(0)
#define TEST_TEARDOWN                           \
    destroy(&state)
#define RUN_STR(res, suc, str)                                  \
    bool suc = runNullTerminatedString(&state, (char *)str);    \
    Value res;                                                  \
    suc = suc && pop(&state, &res);
#endif