#include "tests.h"

#include "basic_tests.cpp"
#include "variable_tests.cpp"
#include "quasiquote_tests.cpp"
#include "lambda_tests.cpp"
#include "macro_tests.cpp"

//TODO auto generate this file... PLEASE!!!
int main(int argsc, char **args) {
    Test tests[] = {
        // basic_tests
        intPassthroughTest,
        negIntPassthroughTest,
        quoteIntTest,
        quoteNegIntTest,
        quoteSymbolTest,
        quoteNull,
        quasiquoteNull,
        explicitQuoteNull,
        dotQuote,
        dotQuasiquote,
        simpleQuote,
        simpleQuasiquote,
        // variable_tests
        globalIntVar,
        globalSymVar,
        localIntVar,
        localSymVar,
        // quasiquote_tests
        unquoteIntVar,
        unquoteSymVar,
        unquoteSpliceIntVar,
        unquoteSpliceSymVar,
        unquoteSpliceGlobalVar,
        quoteEqualQuasiquote,
        quoteEqualQuasiquoteUnquote,
        quoteEqualQuasiquoteUnquoteSplice,
        quasiquoteDefine,
        //lambda_tests
        lambdaIsLambda,
        lambdaReturnInt,
        lambdaReturnSymbol,
        lambdaReturnList,
        lambdaClosing,
        facTest,
        listFuncTest,
        //macro_tests
        letfunTest,
        defunTest,
        quotedMacro,
        multipleMacroLevels,
    };
    int numSuccess = 0;
    for (uint64 i = 0; i < arrayLength(tests); ++i) {
        numSuccess += tests[i]();
    }
    printf("%d tests succeded, %d tests failed\n",
           numSuccess, ((int)arrayLength(tests))-numSuccess);
    return 0;
}