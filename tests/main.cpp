#include "tests.h"

#include "basic_tests.cpp"
#include "variable_tests.cpp"
#include "quasiquote_tests.cpp"
#include "lambda_tests.cpp"
#include "macro_tests.cpp"
#include "set_tests.cpp"
#include "for_tests.cpp"
#include "vector_tests.cpp"
#include "object_tests.cpp"

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
        // set_tests
        setLocalT,
        setGlobalT,
        setUpvalT,
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
        quasiquoteNestingBasic,
        quasiquoteNestingUnquote,
        //lambda_tests
        lambdaIsLambda,
        lambdaReturnInt,
        lambdaReturnSymbol,
        lambdaReturnList,
        lambdaClosing,
        facTest,
        listFuncTest,
        multipleUpvals,
        innerRecursive,
        multiLevelUpval,
        //macro_tests
        letfunTest,
        defunTest,
        quotedMacro,
        multipleMacroLevels,
        //for_tests
        forBasic,
        forExplicitVar,
        forNestedVar,
        forImpNestedVar,
        forSameNestedVar,
        //vector_tests
        basicVectorTest,
        basicVectorSet,
        nestedVector,
        nestedVectorSet,
        appendTest1,
        appendTest2,
        appendTest3,
        //object_tests
        basicObjectTest,
        basicObjectSet,
        nestedObject,
        nestedObjectSet,
        unquotedObjectKey,
        unquotedObjectKeyInMacro,
        wrongObjectKey,
        basicObjectProto,
        basicObjectProto2,
        basicObjectProto3,
        basicObjectProto4,
    };
    int numSuccess = 0;
    for (uint64 i = 0; i < arrayLength(tests); ++i) {
        numSuccess += tests[i]();
    }
    printf("%d tests succeded, %d tests failed\n",
           numSuccess, ((int)arrayLength(tests))-numSuccess);
    return 0;
}