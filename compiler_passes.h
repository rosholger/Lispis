#ifndef COMPILER_PASSES_H
#define COMPILER_PASSES_H
#include "common.h"
struct Expr;
struct LispisState;

struct LexicalScope {
    LexicalScope *parentScope;
    uint32 variableIDs[256]; // TODO: variable size? yes? no?
    uint32 variableIDsTop;
    uint32 upvalsTop;
    bool closedOver;
};

// IN ORDER!!!!
Expr *symbolIdPass(LispisState *state, Expr *expr);
Expr *evalMacroPass(LispisState *state, Expr *expr);
Expr *macroPass(LispisState *state, Expr *expr);
Expr *lambdaPass(LispisState *state, Expr *expr);
Expr *letPass(LispisState *state, Expr *expr);
Expr *definePass(LispisState *state, Expr *expr);
Expr *ifPass(LispisState *state, Expr *expr);
Expr *variablePass(LispisState *state, Expr *expr, LexicalScope *scope);
typedef Expr *(*VariablePass)(LispisState *state,
                              Expr *expr, LexicalScope *scope);
typedef Expr *(*CompilerPass)(LispisState *state, Expr *expr);
void setupSpecialFormSymbols(LispisState *state);
#endif