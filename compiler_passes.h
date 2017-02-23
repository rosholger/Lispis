#ifndef COMPILER_PASSES_H
#define COMPILER_PASSES_H
#include "common.h"
extern uint32 quote;
extern uint32 lambda;
extern uint32 let;
extern uint32 ifSym;
struct Expr;
struct LispisState;
// IN ORDER!!!!
Expr *symbolIdPass(LispisState *state, Expr *expr);
Expr *quotePass(LispisState *state, Expr *expr);
Expr *lambdaPass(LispisState *state, Expr *expr);
Expr *letPass(LispisState *state, Expr *expr);
Expr *ifPass(LispisState *state, Expr *expr);
#endif