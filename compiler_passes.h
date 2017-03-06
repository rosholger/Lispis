#ifndef COMPILER_PASSES_H
#define COMPILER_PASSES_H
#include "common.h"
extern uint32 quote;
extern uint32 quasiquote;
extern uint32 unquote;
extern uint32 unquoteSplice;
extern uint32 defmacro;
extern uint32 lambda;
extern uint32 let;
extern uint32 define;
extern uint32 ifSym;
struct Expr;
struct LispisState;
// IN ORDER!!!!
Expr *symbolIdPass(LispisState *state, Expr *expr);
Expr *evalMacroPass(LispisState *state, Expr *expr);
Expr *quotePass(LispisState *state, Expr *expr);
Expr *macroPass(LispisState *state, Expr *expr);
Expr *lambdaPass(LispisState *state, Expr *expr);
Expr *letPass(LispisState *state, Expr *expr);
Expr *definePass(LispisState *state, Expr *expr);
Expr *ifPass(LispisState *state, Expr *expr);
typedef Expr *(*CompilerPass)(LispisState *state, Expr *expr);
#endif