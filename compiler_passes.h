#ifndef COMPILER_PASSES_H
#define COMPILER_PASSES_H
#include "common.h"
extern String quote;
extern String lambda;
extern String let;
extern String ifSym;
struct Expr;
Expr *quotePass(Expr *expr);
Expr *lambdaPass(Expr *expr);
Expr *letPass(Expr *expr);
Expr *ifPass(Expr *expr);
#endif