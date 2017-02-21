#ifndef COMPILER_PASSES_H
#define COMPILER_PASSES_H
#include "common.h"
extern String quote;
struct Expr;
Expr *firstPass(Expr *expr);
#endif