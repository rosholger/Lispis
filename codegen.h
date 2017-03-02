#ifndef CODEGEN_H
#define CODEGEN_H
#include "common.h"
#include "vm.h"

void compileExpression(LispisState *state, LispisFunction *func,
                       Expr *expr);
void pushOp(LispisState *state, LispisFunction *func, OpCodes op);
void pushInt32(LispisState *state, LispisFunction *func, int32 op);
//Bytecode *compactBytecode(LispisState *state, LispisFunction *func);
#endif