#include <cstdio>
#include <cmath>
#include <cassert>
#include <cstdlib>
#include <cstring>
#include "common.h"
#include "parser.h"
#include "codegen.h"

void dumpTree(LispisState *state, Expr *node, int identLevel) {
    for (int i = 0; i < identLevel; ++i) {
        printf("  ");
    }
    if (node) {
        switch(node->exprType) {
            case EXPR_LIST: {
                printf("(\n");
                for (ExprList *e = node->list; e; e = e->next) {
                    dumpTree(state, e->val, identLevel+1);
                }
                for (int i = 0; i < identLevel; ++i) {
                    printf("  ");
                }
                printf(")\n");
            } break;
            case EXPR_QUOTE: {
                printf("(QUOTE\n");
                dumpTree(state, node->quoted, identLevel+1);
                printf(")");
            } break;
            case EXPR_STRING: {
                printf("\"%.*s\"", (int)node->str.length, node->str.val);
            } break;
            case EXPR_INT: {
                printf("%d", node->intVal);
            } break;
            case EXPR_FLOAT: {
                printf("%f", node->floatVal);
            } break;
            case EXPR_SYMBOL: {
                printf("%.*s", (int)node->str.length, node->str.val);
            } break;
            case EXPR_SYMBOL_ID: {
                String symStr =
                    globalSymbolIdToSymbol(&state->globalSymbolTable,
                                           node->symbolID);
                printf("%.*s", (int)symStr.length, symStr.val);
            } break;
            case EXPR_CALL: {
                printf("(");
                if (node->callee) {
                    dumpTree(state, node->callee, 0);
                    for (ExprList *param = node->arguments;
                         param; param = param->next) {
                        dumpTree(state, param->val, identLevel+1);
                    }
                    for (int i = 0; i < identLevel; ++i) {
                        printf("  ");
                    }
                } else {
                    assert(node->arguments == 0);
                }
                printf(")");
            } break;
            case EXPR_LAMBDA: {
                printf("(LAMBDA (\n");
                if (node->params) {
                    for (ExprList *param = node->params;
                         param; param = param->next) {
                        dumpTree(state, param->val, identLevel+1);
                    }
                }
                for (int i = 0; i < identLevel; ++i) {
                    printf("  ");
                }
                printf(")\n");
                for (ExprList *expr = node->body;
                     expr; expr = expr->next) {
                    dumpTree(state, expr->val, identLevel+1);
                }
                for (int i = 0; i < identLevel; ++i) {
                    printf("  ");
                }
                printf(")\n");
            } break;
            case EXPR_DEFINE: {
                printf("(DEFINE!\n");
                dumpTree(state, node->variable, identLevel + 1);
                dumpTree(state, node->value, identLevel + 1);
                for (int i = 0; i < identLevel; ++i) {
                    printf("  ");
                }
                printf(")\n");
            } break;
            case EXPR_LET: {
                printf("(LET!\n");
                dumpTree(state, node->variable, identLevel + 1);
                dumpTree(state, node->value, identLevel + 1);
                for (int i = 0; i < identLevel; ++i) {
                    printf("  ");
                }
                printf(")\n");
            } break;
            case EXPR_IF: {
                printf("(IF\n");
                dumpTree(state, node->predicate, identLevel + 1);
                dumpTree(state, node->trueBranch, identLevel + 2);
                if (node->falseBranch) {
                    dumpTree(state, node->falseBranch, identLevel + 2);
                }
                for (int i = 0; i < identLevel; ++i) {
                    printf("  ");
                }
                printf(")\n");
            } break;
            default:assert(false);
        }
        printf("\n");
    } else {
        printf("()\n");
    }
}

void dumpBytecode(LispisState *state, LispisFunction *func) {
    // TODO reimplement
    for (uint64 i = 0; i < func->subFunctionsLength; ++i) {
        printf("LAMBDA_ID %llu\n", i);
        dumpBytecode(state, func->subFunctions[i]);
        printf("END_LAMBDA_ID %llu\n\n", i);
    }
    uint64 pc;
    for (pc = 0; func->bytecode[pc].opCode != OP_EXIT; pc++) {
        assert(pc < func->bytecodeTop);
        OpCodes op = func->bytecode[pc].opCode;
        switch (op) {
            case OP_JUMP_IF_TRUE: {
                pc++;
                int64 relativeTarget = func->bytecode[pc].i64;
                printf("JUMP_IF_TRUE %lld\n", relativeTarget);
            } break;
            case OP_JUMP: {
                pc++;
                int64 relativeTarget = func->bytecode[pc].i64;
                printf("JUMP %lld\n", relativeTarget);
            } break;
            case OP_POP_ASSERT_EQUAL: {
                printf("POP_ASSERT_EQUAL\n");
            } break;
            case OP_EVAL_SYMBOL: {
                printf("EVAL_SYMBOL\n");
            } break;
            case OP_LIST: {
                printf("LIST\n");
            }
            case OP_EXIT: {
                printf("EXIT\n");
            } break;
            case OP_PUSH: {
                printf("PUSH ");
                pc++;
                switch (getType(func->bytecode[pc])) {
                    case LISPIS_INT32: {
                        printf("int: %d\n",
                               unpackInt(func->bytecode[pc]));
                    } break;
                    case LISPIS_DOUBLE: {
                        printf("double: %f\n", func->bytecode[pc].f64);
                    } break;
                    case LISPIS_SYM_IDX: {
                        printf("symbol index: %u\n",
                               unpackSymbolID(func->bytecode[pc]));
                    } break;
                    default:assert(false);
                }
            } break;
            case OP_SET_GLOBAL_VARIABLE: {
                printf("SET_GLOBAL_VARIABLE\n");
            } break;
            case OP_SET_LOCAL_VARIABLE: {
                printf("SET_LOCAL_VARIABLE\n");
            } break;
            case OP_CALL: {
                pc++;
                uint64 numArgs = func->bytecode[pc].ui64;
                printf("CALL %llu\n", numArgs);
            } break;
            case OP_RETURN: {
                printf("RETURN\n");
            } break;
            case OP_PUSH_LAMBDA_ID: {
                pc++;
                uint64 id = func->bytecode[pc].ui64;
                printf("PUSH_LAMBDA_ID %llu\n", id);
            } break;
            case OP_CLEAR_STACK: {
                printf("CLEAR_STACK\n");
            } break;
            default:assert(false);
        }
    }
    printf("End Bytecode Section\n\n");
}

void deallocList(ExprList *list) {
    ExprList *e = list;
    while(e) {
        dealloc(e->val);
        ExprList *next = e->next;
        free(e);
        e = next;
    }
}

void dealloc(Expr *expr) {
    if (expr) {
        switch (expr->exprType) {
            case EXPR_FLOAT:
            case EXPR_STRING: // FIX
            case EXPR_SYMBOL_ID:
            case EXPR_INT:
            case EXPR_SYMBOL: {
            } break;
            case EXPR_DEFINE:
            case EXPR_LET: {
                dealloc(expr->variable);
                dealloc(expr->value);
            } break;
            case EXPR_CALL: {
                if (expr->callee) {
                    dealloc(expr->callee);
                    deallocList(expr->arguments);
                } else {
                    assert(expr->arguments == 0);
                }
            } break;
            case EXPR_QUOTE: {
                dealloc(expr->quoted);
            } break;
            case EXPR_LIST: {
                deallocList(expr->list);
            } break;
            case EXPR_LAMBDA: {
                deallocList(expr->params);
                deallocList(expr->body);
            } break;
            case EXPR_IF: {
                dealloc(expr->predicate);
                dealloc(expr->trueBranch);
                dealloc(expr->falseBranch);
            } break;
            default: assert(false);
        }
        free(expr);
    }
}

char *readEntireFile(char *filename) {
    FILE *file = fopen(filename, "rb");
    assert(file);
    fseek(file, 0, SEEK_END);
    long int fileLength = ftell(file);
    fseek(file, 0, SEEK_SET);
    char *ret = (char *)malloc(fileLength+1);
    fread(ret, fileLength, 1, file);
    ret[fileLength] = 0;
    assert(!fclose(file));
    return ret;
}

bool symCmp(String a, uint64 hashA, String b, uint64 hashB) {
    return (hashA == hashB &&
            a.length == b.length &&
            memcmp(a.val, b.val, a.length) == 0);
}

uint64 hashFunc(String str) {
    uint64 hash = 5381;
    int c;

    for (int32 i = 0; i < str.length; ++i) {
        c = str.val[i];
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
    }
    return hash;
}

NanPackingTypes getType(Value v) {
    uint64 retI = v.ui64;
    uint64 type = LISPIS_DOUBLE;
    if (v.f64 != v.f64) {
        type = ((retI) >> 47) & 0xf;
    }
    return (NanPackingTypes)type;
}
