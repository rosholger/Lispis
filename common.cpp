#include <cstdio>
#include <cmath>
#include <cassert>
#include <cstdlib>
#include <cstring>
#include "common.h"
#include "parser.h"
#include "codegen.h"

void dumpTree(Expr *node, int identLevel) {
    for (int i = 0; i < identLevel; ++i) {
        printf("  ");
    }
    if (node) {
        switch(node->exprType) {
            case EXPR_LIST: {
                printf("(\n");
                for (ExprList *e = node->list; e; e = e->next) {
                    dumpTree(e->val, identLevel+1);
                }
                for (int i = 0; i < identLevel; ++i) {
                    printf("  ");
                }
                printf(")\n");
            } break;
            case EXPR_QUOTE: {
                printf("(QUOTE\n");
                dumpTree(node->quoted, identLevel+1);
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
            case EXPR_CALL: {
                printf("(");
                if (node->callee) {
                    dumpTree(node->callee, 0);
                    for (ExprList *param = node->arguments;
                         param; param = param->next) {
                        dumpTree(param->val, identLevel+1);
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
                        dumpTree(param->val, identLevel+1);
                    }
                }
                for (int i = 0; i < identLevel; ++i) {
                    printf("  ");
                }
                printf(")\n");
                for (ExprList *expr = node->body;
                     expr; expr = expr->next) {
                    dumpTree(expr->val, identLevel+1);
                }
                for (int i = 0; i < identLevel; ++i) {
                    printf("  ");
                }
                printf(")\n");
            } break;
            case EXPR_LET: {
                printf("(LET! %.*s \n",
                       node->variable->str.length,
                       node->variable->str.val);
                dumpTree(node->value, identLevel + 1);
                for (int i = 0; i < identLevel; ++i) {
                    printf("  ");
                }
                printf(")\n");
            } break;
            case EXPR_IF: {
                printf("(IF\n");
                dumpTree(node->predicate, identLevel + 1);
                dumpTree(node->trueBranch, identLevel + 2);
                if (node->falseBranch) {
                    dumpTree(node->falseBranch, identLevel + 2);
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

void dumpSymbolSection(CompilerState *state) {
    for (uint64 pc = 0; pc < state->symbolSectionTop;) {
        int32 length = unpackInt(state->symbolSection[pc]);
        pc++;
        if (length) {
            uint32 id = unpackSymbolID(state->symbolSection[pc]);
            ++pc;
            printf("SYMBOL: %u %.*s\n", id, (int)length,
                   state->symbolSection[pc].c);
            pc = pc + length / 8 + (length % 8 == 0 ? 0 : 1);
        } else {
            break;
        }
    }
}

void dumpBytecode(CompilerState *state) {
    for (int i = 0; i < state->childStatesLength; ++i) {
        printf("LAMBDA_ID %d\n", i);
        dumpBytecode(state->childStates + i);
    }
    printf("Symbol Section:\n");
    dumpSymbolSection(state);
    printf("Bytecode Section:\n");
    uint64 pc;
    for (pc = 0; state->bytecode[pc].opCode != OP_EXIT; pc++) {
        assert(pc < state->bytecodeTop);
        OpCodes op = state->bytecode[pc].opCode;
        switch (op) {
            case OP_JUMP_IF_TRUE: {
                pc++;
                int64 relativeTarget = state->bytecode[pc].i64;
                printf("JUMP_IF_TRUE %lld\n", relativeTarget);
            } break;
            case OP_JUMP: {
                pc++;
                int64 relativeTarget = state->bytecode[pc].i64;
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
                switch (getType(state->bytecode[pc])) {
                    case LISPIS_INT32: {
                        printf("int: %d\n",
                               unpackInt(state->bytecode[pc]));
                    } break;
                    case LISPIS_DOUBLE: {
                        printf("double: %f\n", state->bytecode[pc].f64);
                    } break;
                    default:assert(false);
                }
            } break;
            case OP_PUSH_TRANSLATE_SYMBOL: {
                pc++;
                printf("PUSH_TRANSLATE_SYMBOL ");
                printf("symbol index: %u\n",
                       unpackSymbolID(state->bytecode[pc]));
            } break;
            case OP_SET_LOCAL_VARIABLE: {
                printf("SET_LOCAL_VARIABLE\n");
            } break;
            case OP_CALL: {
                pc++;
                uint64 numArgs = state->bytecode[pc].ui64;
                printf("CALL %llu\n", numArgs);
            } break;
            case OP_RETURN: {
                printf("RETURN\n");
            } break;
            case OP_PUSH_LAMBDA_ID: {
                pc++;
                uint64 id = state->bytecode[pc].ui64;
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

void dealloc(Expr *expr) {
    if (expr) {
        if (expr->exprType == EXPR_CALL) {
            if (expr->callee) {
                dealloc(expr->callee);
                ExprList *param = expr->arguments;
                while(param) {
                    dealloc(param->val);
                    ExprList *next = param->next;
                    free(param);
                    param = next;
                }
            } else {
                assert(expr->arguments == 0);
            }
        } else if (expr->exprType == EXPR_QUOTE) {
            dealloc(expr->quoted);
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
