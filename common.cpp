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
                for (ExprList *param = node->params;
                     param; param = param->next) {
                    dumpTree(param->val, identLevel+1);
                }
                for (int i = 0; i < identLevel; ++i) {
                    printf("  ");
                }
            } else {
                assert(node->params == 0);
            }
            printf(")");
        } break;
        default:break;
    }
    printf("\n");
}

void dumpSymbolSection(Bytecode *bytecode) {
    while (bytecode->opCode != OP_EXIT) {
        int32 length = unpackInt(*bytecode);
        bytecode++;
        if (length) {
            uint32 id = unpackSymbolID(*bytecode);
            ++bytecode;
            printf("SYMBOL: %u %.*s\n", id, (int)length, bytecode->c);
            bytecode = bytecode + length / 8 + (length % 8 == 0 ? 0 : 1);
        }
    }
}

void dumpBytecode(Bytecode *bytecode) {
    while (bytecode->opCode != OP_EXIT) {
        OpCodes op = bytecode->opCode;
        bytecode++;
        switch (op) {
            case OP_EVAL_SYMBOL: {
                printf("EVAL_SYMBOL\n");
            } break;
            case OP_LIST: {
                printf("LIST\n");
            }
            case OP_EXIT: {
                printf("EXIT\n");
            } break;
            case OP_SYMBOL_SECTION: {
                printf("SYMBOL_SECTION\n");
                dumpSymbolSection(bytecode);
            } break;
            case OP_PUSH: {
                printf("PUSH ");
                double nan = NAN;
                if ((*(uint64 *)&nan) & (bytecode->ui64)) {
                    uint64 type = ((bytecode->ui64) >> 47) & 0xf;
                    if (LISPIS_INT32 == type) {
                        printf("int: %d\n", bytecode->i32);
                    }
                    if (LISPIS_SYM_IDX == type) {
                        printf("symbol index: %u\n", bytecode->ui32);
                    }
                } else {
                    printf("double: %f\n", *(double *)bytecode);
                }
                bytecode++;
            } break;
            case OP_PUSH_TRANSLATE_SYMBOL: {
                printf("PUSH_TRANSLATE_SYMBOL ");
                double nan = NAN;
                if ((*(uint64 *)&nan) & (bytecode->ui64)) {
                    uint64 type = ((bytecode->ui64) >> 47) & 0xf;
                    if (LISPIS_SYM_IDX == type) {
                        printf("symbol index: %u\n", bytecode->ui32);
                    } else {
                        assert(false);
                    }
                } else {
                    assert(false);
                }
                bytecode++;
            } break;
            case OP_SET_LOCAL_SYMBOL: {
                printf("SET_LOCAL_SYMBOL\n");
            } break;
            case OP_CALL: {
                uint64 numArgs = bytecode->ui64;
                bytecode++;
                printf("CALL %llu\n", numArgs);
            } break;
            case OP_RETURN: {
                printf("RETURN\n");
            } break;
            default:assert(false);
        }
    }
}

void dealloc(Expr *expr) {
    if (expr->exprType == EXPR_CALL) {
        if (expr->callee) {
            dealloc(expr->callee);
            ExprList *param = expr->params;
            while(param) {
                dealloc(param->val);
                ExprList *next = param->next;
                free(param);
                param = next;
            }
        } else {
            assert(expr->params == 0);
        }
    } else if (expr->exprType == EXPR_QUOTE) {
        dealloc(expr->quoted);
    }
    free(expr);
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
