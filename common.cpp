#include <cstdio>
#include <cmath>
#include <cassert>
#include <cstdlib>
#include <cstring>
#include "common.h"
#include "parser.h"
#include "codegen.h"
#include "vm.h"
#include "config.h"

Value exprListToConsList(LispisState *state, ExprList *lst, bool dotted);

Value exprToConsList(LispisState *state, Expr *expr) {
    Value ret;
    switch (expr->exprType) {
        case EXPR_VECTOR: {
            Vector *v = (Vector *)callocGcObject(state,
                                                 sizeof(Vector));
            v->header.type = GC_VECTOR;
            v->numFilled = expr->vec.numElems;
            uint32 numBuckets = v->numFilled/VECTOR_BUCKET_SIZE;
            if (v->numFilled % VECTOR_BUCKET_SIZE) {
                numBuckets++;
            }
            uint32 size = numBuckets * VECTOR_BUCKET_SIZE;
            v->size = size;
            v->firstBucket = 0;
            if (numBuckets) {
                v->firstBucket =
                    (VectorBucket *)calloc(1, sizeof(VectorBucket));
                VectorBucket *prevBucket = v->firstBucket;
                for (uint32 i = 1; i < numBuckets; ++i) {
                    prevBucket->next =
                        (VectorBucket *)calloc(1,
                                               sizeof(VectorBucket));
                    prevBucket = prevBucket->next;
                }
            }
            ExprList *e = expr->vec.elems;
            // -1 hack to simplify loop
            VectorBucket *bucket = v->firstBucket;
            for (int i = 0; i < v->numFilled; ++i) {
                int32 indexInBucket = i % VECTOR_BUCKET_SIZE;
                if (i && !indexInBucket) {
                    bucket = bucket->next;
                }
                bucket->elems[indexInBucket] =
                    exprToConsList(state, e->val);
                e = e->next;
            }
            ret = nanPackPointer(v, LISPIS_VECTOR);
        } break;
        case EXPR_OBJECT: {
            Object *object = (Object *)callocGcObject(state,
                                                      sizeof(Object));
            object->header.type = GC_OBJECT;
            object->size = expr->obj.numElems;
            object->elems = (KeyValPair *)calloc(object->size,
                                                 sizeof(KeyValPair));
            ExprList *e = expr->obj.elems;
            for (int i = 0; i < object->size; ++i) {
                Expr *keyExpr = e->val->keyValPair.key;
                Expr *val = e->val->keyValPair.val;
                Value keyVal = exprToConsList(state, keyExpr);
                pushError(state, getType(keyVal) == LISPIS_SYM_IDX,
                          "object-key-not-symbol");
                uint32 key = unpackSymbolID(keyVal);
                uint32 id = key % object->size;
                // HASH
                uint32 startID = id;
                while (object->elems[id].filled &&
                       object->elems[id].key != key) {
                    id++;
                    id = id % object->size;
                    pushError(state, id != startID,
                              "object-expansion-not-yet-implemented");
                }
                object->elems[id].val =
                    exprToConsList(state, val);
                object->elems[id].key = key;
                object->elems[id].filled = true;
                e = e->next;
            }
            ret = nanPackPointer(object, LISPIS_OBJECT);
        } break;
        case EXPR_SYMBOL_ID: {
            ret = nanPackSymbolIdx(expr->symbolID);
        } break;
        case EXPR_INT: {
            ret = nanPackInt32(expr->intVal);
        } break;
        case EXPR_DOUBLE: {
            ret.f64 = expr->doubleVal;
        } break;
        case EXPR_CALL: {
            Value args = exprListToConsList(state, expr->arguments,
                                            expr->dotted);
            if (expr->callee) {
                ret = cons(state,
                           exprToConsList(state, expr->callee), args);
            } else {
                assert(unpackCons(args) == 0);
                ret = args;
            }
        } break;
        default:assert(false);
    }
    return ret;
}

Value exprListToConsList(LispisState *state, ExprList *lst, bool dotted) {
    if (!lst) {
        return emptyList();
    }
    if (!lst->next && dotted) {
        return exprToConsList(state, lst->val);
    }
    return cons(state, exprToConsList(state, lst->val),
                exprListToConsList(state, lst->next, dotted));
}

ExprList *consListToArgumentList(LispisState *state, Value consList,
                                 int line, Expr *e) {
    if (isNill(consList)) {
        e->dotted = false;
        return 0;
    }
    ExprList *ret = (ExprList *)malloc(sizeof(ExprList));
    if (getType(consList) != LISPIS_CONS) {
        e->dotted = true;
        ret->next = 0;
        ret->val = consListToExpr(state, consList, line);
        return ret;
    }
    Pair *p = unpackCons(consList);
    ret->val = consListToExpr(state, p->car, line);
    ret->next = consListToArgumentList(state, p->cdr, line, e);
    return ret;
}

Expr *keyValPairToExpr(LispisState *state, KeyValPair pair, int line) {
    Expr *ret = (Expr *)calloc(1, sizeof(Expr));
    ret->exprType = EXPR_KEY_VALUE_PAIR;
    ret->line = line;
    ret->keyValPair.key = (Expr *)calloc(1, sizeof(Expr));
    Expr *key = ret->keyValPair.key;
    // Unquoted keys... how?
    // Possibly translating objects to a make-object call
    // with unquoted keys quoted an unquoted keys not quoted...
    key->exprType = EXPR_SYMBOL_ID; 
    key->line = line;
    key->symbolID = pair.key;
    
    ret->keyValPair.val = consListToExpr(state, pair.val, line);
    return ret;
}

Expr *consListToExpr(LispisState *state, Value consList, int line) {
    Expr *ret = (Expr *)calloc(1, sizeof(Expr));
    ret->line = line;
    switch (getType(consList)) {
        case LISPIS_BOOLEAN: {
            ret->exprType = EXPR_BOOLEAN;
            ret->boolean = unpackBoolean(consList);
        } break;
        case LISPIS_OBJECT: {
            ret->exprType = EXPR_OBJECT;
            Object *o = unpackObject(consList);
            ret->obj.numElems = o->size;
            ret->obj.elems = 0;
            if (o->size) {
                int first = 0;
                for (; first < o->size; ++first) {
                    if (o->elems[first].filled) {
                        break;
                    }
                }
                if (first < o->size) {
                    ret->obj.elems = (ExprList *)malloc(sizeof(ExprList));
                    ret->obj.elems->val =
                        keyValPairToExpr(state, o->elems[first], line);
                    ret->obj.elems->next = 0;
                    ExprList *prev = ret->obj.elems;
                    for (int i = first+1; i < o->size; ++i) {
                        if (o->elems[i].filled) {
                            prev->next =
                                (ExprList *)malloc(sizeof(ExprList));
                            prev = prev->next;
                            prev->next = 0;
                            prev->val = 
                                keyValPairToExpr(state, o->elems[i],
                                                 line);
                        }
                    }
                }
            }
        } break;
        case LISPIS_VECTOR: {
            ret->exprType = EXPR_VECTOR;
            Vector *v = unpackVector(consList);
            ret->vec.numElems = v->numFilled;
            ret->vec.elems = 0;
            if (v->numFilled) {
                ret->vec.elems = (ExprList *)malloc(sizeof(ExprList));
                ret->vec.elems->val =
                    consListToExpr(state, v->firstBucket->elems[0],
                                   line);
                ret->vec.elems->next = 0;
                ExprList *prev = ret->vec.elems;
                VectorBucket *bucket = v->firstBucket;
                for (int i = 1; i < v->numFilled; ++i) {
                    int32 indexInBucket = i % VECTOR_BUCKET_SIZE;
                    if (i && !indexInBucket) {
                        bucket = bucket->next;
                    }
                    prev->next = (ExprList *)malloc(sizeof(ExprList));
                    prev = prev->next;
                    prev->next = 0;
                    prev->val =
                        consListToExpr(state,
                                       bucket->elems[indexInBucket],
                                       line);
                }
            }
        } break;
        case LISPIS_INT32: {
            ret->exprType = EXPR_INT;
            ret->intVal = unpackInt(consList);
        } break;
        case LISPIS_SYM_IDX: {
            ret->exprType = EXPR_SYMBOL_ID;
            ret->symbolID = unpackSymbolID(consList);
        } break;
        case LISPIS_CONS: {
            ret->exprType = EXPR_CALL;
            if (!isNill(consList)) {
                ret->callee = consListToExpr(state,
                                             unpackCons(consList)->car,
                                             line);
                Value args = unpackCons(consList)->cdr;
                ret->arguments = consListToArgumentList(state, args,
                                                        line, ret);
            }
        } break;
        default: assert(false);
    }
    return ret;
}

void dumpTree(LispisState *state, Expr *node, int identLevel) {
    for (int i = 0; i < identLevel; ++i) {
        printf("  ");
    }
    if (node) {
        switch(node->exprType) {
            case EXPR_BOOLEAN: {
                printf("%s\n", node->boolean ? "TRUE" : "FALSE");
            } break;
            case EXPR_VECTOR: {
                printf("[\n");
                for (ExprList *e = node->vec.elems; e; e = e->next) {
                    dumpTree(state, e->val, identLevel+1);
                }
                for (int i = 0; i < identLevel; ++i) {
                    printf("  ");
                }
                printf("]\n");
            } break;
            case EXPR_OBJECT: {
                printf("{\n");
                for (ExprList *e = node->obj.elems; e; e = e->next) {
                    dumpTree(state, e->val, identLevel+1);
                }
                for (int i = 0; i < identLevel; ++i) {
                    printf("  ");
                }
                printf("}\n");
            } break;
            case EXPR_KEY_VALUE_PAIR: {
                printf("(\n");
                dumpTree(state, node->keyValPair.key, identLevel+1);
                dumpTree(state, node->keyValPair.val, identLevel+1);
                for (int i = 0; i < identLevel; ++i) {
                    printf("  ");
                }
                printf(")\n");
            } break;
            case EXPR_REF_SET: {
                printf("(ref-set\n");
                dumpTree(state, node->refSet.obj, identLevel+1);
                dumpTree(state, node->refSet.ref, identLevel+1);
                dumpTree(state, node->refSet.val, identLevel+1);
                for (int i = 0; i < identLevel; ++i) {
                    printf("  ");
                }
                printf(")\n");
            } break;
            case EXPR_REF: {
                printf("(ref\n");
                dumpTree(state, node->ref.obj, identLevel+1);
                dumpTree(state, node->ref.ref, identLevel+1);
                for (int i = 0; i < identLevel; ++i) {
                    printf("  ");
                }
                printf(")\n");
            } break;
            case EXPR_FOR: {
                printf("(FOR (\n");
                dumpTree(state, node->init, identLevel+1);
                dumpTree(state, node->pred, identLevel+1);
                dumpTree(state, node->upd, identLevel+1);
                for (int i = 0; i < identLevel+1; ++i) {
                    printf("  ");
                }
                printf(")\n");
                for (ExprList *e = node->body; e; e = e->next) {
                    dumpTree(state, e->val, identLevel+2);
                }
                for (int i = 0; i < identLevel; ++i) {
                    printf("  ");
                }
                printf(")\n");
            } break;
            case EXPR_QUASIQUOTE: {
                printf("(QUASIQUOTE\n");
                for (QuasiquoteList *e = node->quasiquoteList;
                     e; e = e->next) {
                    if (node->dotted && !e->next) {
                        for (int i = 0; i < identLevel+1; ++i) {
                            printf("  ");
                        }
                        printf(".\n");
                    }
                    if (!node->dotted || e->val) {
                        if (e->unquoted) {
                            for (int i = 0; i < identLevel+1; ++i) {
                                printf("  ");
                            }
                            printf("(UNQUOTE\n");
                            dumpTree(state, e->val, identLevel+2);
                        } else if (e->unquoteSpliced) {
                            for (int i = 0; i < identLevel+1; ++i) {
                                printf("  ");
                            }
                            printf("(UNQUOTE-SPLICE\n");
                            dumpTree(state, e->val, identLevel+2);
                        } else {
                            dumpTree(state, e->val, identLevel+1);
                        }
                        if (e->unquoted) {
                            for (int i = 0; i < identLevel+1; ++i) {
                                printf("  ");
                            }
                            printf(")\n");
                        }
                        if (e->unquoteSpliced) {
                            for (int i = 0; i < identLevel+1; ++i) {
                                printf("  ");
                            }
                            printf(")\n");
                        }
                    }
                }
                for (int i = 0; i < identLevel; ++i) {
                    printf("  ");
                }
                printf(")\n");
            } break;
            case EXPR_DO: {
                printf("(DO\n");
                for (ExprList *e = node->list; e; e = e->next) {
                    if (node->dotted && !e->next) {
                        for (int i = 0; i < identLevel+1; ++i) {
                            printf("  ");
                        }
                        printf(".\n");
                    }
                    if (!node->dotted || e->val) {
                        dumpTree(state, e->val, identLevel+1);
                    }
                }
                for (int i = 0; i < identLevel; ++i) {
                    printf("  ");
                }
                printf(")\n");
            } break;
            case EXPR_LIST: {
                printf("(\n");
                for (ExprList *e = node->list; e; e = e->next) {
                    if (node->dotted && !e->next) {
                        for (int i = 0; i < identLevel+1; ++i) {
                            printf("  ");
                        }
                        printf(".\n");
                    }
                    if (!node->dotted || e->val) {
                        dumpTree(state, e->val, identLevel+1);
                    }
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
            case EXPR_DOUBLE: {
                printf("%f", node->doubleVal);
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
            case EXPR_VARIABLE: {
                String symStr =
                    globalSymbolIdToSymbol(&state->globalSymbolTable,
                                           node->var.symbolID);
                if (node->var.kind == VAR_LOCAL) {
                    printf("%.*s(%u)", (int)symStr.length, symStr.val,
                           node->var.variableID);
                } else {
                    printf("%.*s", (int)symStr.length, symStr.val);
                }
            } break;
            case EXPR_CALL: {
                printf("(");
                if (node->callee) {
                    dumpTree(state, node->callee, 0);
                    for (ExprList *param = node->arguments;
                         param; param = param->next) {
                        if (node->dotted && !param->next) {
                            for (int i = 0; i < identLevel+1; ++i) {
                                printf("  ");
                            }
                            printf(".\n");
                        }
                        if (!node->dotted || param->val) {
                            dumpTree(state, param->val, identLevel+1);
                        }
                    }
                    for (int i = 0; i < identLevel; ++i) {
                        printf("  ");
                    }
                } else {
                    assert(node->arguments == 0);
                }
                printf(")");
            } break;
            case EXPR_MACRO: {
                printf("(MACRO %u (\n", node->macro.name);
                if (node->macro.params) {
                    for (ExprList *param = node->macro.params;
                         param; param = param->next) {
                        if (node->macro.varargs && !param->next) {
                            for (int i = 0; i < identLevel+2; ++i) {
                                printf("  ");
                            }
                            printf(".\n");
                        }
                        if (!node->macro.varargs || param->val) {
                            dumpTree(state, param->val, identLevel+2);
                        }
                    }
                }
                for (int i = 0; i < identLevel+1; ++i) {
                    printf("  ");
                }
                printf(")\n");
                for (ExprList *expr = node->macro.body;
                     expr; expr = expr->next) {
                    dumpTree(state, expr->val, identLevel+1);
                }
                for (int i = 0; i < identLevel; ++i) {
                    printf("  ");
                }
                printf(")\n");
            } break;
            case EXPR_LAMBDA: {
                printf("(LAMBDA (\n");
                if (node->lambda.params) {
                    for (ExprList *param = node->lambda.params;
                         param; param = param->next) {
                        if (node->lambda.varargs && !param->next) {
                            for (int i = 0; i < identLevel+2; ++i) {
                                printf("  ");
                            }
                            printf(".\n");
                        }
                        if (!node->lambda.varargs || param->val) {
                            dumpTree(state, param->val, identLevel+2);
                        }
                    }
                }
                for (int i = 0; i < identLevel+1; ++i) {
                    printf("  ");
                }
                printf(")\n");
                for (ExprList *expr = node->lambda.body;
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
            case EXPR_SET: {
                printf("(SET!\n");
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
        printf("%03llu: ", pc);
        switch (op) {
            case OP_APPEND: {
                printf("APPEND\n");
            } break;
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
            case OP_PUSH_NULL: {
                printf("PUSH_NULL\n");
            } break;
            case OP_POP_ASSERT_EQUAL: {
                printf("POP_ASSERT_EQUAL\n");
            } break;
            case OP_PUSH_LOCAL: {
                pc++;
                printf("PUSH_LOCAL %llu\n", func->bytecode[pc].ui64);
            } break;
            case OP_PUSH_GLOBAL: {
                printf("PUSH_GLOBAL\n");
            } break;
            case OP_PUSH_UPVAL: {
                pc++;
                printf("PUSH_UPVAL %llu\n", func->bytecode[pc].ui64);
            } break;
            case OP_LIST: {
                printf("LIST\n");
            } break;
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
                    case LISPIS_BOOLEAN: {
                        printf("%s\n",
                               unpackBoolean(func->bytecode[pc]) ?
                               "TRUE" : "FALSE");
                    } break;
                    default:assert(false);
                }
            } break;
            case OP_SET_GLOBAL: {
                printf("SET_GLOBAL\n");
            } break;
            case OP_SET_UPVAL: {
                pc++;
                printf("SET_UPVAL %llu\n", func->bytecode[pc].ui64);
            } break;
            case OP_SET_LOCAL: {
                pc++;
                printf("SET_LOCAL %llu\n", func->bytecode[pc].ui64);
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
            case OP_POP_ASSERT_LESS_OR_EQUAL: {
                printf("POP_ASSERT_LESS_OR_EQUAL\n");
            } break;
            case OP_COLLECT_VARARGS: {
                pc++;
                int32 numFormals = func->bytecode[pc].i32;
                printf("COLLECT_VARARGS %d\n",
                       numFormals);
            } break;
            case OP_ALLOC_VECTOR: {
                pc++;
                uint64 size = func->bytecode[pc].ui64;
                printf("ALLOC_VECTOR %llu\n", size);
            } break;
            case OP_ALLOC_OBJECT: {
                pc++;
                uint64 size = func->bytecode[pc].ui64;
                printf("ALLOC_OBJECT %llu\n", size);
            } break;
            case OP_SET_ELEM: {
                printf("SET_ELEM\n");
            } break;
            case OP_PUSH_ELEM: {
                printf("PUSH_ELEM\n");
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
            case EXPR_REF: {
                dealloc(expr->ref.obj);
                dealloc(expr->ref.ref);
            } break;
            case EXPR_REF_SET: {
                dealloc(expr->refSet.obj);
                dealloc(expr->refSet.ref);
                dealloc(expr->refSet.val);
            } break;
            case EXPR_VECTOR: {
                deallocList(expr->vec.elems);
            } break;
            case EXPR_OBJECT: {
                deallocList(expr->obj.elems);
            } break;
            case EXPR_KEY_VALUE_PAIR: {
                dealloc(expr->keyValPair.key);
                dealloc(expr->keyValPair.val);
            } break;
            case EXPR_DOUBLE:
            case EXPR_STRING: // FIX
            case EXPR_BOOLEAN:
            case EXPR_SYMBOL_ID:
            case EXPR_INT:
            case EXPR_VARIABLE:
            case EXPR_SYMBOL: {
            } break;
            case EXPR_FOR: {
                dealloc(expr->init);
                dealloc(expr->upd);
                dealloc(expr->pred);
                deallocList(expr->body);
            } break;
            case EXPR_SET:
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
            case EXPR_DO:
            case EXPR_LIST: {
                deallocList(expr->list);
            } break;
            case EXPR_MACRO: {
                deallocList(expr->macro.params);
                deallocList(expr->macro.body);
            } break;
            case EXPR_LAMBDA: {
                deallocList(expr->lambda.params);
                deallocList(expr->lambda.body);
            } break;
            case EXPR_IF: {
                dealloc(expr->predicate);
                dealloc(expr->trueBranch);
                dealloc(expr->falseBranch);
            } break;
            case EXPR_QUASIQUOTE: {
                QuasiquoteList *e = expr->quasiquoteList;
                while(e) {
                    dealloc(e->val);
                    QuasiquoteList *next = e->next;
                    free(e);
                    e = next;
                }
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

void printValueRec(SymbolTable *globalSymbolTable,
                   Value v);

void printList(SymbolTable *globalSymbolTable,
               Value v) {
    Pair *p = unpackCons(v);
    if (p) {
        assert(v.ui64 != p->car.ui64);
        assert(v.ui64 != p->cdr.ui64);
        printValueRec(globalSymbolTable, p->car);
        if (getType(p->cdr) != LISPIS_CONS) {
            printf(" . ");
            printValueRec(globalSymbolTable, p->cdr);
        } else if (!isNill(p->cdr)) {
            printf(" ");
            printList(globalSymbolTable, p->cdr);
        }
    } else {
        printf("()");
    }
}

void printValueRec(SymbolTable *globalSymbolTable,
                   Value v) {
    switch (getType(v)) {
        case LISPIS_OBJECT: {
            printf("{");
            Object *obj = unpackObject(v);
            for (int i = 0, num = 0; i < obj->size; ++i) {
                if (obj->elems[i].filled) {
                    if (num != 0) {
                        printf(" ");
                    }
                    printf("(");
                    String key =
                        globalSymbolIdToSymbol(globalSymbolTable,
                                               obj->elems[i].key);
                    printf("%.*s ", key.length, key.val);
                    printValueRec(globalSymbolTable, obj->elems[i].val);
                    printf(")");
                    num++;
                }
            }
            printf("}");
        } break;
        case LISPIS_VECTOR: {
            Vector *vec = unpackVector(v);
            printf("[");
            VectorBucket *bucket = vec->firstBucket;
            for (int i = 0; i < vec->numFilled; ++i) {
                int indexInBucket = i % VECTOR_BUCKET_SIZE;
                if (i && !indexInBucket) {
                    bucket = bucket->next;
                }
                printValueRec(globalSymbolTable,
                              bucket->elems[indexInBucket]);
                if (i+1 < vec->numFilled) {
                    printf(" ");
                }
            }
            printf("]");
        } break;
        case LISPIS_BOOLEAN: {
            printf("%s", unpackBoolean(v) ? "true" : "false");
        } break;
        case LISPIS_INT32: {
            printf("%d", unpackInt(v));
        } break;
        case LISPIS_SYM_IDX: {
            String sym = globalSymbolIdToSymbol(globalSymbolTable,
                                                v.ui32);
            printf("%.*s", sym.length, sym.val);
        } break;
        case LISPIS_DOUBLE: {
            printf("%f", v.f64);
        } break;
        case LISPIS_CONS: {
            printf("(");
            printList(globalSymbolTable, v);
            printf(")");
        } break;
            //case NAN_PACKINT_USERP: {
            //} break;
        case LISPIS_CFUNC: {
            CFunction *cfunc = unpackCFunc(v);
            printf("<#CFunction %p>", cfunc);
        } break;
        case LISPIS_LFUNC: {
            LispisFunctionObject *lfunc = unpackLFunc(v);
            printf("<#Function %p>", lfunc);
        } break;
            //case NAN_PACKING_LFUNC: {
            //} break;
        case LISPIS_UNDEF: {
            printf("<#Undef>");
        } break;
        default:assert(false);
    }
}

void printValue(SymbolTable *globalSymbolTable, Value v) {
    printValueRec(globalSymbolTable, v);
    printf("\n");
}