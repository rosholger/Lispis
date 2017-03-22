#ifndef COMMON_H
#define COMMON_H

#include "config.h"
#include <cassert>
#include <cmath>

#define arrayLength(array) (sizeof(array)/sizeof(*array))

typedef unsigned int uint32;
typedef unsigned char uint8;
typedef unsigned long long int uint64;
typedef int int32;
typedef long long int int64;

struct String {
    char *val;
    int32 length;
};

enum OpCodes {
    OP_EXIT,
    OP_PUSH,
    OP_COLLECT_VARARGS,
    OP_SET_LOCAL,
    OP_SET_UPVAL,
    OP_SET_GLOBAL,
    OP_CALL,
    OP_RETURN,
    OP_SYMBOL_SECTION,
    OP_LIST,
    //OP_EVAL_SYMBOL,
    OP_PUSH_LOCAL,
    OP_PUSH_GLOBAL,
    OP_PUSH_UPVAL,
    OP_CLEAR_STACK,
    OP_POP_ASSERT_EQUAL,
    OP_POP_ASSERT_LESS_OR_EQUAL,
    OP_PUSH_LAMBDA_ID,
    OP_JUMP_IF_TRUE,
    OP_PUSH_NULL,
    OP_JUMP,
    OP_APPEND,
    OP_ALLOC_OBJECT,
    OP_ALLOC_VECTOR,
    OP_SET_ELEM,
    OP_PUSH_ELEM,
    // optimization, (refset obj '*proto*) gets compiled to this
    // OP_SET_ELEM where elem is *proto* also sets the proto
    OP_SET_PROTO,
    // optimization, (ref obj '*proto*) gets compiled to this
    // OP_PUSH_ELEM where elem is *proto* also pushes the proto
    OP_PUSH_PROTO, //optimization (
};

// TODO: switch all cases of bytecode from uint64 * to Bytecode *. done?
union Bytecode {
    uint64 ui64;
    int64 i64;
    double f64;
    struct {
        uint32 ui32;
        uint32 dummy0;
    };
    struct {
        int32 i32;
        int32 dummy1;
    };
    char c[8]; 
    struct {
        OpCodes opCode;
        char dummy3[sizeof(uint64)-sizeof(OpCodes)];
    };
};

struct Expr;
struct ExprList;
struct LispisState;
struct LispisFunction;
struct SymbolTable;
String globalSymbolIdToSymbol(SymbolTable *globalSymbolTable,
                              uint32 globalSymbolId);
void dumpTree(LispisState *state, Expr *node, int identLevel);
void dumpSymbolSection(LispisState *state,
                       LispisFunction *func);
void dumpBytecode(LispisState *state, LispisFunction *func);
void dealloc(Expr *expr);
char *readEntireFile(char *filename);
bool symCmp(String a, uint64 hashA, String b, uint64 hashB);
uint64 hashFunc(String str);

enum NanPackingTypes {
    LISPIS_UNDEF,
    LISPIS_OP,
    LISPIS_INT32,
    LISPIS_SYM_IDX,
    LISPIS_USERP,
    LISPIS_CFUNC,
    LISPIS_LFUNC,
    LISPIS_CONS,
    LISPIS_BOOLEAN,
    LISPIS_VECTOR,
    LISPIS_OBJECT,



    LISPIS_DOUBLE, // ALWAYS LAST and <= 16!
};

typedef Bytecode Value;

void printValue(SymbolTable *globalSymbolTable,
                Value v);
// after symbolIdPass, NOT after the following passes
Value exprToConsList(LispisState *state, Expr *expr);
Expr *consListToExpr(LispisState *state, Value consList, int line);

inline
NanPackingTypes getType(Value v) {
    uint64 retI = v.ui64;
    uint64 type = LISPIS_DOUBLE;
    if (v.f64 != v.f64) {
        type = ((retI) >> 47) & 0xf;
    }
    return (NanPackingTypes)type;
}

struct Pair;

inline
int32 unpackInt(Value v) {
    assert(getType(v) == LISPIS_INT32);
    return v.i32;
}

inline
bool unpackBoolean(Value v) {
    assert(getType(v) == LISPIS_BOOLEAN);
    return (bool)v.ui32;
}

inline
uint32 unpackSymbolID(Value v) {
    assert(getType(v) == LISPIS_SYM_IDX);
    return v.ui32;
}

inline
void *unpackPointer(Value v, NanPackingTypes typeID) {
    uint64 unpacked = v.ui64 & 0xFFFFFFFFFFF;
    assert(getType(v) == typeID);
    // TODO: Do i need this?
    //if (unpacked > 0x00008FFFFFFFFFFF) {
    //unpacked |= 0xFFFF000000000000;
    //}
    return (void *)unpacked;
}

struct CFunction;
struct LispisFunctionObject;

inline
Pair *unpackCons(Value v) {
    return (Pair *)unpackPointer(v, LISPIS_CONS);
}

inline
CFunction *unpackCFunc(Value v) {
    return (CFunction *)unpackPointer(v, LISPIS_CFUNC);
}

inline
LispisFunctionObject *unpackLFunc(Value v) {
    return (LispisFunctionObject *)unpackPointer(v, LISPIS_LFUNC);
}

struct Vector;

inline
Vector *unpackVector(Value v) {
    return (Vector *)unpackPointer(v, LISPIS_VECTOR);
}

struct Object;

inline
Object *unpackObject(Value v) {
    return (Object *)unpackPointer(v, LISPIS_OBJECT);
}

inline
Value nanPackPointer(void *p, uint32 typeID) {
    double nan = NAN;
    uint64 retI = (0xFFFFFFFFFFFF & ((uint64)p));
    uint64 nanVal = ((*(uint64 *)&nan) |
                     retI |
                     ((uint64)typeID << 47));
    Value ret;
    ret.ui64 = nanVal;
    return ret;
}

inline
Value nanPack(uint32 val, uint32 typeID) {
    Value ret;
    ret.f64 = NAN;
    ret.ui64 |= (uint64)val | ((uint64)typeID << 47);
    return ret;
}

inline
Value nanPackInt32(int32 a) {
    uint32 u = *(uint32 *)&a;
    return nanPack(u, LISPIS_INT32);
}

inline
Value nanPackSymbolIdx(uint32 s) {
    return nanPack(s, LISPIS_SYM_IDX);
}

inline
Value nanPackDouble(double d) {
    Value ret;
    ret.f64 = d;
    return ret;
}

inline
Value nanPackBoolean(bool b) {
    return nanPack(b, LISPIS_BOOLEAN);
}
#endif