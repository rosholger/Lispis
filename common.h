#ifndef COMMON_H
#define COMMON_H

typedef unsigned int uint32;
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
    OP_PUSH_TRANSLATE_SYMBOL,
    OP_SET_LOCAL_SYMBOL,
    OP_CALL,
    OP_RETURN,
    OP_SYMBOL_SECTION,
    OP_LIST,
    OP_EVAL_SYMBOL,
    OP_CLEAR_STACK
};

// TODO: switch all cases of bytecode from uint64 * to Bytecode *
// TODO: typedef Bytecode as Value
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
void dumpTree(Expr *node, int identLevel);
void dumpSymbolSection(Bytecode *bytecode);
void dumpBytecode(Bytecode *bytecode);
void dealloc(Expr *expr);
char *readEntireFile(char *filename);
bool symCmp(String a, uint64 hashA, String b, uint64 hashB);
uint64 hashFunc(String str);

enum NanPackingTypes {
    LISPIS_UNDEF,
    LISPIS_INT32,
    LISPIS_SYM_IDX,
    LISPIS_USERP,
    LISPIS_CFUNC,
    LISPIS_LFUNC,
    LISPIS_CONS,



    LISPIS_DOUBLE, // ALWAYS LAST!
};

typedef Bytecode Value;

NanPackingTypes getType(Value v);
int32 unpackInt(Value v);
uint32 unpackSymbolID(Value v);
void *unpackPointer(Value v, NanPackingTypes typeID);
Value nanPack(uint32 val, uint32 typeID);
Value nanPackInt32(int32 a);
Value nanPackDouble(double d);
Value nanPackSymbolIdx(uint32 s);
Value nanPackPointer(void *p, uint32 typeID);
#endif