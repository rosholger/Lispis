#ifndef COMMON_H
#define COMMON_H

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
    OP_SET_LOCAL_VARIABLE,
    OP_SET_GLOBAL_VARIABLE,
    OP_CALL,
    OP_RETURN,
    OP_SYMBOL_SECTION,
    OP_LIST,
    OP_EVAL_SYMBOL,
    OP_CLEAR_STACK,
    OP_POP_ASSERT_EQUAL,
    OP_PUSH_LAMBDA_ID,
    OP_JUMP_IF_TRUE,
    OP_JUMP,
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



    LISPIS_DOUBLE, // ALWAYS LAST!
};

typedef Bytecode Value;

NanPackingTypes getType(Value v);
int32 unpackInt(Value v);
uint32 unpackSymbolID(Value v);
bool unpackBoolean(Value v);
void *unpackPointer(Value v, NanPackingTypes typeID);
Value nanPack(uint32 val, uint32 typeID);
Value nanPackInt32(int32 a);
Value nanPackDouble(double d);
Value nanPackSymbolIdx(uint32 s);
Value nanPackPointer(void *p, uint32 typeID);
Value nanPackBoolean(bool b);
#endif