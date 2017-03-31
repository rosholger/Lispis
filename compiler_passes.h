#ifndef COMPILER_PASSES_H
#define COMPILER_PASSES_H
#include "common.h"

// TODO: change for from C-style to python-style
// ie instead of (for (init pred upd) . body)
// we want to have (for var iter . body)
// where iter is a zero arity function that when called returns
// the "next" value and *undefined* means finished

// This also fixes that implicit variable cant be initialized with
// functions, HUGE gotcha

struct Expr;
struct LispisState;

struct NormalToUnique {
    uint32 normal;
    uint32 uniqueIndex;
};

enum ScopeType {
    REAL_SCOPE,
    QUASI_SCOPE
};

struct LexicalScope;

struct RealLexicalScope {
    uint32 variableIDs[256]; // TODO: variable size? yes? no?
    uint32 variableIDsTop;
    uint32 upvalsTop;
    uint32 totalVariablesSize;
};

struct QuasiLexicalScope {
    uint32 variableIDsBottom;
};

struct LexicalScope {
    LexicalScope *parentScope;
    ScopeType type;
    bool closedOver;
    union {
        RealLexicalScope real;
        QuasiLexicalScope quasi;
    };
};

LexicalScope makeRealScope(LexicalScope *parent);
LexicalScope makeQuasiScope(LexicalScope *parent);

// Change to a class hierarchy... :(
// IN ORDER!!!!
class CompilerPass {
 public:
    virtual Expr *startTransforming(LispisState *state, Expr *expr);
    virtual Expr *transform(LispisState *state, Expr *expr) = 0;
};

class SymbolIdPass: public CompilerPass {
    virtual Expr *transform(LispisState *state, Expr *expr);
};

class MacroExpansionPass: public CompilerPass {
    bool *expandedAMacro;
    virtual Expr *startTransforming(LispisState *state, Expr *expr);
    virtual Expr *transform(LispisState *state, Expr *expr);
};

class DefmacroPass: public CompilerPass {
    virtual Expr *transform(LispisState *state, Expr *expr);
};

class LambdaPass: public CompilerPass {
    virtual Expr *transform(LispisState *state, Expr *expr);
};

class LetPass: public CompilerPass {
    virtual Expr *transform(LispisState *state, Expr *expr);
};

class SetPass: public CompilerPass {
    virtual Expr *transform(LispisState *state, Expr *expr);
};

class DefinePass: public CompilerPass {
    virtual Expr *transform(LispisState *state, Expr *expr);
};

class IfPass: public CompilerPass {
    virtual Expr *transform(LispisState *state, Expr *expr);
};

class ForPass: public CompilerPass {
    virtual Expr *transform(LispisState *state, Expr *expr);
};

class DoPass: public CompilerPass {
    virtual Expr *transform(LispisState *state, Expr *expr);
};

class RefPass: public CompilerPass {
    virtual Expr *transform(LispisState *state, Expr *expr);
};

class VariablePass: public CompilerPass {
 public:
    LexicalScope *currentScope;
    virtual Expr *transform(LispisState *state, Expr *expr);
};

void setupSpecialFormSymbols(LispisState *state);
#endif