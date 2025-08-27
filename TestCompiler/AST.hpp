#pragma once

#include <memory>
#include <vector>
#include <string>
#include <optional>
#include "token.hpp"

struct Visitor; 

struct Expr {
    int line;
    virtual void accept(Visitor& v) = 0;
    virtual ~Expr() = default;
};

struct BinaryExpr : public Expr {
    Token op;
    std::unique_ptr<Expr> left;
    std::unique_ptr<Expr> right;
    void accept(Visitor& v) override;
};

struct UnaryExpr : public Expr {
    Token op;
    std::unique_ptr<Expr> operand;
    void accept(Visitor& v) override;
};

struct CallExpr : public Expr {
    std::unique_ptr<Expr> callee;
    std::vector<std::unique_ptr<Expr>> args;
    void accept(Visitor& v) override;
};

struct Identifier : Expr {
    Token token;
    void accept(Visitor& v) override;
};

struct IntLiteral : Expr {
    int value;
    void accept(Visitor& v) override;
};

struct StringLiteral : Expr {
    std::string value;
    void accept(Visitor& v) override;
};

struct BoolLiteral : Expr {
    bool value;
    void accept(Visitor& v) override;
};

struct GroupedExpr : Expr {
    std::unique_ptr<Expr> expr;
    void accept(Visitor& v) override;
};

struct NullExpr : Expr {
    void accept(Visitor& v) override;
};

// *******************

struct Stmt {
    int line;
    virtual void accept(Visitor& v) = 0;
    virtual ~Stmt() = default;
};

struct Type {
    enum class Kind { Int, Bool, String, Named, Void} kind;
    std::string name;
    int line = 0;
};

struct Param {
    std::string name;
    Type type;
};

struct Block : Stmt {
    std::vector<std::unique_ptr<Stmt>> statements;
    void accept(Visitor& v) override;
};

struct VariableDecl : Stmt {
    Token token;
    std::unique_ptr<Type> type;
    std::unique_ptr<Expr> initializer;
    void accept(Visitor& v) override;
};

struct Assignment : Stmt {
    Token target;
    std::unique_ptr<Expr> expr;
    void accept(Visitor& v) override;
};

struct PrintStmt : Stmt {
    std::unique_ptr<Expr> expr;
    void accept(Visitor& v) override;
};

struct ReturnStmt : Stmt {
    std::unique_ptr<Expr> expr;
    void accept(Visitor& v) override;
};

struct IfStmt : Stmt {
    std::unique_ptr<Expr> condition;
    std::unique_ptr<Block> thenBlock;
    std::optional<std::unique_ptr<Block>> elseBlock;
    void accept(Visitor& v) override;
};

struct WhileStmt : Stmt {
    std::unique_ptr<Expr> condition;
    std::unique_ptr<Block> body;
    void accept(Visitor& v) override;
};

struct ExprStmt : Stmt {
    std::unique_ptr<Expr> expr;
    void accept(Visitor& v) override;
};

struct FunctionDecl : Stmt {
    Token name;
    std::vector<Param> parameters;
    std::unique_ptr<Block> body;
    Type returnType;
    void accept(Visitor& v) override;
};

struct Program {
    std::vector<std::unique_ptr<FunctionDecl>> functions;
    std::vector<std::unique_ptr<Stmt>> statements;
};
