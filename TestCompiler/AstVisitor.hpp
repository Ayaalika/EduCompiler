#pragma once
#include "AST.hpp"

struct Visitor {
    virtual void visit(Identifier& expr) = 0;
    virtual void visit(IntLiteral& expr) = 0;
    virtual void visit(StringLiteral& expr) = 0;
    virtual void visit(BoolLiteral& expr) = 0;
    virtual void visit(BinaryExpr& expr) = 0;
    virtual void visit(UnaryExpr& expr) = 0;
    virtual void visit(CallExpr& expr) = 0;
    virtual void visit(GroupedExpr& expr) = 0;
    virtual void visit(NullExpr& expr) = 0;

    virtual void visit(VariableDecl& stmt) = 0;
    virtual void visit(Assignment& stmt) = 0;
    virtual void visit(PrintStmt& stmt) = 0;
    virtual void visit(ReturnStmt& stmt) = 0;
    virtual void visit(IfStmt& stmt) = 0;
    virtual void visit(WhileStmt& stmt) = 0;
    virtual void visit(ExprStmt& stmt) = 0;
    virtual void visit(Block& stmt) = 0;
    virtual void visit(FunctionDecl& stmt) = 0;

    virtual ~Visitor() = default;
};