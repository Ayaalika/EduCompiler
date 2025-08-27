#include "AST.hpp"
#include "AstVisitor.hpp"

void BinaryExpr::accept(Visitor& v) { v.visit(*this); }
void UnaryExpr::accept(Visitor& v) { v.visit(*this); }
void CallExpr::accept(Visitor& v) { v.visit(*this); }
void Identifier::accept(Visitor& v) { v.visit(*this); }
void IntLiteral::accept(Visitor& v) { v.visit(*this); }
void StringLiteral::accept(Visitor& v) { v.visit(*this); }
void BoolLiteral::accept(Visitor& v) { v.visit(*this); }
void GroupedExpr::accept(Visitor& v) { v.visit(*this); }
void NullExpr::accept(Visitor& v) { v.visit(*this); }

void VariableDecl::accept(Visitor& v) { v.visit(*this); }
void Assignment::accept(Visitor& v) { v.visit(*this); }
void PrintStmt::accept(Visitor& v) { v.visit(*this); }
void ReturnStmt::accept(Visitor& v) { v.visit(*this); }
void IfStmt::accept(Visitor& v) { v.visit(*this); }
void WhileStmt::accept(Visitor& v) { v.visit(*this); }
void ExprStmt::accept(Visitor& v) { v.visit(*this); }
void Block::accept(Visitor& v) { v.visit(*this); }
void FunctionDecl::accept(Visitor& v) { v.visit(*this); }
