#pragma once

#include "SymbolTable.hpp"
#include <iostream>
#include <unordered_set>
#include "AST.hpp"
#include "AstVisitor.hpp"
#include "token.hpp"

#include <string>
#include <vector>
#include <optional>
#include <unordered_map>

class SemanticAnalyzer : public Visitor {
public:
    SemanticAnalyzer() {
        enterScope();
        registerBuiltinFunctions();
    }
    ~SemanticAnalyzer() override = default;

    std::vector<std::string> analyze(Program& program);
    std::optional<Type> analyze(Expr& expr);             
    void analyze(Stmt& stmt);                             

    void visit(Identifier& expr) override;
    void visit(IntLiteral& expr) override;
    void visit(StringLiteral& expr) override;
    void visit(BoolLiteral& expr) override;
    void visit(BinaryExpr& expr) override;
    void visit(UnaryExpr& expr) override;
    void visit(CallExpr& expr) override;
    void visit(GroupedExpr& expr) override;
    void visit(NullExpr& expr) override;

    void visit(VariableDecl& stmt) override;
    void visit(Assignment& stmt) override;
    void visit(PrintStmt& stmt) override;
    void visit(ReturnStmt& stmt) override;
    void visit(IfStmt& stmt) override;
    void visit(WhileStmt& stmt) override;
    void visit(ExprStmt& stmt) override;
    void visit(Block& stmt) override;
    void visit(FunctionDecl& stmt) override;

    bool areTypesCompatible(const Type& expected, const Type& actual) const;
    bool typesEqual(const Type& a, const Type& b) const;
    std::string typeToString(const Type& t) const;
    Type makeBuiltinType(Type::Kind k) const;
    Type::Kind getKindFromName(const std::string& typeName) const;
    bool isValidType(const std::string& typeName) const;

    bool declareVariable(const Symbol& symbol); 
    bool declareFunction(const Symbol& symbol);  
    bool isVariableDeclared(const std::string& name) const;
    bool isFunctionDeclared(const std::string& name) const;
    bool isSymbolDeclared(const std::string& name) const;
    std::optional<Symbol> resolveSymbol(const std::string& name) const;
    Symbol makeSymbol(const std::string& name, const Type& type,bool isFunction = false, std::vector<Type> params = {});

    std::optional<Type> getExprType(const Expr& expr) const; 
    void setExprType(const Expr* expr, const Type& t);
    void setLiteralType(const Expr* expr, Type::Kind kind, const std::string& name);

    void semanticError(const std::string& msg, int line = -1);
    void semanticErrorAtToken(const Token* token, const std::string& msg);

    void pushReturnType(std::optional<Type> t);
    void popReturnType();
    std::optional<Type> currentReturnType() const;

    void enterScope();
    void exitScope();

    void checkReturnType(const std::optional<Type>& actual, int line);

    void registerBuiltinFunctions();

    const std::unordered_map<const Expr*, Type>& getInferredTypes() const {
        return inferredTypes_;
    }

    const std::vector<std::string>& errors() const { return errors_; }
private:
    ScopeStack scopes_;                                    
    std::unordered_map<const Expr*, Type> inferredTypes_;       
    std::vector<std::optional<Type>> returnTypeStack_;            
    std::vector<std::string> errors_;                    
    std::optional<Symbol> currentFunction_;                    
    std::unordered_map<std::string, Symbol> builtinRegistry_;   

    void pushErrorMessage(const std::string& s);
};

std::vector<std::string> SemanticAnalyzer::analyze(Program& program) {
    errors_.clear();
    inferredTypes_.clear();
    returnTypeStack_.clear();
    currentFunction_.reset();
    scopes_.clear();
    enterScope(); 
    registerBuiltinFunctions();

    for (auto& fn : program.functions) {
        if (!fn)continue;
        if (!fn->name.value.has_value()) {
            semanticError("Function with no name", fn->name.line);
            continue;
        }

        std::string funcName = fn->name.value.value();
        std::vector<Type>params;
        for (const auto& p : fn->parameters) {
            params.push_back(p.type);
        }
        Symbol s = makeSymbol(funcName, fn->returnType, true, params);
        s.isInitialized = true;

        if (!declareFunction(s)) {
            semanticErrorAtToken(&fn->name, "Function '" + funcName + "' already declared");
        }
    }
    for (auto& stmt : program.statements) {
        if (stmt) analyze(*stmt);
    }
    for (auto& fn : program.functions) {
        if (fn) {
            fn->accept(*this);
        }
    }
    exitScope();
    return errors_;
}

bool SemanticAnalyzer::declareVariable(const Symbol& symbol) {
    if (!scopes_.declare(symbol.name, symbol)) {
        semanticError("Variable '" + symbol.name + "' already declared in current scope.");
        return false;
    }
    return true;
}

bool SemanticAnalyzer::declareFunction(const Symbol& symbol) {
    Symbol s = symbol;
    s.isFunction = true;
    if (!scopes_.declare(s.name, s)) {
        semanticError("Function '" + symbol.name + "' already declared in current scope.");
        return false;
    }
    return true;
}

bool SemanticAnalyzer::isVariableDeclared(const std::string& name) const {
    auto cur = scopes_.containsInCurrentScope(name);
    return cur.has_value() && !cur->isFunction;
}

bool SemanticAnalyzer::isFunctionDeclared(const std::string& name) const {
    auto sym = scopes_.resolve(name);
    return sym.has_value() && sym->isFunction;
}

bool SemanticAnalyzer::isSymbolDeclared(const std::string& name) const {
    return scopes_.isDeclared(name);
}

std::optional<Symbol> SemanticAnalyzer::resolveSymbol(const std::string& name) const {
    return scopes_.resolve(name);
}

void SemanticAnalyzer::registerBuiltinFunctions() {
    Type str = makeBuiltinType(Type::Kind::String);
    Type v = makeBuiltinType(Type::Kind::Void);

    Symbol printSym = makeSymbol("print", v, true, { str });
    printSym.isInitialized = true;
    builtinRegistry_["print"] = printSym;

    if (!scopes_.isDeclared("print")) {
        declareFunction(printSym);
    }
}

std::optional<Type> SemanticAnalyzer::analyze(Expr& expr) {
    expr.accept(*this);
    return getExprType(expr);
}

void SemanticAnalyzer::analyze(Stmt& stmt) {
    stmt.accept(*this);
}

void SemanticAnalyzer::checkReturnType(const std::optional<Type>& actual, int line) {
    auto expected = currentReturnType();
    if (!expected.has_value()) {
        semanticError("Return statement not inside a function", line);
        return;
    }
    if (!actual.has_value()) {
        if (!(expected->kind == Type::Kind::Named && expected->name == "void")) {
            semanticError("Return with no value in function expecting '" + typeToString(*expected) + "'", line);
        }
        return;
    }
    if (!areTypesCompatible(*expected, *actual)) {
        semanticError("Return type mismatch: expected '" + typeToString(*expected) + "', got '" + typeToString(*actual) + "'", line);
    }
}

Type::Kind SemanticAnalyzer::getKindFromName(const std::string& typeName) const {
    if (typeName == "int") return Type::Kind::Int;
    if (typeName == "bool") return Type::Kind::Bool;
    if (typeName == "string") return Type::Kind::String;
    if (typeName == "void") return Type::Kind::Void;
    return Type::Kind::Named;
}

bool SemanticAnalyzer::isValidType(const std::string& typeName) const {
    return (typeName == "int" || typeName == "bool" || typeName == "string" || typeName == "void");
}

std::string SemanticAnalyzer::typeToString(const Type& t) const {
    switch (t.kind) {
    case Type::Kind::Int: return "int";
    case Type::Kind::Bool: return "bool";
    case Type::Kind::String: return "string";
    case Type::Kind::Named: return t.name;
    default: return "<unknown>";
    }
}

void SemanticAnalyzer::pushReturnType(std::optional<Type> t) {
    returnTypeStack_.push_back(t);
}

void SemanticAnalyzer::popReturnType() {
    if (!returnTypeStack_.empty()) returnTypeStack_.pop_back();
}

std::optional<Type> SemanticAnalyzer::currentReturnType() const {
    if (returnTypeStack_.empty()) return std::nullopt;
    return returnTypeStack_.back();
}

Type SemanticAnalyzer::makeBuiltinType(Type::Kind k) const {
    Type t;
    t.kind = k;
    switch (k) {
    case Type::Kind::Int: t.name = "int"; break;
    case Type::Kind::Bool: t.name = "bool"; break;
    case Type::Kind::String: t.name = "string"; break;
    case Type::Kind::Void:t.name = "void"; break;
    case Type::Kind::Named: t.name = "<named>"; break;
    }
    return t;
}

std::optional<Type> SemanticAnalyzer::getExprType(const Expr& expr) const {
    auto it = inferredTypes_.find(&expr);
    if (it == inferredTypes_.end())return std::nullopt;
    return it->second;
}

void SemanticAnalyzer::pushErrorMessage(const std::string& s) {
    errors_.push_back(s);
}

void SemanticAnalyzer::semanticError(const std::string& msg, int line) {
    if (line > 0) pushErrorMessage("Line " + std::to_string(line) + ": " + msg);
    else pushErrorMessage(msg);
}

void SemanticAnalyzer::semanticErrorAtToken(const Token* token, const std::string& msg) {
    if (token) semanticError(msg, token->line);
    else semanticError(msg, -1);
}

Symbol SemanticAnalyzer::makeSymbol(const std::string& name, const Type& type, bool isFunction, std::vector<Type> params) {
    Symbol s;
    s.name = name;
    s.type = type;
    s.isFunction = isFunction;
    s.parameters = std::move(params);
    s.isInitialized = true;
    return s;
}

void SemanticAnalyzer::enterScope() {
    scopes_.beginScope();
}

void SemanticAnalyzer::exitScope() {
    scopes_.endScope();
}

void SemanticAnalyzer::setLiteralType(const Expr* expr, Type::Kind kind, const std::string& name) {
    Type type;
    type.kind = kind;
    type.name = name;
    setExprType(expr, type);
}

void SemanticAnalyzer::setExprType(const Expr* expr, const Type& t) {
    inferredTypes_[expr] = t;
}

void SemanticAnalyzer::visit(IntLiteral& expr){
    setLiteralType(&expr, Type::Kind::Int, "int");
}

void SemanticAnalyzer:: visit(StringLiteral& expr){
    setLiteralType(&expr, Type::Kind::String, "string");
}

void SemanticAnalyzer:: visit(BoolLiteral& expr){
    setLiteralType(&expr, Type::Kind::Bool, "bool");
}

void SemanticAnalyzer::visit(Identifier& expr) {
    if (!expr.token.value.has_value()) {
        semanticErrorAtToken(&expr.token, "Invalid identifier token");
        return;
    }
    const std::string name = expr.token.value.value();
    auto symbol = resolveSymbol(name);
    if (!symbol.has_value()) {
        semanticErrorAtToken(&expr.token, "Undefined identifier '" + name + "'");
        return;
    }
    Symbol sym = *symbol;
    if (sym.isFunction) {
        semanticErrorAtToken(&expr.token, "Function '" + name + "' used without call");
        return;
    }
    if (!sym.isInitialized) {
        semanticErrorAtToken(&expr.token, "Variable '" + name + "' used before initialization");
        return;
    }
    setExprType(&expr, symbol->type);
}

void SemanticAnalyzer::visit(BinaryExpr& expr) {
    if (!expr.left || !expr.right) {
        semanticError("Malformed binary expression", expr.line);
        return;
    }

    expr.left->accept(*this);
    expr.right->accept(*this);

    auto Lt = getExprType(*expr.left);
    auto Rt = getExprType(*expr.right);

    if (!Lt.has_value() || !Rt.has_value()) return;

    Type result;
    bool ok = true;

    switch (expr.op.type) {
    case TokenType::plus:
        if (Lt->kind == Type::Kind::Int && Rt->kind == Type::Kind::Int)
            result = makeBuiltinType(Type::Kind::Int);
        else if (Lt->kind == Type::Kind::String && Rt->kind == Type::Kind::String)
            result = makeBuiltinType(Type::Kind::String);
        else ok = false;
        break;
    case TokenType::minus:
    case TokenType::mul:
    case TokenType::div:
        if (Lt->kind == Type::Kind::Int && Rt->kind == Type::Kind::Int)
            result = makeBuiltinType(Type::Kind::Int);
        else ok = false;
        break;
    case TokenType::eq:
    case TokenType::neq:
        if (typesEqual(*Lt, *Rt))
            result = makeBuiltinType(Type::Kind::Bool);
        else ok = false;
        break;
    case TokenType::lt:
    case TokenType::lte:
    case TokenType::gt:
    case TokenType::gte:
        if (Lt->kind == Type::Kind::Int && Rt->kind == Type::Kind::Int)
            result = makeBuiltinType(Type::Kind::Bool);
        else ok = false;
        break;
    default:
        ok = false;
        break;
    }
    if (!ok) {
        semanticErrorAtToken(&expr.op, std::string("Type error in binary operator '") + to_string(expr.op.type) + "'");
        return;
    }
    setExprType(&expr, result);
}

void SemanticAnalyzer::visit(UnaryExpr& expr) {
    if (!expr.operand) {
        semanticError("Malformed unary expression", expr.line);
        return;
    }
    expr.operand->accept(*this);
    auto ot = getExprType(*expr.operand);
    if (!ot.has_value())return;

    Type result;
    bool ok = true;
    switch (expr.op.type) {
    case TokenType::minus:
        if (ot->kind == Type::Kind::Int) {
            result = makeBuiltinType(Type::Kind::Int);
        }
        else ok = false;
        break;
    case TokenType::bang:
        if (ot->kind == Type::Kind::Bool) {
            result = makeBuiltinType(Type::Kind::Bool);
        }
        else ok = false;
        break;
    default:
        ok = false;
        break;
    }

    if (!ok) {
        semanticErrorAtToken(&expr.op, "Invalid operand type for unary operator");
        return;
    }

    setExprType(&expr, result);
}

bool SemanticAnalyzer::typesEqual(const Type& a, const Type& b) const {
    if (a.kind != b.kind) return false;
    if (a.kind == Type::Kind::Named) return a.name == b.name;
    return true;
}

bool SemanticAnalyzer::areTypesCompatible(const Type& expected, const Type& actual) const {
    if (expected.kind != actual.kind) return false;
    if (expected.kind == Type::Kind::Named) {
        return expected.name == actual.name;
    }
    return true;
}

void SemanticAnalyzer::visit(CallExpr& expr) {
    if (!expr.callee) {
        semanticError("Call with empty callee", expr.line);
        return;
    }

    auto ident = dynamic_cast<Identifier*>(expr.callee.get());
    if (!ident) {
        semanticError("Only simple function calls are supported", expr.line);
        return;
    }

    if (!ident->token.value.has_value()) {
        semanticErrorAtToken(&ident->token, "Invalid function identifier");
        return;
    }

    std::string funcName = ident->token.value.value();
    auto symOpt = resolveSymbol(funcName);

    if (!symOpt.has_value()) {
        semanticErrorAtToken(&ident->token, "Call to undefined function '" + funcName + "'");
        return;
    }

    const Symbol fnSym = *symOpt;

    if (!fnSym.isFunction) {
        semanticErrorAtToken(&ident->token, "'" + funcName + "' is not a function");
        return;
    }

    if (fnSym.parameters.size() != expr.args.size()) {
        semanticErrorAtToken(&ident->token,
            "Function '" + funcName + "' expects " + std::to_string(fnSym.parameters.size()) +
            " arguments but got " + std::to_string(expr.args.size()));
        return;
    }

    for (size_t i = 0; i < expr.args.size(); i++) {
        if (!expr.args[i]) continue;
        expr.args[i]->accept(*this);
        auto argType = getExprType(*expr.args[i]);
        if (!argType.has_value()) continue;
        if (!areTypesCompatible(fnSym.parameters[i], *argType)) {
            semanticErrorAtToken(&ident->token,
                "Argument " + std::to_string(i + 1) + " type mismatch: expected '" +
                typeToString(fnSym.parameters[i]) + "', got '" + typeToString(*argType) + "'");
        }
    }
    setExprType(&expr, fnSym.type);
}

void SemanticAnalyzer::visit(GroupedExpr& expr) {
    if (expr.expr) {
        expr.expr->accept(*this);
        auto t = getExprType(*expr.expr);
        if (t.has_value()) setExprType(&expr, *t);
    }
}

void SemanticAnalyzer::visit(NullExpr& expr) {
    Type t;
    t.kind = Type::Kind::Named;
    t.name = "null";
    setExprType(&expr, t);
}

void SemanticAnalyzer::visit(ExprStmt& stmt) {
    if (stmt.expr) stmt.expr->accept(*this);
}

void SemanticAnalyzer::visit(Block& stmt) {
    enterScope();
    for (auto& s : stmt.statements) {
        if (s) s->accept(*this);
    }
    exitScope();
}

void SemanticAnalyzer::visit(PrintStmt& stmt) {
    if (stmt.expr) {
        auto type = analyze(*stmt.expr);
        if (!type.has_value()) {
            semanticError("Invalid expression in statement", stmt.line);
        }
    }
}

void SemanticAnalyzer::visit(ReturnStmt& stmt) {
    if (!currentFunction_.has_value()) {
        semanticError("Return statement outside function", stmt.line);
        return;
    }

    std::optional<Type> exprType;
    if (stmt.expr) {
        exprType = analyze(*stmt.expr);
        if (!exprType.has_value()) {
            semanticError("Invalid return expression", stmt.line);
            return;
        }
    }

    Type expectedType = currentFunction_->type;

    if (stmt.expr) {
        if (!areTypesCompatible(expectedType, *exprType)) {
            semanticError("Return type mismatch: expected '" +
                typeToString(expectedType) + "', got '" +
                typeToString(*exprType) + "'", stmt.line);
        }
    }
    else {
        if (!(expectedType.kind == Type::Kind::Named && expectedType.name == "void")) {
            semanticError("Empty return in non-void function", stmt.line);
        }
    }
}

void SemanticAnalyzer::visit(VariableDecl& stmt) {
    if (!stmt.token.value.has_value()) {
        semanticError("Variable declaration without a name", stmt.line);
        return;
    }
    const std::string name = stmt.token.value.value();

    if (scopes_.containsInCurrentScope(name)) {
        semanticErrorAtToken(&stmt.token, "Variable '" + name + "' already declared in this scope");
        return;
    }
    std::optional<Type> initType;
    if (stmt.initializer) {
        stmt.initializer->accept(*this);
        initType = getExprType(*stmt.initializer);
        if (!initType.has_value()) {
            semanticErrorAtToken(&stmt.token, "Invalid initializer for variable '" + name + "'");
            return;
        }
    }
    std::optional<Type> declaredType;
    if (stmt.type) {
        declaredType = *stmt.type;
        if (!isValidType(declaredType->name)) {
            semanticErrorAtToken(&stmt.token, "Unknown type '" + declaredType->name + "'");
            return;
        }
    }
    if (!declaredType.has_value() && !initType.has_value()) {
        semanticErrorAtToken(&stmt.token, "Variable '" + name + "' requires either a type or an initializer");
        return;
    }
    Type finalType;
    if (declaredType.has_value()) {
        finalType = *declaredType;
        if (initType.has_value() && !areTypesCompatible(finalType, *initType)) {
            semanticErrorAtToken(&stmt.token,
                "Cannot initialize variable of type '" + typeToString(finalType) +
                "' with expression of type '" + typeToString(*initType) + "'");
            return;
        }
    }
    else {
        finalType = *initType;
    }
    Symbol sym = makeSymbol(name, finalType, false, {});
    sym.isInitialized = initType.has_value();
    sym.isMutable = true;

    if (!declareVariable(sym)) {
        semanticErrorAtToken(&stmt.token, "Failed to declare variable '" + name + "'");
    }
}

void SemanticAnalyzer::visit(Assignment& stmt) {
    if (!stmt.target.value.has_value()) {
        semanticError("Assignment to unnamed target", stmt.line);
        return;
    }
    std::string targetName = stmt.target.value.value();
    auto symOpt = resolveSymbol(targetName);
    if (!symOpt.has_value()) {
        semanticErrorAtToken(&stmt.target, "Assignment to undefined variable '" + targetName + "'");
        return;
    }
    Symbol sym = *symOpt;
    if (sym.isFunction) {
        semanticErrorAtToken(&stmt.target, "Cannot assign to function name '" + targetName + "'");
        return;
    }
    if (!stmt.expr) {
        semanticError("Assignment without expression", stmt.line);
        return;
    }
    stmt.expr->accept(*this);
    auto exprT = getExprType(*stmt.expr);
    if (!exprT.has_value()) return;

    if (!areTypesCompatible(sym.type, *exprT)) {
        semanticErrorAtToken(&stmt.target, "Cannot assign value of type '" + typeToString(*exprT) + "' to variable '" + targetName + "' of type '" + typeToString(sym.type) + "'");
        return;
    }
    sym.isInitialized = true;
    if (!scopes_.updateSymbol(targetName, sym)) {
        semanticError("Internal error: failed to update symbol '" + targetName + "'", stmt.line);
    }
}

void SemanticAnalyzer::visit(IfStmt& stmt ){
    if (stmt.condition) {
        stmt.condition->accept(*this);
        auto ct = getExprType(*stmt.condition);
        if (!ct.has_value() || ct->kind != Type::Kind::Bool) {
            semanticError("Condition in if must be a boolean", stmt.condition->line);
        }
    }

    if (stmt.thenBlock) stmt.thenBlock->accept(*this);

    if (stmt.elseBlock && stmt.elseBlock.has_value()) {
        stmt.elseBlock.value()->accept(*this);
    }
}

void SemanticAnalyzer::visit(WhileStmt& stmt) {
    if (stmt.condition) {
        stmt.condition->accept(*this);
        auto ct = getExprType(*stmt.condition);
        if (!ct.has_value() || ct->kind != Type::Kind::Bool) {
            semanticError("Condition in while must be a boolean", stmt.condition->line);
        }
    }
    if (stmt.body) stmt.body->accept(*this);
}

void SemanticAnalyzer::visit(FunctionDecl& stmt) {
    if (!stmt.name.value.has_value()) {
        semanticError("Function declaration without a name", stmt.name.line);
        return;
    }

    std::string fname = stmt.name.value.value();
    auto symOpt = resolveSymbol(fname);
    if (!symOpt.has_value() || !symOpt->isFunction) {
        semanticErrorAtToken(&stmt.name, "Function '" + fname + "' not properly registered");
        return;
    }

    currentFunction_ = symOpt;
    pushReturnType(symOpt->type);
    enterScope();

    for (const auto& p : stmt.parameters) {
        Symbol psym = makeSymbol(p.name, p.type, false, {});
        psym.isInitialized = true;
        if (!declareVariable(psym)) {
            semanticErrorAtToken(&stmt.name, "Duplicate parameter name: " + p.name);
        }
    }

    if (stmt.body) stmt.body->accept(*this);

    exitScope();
    popReturnType();
    currentFunction_.reset();
}