#pragma once

#include <string>
#include <vector>
#include <unordered_map>
#include <ostream>
#include <memory>
#include <iomanip>
#include <cstdio>
#include "AST.hpp"
#include "AstVisitor.hpp"
#include "SymbolTable.hpp"
#include "SemanticAnalyzer.hpp"
#include <optional>
#include <algorithm>

#define GREEN "\033[32m"
#define RESET "\033[0m"

enum class OpCode {
    NOP,
    PUSH_INT, PUSH_STR,
    LOAD_LOCAL, STORE_LOCAL,
    LOAD_GLOBAL, STORE_GLOBAL,
    ADD, SUB, MUL, DIV,
    EQ, NEQ, LT, LTE, GT, GTE,
    NEG, NOT,
    JMP, JMP_IF_FALSE,
    CALL, RET,
    POP,
    PRINT
};

enum class ValueType { INT, STRING, BOOL };

struct Instruction {
    OpCode op;
    int line;
    union {
        struct { ValueType type; int a, b; } typed;
        struct { int target; } jump;
        struct { int funcIndex; int argCount; } call;
    } data;

    static Instruction makeTyped(OpCode op, int line, ValueType type, int a, int b) {
        Instruction i;
        i.op = op; i.line = line;
        i.data.typed = { type, a, b };
        return i;
    }

    static Instruction makeJump(OpCode op, int line, int target) {
        Instruction i;
        i.op = op; i.line = line;
        i.data.jump = { target };
        return i;
    }
};

struct FunctionContext {
    std::string name;
    int index = -1;
    std::unordered_map<std::string, int> localSlots;
    int nextLocalSlot = 0;
    int entryLabel = -1;

    int ensureSlot(const std::string& name) {
        auto it = localSlots.find(name);
        if (it != localSlots.end()) return it->second;
        int s = nextLocalSlot++;
        localSlots[name] = s;
        return s;
    }

    int findSlot(const std::string& name) const {
        auto it = localSlots.find(name);
        return it == localSlots.end() ? -1 : it->second;
    }
};

class CodeGenerator : public Visitor {
public:
    explicit CodeGenerator(const SemanticAnalyzer* sema, ScopeStack* scopes = nullptr) : sema_(sema), scopes_(scopes) {}
    ~CodeGenerator() override = default;

    void generate(Program& program);

    void emitToStream(std::ostream& os) const;

    const std::vector<Instruction>& instructions() const { return code_; }
    const std::vector<std::string>& stringPool() const { return stringPool_; }
    const std::vector<int>& intPool() const { return intPool_; }
    const std::vector<std::string>& functionTable() const { return functionTable_; }

private:
    std::vector<Instruction> code_;
    std::vector<std::string> stringPool_;
    std::vector<int> intPool_;
    std::unordered_map<std::string, int> functionIndex_;
    std::vector<std::string> functionTable_;
    const SemanticAnalyzer* sema_;

    int currentLine_ = 1;
    int labelCounter_ = 0;
    std::unordered_map<int, int> labelPos_;
    std::vector<std::pair<int, int>> unresolvedJumps_;
    std::vector<FunctionContext> funcContexts_;
    FunctionContext* curFunc_ = nullptr;
    std::vector<int> functionEntryPos_;
    std::vector<int> functionEntryInstr_;
    ScopeStack* scopes_ = nullptr;

    std::unordered_map<std::string, int> globalSlots_;
    int nextGlobalSlot_ = 0;

    int ensureGlobalSlot(const std::string& name);
    int findGlobalSlot(const std::string& name) const;
    int ensureLocalSlot(const std::string& name);
    int findLocalSlot(const std::string& name);

    int makeLabel();
    int addStringConst(const std::string& s);
    int addIntConst(int v);
    int emit(OpCode op, ValueType type = ValueType::INT, int a = 0, int b = 0);
    int emitJump(OpCode jmpOp, int label);
    void emitLabel(int label);
    void patchAllJumps();

    void beginFunction(const std::string& name, int funcIndex);
    void endFunction();
    
    std::string escapeString(const std::string& s) const;
    const char* opCodeToString(OpCode op) const;
    ValueType mapType(const Type& t) const;

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

    void cgError(const std::string& msg, int line = -1) const;
};

int CodeGenerator::ensureGlobalSlot(const std::string& name) {
    auto it = globalSlots_.find(name);
    if (it != globalSlots_.end()) return it->second;
    int s = nextGlobalSlot_++;
    globalSlots_[name] = s;
    return s;
}

int CodeGenerator::findGlobalSlot(const std::string& name) const {
    auto it = globalSlots_.find(name);
    return it == globalSlots_.end() ? -1 : it->second;
}

int CodeGenerator::ensureLocalSlot(const std::string& name) {
    if (!curFunc_) {
        cgError("Local variable outside function: " + name);
        return -1;
    }
    return curFunc_->ensureSlot(name);
}

int CodeGenerator::findLocalSlot(const std::string& name) {
    if (!curFunc_) {
        cgError("Local variable reference outside function: " + name);
        return -1;
    }
    return curFunc_->findSlot(name);
}

int CodeGenerator::makeLabel() {
    return ++labelCounter_;
}

int CodeGenerator::emitJump(OpCode jmpOp, int label) {
    if (jmpOp != OpCode::JMP && jmpOp != OpCode::JMP_IF_FALSE) {
        cgError("Invalid jump opcode", currentLine_);
    }

    unresolvedJumps_.emplace_back(static_cast<int>(code_.size()), label);

    Instruction inst;
    inst.op = jmpOp;
    inst.line = currentLine_;
    inst.data.jump = { label };

    code_.emplace_back(inst);

    return static_cast<int>(code_.size() - 1);
}

void CodeGenerator::emitLabel(int label) {
    if (labelPos_.count(label)) {
        cgError("Duplicate label: " + std::to_string(label), currentLine_);
    }
    labelPos_[label] = static_cast<int>(code_.size());
}

void CodeGenerator::patchAllJumps() {
    for (auto& [jmpPos, label] : unresolvedJumps_) {
        if (labelPos_.count(label)) {
            code_[jmpPos].data.jump.target = labelPos_[label];
        }
        else {
            cgError("Undefined label: " + std::to_string(label));
        }
    }
    unresolvedJumps_.clear();
}

int CodeGenerator::addStringConst(const std::string& s) {
    for (int i = 0; i < static_cast<int>(stringPool_.size()); ++i) {
        if (stringPool_[i] == s) return i;
    }
    stringPool_.push_back(s);
    return static_cast<int>(stringPool_.size() - 1);
}

int CodeGenerator::addIntConst(int v) {
    for (int i = 0; i < static_cast<int>(intPool_.size()); ++i) {
        if (intPool_[i] == v) return i;
    }
    intPool_.push_back(v);
    return static_cast<int>(intPool_.size() - 1);
}

int CodeGenerator::emit(OpCode op, ValueType type, int a, int b) {
    Instruction inst;
    inst.op = op;
    inst.line = currentLine_;
    switch (op)
    {
    case OpCode::NOP:
    case OpCode::POP:
    case OpCode::RET:
    case OpCode::PRINT:
        inst.data.typed = { type, 0, 0 };
        break;
    case OpCode::PUSH_INT:
        inst.data.typed = { ValueType::INT, a, 0 };
        break;
    case OpCode::PUSH_STR:
        inst.data.typed = { ValueType::STRING, a, 0 };
        break;
    case OpCode::LOAD_LOCAL:
    case OpCode::STORE_LOCAL:
    case OpCode::LOAD_GLOBAL:
    case OpCode::STORE_GLOBAL:
        inst.data.typed = { type, a, 0 };
        break;
    case OpCode::ADD:
    case OpCode::SUB:
    case OpCode::MUL:
    case OpCode::DIV:
    case OpCode::EQ:
    case OpCode::NEQ:
    case OpCode::LT:
    case OpCode::LTE:
    case OpCode::GT:
    case OpCode::GTE:
        inst.data.typed = { type, 0, 0 };
        break;
    case OpCode::NEG:
    case OpCode::NOT:
        inst.data.typed = { type, 0, 0 };
        break;
    case OpCode::JMP:
    case OpCode::JMP_IF_FALSE:
        inst.data.jump = { a };
        break;
    case OpCode::CALL:
        inst.data.call = { a, b };
        break;
    default:
        cgError("Unknown opcode", inst.line);
    }
    code_.emplace_back(inst);
    return static_cast<int>(code_.size() - 1);
}

void CodeGenerator::beginFunction(const std::string& name, int funcIndex) {
    funcContexts_.emplace_back();
    curFunc_ = &funcContexts_.back();
    curFunc_->name = name;
    curFunc_->index = funcIndex;
    curFunc_->entryLabel = makeLabel();

    if (funcIndex >= 0 && funcIndex < static_cast<int>(functionEntryPos_.size())) {
        functionEntryPos_[funcIndex] = curFunc_->entryLabel;
    }
    else {
        functionEntryPos_.push_back(curFunc_->entryLabel);
    }

    emitLabel(curFunc_->entryLabel);
}

void CodeGenerator::endFunction() {
    if (funcContexts_.empty()) {
        cgError("endFunction called without active function");
        return;
    }

    if (code_.empty() || code_.back().op != OpCode::RET) {
        emit(OpCode::RET);
    }

    funcContexts_.pop_back();
    curFunc_ = funcContexts_.empty() ? nullptr : &funcContexts_.back();
}

std::string CodeGenerator::escapeString(const std::string& s) const {
    std::string result;
    for (char c : s) {
        switch (c) {
        case '\n': result += "\\n"; break;
        case '\t': result += "\\t"; break;
        case '\"': result += "\\\""; break;
        case '\\': result += "\\\\"; break;
        default: result += c; break;
        }
    }
    return result;
}

const char* CodeGenerator::opCodeToString(OpCode op) const {
    switch (op) {
    case OpCode::NOP: return "NOP";
    case OpCode::PUSH_INT: return "PUSH_INT";
    case OpCode::PUSH_STR: return "PUSH_STR";
    case OpCode::LOAD_LOCAL: return "LOAD_LOCAL";
    case OpCode::STORE_LOCAL: return "STORE_LOCAL";
    case OpCode::LOAD_GLOBAL: return "LOAD_GLOBAL";
    case OpCode::STORE_GLOBAL: return "STORE_GLOBAL";
    case OpCode::ADD: return "ADD";
    case OpCode::SUB: return "SUB";
    case OpCode::MUL: return "MUL";
    case OpCode::DIV: return "DIV";
    case OpCode::EQ: return "EQ";
    case OpCode::NEQ: return "NEQ";
    case OpCode::LT: return "LT";
    case OpCode::LTE: return "LTE";
    case OpCode::GT: return "GT";
    case OpCode::GTE: return "GTE";
    case OpCode::NEG: return "NEG";
    case OpCode::NOT: return "NOT";
    case OpCode::JMP: return "JMP";
    case OpCode::JMP_IF_FALSE: return "JMP_IF_FALSE";
    case OpCode::CALL: return "CALL";
    case OpCode::RET: return "RET";
    case OpCode::POP: return "POP";
    case OpCode::PRINT: return "PRINT";
    default: {
        static char unknown[20];
        std::snprintf(unknown, sizeof(unknown), "UNKNOWN(%d)", static_cast<int>(op));
        return unknown;
    }
    }
}

ValueType CodeGenerator::mapType(const Type& t) const {
    if (t.kind == Type::Kind::Int) return ValueType::INT;
    if (t.kind == Type::Kind::String) return ValueType::STRING;
    if (t.kind == Type::Kind::Bool) return ValueType::BOOL;
    if (t.kind == Type::Kind::Named) {
        if (t.name == "int") return ValueType::INT;
        if (t.name == "string") return ValueType::STRING;
        if (t.name == "bool") return ValueType::BOOL;
    }
    cgError("Unsupported type: " + t.name);
    return ValueType::INT;
}

void CodeGenerator::cgError(const std::string& msg, int line) const {
    if (line != -1) std::cerr << "Error (Line " << line << "): " << msg << std::endl;
    else std::cerr << "Error: " << msg << std::endl;
}

void CodeGenerator::generate(Program& program) {
    currentLine_ = 1;

    functionTable_.clear();
    for (const auto& func : program.functions) {
        if (!func) continue;
        if (!func->name.value.has_value()) {
            cgError("Function with no name", func->name.line);
            continue;
        }
        std::string fname = func->name.value.value();
        if (functionIndex_.find(fname) == functionIndex_.end()) {
            functionIndex_[fname] = static_cast<int>(functionTable_.size());
            functionTable_.push_back(fname);
        }
    }

    functionEntryPos_.assign(functionTable_.size(), -1);

    for (const auto& func : program.functions) {
        if (!func) continue;
        currentLine_ = func->name.line;
        func->accept(*this);
    }

    if (!program.statements.empty()) {

        if (functionIndex_.find("main") == functionIndex_.end()) {
            int idx = static_cast<int>(functionTable_.size());
            functionIndex_["main"] = idx;
            functionTable_.push_back("main");
            functionEntryPos_.push_back(-1);
        }
        else {
            cgError("Global statements cannot coexist with main function", 1);
        }

        int mainIdx = functionIndex_.at("main");
        beginFunction("main", mainIdx);

        for (const auto& stmt : program.statements) {
            if (!stmt) continue;
            currentLine_ = stmt->line;
            stmt->accept(*this);
        }

        if (code_.empty() || code_.back().op != OpCode::RET) {
            emit(OpCode::PUSH_INT, ValueType::INT, addIntConst(0));
            emit(OpCode::RET);
        }

        endFunction();
    }

    patchAllJumps();

    functionEntryInstr_.assign(functionEntryPos_.size(), -1);
    for (size_t i = 0; i < functionEntryPos_.size(); ++i) {
        int lbl = functionEntryPos_[i];
        if (lbl != -1 && labelPos_.count(lbl)) {
            functionEntryInstr_[i] = labelPos_.at(lbl);
        }
    }

    if (program.statements.empty() && functionIndex_.find("main") == functionIndex_.end()) {
        cgError("Program must have a main function or global statements", 1);
    }
}

void CodeGenerator::emitToStream(std::ostream& os) const {
    os << "\nInstructions: " << code_.size() << ", Functions: " << functionTable_.size() << "\n\n";

    if (!stringPool_.empty()) {
        os << "[String Constants]\n";
        for (size_t i = 0; i < stringPool_.size(); ++i) {
            os << "  " << i << ": \"" << escapeString(stringPool_[i]) << "\"\n";
        }
        os << "\n";
    }

    if (!intPool_.empty()) {
        os << "[Integer Constants]\n";
        for (size_t i = 0; i < intPool_.size(); ++i) {
            os << "  " << i << ": " << intPool_[i] << "\n";
        }
        os << "\n";
    }

    os << "[Functions]\n";
    for (size_t i = 0; i < functionTable_.size(); ++i) {
        os << "  " << i << ": " << functionTable_[i];
        if (i < functionEntryInstr_.size() && functionEntryInstr_[i] != -1) {
            os << " (entry " << functionEntryInstr_[i] << ")";
        }
        os << "\n";
    }
    os << "\n";
    os << "\n[Statistics]\n";
    os << "Labels: " << labelPos_.size() << "\n";
    os << "String constants: " << stringPool_.size() << "\n";
    os << "Integer constants: " << intPool_.size() << "\n";
    os << "\n";

    os << "[Instructions]\n\n";

    os << GREEN << "global _start:\nstart:\n" << RESET;
    for (size_t i = 0; i < code_.size(); ++i) {
        const auto& inst = code_[i];
        os << GREEN;
        os << std::setw(4) << "  "
            << std::left << std::setw(15) << opCodeToString(inst.op);

        switch (inst.op) {
        case OpCode::PUSH_INT:   os << "INT[" << inst.data.typed.a << "]"; break;
        case OpCode::PUSH_STR:   os << "STR[" << inst.data.typed.a << "]"; break;
        case OpCode::LOAD_LOCAL:
        case OpCode::STORE_LOCAL: os << "LOCAL[" << inst.data.typed.a << "]"; break;
        case OpCode::LOAD_GLOBAL:
        case OpCode::STORE_GLOBAL: os << "GLOBAL[" << inst.data.typed.a << "]"; break;
        case OpCode::JMP:
        case OpCode::JMP_IF_FALSE: os << "-> " << inst.data.jump.target; break;
        case OpCode::CALL:
            os << "FN[" << inst.data.call.funcIndex << "](" << inst.data.call.argCount << ")";
            if (inst.data.call.funcIndex < static_cast<int>(functionTable_.size())) {
                os << " " << functionTable_[inst.data.call.funcIndex];
            }
            break;
        default: break;
        }
        os << RESET;
        os << "\n";
    }
    os << GREEN << " syscall" << RESET;
}

void CodeGenerator::visit(IntLiteral& expr) {
    currentLine_ = expr.line;
    int idx = addIntConst(expr.value);
    emit(OpCode::PUSH_INT, ValueType::INT, idx);
}

void CodeGenerator::visit(StringLiteral& expr) {
    currentLine_ = expr.line;
    int idx = addStringConst(expr.value);
    emit(OpCode::PUSH_STR, ValueType::STRING, idx);
}

void CodeGenerator::visit(BoolLiteral& expr) {
    currentLine_ = expr.line;
    int idx = addIntConst(expr.value ? 1 : 0);
    emit(OpCode::PUSH_INT, ValueType::BOOL, idx);
}

void CodeGenerator::visit(Identifier& expr) {
    currentLine_ = expr.line;
    if (!expr.token.value.has_value()) { cgError("Invalid identifier", expr.line); return; }
    std::string name = expr.token.value.value();

    std::optional<Symbol> symOpt = std::nullopt;
    if (scopes_) symOpt = scopes_->resolve(name);

    if (curFunc_) {
        int slot = curFunc_->findSlot(name);
        if (slot >= 0) {
            Type t;
            if (sema_) {
                auto tOpt = sema_->getExprType(expr);
                if (tOpt.has_value()) t = *tOpt;
                else if (symOpt.has_value()) t = symOpt->type;
                else t = sema_->makeBuiltinType(Type::Kind::Int); 
            }
            else if (symOpt.has_value()) {
                t = symOpt->type;
            }
            else {
                t = Type{ Type::Kind::Int, "int", expr.line };
            }
            emit(OpCode::LOAD_LOCAL, mapType(t), slot);
            return;
        }
    }
    if (symOpt.has_value()) {
        if (symOpt->isFunction) {
            cgError("Function '" + name + "' used without call", expr.line);
            return;
        }
        int gslot = findGlobalSlot(name);
        if (gslot == -1) gslot = ensureGlobalSlot(name);
        emit(OpCode::LOAD_GLOBAL, mapType(symOpt->type), gslot);
        return;
    }

    if (sema_) {
        auto tOpt = sema_->getExprType(expr);
        if (tOpt.has_value()) {
            int gslot = findGlobalSlot(name);
            if (gslot == -1) gslot = ensureGlobalSlot(name);
            emit(OpCode::LOAD_GLOBAL, mapType(*tOpt), gslot);
            return;
        }
    }

    cgError("Undefined variable: '" + name + "'", expr.line);
}

void CodeGenerator::visit(BinaryExpr& expr) {
    currentLine_ = expr.line;

    if (expr.left) expr.left->accept(*this);
    if (expr.right) expr.right->accept(*this);

    OpCode op;
    switch (expr.op.type) {
    case TokenType::plus:    op = OpCode::ADD; break;
    case TokenType::minus:   op = OpCode::SUB; break;
    case TokenType::mul:     op = OpCode::MUL; break;
    case TokenType::div:     op = OpCode::DIV; break;
    case TokenType::eq:      op = OpCode::EQ; break;
    case TokenType::neq:     op = OpCode::NEQ; break;
    case TokenType::lt:      op = OpCode::LT; break;
    case TokenType::lte:     op = OpCode::LTE; break;
    case TokenType::gt:      op = OpCode::GT; break;
    case TokenType::gte:     op = OpCode::GTE; break;
    default:
        cgError("Unknown binary operator", expr.line);
        return;
    }

    emit(op);
}

void CodeGenerator::visit(UnaryExpr& expr) {
    currentLine_ = expr.line;

    if (expr.operand) expr.operand->accept(*this);

    OpCode op;
    switch (expr.op.type) {
    case TokenType::minus:
        op = OpCode::NEG;
        break;
    case TokenType::bang:
        op = OpCode::NOT;
        break;
    default:
        cgError("Unknown unary operator", expr.line);
        return;
    }

    emit(op);
}

void CodeGenerator::visit(CallExpr& expr) {
    currentLine_ = expr.line;
    for (auto& a : expr.args) {
        if (!a) continue;
        a->accept(*this);
    }

    auto ident = dynamic_cast<Identifier*>(expr.callee.get());
    if (!ident || !ident->token.value.has_value()) {
        cgError("Unsupported call target", expr.line);
        return;
    }

    std::string fname = ident->token.value.value();
    auto it = functionIndex_.find(fname);
    if (it == functionIndex_.end()) {
        cgError("Call to unknown function: " + fname, expr.line);
        return;
    }
    int funcIdx = it->second;
    int argCount = static_cast<int>(expr.args.size());

    emit(OpCode::CALL, ValueType::INT, funcIdx, argCount);
}

void CodeGenerator::visit(GroupedExpr& expr) {
    currentLine_ = expr.line;
    if (expr.expr) expr.expr->accept(*this);
}

void CodeGenerator::visit(NullExpr& expr) {
    currentLine_ = expr.line;
    int idx = addIntConst(0);
    emit(OpCode::PUSH_INT, ValueType::INT, idx);
}

void CodeGenerator::visit(VariableDecl& stmt) {
    currentLine_ = stmt.line;
    if (!stmt.token.value.has_value()) { cgError("Unnamed variable", stmt.line); return; }
    std::string name = stmt.token.value.value();

    if (stmt.initializer) {
        stmt.initializer->accept(*this);
    }
    else {
        int idx = addIntConst(0);
        emit(OpCode::PUSH_INT, ValueType::INT, idx);
    }

    if (curFunc_) {
        int slot = ensureLocalSlot(name);
        Type t;
        if (stmt.type) t = *stmt.type;
        else if (sema_) {
            if (stmt.initializer) {
                auto tOpt = sema_->getExprType(*stmt.initializer);
                if (tOpt.has_value()) t = *tOpt;
                else t = sema_->makeBuiltinType(Type::Kind::Int);
            }
            else t = sema_->makeBuiltinType(Type::Kind::Int);
        }
        else {
            t = Type{ Type::Kind::Int, "int", stmt.line };
        }
        emit(OpCode::STORE_LOCAL, mapType(t), slot);
    }
    else {
        int gslot = ensureGlobalSlot(name);
        Type t;
        if (stmt.type) t = *stmt.type;
        else if (sema_) {
            if (stmt.initializer) {
                auto tOpt = sema_->getExprType(*stmt.initializer);
                if (tOpt.has_value()) t = *tOpt;
                else t = sema_->makeBuiltinType(Type::Kind::Int);
            }
            else t = sema_->makeBuiltinType(Type::Kind::Int);
        }
        else {
            t = Type{ Type::Kind::Int, "int", stmt.line };
        }
        emit(OpCode::STORE_GLOBAL, mapType(t), gslot);
    }
}

void CodeGenerator::visit(Assignment& stmt) {
    currentLine_ = stmt.line;
    if (!stmt.target.value.has_value()) { cgError("Assignment target unnamed", stmt.line); return; }
    std::string name = stmt.target.value.value();

    if (!stmt.expr) { cgError("Assignment with no value", stmt.line); return; }
    stmt.expr->accept(*this);

    if (curFunc_) {
        int slot = curFunc_->findSlot(name);
        if (slot >= 0) {
            std::optional<Symbol> sym = (scopes_ ? scopes_->resolve(name) : std::nullopt);
            Type t = (sym.has_value() ? sym->type : (sema_ ? sema_->makeBuiltinType(Type::Kind::Int) : Type{ Type::Kind::Int, "int", stmt.line }));
            emit(OpCode::STORE_LOCAL, mapType(t), slot);
            return;
        }
    }

    int gslot = findGlobalSlot(name);
    if (gslot != -1) {
        std::optional<Symbol> sym = (scopes_ ? scopes_->resolve(name) : std::nullopt);
        Type t = (sym.has_value() ? sym->type : (sema_ ? sema_->makeBuiltinType(Type::Kind::Int) : Type{ Type::Kind::Int, "int", stmt.line }));
        emit(OpCode::STORE_GLOBAL, mapType(t), gslot);
        return;
    }

    cgError("Assignment to undeclared variable: " + name, stmt.line);
}

void CodeGenerator::visit(PrintStmt& stmt) {
    currentLine_ = stmt.line;
    if (!stmt.expr) { cgError("Print with no expression", stmt.line); return; }
    stmt.expr->accept(*this);

    ValueType vt = ValueType::INT;
    if (sema_) {
        auto tOpt = sema_->getExprType(*stmt.expr);
        if (tOpt.has_value()) {
            vt = mapType(*tOpt);
        }
    }
    emit(OpCode::PRINT, vt);
}

void CodeGenerator::visit(ReturnStmt& stmt) {
    currentLine_ = stmt.line;
    if (stmt.expr) {
        stmt.expr->accept(*this);
    }
    emit(OpCode::RET);
}

void CodeGenerator::visit(IfStmt& stmt) {
    currentLine_ = stmt.line;
    if (!stmt.condition) { cgError("If without condition", stmt.line); return; }
    stmt.condition->accept(*this);

    int elseLabel = makeLabel();
    int endLabel = makeLabel();

    emitJump(OpCode::JMP_IF_FALSE, elseLabel);

    if (stmt.thenBlock) stmt.thenBlock->accept(*this);

    emitJump(OpCode::JMP, endLabel);

    emitLabel(elseLabel);
    if (stmt.elseBlock && stmt.elseBlock.has_value()) {
        stmt.elseBlock.value()->accept(*this);
    }
    emitLabel(endLabel);
}

void CodeGenerator::visit(WhileStmt& stmt) {
    currentLine_ = stmt.line;
    int start = makeLabel();
    int end = makeLabel();

    emitLabel(start);
    if (stmt.condition) stmt.condition->accept(*this);
    emitJump(OpCode::JMP_IF_FALSE, end);

    if (stmt.body) stmt.body->accept(*this);
    emitJump(OpCode::JMP, start);

    emitLabel(end);
}

void CodeGenerator::visit(ExprStmt& stmt) {
    currentLine_ = stmt.line;
    if (stmt.expr) {
        stmt.expr->accept(*this);
        emit(OpCode::POP);
    }
}

void CodeGenerator::visit(Block& stmt) {
    currentLine_ = stmt.line;
    if (scopes_) scopes_->beginScope();
    for (auto& s : stmt.statements) {
        if (s) s->accept(*this);
    }
    if (scopes_) scopes_->endScope();
}

void CodeGenerator::visit(FunctionDecl& stmt) {
    currentLine_ = stmt.line;
    if (!stmt.name.value.has_value()) { cgError("Unnamed function", stmt.line); return; }
    std::string fname = stmt.name.value.value();

    auto it = functionIndex_.find(fname);
    if (it == functionIndex_.end()) {
        cgError("Function not registered: " + fname, stmt.line);
        return;
    }
    int fidx = it->second;
    beginFunction(fname, fidx);

    for (const auto& p : stmt.parameters) {
        curFunc_->ensureSlot(p.name);
    }

    for (int i = static_cast<int>(stmt.parameters.size()) - 1; i >= 0; --i) {
        const auto& p = stmt.parameters[i];
        int slot = curFunc_->findSlot(p.name);
        if (slot < 0) {
            cgError("Missing slot for parameter " + p.name, stmt.line);
            continue;
        }
        emit(OpCode::STORE_LOCAL, mapType(p.type), slot);
    }

    if (stmt.body) stmt.body->accept(*this);

    endFunction();
}
