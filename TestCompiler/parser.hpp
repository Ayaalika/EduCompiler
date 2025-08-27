#pragma once

#include "token.hpp"
#include "AST.hpp"
#include <vector>
#include <memory>
#include <optional>

enum class Precedence {
    NONE,
    ASSIGNMENT,
    OR,       
    AND,    
    EQUALITY,  
    COMPARISON,
    TERM,      
    FACTOR,   
    UNARY, 
    CALL,   
    PRIMARY
};

Precedence getPrecedence(TokenType type) {
    switch (type) {
    case TokenType::eq:
    case TokenType::neq: return Precedence::EQUALITY;
    case TokenType::lt:
    case TokenType::lte:
    case TokenType::gt:
    case TokenType::gte: return Precedence::COMPARISON;
    case TokenType::plus:
    case TokenType::minus: return Precedence::TERM;
    case TokenType::mul:
    case TokenType::div: return Precedence::FACTOR;
    case TokenType::lparen: return Precedence::CALL;
    default: return Precedence::NONE;
    }
}

class Parser {
public:
    explicit Parser(std::vector<Token*> tokens) : tokens_(std::move(tokens)), current_(0) {}

    std::unique_ptr<Program> parse();
private:
    std::vector<Token*> tokens_;
    size_t current_ ;
    bool panicMode = false;

    const Token* advance();
    bool match(TokenType type);
    bool check(TokenType type) const;
    bool isAtEnd() const;
    [[nodiscard]] Token* peek(const int offset = 0)const;
    const Token* previous() const;
    const Token* consume(TokenType type, const char* message);
    void error(const Token* token, const std::string& msg);
    const Token* peekNext() const;
    bool checkNext(TokenType type) const;
    void synchronize();

    template<typename T, typename... Args>
    std::unique_ptr<T> makeNode(int line, Args&&... args);

    std::unique_ptr<Stmt> declaration();
    std::unique_ptr<Stmt> variableDecl();
    std::unique_ptr<FunctionDecl> functionDecl(); 
    std::vector<Param> paramList();
    std::unique_ptr<Type> type();

    std::unique_ptr<Stmt> statement();
    std::unique_ptr<Stmt> ifStatement();
    std::unique_ptr<Stmt> whileStatement();
    std::unique_ptr<Stmt> returnStatement();
    std::unique_ptr<Stmt> printStatement();
    std::unique_ptr<Block> block();
    std::unique_ptr<Stmt> exprStatement();
    std::unique_ptr<Stmt> assignmentStmt();

    std::unique_ptr<Expr> expression();
    std::unique_ptr<Expr>parsePrecedence(Precedence precedence);
    std::unique_ptr<Expr> parsePrefix(const Token* token);
    std::unique_ptr<Expr> parseInfix(std::unique_ptr<Expr> left, const Token* token);
    std::vector<std::unique_ptr<Expr>> parseArguments();
    std::vector<std::unique_ptr<Expr>> argumentList();

};

[[nodiscard]] Token* Parser::peek(const int offset) const {
    return current_ + offset < tokens_.size() ? tokens_.at(current_ + offset) : nullptr;
}

const Token* Parser::previous() const {
    return tokens_[current_ - 1];
}

bool Parser::isAtEnd() const {
    return current_ >= tokens_.size() || peek()->type == TokenType::END_OF_FILE;
}

const Token* Parser::advance() {
    if (!isAtEnd()) current_++;
    return previous();
}

bool Parser::match(TokenType type) {
    if (check(type)) {
        advance();
        return true;
    }
    return false;
}

bool Parser::check(TokenType type) const {
    return !isAtEnd() && peek()->type == type;
}

const Token* Parser::consume(TokenType type, const char* message) {
    if (check(type)) {
        return advance();
    }

    error(peek(), message);
    synchronize();
    return nullptr;  
}

const Token* Parser::peekNext() const {
    if (current_ + 1 >= tokens_.size()) return tokens_.back();
    return tokens_[current_ + 1];
}

bool Parser::checkNext(TokenType type) const {
    return peekNext()->type == type;
}

void Parser::error(const Token* token, const std::string& msg) {
    if (panicMode) return;   
    panicMode = true;

    std::cerr << "[Parse Error] ";
    if (token) {
        std::cerr << "at line " << token->line << ", near '";

        if (token->value) {
            std::cerr << *token->value;
        }
        else {
            std::cerr << to_string(token->type);
        }
        std::cerr << "': ";
    }
    std::cerr << msg << "\n";
}

void Parser::synchronize() {
    advance(); 
    while (!isAtEnd()) {
        if (previous()->type == TokenType::semi) {
            panicMode = false; 
            return;
        }
        switch (peek()->type) {
        case TokenType::kw_fn:
        case TokenType::kw_let:
        case TokenType::kw_if:
        case TokenType::kw_while:
        case TokenType::kw_return:
        case TokenType::kw_print:
            panicMode = false; 
            return;
        default:
            break;
        }
        advance();
    }
    panicMode = false;
}

std::unique_ptr<Program> Parser::parse() {
    auto program = std::make_unique<Program>();

    while (!isAtEnd()) {
        if (match(TokenType::kw_fn)) {
            auto fnDecl = functionDecl();
            if (fnDecl) {
                program->functions.push_back(std::move(fnDecl));
            }
        }
        else {
            auto stmt = declaration();
            if (stmt) {
                program->statements.push_back(std::move(stmt));
            }
        }
    }

    return program;
}

std::unique_ptr<Stmt> Parser::declaration() {
    if (match(TokenType::kw_let)) {
        return variableDecl();
    }
    else {
        return statement();
    }
}

std::unique_ptr<Stmt> Parser::variableDecl() {
    const Token* nameToken = consume(TokenType::identifier, "Expect variable name");
    std::unique_ptr<Type> varType = nullptr;

    if (match(TokenType::colon)) {
        varType = type();
    }

    consume(TokenType::assign, "Expect '=' after variable declaration");
    auto initializerExpr = expression();
    consume(TokenType::semi, "Expect ';' after variable declaration");

    auto varDecl = makeNode<VariableDecl>(nameToken->line);
    varDecl->token = *nameToken;
    varDecl->type = std::move(varType);
    varDecl->initializer = std::move(initializerExpr);

    return varDecl;
}

std::unique_ptr<Type> Parser::type() {
    int line = peek()->line;
    auto typeNode = makeNode<Type>(line);

    if (match(TokenType::kw_int)) {
        typeNode->kind = Type::Kind::Int;
        typeNode->name = "int";
    }
    else if (match(TokenType::kw_bool)) {
        typeNode->kind = Type::Kind::Bool;
        typeNode->name = "bool";
    }
    else if (match(TokenType::kw_string)) {
        typeNode->kind = Type::Kind::String;
        typeNode->name = "string";
    }
    else {
        const Token* ident = consume(TokenType::identifier, "Expect type name after ':'");
        typeNode->kind = Type::Kind::Named;
        typeNode->name = ident->value.value();
    }

    return typeNode;
}

std::unique_ptr<FunctionDecl> Parser::functionDecl() {
    const Token* nameToken = consume(TokenType::identifier, "Expect function name after 'fn'");
    consume(TokenType::lparen, "Expect '(' after function name");

    std::vector<Param> params;
    if (!check(TokenType::rparen)) {
        params = paramList();
    }

    consume(TokenType::rparen, "Expect ')' after parameters");
    Type returnType;
    returnType.kind = Type::Kind::Void;
    returnType.name = "void";

    if (match(TokenType::arrow)) {  
        returnType = *type();
    }

    auto body = block();

    auto funDecl = makeNode<FunctionDecl>(nameToken->line);
    funDecl->name = *nameToken;
    funDecl->parameters = std::move(params);
    funDecl->body = std::move(body);
    funDecl->returnType = returnType;

    return funDecl;
}

std::vector<Param> Parser::paramList() {
    std::vector<Param> params;
    do {
        const Token* nameTok = consume(TokenType::identifier, "Expect parameter name");
        consume(TokenType::colon, "Expect ':' after parameter name");
        auto typeNode = type();   
        Param p;
        p.name = nameTok->value.value();
        p.type = *typeNode;
        params.push_back(std::move(p));
    } while (match(TokenType::comma));
    return params;
}

std::unique_ptr<Block> Parser::block() {
    consume(TokenType::lbrace, "Expect '{' to start block");
    std::vector<std::unique_ptr<Stmt>> statements;

    while (!check(TokenType::rbrace) && !isAtEnd()) {
        statements.push_back(declaration());
    }

    consume(TokenType::rbrace, "Expect '}' after block");

    int line = statements.empty() ? previous()->line : statements.front()->line;
    auto block = makeNode<Block>(line);
    block->statements = std::move(statements);

    return block;
}

std::unique_ptr<Stmt> Parser::statement() {
    if (match(TokenType::kw_if)) return ifStatement();
    if(match(TokenType::kw_while))  return whileStatement();
    if (match(TokenType::kw_return))  return returnStatement();
    if (match(TokenType::kw_print)) return printStatement();
    if (check(TokenType::lbrace))  return block();
    if (check(TokenType::identifier) && checkNext(TokenType::assign))return assignmentStmt();
    return exprStatement();
}

std::unique_ptr<Stmt> Parser::ifStatement() {
    consume(TokenType::lparen, "Expect '(' after 'if'");
    auto condition = expression();
    consume(TokenType::rparen, "Expect ')' after condition");
    auto thenBlock = block();

    std::optional<std::unique_ptr<Block>> elseBlock = std::nullopt;
    if (match(TokenType::kw_else)) {
        elseBlock = block();
    }

    auto Ifstmt = makeNode<IfStmt>(condition->line);
    Ifstmt->condition = std::move(condition);
    Ifstmt->thenBlock = std::move(thenBlock);
    Ifstmt->elseBlock = std::move(elseBlock);

    return Ifstmt;
}

std::unique_ptr<Stmt> Parser::whileStatement() {
    consume(TokenType::lparen, "Expect '(' after 'while'");
    auto condition = expression();
    consume(TokenType::rparen, "Expect ')' after condition");
    auto body = block();

    auto whileStmt = makeNode<WhileStmt>(condition->line);
    whileStmt->condition = std::move(condition);
    whileStmt->body = std::move(body);

    return whileStmt;
}

std::unique_ptr<Stmt> Parser::returnStatement() {
    std::unique_ptr<Expr> exprReturn = nullptr;

    if (!check(TokenType::semi)) {
        exprReturn = expression();
    }

    consume(TokenType::semi, "Expect ';' after return statement.");

    int line = exprReturn ? exprReturn->line : peek()->line;
    auto returnStmt = makeNode<ReturnStmt>(line);
    returnStmt->expr = std::move(exprReturn);

    return returnStmt;
}

std::unique_ptr<Stmt> Parser::printStatement() {
    auto value = expression();
    consume(TokenType::semi, "Expect ';' after print statement");

    auto printStmt = makeNode<PrintStmt>(value->line);
    printStmt->expr = std::move(value);

    return printStmt;
}

std::unique_ptr<Stmt> Parser::exprStatement() {
    auto expr = expression();
    consume(TokenType::semi, "Expect ';' after expression");

    auto exprStmt = makeNode<ExprStmt>(expr->line);
    exprStmt->expr = std::move(expr);

    return exprStmt;
}

std::unique_ptr<Stmt> Parser::assignmentStmt() {
    const Token* nameToken = consume(TokenType::identifier, "Expect variable name");
    consume(TokenType::assign, "Expect '=' after variable name");
    auto value = expression();
    consume(TokenType::semi, "Expect ';' after assignment");

    auto assignStmt = makeNode<Assignment>(nameToken->line);
    assignStmt->target = *nameToken;
    assignStmt->expr = std::move(value);

    return assignStmt;
}

std::unique_ptr<Expr>Parser::expression() {
    return parsePrecedence(Precedence::ASSIGNMENT);
}

std::unique_ptr<Expr> Parser::parsePrecedence(Precedence precedence) {
    const Token* token = advance();

    std::unique_ptr<Expr> left = parsePrefix(token);
    if (!left) return nullptr;

    while (!isAtEnd() && precedence <= getPrecedence(peek()->type)) {
        token = advance();
        left = parseInfix(std::move(left), token);
        if (!left) return nullptr;
    }

    return left;
}

std::unique_ptr<Expr> Parser::parsePrefix(const Token* token) {
    switch (token->type) {
    case TokenType::int_lit: {
        auto lit = makeNode<IntLiteral>(token->line);
        lit->value = std::stoi(token->value.value());
        return lit;
    }
    case TokenType::string_lit: {
        auto lit = makeNode<StringLiteral>(token->line);
        lit->value = token->value.value();
        return lit;
    }
    case TokenType::kw_true:
    case TokenType::kw_false: {
        auto lit = makeNode<BoolLiteral>(token->line);
        lit->value = (token->type == TokenType::kw_true);
        return lit;
    }
    case TokenType::identifier: {
        auto id = makeNode<Identifier>(token->line);
        id->token = *token;
        return id;
    }
    case TokenType::minus:
    case TokenType::bang: {
        auto expr = makeNode<UnaryExpr>(token->line);
        expr->op = *token;
        expr->operand = parsePrecedence(Precedence::UNARY);
        return expr;
    }
    case TokenType::lparen: {
        auto expr = parsePrecedence(Precedence::ASSIGNMENT);
        consume(TokenType::rparen, "Expect ')' after expression");
        auto group = makeNode<GroupedExpr>(expr->line);
        group->expr = std::move(expr);
        return group;
    }
    default:
        error(token, "Unexpected expression");
        return nullptr;
    }
}

std::unique_ptr<Expr> Parser::parseInfix(std::unique_ptr<Expr> left, const Token* token) {
    if (token->type == TokenType::lparen) {
        auto call = makeNode<CallExpr>(token->line);
        call->callee = std::move(left);
        call->args = std::move(parseArguments());
        return call;
    }

    auto precedence = getPrecedence(token->type);
    auto right = parsePrecedence(static_cast<Precedence>(static_cast<int>(precedence) + 1));

    auto expr = makeNode<BinaryExpr>(token->line);
    expr->op = *token;
    expr->left = std::move(left);
    expr->right = std::move(right);
    return expr;
}

std::vector<std::unique_ptr<Expr>> Parser::parseArguments() {
    std::vector<std::unique_ptr<Expr>> args;
    if (!check(TokenType::rparen)) {
        do {
            args.push_back(expression());
        } while (match(TokenType::comma));
    }
    consume(TokenType::rparen, "Expect ')' after arguments");
    return args;
}

std::vector<std::unique_ptr<Expr>>  Parser::argumentList() {
    std::vector<std::unique_ptr<Expr>> args;
    do {
        args.push_back(expression());
     } while (match(TokenType::comma));
    return args;
}

template<typename T, typename... Args>
std::unique_ptr<T>Parser::makeNode(int line, Args&&... args) {
    auto node = std::make_unique<T>(std::forward<Args>(args)...);
    node->line = line;
    return node;
}