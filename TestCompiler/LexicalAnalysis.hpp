#pragma once

#include "TokenPool .hpp"
#include "token.hpp"
#include <string>
#include <vector>
#include <optional>
#include <iostream>
#include <cassert>
#include <cctype>
#include <unordered_map>
#include <memory>
#include <cstdlib>
#include <regex>

class Tokenizer {
public:
    explicit Tokenizer(std::string source)
        : src_(std::move(source)), src_view_(src_), len_(src_.size()) {}


    std::vector<Token*> tokenize() {
        pool_.reset();
        tokens_.clear();
        line_ = 1;
        i_ = 0;

        while (i_ < len_) {
            char c = peek();

            if (std::isspace(static_cast<unsigned char>(c))) {
                if (c == '\n') ++line_;
                advance();
                continue;
            }

            if (c == '/') {
                if (match("//")) { skipLineComment(); continue; }
                if (match("/*")) { skipBlockComment(); continue; }
            }

            if (isIdentStart(c)) { readIdentifierOrKeyword(); continue; }

            if (std::isdigit(static_cast<unsigned char>(c))) { readNumber(); continue; }

            if (c == '"') { readString(); continue; }

            if (tryOp()) continue;

            error("Unexpected character");
        }

        makeToken(TokenType::END_OF_FILE);
        return tokens_;
    }

private:
    const std::string src_;
    const std::string_view src_view_;
    size_t len_, i_{};
    int line_{ 1 };
    TokenPool pool_;
    std::vector<Token*> tokens_;

    [[noreturn]] void error(const char* msg) const {
        std::cerr << "[Lexer Error] Line " << line_ << ": " << msg << "\n";
        std::exit(EXIT_FAILURE);
    }

    char peek(size_t off = 0) const {
        return (i_ + off < len_) ? src_[i_ + off] : '\0';
    }

    void advance(size_t n = 1) { i_ += n; }

    bool match(const char* s) {
        size_t L = std::strlen(s);
        if (i_ + L > len_) return false;
        if (src_.compare(i_, L, s) == 0) {
            advance(L);
            return true;
        }
        return false;
    }

    void skipLineComment() {
        while (i_ < len_ && peek() != '\n') advance();
    }

    void skipBlockComment() {
        int nested = 1;
        while (i_ < len_ && nested > 0) {
            if (peek() == '\n') ++line_;
            if (match("/*")) { ++nested; continue; }
            if (match("*/")) { --nested; continue; }
            advance();
        }
        if (nested != 0) error("Unterminated block comment");
    }

    static bool isIdentStart(char c) {
        return std::isalpha(static_cast<unsigned char>(c)) || c == '_';
    }

    static bool isIdentChar(char c) {
        return std::isalnum(static_cast<unsigned char>(c)) || c == '_';
    }

    Token* makeToken(TokenType type, std::optional<std::string_view> val = std::nullopt) {
        Token* t = pool_.allocate();
        t->type = type;
        t->line = line_;
        if (val) {
            t->value = std::string(*val);
        }
        else {
            t->value = std::nullopt;
        }
        tokens_.push_back(t);
        return t;
    }

    char parseEscape() {
        if (i_ >= len_) error("Unterminated escape sequence in string");
        char esc = peek();
        advance();
        switch (esc) {
        case 'n': return '\n';
        case 't': return '\t';
        case '\\': return '\\';
        case '"': return '"';
        default: error("Unknown escape sequence in string");
        }
        return '?';
    }

    void readIdentifierOrKeyword() {
        size_t start = i_;
        while (isIdentChar(peek())) advance();
        auto sv = src_view_.substr(start, i_ - start);
        auto it = kKeywordMap.find(sv);
        if (it != kKeywordMap.end()) {
            makeToken(it->second);
        }
        else {
            makeToken(TokenType::identifier, sv);
        }
    }

    void readNumber() {
        size_t start = i_;
        while (std::isdigit(static_cast<unsigned char>(peek()))) advance();
        auto sv = src_view_.substr(start, i_ - start);
        makeToken(TokenType::int_lit, sv);
    }

    void readString() {
        advance();
        std::string buffer;
        buffer.reserve(64);
        while (i_ < len_) {
            char c = peek();
            if (c == '"') {
                advance();
                makeToken(TokenType::string_lit, buffer);
                return;
            }
            if (c == '\\') {
                advance();
                buffer.push_back(parseEscape());
            }
            else {
                if (c == '\n') ++line_;
                buffer.push_back(c);
                advance();
            }
        }
        error("Unterminated string literal");
    }

    bool tryOp() {
        char c1 = peek();
        char c2 = peek(1);
        switch (c1) {
        case '=': if (c2 == '=') { advance(2); return makeToken(TokenType::eq), true; } break;
        case '!': if (c2 == '=') { advance(2); return makeToken(TokenType::neq), true; } break;
        case '<': if (c2 == '=') { advance(2); return makeToken(TokenType::lte), true; } break;
        case '>': if (c2 == '=') { advance(2); return makeToken(TokenType::gte), true; } break;
        case '-': if (c2 == '>') { advance(2); return makeToken(TokenType::arrow), true; } break;
        }

        auto it = kOneCharOps.find(c1);
        if (it != kOneCharOps.end()) {
            makeToken(it->second);
            advance();
            return true;
        }
        return false;
    }

    static const std::unordered_map<std::string_view, TokenType> kKeywordMap;
    static const std::unordered_map<char, TokenType> kOneCharOps;
};

const std::unordered_map<std::string_view, TokenType> Tokenizer::kKeywordMap = {
    {"return", TokenType::kw_return}, {"let", TokenType::kw_let},
    {"print", TokenType::kw_print}, {"fn", TokenType::kw_fn},
    {"if", TokenType::kw_if}, {"else", TokenType::kw_else},
    {"while", TokenType::kw_while}, {"true", TokenType::kw_true},
    {"false", TokenType::kw_false}, {"int", TokenType::kw_int},
    {"bool", TokenType::kw_bool}, {"string", TokenType::kw_string},
};

const std::unordered_map<char, TokenType> Tokenizer::kOneCharOps = {
    {'=', TokenType::assign},
    {'<', TokenType::lt}, {'>', TokenType::gt},
    {'+', TokenType::plus}, {'-', TokenType::minus},
    {'*', TokenType::mul}, {'/', TokenType::div},
    {';', TokenType::semi},
    {'(', TokenType::lparen}, {')', TokenType::rparen},
    {'{', TokenType::lbrace}, {'}', TokenType::rbrace},
    {',', TokenType::comma},
    {':', TokenType::colon}, {'!', TokenType::bang},
};

//#pragma once
//
//#include "TokenPool .hpp"
//#include "token.hpp"
//#include <string>
//#include <vector>
//#include <optional>
//#include <iostream>
//#include <regex>
//#include <unordered_map>
//#include <cstdlib>
//#include <algorithm>
//
//class Tokenizer {
//public:
//    explicit Tokenizer(std::string source)
//        : src_(std::move(source)), src_view_(src_), len_(src_.size()) {}
//
//    std::vector<Token*> tokenize() {
//        pool_.reset();
//        tokens_.clear();
//        line_ = 1;
//        i_ = 0;
//
//        initPatterns();
//
//        while (i_ < len_) {
//            if (peek() == '/' && peek(1) == '*') {
//                skipBlockComment();
//                continue;
//            }
//
//            std::string str = std::string(src_view_.substr(i_));
//            bool matched = false;
//
//            for (size_t pi = 0; pi < patterns_.size(); ++pi) {
//                std::smatch m;
//                if (std::regex_search(str, m, regs_[pi], std::regex_constants::match_continuous)) {
//                    matched = true;
//                    std::string lex = m.str(0);
//
//                    switch (patterns_[pi].second) {
//                    case SkipSpace: handleWhitespace(lex); break;
//                    case SkipLineComment: skipLineComment(lex); break;
//                    case String: readString(lex); break;
//                    case Int: readNumber(lex); break;
//                    case Identifier: readIdentifierOrKeyword(lex); break;
//                    case TwoCharOps:
//                    case OneCharOps:
//                        if (!tryOp(lex)) error("Unknown operator");
//                        break;
//                    }
//                    break;
//                }
//            }
//
//            if (!matched) {
//                error("Unexpected character");
//            }
//        }
//
//        makeToken(TokenType::END_OF_FILE);
//        return tokens_;
//    }
//
//private:
//    enum PatternType { SkipSpace, SkipLineComment, String, Int, Identifier, TwoCharOps, OneCharOps };
//
//    const std::string src_;
//    const std::string_view src_view_;
//    size_t len_, i_{};
//    int line_{ 1 };
//    TokenPool pool_;
//    std::vector<Token*> tokens_;
//    std::vector<std::pair<std::string, PatternType>> patterns_;
//    std::vector<std::regex> regs_;
//
//    static const std::unordered_map<std::string_view, TokenType> kKeywordMap;
//    static const std::unordered_map<char, TokenType> kOneCharOps;
//  
//    void initPatterns() {
//        patterns_ = {
//            {R"(^[ \t\r\n]+)", SkipSpace},
//            {R"(^//[^\n]*)", SkipLineComment},
//            {R"(^"([^"\\]|\\.)*")", String},
//            {R"(^\d+)", Int},
//            {R"(^[A-Za-z_][A-Za-z0-9_]*)", Identifier},
//            {R"(^(==|!=|<=|>=|->))", TwoCharOps},
//            {R"(^[=<>+\-*/;(){}\[:,\]!])", OneCharOps}
//        };
//        regs_.clear();
//        for (auto& p : patterns_) regs_.emplace_back(p.first);
//    }
//
//    void advance(size_t n = 1) { i_ += n; }
//
//    void handleWhitespace(const std::string& lex) {
//        line_ += std::count(lex.begin(), lex.end(), '\n');
//        advance(lex.size());
//    }
//
//    void skipLineComment(const std::string& lex) {
//        line_ += std::count(lex.begin(), lex.end(), '\n');
//        advance(lex.size());
//    }
//
//    char parseEscape(char esc) {
//        switch (esc) {
//        case 'n': return '\n';
//        case 't': return '\t';
//        case '\\': return '\\';
//        case '"': return '"';
//        default: error("Unknown escape sequence in string");
//        }
//        return '?';
//    }
//
//    void readString(const std::string& lex) {
//        if (lex.size() < 2 || lex.front() != '"' || lex.back() != '"')
//            error("Unterminated string literal");
//
//        std::string inner = lex.substr(1, lex.size() - 2);
//        std::string unescaped;
//        unescaped.reserve(inner.size());
//
//        for (size_t p = 0; p < inner.size(); ++p) {
//            char ch = inner[p];
//            if (ch == '\\') {
//                if (p + 1 >= inner.size()) error("Unterminated escape sequence in string");
//                char esc = inner[++p];
//                unescaped.push_back(parseEscape(esc));
//            }
//            else {
//                unescaped.push_back(ch);
//            }
//        }
//        makeToken(TokenType::string_lit, unescaped);
//        line_ += std::count(lex.begin(), lex.end(), '\n');
//        advance(lex.size());
//    }
//
//    void readNumber(const std::string& lex) {
//        makeToken(TokenType::int_lit, lex);
//        advance(lex.size());
//    }
//
//    void readIdentifierOrKeyword(const std::string& lex) {
//        auto it = kKeywordMap.find(std::string_view(lex));
//        if (it != kKeywordMap.end()) {
//            makeToken(it->second);
//        }
//        else {
//            makeToken(TokenType::identifier, lex);
//        }
//        advance(lex.size());
//    }
//    
//    bool tryOp(const std::string& lex) {
//        if (lex == "==") makeToken(TokenType::eq);
//        else if (lex == "!=") makeToken(TokenType::neq);
//        else if (lex == "<=") makeToken(TokenType::lte);
//        else if (lex == ">=") makeToken(TokenType::gte);
//        else if (lex == "->") makeToken(TokenType::arrow);
//        else {
//            auto it = kOneCharOps.find(lex[0]);
//            if (it != kOneCharOps.end()) {
//                makeToken(it->second);
//            }
//            else {
//                return false;
//            }
//        }
//        advance(lex.size());
//        return true;
//    }
//
//    [[noreturn]] void error(const char* msg) const {
//        std::cerr << "[Lexer Error] Line " << line_ << ": " << msg << "\n";
//        std::exit(EXIT_FAILURE);
//    }
//
//    char peek(size_t off = 0) const {
//        return (i_ + off < len_) ? src_[i_ + off] : '\0';
//    }
//   
//    void skipBlockComment() {
//        advance(2);
//        int nested = 1;
//        while (i_ < len_ && nested > 0) {
//            if (peek() == '\n') ++line_;
//            if (peek() == '/' && peek(1) == '*') {
//                nested++; advance(2); continue;
//            }
//            if (peek() == '*' && peek(1) == '/') {
//                nested--;  advance(2); continue;
//            }
//            advance();
//        }
//        if (nested != 0) error("Unterminated block comment");
//    }
//
//    Token* makeToken(TokenType type, std::optional<std::string_view> val = std::nullopt) {
//        Token* t = pool_.allocate();
//        t->type = type;
//        t->line = line_;
//        if (val) t->value = std::string(*val);
//        else t->value = std::nullopt;
//        tokens_.push_back(t);
//        return t;
//    }
//
//    Token* makeToken(TokenType type, const std::string& val) {
//        Token* t = pool_.allocate();
//        t->type = type;
//        t->line = line_;
//        t->value = val;
//        tokens_.push_back(t);
//        return t;
//    }
//};
//
//const std::unordered_map<std::string_view, TokenType> Tokenizer::kKeywordMap = {
//    {"return", TokenType::kw_return}, {"let", TokenType::kw_let},
//    {"print", TokenType::kw_print}, {"fn", TokenType::kw_fn},
//    {"if", TokenType::kw_if}, {"else", TokenType::kw_else},
//    {"while", TokenType::kw_while}, {"true", TokenType::kw_true},
//    {"false", TokenType::kw_false}, {"int", TokenType::kw_int},
//    {"bool", TokenType::kw_bool}, {"string", TokenType::kw_string},
//};
//
//const std::unordered_map<char, TokenType> Tokenizer::kOneCharOps = {
//    {'=', TokenType::assign},
//    {'<', TokenType::lt}, {'>', TokenType::gt},
//    {'+', TokenType::plus}, {'-', TokenType::minus},
//    {'*', TokenType::mul}, {'/', TokenType::div},
//    {';', TokenType::semi},
//    {'(', TokenType::lparen}, {')', TokenType::rparen},
//    {'{', TokenType::lbrace}, {'}', TokenType::rbrace},
//    {',', TokenType::comma},
//    {':', TokenType::colon}, {'!', TokenType::bang},
//};
