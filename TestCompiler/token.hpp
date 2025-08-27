#pragma once

#include <string>
#include <vector>
#include <optional>
#include <iostream>
#include <cassert>
#include <cctype>

enum class TokenType {
    kw_return, kw_let, kw_print, kw_fn,
    kw_if, kw_else, kw_while,
    kw_true, kw_false,
    kw_int, kw_bool, kw_string,

    identifier, int_lit, string_lit,

    plus, minus, mul, div,
    assign, eq, neq,
    lt, lte, gt, gte,
    semi, lparen, rparen, lbrace, rbrace,

    END_OF_FILE, colon, comma, bang, arrow
};

struct Token {
    TokenType type;
    int line;
    std::optional<std::string> value{};
};

inline std::string to_string(const TokenType type) {
    switch (type) {
    case TokenType::kw_return: return "return";
    case TokenType::kw_let: return "let";
    case TokenType::kw_print: return "print";
    case TokenType::kw_fn: return "fn";
    case TokenType::kw_if: return "if";
    case TokenType::kw_else: return "else";
    case TokenType::kw_while: return "while";
    case TokenType::kw_true: return "true";
    case TokenType::kw_false: return "false";
    case TokenType::kw_int: return "int";
    case TokenType::kw_bool: return "bool";
    case TokenType::kw_string: return "string";
    case TokenType::identifier: return "identifier";
    case TokenType::int_lit: return "int literal";
    case TokenType::string_lit: return "string literal";
    case TokenType::plus: return "+";
    case TokenType::minus: return "-";
    case TokenType::mul: return "*";
    case TokenType::div: return "/";
    case TokenType::assign: return "=";
    case TokenType::eq: return "==";
    case TokenType::neq: return "!=";
    case TokenType::lt: return "<";
    case TokenType::lte: return "<=";
    case TokenType::gt: return ">";
    case TokenType::gte: return ">=";
    case TokenType::semi: return ";";
    case TokenType::lparen: return "(";
    case TokenType::rparen: return ")";
    case TokenType::lbrace: return "{";
    case TokenType::rbrace: return "}";
    case TokenType::colon: return ":";
    case TokenType::END_OF_FILE: return "EOF";
    case TokenType::comma: return ",";
    case TokenType::bang:    return "!";
    case TokenType::arrow: return "->";
    }
    assert(false && "Unhandled TokenType in to_string()");
}