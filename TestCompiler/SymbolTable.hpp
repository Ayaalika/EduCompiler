#pragma once

#include <memory>
#include <vector>
#include <string>
#include <optional>
#include "AST.hpp"
#include <unordered_map>

struct Symbol {
	std::string name;
	Type type;
	bool isFunction = false;
    bool isInitialized = false; 
    bool isMutable = true;      
	std::vector<Type>parameters;
};

class ScopeStack {
    using Scope = std::unordered_map<std::string, Symbol>;
    std::vector<Scope>scopes;
public:
    void beginScope() {
        scopes.push_back({});
    }

    void endScope() {
        if (!scopes.empty()) {
            scopes.pop_back();
        }
    }

    void clear() {
        scopes.clear();
    }

    bool isEmpty() const {
        return scopes.empty();
    }

    bool declare(const std::string& name, const Symbol& symbol) {
        if (scopes.empty()) beginScope();
        auto& current = scopes.back();
        if (current.count(name)) return false;
        current[name] = symbol;
        return true;
    }

    bool updateSymbol(const std::string& name, const Symbol& updated) {
        for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
            if (it->count(name)) {
                (*it)[name] = updated;
                return true;
            }
        }
        return false;
    }

    std::optional<Symbol> resolve(const std::string& name) const {
        for (auto it = scopes.rbegin(); it != scopes.rend(); it++) {
            auto symbol = it->find(name);
            if (symbol != it->end()) {
                return symbol->second;
            }
        }
        return std::nullopt;
    }

    std::optional<Symbol> containsInCurrentScope(const std::string& name) const {
        if (scopes.empty()) return std::nullopt;
        auto it = scopes.back().find(name);
        return it != scopes.back().end() ? std::make_optional(it->second) : std::nullopt;
    }

    bool isDeclared(const std::string& name) const {
        return resolve(name).has_value();
    }

    bool isInGlobalScope() const {
        return scopes.size() == 1;
    }

    size_t scopeDepth() const {
        return scopes.size();
    }
};