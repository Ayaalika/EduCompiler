#pragma once

#include "token.hpp"
#include <iostream>
#include <vector>
#include <memory>

class TokenPool {
private:
    static constexpr size_t BLOCK_SIZE = 1024;
    std::vector<std::unique_ptr<Token[]>>blocks_;
    size_t  pos_{ BLOCK_SIZE };

    void refill() {
        blocks_.emplace_back(std::make_unique < Token[]>(BLOCK_SIZE));
        pos_ = 0;
    }

public:
    TokenPool() { refill(); }

    Token* allocate() {
        if (pos_ >= BLOCK_SIZE) refill();
        return &blocks_.back()[pos_++];
    }

    void reset() {
        blocks_.clear();
        pos_ = BLOCK_SIZE;
        refill();
    }
};
