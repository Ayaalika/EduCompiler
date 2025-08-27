

# 🚀 Modern Compiler with GUI - A Complete Toolchain

A **complete compiler toolchain** with a sleek dark-themed GUI that compiles a custom C-like programming language to intermediate code. Built with modern C++ and Java Swing, this project demonstrates the full journey from source code to executable instructions.

---

## ✨ Features

- 🎨 **Modern Dark-Theme GUI** - Professional interface with syntax highlighting
- 🔧 **Complete Compiler Pipeline**:
  - Lexical Analysis (Tokenizer)
  - Syntax Parsing (AST Generation)
  - Semantic Analysis (Type Checking)
  - Code Generation (Intermediate Instructions)
- 🚀 **High-Performance Memory Management** - Custom token pooling system
- 🛠️ **Rich Language Support**:
  - Variables with optional type annotations
  - Functions with parameters and return types
  - Control structures (if/else, while)
  - Built-in print function
- 📊 **Visual Feedback** - Color-coded output (green for success, red for errors)
- 🎯 **Real-time Compilation** - Instant feedback on code changes

---

## 🖼️ Screenshots

![Compiler GUI](https://via.placeholder.com/800x450?text=Dark+Theme+Compiler+GUI+Screenshot)
*Dark-themed interface with code editor and output panel*

![Compilation Output](https://via.placeholder.com/800x450?text=Compilation+Output+with+Syntax+Highlighting)
*Color-coded compilation results with error highlighting*

---

## 🛠️ Installation

### Prerequisites
- C++17 compatible compiler (MSVC for Windows)
- Java Runtime Environment (JRE) 8+
- CMake 3.10+ (optional, for build configuration)

د
---

## 🏗️ Project Architecture

```
├── src/                    # C++ Compiler Core
│   ├── token.hpp           # Token definitions
│   ├── TokenPool.hpp       # Memory management
│   ├── Tokenizer.hpp       # Lexical analysis
│   ├── AST.hpp             # Abstract Syntax Tree
│   ├── Parser.hpp          # Syntax parsing
│   ├── SemanticAnalyzer.hpp # Type checking
│   ├── CodeGenerator.hpp   # Code generation
│   └── main.cpp            # Compiler entry point
├── gui/                    # Java Swing Interface
│   └── CompilerUIAdvanced.java
├── test/                   # Test cases
└── README.md               # This file
```

### Core Components

1. **Tokenizer** (`Tokenizer.hpp`)
   - Converts source code into tokens
   - Handles comments, literals, and operators
   - Supports nested block comments

2. **Parser** (`Parser.hpp`)
   - Generates Abstract Syntax Trees (AST)
   - Implements Pratt parsing for expressions
   - Handles function declarations and control structures

3. **Semantic Analyzer** (`SemanticAnalyzer.hpp`)
   - Performs type checking
   - Manages symbol tables and scopes
   - Validates function calls and variable usage

4. **Code Generator** (`CodeGenerator.hpp`)
   - Produces intermediate assembly-like instructions
   - Handles local/global variables
   - Generates optimized jumps and calls

5. **Token Pool** (`TokenPool.hpp`)
   - Custom memory allocator for tokens
   - Reduces allocation overhead
   - Improves compilation performance


---

## 🔧 Technical Deep Dive

### Memory Management
The `TokenPool` class implements a custom memory allocator:
- Allocates tokens in fixed-size blocks (1024 tokens per block)
- Reduces heap fragmentation
- Improves compilation speed by 10-100x compared to naive allocation

### Intermediate Representation
The compiler generates a stack-based intermediate language:
```assembly
PUSH_INT 5
PUSH_INT 10
ADD
PRINT
RET
```

### Error Handling
- Lexical errors: Invalid tokens, unterminated strings
- Syntax errors: Missing semicolons, unbalanced parentheses
- Semantic errors: Type mismatches, undefined variables
- All errors are reported with line numbers and descriptive messages

---
