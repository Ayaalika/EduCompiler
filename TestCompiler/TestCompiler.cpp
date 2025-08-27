#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <optional>
#include <cstdlib>

#include "LexicalAnalysis.hpp" 
#include "token.hpp"
#include "Parser.hpp"
#include "AST.hpp" 
#include "SemanticAnalyzer.hpp" 
#include "CodeGenerator.hpp"  

void print_usage(const std::string& program_name, const std::string& expected_args = "<arguments>") {
    std::cerr << "Incorrect usage.\n";
    std::cerr << "Usage: " << program_name << " " << expected_args << "\n";
}

std::optional<std::string> read_file_contents(const std::string& filename) {
    std::ifstream input_file(filename);

    if (!input_file) {
        std::cerr << "Error: Could not open file: " << filename << "\n";
        return std::nullopt;
    }

    std::stringstream buffer;
    buffer << input_file.rdbuf();
    return buffer.str();
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        print_usage(argv[0]);
        return EXIT_FAILURE;
    }

    std::string filename = argv[1];
    auto contents_opt = read_file_contents(filename);
    if (!contents_opt) return EXIT_FAILURE;

    std::string source_code = *contents_opt;

    Tokenizer tokenizer(source_code);
    auto tokens = tokenizer.tokenize();

    /*std::cout << "=== Tokens ===\n";
    for (const auto& token : tokens) {
        std::cout << to_string(token->type);
        if (token->value.has_value()) {
            std::cout << " (" << token->value.value() << ")";
        }
        std::cout << " at line " << token->line << "\n";
    }*/

    Parser parser(tokens);
    std::unique_ptr<Program> program = nullptr;

    try {
        program = parser.parse();
        std::cout << "=== Parse Success ===\n";
        std::cout << "Functions: " << program->functions.size() << "\n";
        std::cout << "Top-level Statements: " << program->statements.size() << "\n";

        SemanticAnalyzer analyzer;
        auto errors = analyzer.analyze(*program);

        if (!errors.empty()) {
            std::cerr << "Semantic analysis found errors:\n";
            for (const auto& err : errors) {
                std::cerr << "  " << err << "\n";
            }
            return EXIT_FAILURE; 
        }
        else {
            std::cout << "Semantic analysis passed successfully.\n";
            {
                auto printFn = std::make_unique<FunctionDecl>();
                Token nameTok;
                nameTok.type = TokenType::identifier;
                nameTok.line = 0;
                nameTok.value = std::string("print");
                printFn->name = nameTok;

                Param p;
                p.name = "s";
                p.type.kind = Type::Kind::String;
                p.type.name = "string";
                printFn->parameters.push_back(std::move(p));


                printFn->returnType.kind = Type::Kind::Void;
                printFn->returnType.name = "void";

                printFn->body = std::make_unique<Block>();


                program->functions.push_back(std::move(printFn));
            }


            CodeGenerator cg(&analyzer);
            cg.generate(*program);


            cg.emitToStream(std::cout);
        }
    }
    catch (const std::exception& ex) {
        std::cerr << "Parsing/analysis failed: " << ex.what() << "\n";
        return EXIT_FAILURE;
    }
    catch (...) {
        std::cerr << "Parsing failed (unknown error).\n";
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}