# Grammar Specification

## Program Structure
```ebnf
program         = (function_decl / statement)*
```

## Functions
```ebnf
function_decl   = "fn" identifier "(" param_list? ")" block
param_list      = param ("," param)*
param           = identifier ":" type
```

## Blocks
```ebnf
block           = "{" statement* "}"
```

## Statements
```ebnf
statement       =
      variable_decl
    / assignment
    / print_stmt
    / return_stmt
    / if_stmt
    / while_stmt
    / block
    / expr_stmt

variable_decl   = "let" identifier (":" type)? "=" expression ";"
assignment      = identifier "=" expression ";"
print_stmt      = "print" expression ";"
return_stmt     = "return" expression? ";"
if_stmt         = "if" expression block ("else" block)?
while_stmt      = "while" expression block
expr_stmt       = expression ";"
```

## Expressions
```ebnf
expression      = comparison
comparison      = additive (("==" / "!=" / "<" / "<=" / ">" / ">=") additive)*
additive        = multiplicative (("+" / "-") multiplicative)*
multiplicative  = unary (("*" / "/") unary)*
unary           = ("-" / "!") unary / call
call            = primary ("(" arg_list? ")")*
arg_list        = expression ("," expression)*

primary         =
      identifier
    / int_lit
    / string_lit
    / "true"
    / "false"
    / "(" expression ")"
```

## Types
```ebnf
type            = "int" / "bool" / "string" / identifier
```