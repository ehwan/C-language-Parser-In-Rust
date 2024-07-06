# C language lexer & parser & virtual executer written in Rust

C language lexer & parser & virtual executer from scratch in Rust.

## syntax not supported
 - Some Preprocessor ( `#include`, `#pragma` )
 - `union` `enum`
 - type qualifiers (`volatile`, `restrict` `static` `extern`)

## Features
 - Tokenizer (Lexer)
 - Preprocessor
 - Parser ( AST Builder )
 - Code Generator
 - Virtual Machine (Instruction Executor)

### Note
This process will not generate binary or assembly code. Instead, it will produce a sequence of virtual instructions (`src/virtualmachine/instruction`) which will be executed at the software level.


## How to run
```sh
cargo run
```
 To execute the program, pass the C code to stdin. 
 Once you are done, press ^D to finish the input. The program will tokenize, parse, generate instructions, and execute the code.


Sample C codes (only with implemented features) are in `samples/` directory. Try them with `cat samples/sample.c | cargo run`

## Example
```c
/// samples/sample.c

// declaration of function fibonacci sequence
int fibonacci(int);

#define MY_MACRO_FUNC(x, y) y + x

#if MY_MACRO_FUNC(1, 2) == 3

// main function
int main()
{
  print_str("Hello, World!"); // built in function 'print_str'
  int var = 10;
  int* ptr = &var;
  *ptr = MY_MACRO_FUNC(40, 60);
  print(ptr, *ptr, var); // built in function 'print'

  print(MY_MACRO_FUNC(10, 20));

  // print fibonacci sequence
  print_str("Fibonacci sequence:");
  int i;
  for (i = 1; i <= 10; i++)
  {
    print(i, fibonacci(i));
  }

  return 0;
}

// definition of function fibonacci sequence using recursion
int fibonacci(int n)
{
  if (n <= 2)
    return 1;
  else
    return fibonacci(n - 1) + fibonacci(n - 2);
}

#else

THIS WILL BE IGNORED

#endif
```

Pass c code to stdin
```sh
cat samples/sample.c | cargo run
```

The result will be:

```
Enter your code (and ^D for EOF):
================================================================================
===============================Phase1: Tokenizing===============================
================================================================================
LINE | ---------------------------------Result----------------------------------
   0: 
   1: Int Identifier("fibonacci") LeftParen Int RightParen SemiColon 
   2: 
   3: PreprocessorDefine Identifier("MY_MACRO_FUNC") LeftParen Identifier("x") Comma Identifier("y") RightParen Identifier("y") Plus Identifier("x") 
   4: 
   5: PreprocessorIf Identifier("MY_MACRO_FUNC") LeftParen ConstantInteger(1) Comma ConstantInteger(2) RightParen EqOp ConstantInteger(3) 
   6: 
   7: 
   8: Int Identifier("main") LeftParen RightParen 
   9: LeftBrace 
  10: Identifier("print_str") LeftParen StringLiteral("Hello, World!") RightParen SemiColon 
  11: Int Identifier("var") Equal ConstantInteger(10) SemiColon 
  12: Int Star Identifier("ptr") Equal Ampersand Identifier("var") SemiColon 
  13: Star Identifier("ptr") Equal Identifier("MY_MACRO_FUNC") LeftParen ConstantInteger(40) Comma ConstantInteger(60) RightParen SemiColon 
  14: Identifier("print") LeftParen Identifier("ptr") Comma Star Identifier("ptr") Comma Identifier("var") RightParen SemiColon 
  15: 
  16: Identifier("print") LeftParen Identifier("MY_MACRO_FUNC") LeftParen ConstantInteger(10) Comma ConstantInteger(20) RightParen RightParen SemiColon 
  17: 
  18: 
  19: Identifier("print_str") LeftParen StringLiteral("Fibonacci sequence:") RightParen SemiColon 
  20: Int Identifier("i") SemiColon 
  21: For LeftParen Identifier("i") Equal ConstantInteger(1) SemiColon Identifier("i") LeOp ConstantInteger(10) SemiColon Identifier("i") IncOp RightParen 
  22: LeftBrace 
  23: Identifier("print") LeftParen Identifier("i") Comma Identifier("fibonacci") LeftParen Identifier("i") RightParen RightParen SemiColon 
  24: RightBrace 
  25: 
  26: Return ConstantInteger(0) SemiColon 
  27: RightBrace 
  28: 
  29: 
  30: Int Identifier("fibonacci") LeftParen Int Identifier("n") RightParen 
  31: LeftBrace 
  32: If LeftParen Identifier("n") LeOp ConstantInteger(2) RightParen 
  33: Return ConstantInteger(1) SemiColon 
  34: Else 
  35: Return Identifier("fibonacci") LeftParen Identifier("n") Minus ConstantInteger(1) RightParen Plus Identifier("fibonacci") LeftParen Identifier("n") Minus ConstantInteger(2) RightParen SemiColon 
  36: RightBrace 
  37: 
  38: PreprocessorElse 
  39: 
  40: Identifier("THIS") Identifier("WILL") Identifier("BE") Identifier("IGNORED") 
  41: 
  42: PreprocessorEndIf 
================================================================================
=============================Phase2: Line Analysis==============================
================================================================================
LINE | ---------------------------------Result----------------------------------
   0: RawTokens { tokens: [Int, Identifier("fibonacci"), LeftParen, Int, RightParen, SemiColon] }
   1: DefineFunction { name: "MY_MACRO_FUNC", param_count: 2, replacement: [PreprocessorPlaceholder(1), Plus, PreprocessorPlaceholder(0)] }
   2: If { expression_tokens: [Identifier("MY_MACRO_FUNC"), LeftParen, ConstantInteger(1), Comma, ConstantInteger(2), RightParen, EqOp, ConstantInteger(3)] }
   3: RawTokens { tokens: [Int, Identifier("main"), LeftParen, RightParen] }
   4: RawTokens { tokens: [LeftBrace] }
   5: RawTokens { tokens: [Identifier("print_str"), LeftParen, StringLiteral("Hello, World!"), RightParen, SemiColon] }
   6: RawTokens { tokens: [Int, Identifier("var"), Equal, ConstantInteger(10), SemiColon] }
   7: RawTokens { tokens: [Int, Star, Identifier("ptr"), Equal, Ampersand, Identifier("var"), SemiColon] }
   8: RawTokens { tokens: [Star, Identifier("ptr"), Equal, Identifier("MY_MACRO_FUNC"), LeftParen, ConstantInteger(40), Comma, ConstantInteger(60), RightParen, SemiColon] }
   9: RawTokens { tokens: [Identifier("print"), LeftParen, Identifier("ptr"), Comma, Star, Identifier("ptr"), Comma, Identifier("var"), RightParen, SemiColon] }
  10: RawTokens { tokens: [Identifier("print"), LeftParen, Identifier("MY_MACRO_FUNC"), LeftParen, ConstantInteger(10), Comma, ConstantInteger(20), RightParen, RightParen, SemiColon] }
  11: RawTokens { tokens: [Identifier("print_str"), LeftParen, StringLiteral("Fibonacci sequence:"), RightParen, SemiColon] }
  12: RawTokens { tokens: [Int, Identifier("i"), SemiColon] }
  13: RawTokens { tokens: [For, LeftParen, Identifier("i"), Equal, ConstantInteger(1), SemiColon, Identifier("i"), LeOp, ConstantInteger(10), SemiColon, Identifier("i"), IncOp, RightParen] }
  14: RawTokens { tokens: [LeftBrace] }
  15: RawTokens { tokens: [Identifier("print"), LeftParen, Identifier("i"), Comma, Identifier("fibonacci"), LeftParen, Identifier("i"), RightParen, RightParen, SemiColon] }
  16: RawTokens { tokens: [RightBrace] }
  17: RawTokens { tokens: [Return, ConstantInteger(0), SemiColon] }
  18: RawTokens { tokens: [RightBrace] }
  19: RawTokens { tokens: [Int, Identifier("fibonacci"), LeftParen, Int, Identifier("n"), RightParen] }
  20: RawTokens { tokens: [LeftBrace] }
  21: RawTokens { tokens: [If, LeftParen, Identifier("n"), LeOp, ConstantInteger(2), RightParen] }
  22: RawTokens { tokens: [Return, ConstantInteger(1), SemiColon] }
  23: RawTokens { tokens: [Else] }
  24: RawTokens { tokens: [Return, Identifier("fibonacci"), LeftParen, Identifier("n"), Minus, ConstantInteger(1), RightParen, Plus, Identifier("fibonacci"), LeftParen, Identifier("n"), Minus, ConstantInteger(2), RightParen, SemiColon] }
  25: RawTokens { tokens: [RightBrace] }
  26: Else
  27: RawTokens { tokens: [Identifier("THIS"), Identifier("WILL"), Identifier("BE"), Identifier("IGNORED")] }
  28: EndIf
================================================================================
=============================Phase3: Preprocessing==============================
================================================================================
LINE | ---------------------------------Result----------------------------------
   0: [Int, Identifier("fibonacci"), LeftParen, Int, RightParen, SemiColon]
   1: [Int, Identifier("main"), LeftParen, RightParen]
   2: [LeftBrace]
   3: [Identifier("print_str"), LeftParen, StringLiteral("Hello, World!"), RightParen, SemiColon]
   4: [Int, Identifier("var"), Equal, ConstantInteger(10), SemiColon]
   5: [Int, Star, Identifier("ptr"), Equal, Ampersand, Identifier("var"), SemiColon]
   6: [Star, Identifier("ptr"), Equal, ConstantInteger(60), Plus, ConstantInteger(40), SemiColon]
   7: [Identifier("print"), LeftParen, Identifier("ptr"), Comma, Star, Identifier("ptr"), Comma, Identifier("var"), RightParen, SemiColon]
   8: [Identifier("print"), LeftParen, ConstantInteger(20), Plus, ConstantInteger(10), RightParen, SemiColon]
   9: [Identifier("print_str"), LeftParen, StringLiteral("Fibonacci sequence:"), RightParen, SemiColon]
  10: [Int, Identifier("i"), SemiColon]
  11: [For, LeftParen, Identifier("i"), Equal, ConstantInteger(1), SemiColon, Identifier("i"), LeOp, ConstantInteger(10), SemiColon, Identifier("i"), IncOp, RightParen]
  12: [LeftBrace]
  13: [Identifier("print"), LeftParen, Identifier("i"), Comma, Identifier("fibonacci"), LeftParen, Identifier("i"), RightParen, RightParen, SemiColon]
  14: [RightBrace]
  15: [Return, ConstantInteger(0), SemiColon]
  16: [RightBrace]
  17: [Int, Identifier("fibonacci"), LeftParen, Int, Identifier("n"), RightParen]
  18: [LeftBrace]
  19: [If, LeftParen, Identifier("n"), LeOp, ConstantInteger(2), RightParen]
  20: [Return, ConstantInteger(1), SemiColon]
  21: [Else]
  22: [Return, Identifier("fibonacci"), LeftParen, Identifier("n"), Minus, ConstantInteger(1), RightParen, Plus, Identifier("fibonacci"), LeftParen, Identifier("n"), Minus, ConstantInteger(2), RightParen, SemiColon]
  23: [RightBrace]
================================================================================
======================Phase4: Building AbstractSyntaxTree=======================
================================================================================
ASTs: 
TranslationUnit {
    statements: [
        FunctionDeclaration {
            return_type: Int32,
            name: "fibonacci",
            params: [
                Int32,
            ],
        },
        FunctionDefinitionStatement {
            return_type: Int32,
            name: "main",
            params: [],
            body: CompoundStatement {
                statements: [
                    ExpressionStatement {
                        expression: PostParen {
                            src: PrimaryIdentifier {
                                name: "print_str",
                            },
                            args: [
                                StringLiteral {
                                    value: "Hello, World!",
                                },
                            ],
                        },
                    },
                    DeclarationStatement {
                        vars: [
                            (
                                "var",
                                Int32,
                                Some(
                                    ConstantInteger {
                                        value: 10,
                                    },
                                ),
                            ),
                        ],
                    },
                    DeclarationStatement {
                        vars: [
                            (
                                "ptr",
                                Pointer(
                                    Int32,
                                ),
                                Some(
                                    UnaryExpression {
                                        op: AddressOf,
                                        src: PrimaryIdentifier {
                                            name: "var",
                                        },
                                    },
                                ),
                            ),
                        ],
                    },
                    ExpressionStatement {
                        expression: AssignExpression {
                            lhs: UnaryExpression {
                                op: Dereference,
                                src: PrimaryIdentifier {
                                    name: "ptr",
                                },
                            },
                            rhs: AdditiveExpression {
                                op: Add,
                                lhs: ConstantInteger {
                                    value: 60,
                                },
                                rhs: ConstantInteger {
                                    value: 40,
                                },
                            },
                        },
                    },
                    ExpressionStatement {
                        expression: PostParen {
                            src: PrimaryIdentifier {
                                name: "print",
                            },
                            args: [
                                PrimaryIdentifier {
                                    name: "ptr",
                                },
                                UnaryExpression {
                                    op: Dereference,
                                    src: PrimaryIdentifier {
                                        name: "ptr",
                                    },
                                },
                                PrimaryIdentifier {
                                    name: "var",
                                },
                            ],
                        },
                    },
                    ExpressionStatement {
                        expression: PostParen {
                            src: PrimaryIdentifier {
                                name: "print",
                            },
                            args: [
                                AdditiveExpression {
                                    op: Add,
                                    lhs: ConstantInteger {
                                        value: 20,
                                    },
                                    rhs: ConstantInteger {
                                        value: 10,
                                    },
                                },
                            ],
                        },
                    },
                    ExpressionStatement {
                        expression: PostParen {
                            src: PrimaryIdentifier {
                                name: "print_str",
                            },
                            args: [
                                StringLiteral {
                                    value: "Fibonacci sequence:",
                                },
                            ],
                        },
                    },
                    DeclarationStatement {
                        vars: [
                            (
                                "i",
                                Int32,
                                None,
                            ),
                        ],
                    },
                    ForStatement {
                        init: AssignExpression {
                            lhs: PrimaryIdentifier {
                                name: "i",
                            },
                            rhs: ConstantInteger {
                                value: 1,
                            },
                        },
                        cond: ComparisonExpression {
                            op: LessThanOrEqual,
                            lhs: PrimaryIdentifier {
                                name: "i",
                            },
                            rhs: ConstantInteger {
                                value: 10,
                            },
                        },
                        next: Some(
                            PostIncrement {
                                src: PrimaryIdentifier {
                                    name: "i",
                                },
                            },
                        ),
                        statement: CompoundStatement {
                            statements: [
                                ExpressionStatement {
                                    expression: PostParen {
                                        src: PrimaryIdentifier {
                                            name: "print",
                                        },
                                        args: [
                                            PrimaryIdentifier {
                                                name: "i",
                                            },
                                            PostParen {
                                                src: PrimaryIdentifier {
                                                    name: "fibonacci",
                                                },
                                                args: [
                                                    PrimaryIdentifier {
                                                        name: "i",
                                                    },
                                                ],
                                            },
                                        ],
                                    },
                                },
                            ],
                        },
                    },
                    ReturnStatement {
                        expr: Some(
                            ConstantInteger {
                                value: 0,
                            },
                        ),
                    },
                ],
            },
        },
        FunctionDefinitionStatement {
            return_type: Int32,
            name: "fibonacci",
            params: [
                (
                    Some(
                        "n",
                    ),
                    Int32,
                ),
            ],
            body: CompoundStatement {
                statements: [
                    IfStatement {
                        cond: ComparisonExpression {
                            op: LessThanOrEqual,
                            lhs: PrimaryIdentifier {
                                name: "n",
                            },
                            rhs: ConstantInteger {
                                value: 2,
                            },
                        },
                        then_statement: ReturnStatement {
                            expr: Some(
                                ConstantInteger {
                                    value: 1,
                                },
                            ),
                        },
                        else_statement: Some(
                            ReturnStatement {
                                expr: Some(
                                    AdditiveExpression {
                                        op: Add,
                                        lhs: PostParen {
                                            src: PrimaryIdentifier {
                                                name: "fibonacci",
                                            },
                                            args: [
                                                AdditiveExpression {
                                                    op: Sub,
                                                    lhs: PrimaryIdentifier {
                                                        name: "n",
                                                    },
                                                    rhs: ConstantInteger {
                                                        value: 1,
                                                    },
                                                },
                                            ],
                                        },
                                        rhs: PostParen {
                                            src: PrimaryIdentifier {
                                                name: "fibonacci",
                                            },
                                            args: [
                                                AdditiveExpression {
                                                    op: Sub,
                                                    lhs: PrimaryIdentifier {
                                                        name: "n",
                                                    },
                                                    rhs: ConstantInteger {
                                                        value: 2,
                                                    },
                                                },
                                            ],
                                        },
                                    },
                                ),
                            },
                        ),
                    },
                ],
            },
        },
    ],
}
================================================================================
============================Generating Instructions=============================
================================================================================
ADDR | ---------------------------------Result----------------------------------
Instructions: 
   0: DefineLabel { label: "main" }
   1: PushStack { operand: Register(5) }
   2: MoveRegister { operand_from: Register(6), operand_to: Register(5) }
   3: MoveRegister { operand_from: Value(UInt64(0)), operand_to: Register(0) }
   4: PrintStr { str: Register(0) }
   5: MoveRegister { operand_from: Value(Int32(10)), operand_to: Register(0) }
   6: Assign { lhs_type: Int32, lhs: Register(1), rhs: Register(0) }
   7: PushStack { operand: Register(1) }
   8: MoveRegister { operand_from: Register(5), operand_to: Register(0) }
   9: AddAssign { lhs: Register(0), rhs: Value(Int64(0)) }
  10: Assign { lhs_type: Pointer(Int32), lhs: Register(1), rhs: Register(0) }
  11: PushStack { operand: Register(1) }
  12: MoveRegister { operand_from: Value(Int32(40)), operand_to: Register(0) }
  13: PushStack { operand: Register(0) }
  14: MoveRegister { operand_from: Value(Int32(60)), operand_to: Register(0) }
  15: Assign { lhs_type: UInt32, lhs: Register(0), rhs: Register(0) }
  16: PopStack { operand: Register(1) }
  17: AddAssign { lhs: Register(0), rhs: Register(1) }
  18: PushStack { operand: Register(0) }
  19: MoveRegister { operand_from: Register(5), operand_to: Register(0) }
  20: AddAssign { lhs: Register(0), rhs: Value(Int64(1)) }
  21: MoveRegister { operand_from: Derefed(0, 0), operand_to: Register(0) }
  22: PopStack { operand: Register(1) }
  23: Assign { lhs_type: Int32, lhs: Derefed(0, 0), rhs: Register(1) }
  24: MoveRegister { operand_from: Register(5), operand_to: Register(0) }
  25: AddAssign { lhs: Register(0), rhs: Value(Int64(0)) }
  26: PushStack { operand: Derefed(0, 0) }
  27: MoveRegister { operand_from: Register(5), operand_to: Register(0) }
  28: AddAssign { lhs: Register(0), rhs: Value(Int64(1)) }
  29: MoveRegister { operand_from: Derefed(0, 0), operand_to: Register(0) }
  30: PushStack { operand: Derefed(0, 0) }
  31: MoveRegister { operand_from: Register(5), operand_to: Register(0) }
  32: AddAssign { lhs: Register(0), rhs: Value(Int64(1)) }
  33: PushStack { operand: Derefed(0, 0) }
  34: PushStack { operand: Value(UInt64(3)) }
  35: Print
  36: MoveRegister { operand_from: Value(Int32(10)), operand_to: Register(0) }
  37: PushStack { operand: Register(0) }
  38: MoveRegister { operand_from: Value(Int32(20)), operand_to: Register(0) }
  39: Assign { lhs_type: UInt32, lhs: Register(0), rhs: Register(0) }
  40: PopStack { operand: Register(1) }
  41: AddAssign { lhs: Register(0), rhs: Register(1) }
  42: PushStack { operand: Register(0) }
  43: PushStack { operand: Value(UInt64(1)) }
  44: Print
  45: MoveRegister { operand_from: Value(UInt64(14)), operand_to: Register(0) }
  46: PrintStr { str: Register(0) }
  47: PushStack { operand: Value(Int32(0)) }
  48: MoveRegister { operand_from: Value(Int32(1)), operand_to: Register(0) }
  49: PushStack { operand: Register(0) }
  50: MoveRegister { operand_from: Register(5), operand_to: Register(0) }
  51: AddAssign { lhs: Register(0), rhs: Value(Int64(2)) }
  52: PopStack { operand: Register(1) }
  53: Assign { lhs_type: Int32, lhs: Derefed(0, 0), rhs: Register(1) }
  54: DefineLabel { label: ".__L0__" }
  55: MoveRegister { operand_from: Register(5), operand_to: Register(0) }
  56: AddAssign { lhs: Register(0), rhs: Value(Int64(2)) }
  57: PushStack { operand: Derefed(0, 0) }
  58: MoveRegister { operand_from: Value(Int32(10)), operand_to: Register(0) }
  59: PopStack { operand: Register(1) }
  60: LessThan { lhs: Register(0), rhs: Register(1), to: Register(0) }
  61: LogicalNot { operand: Register(0) }
  62: JumpZero { label: ".__L1__", operand_cond: Register(0) }
  63: MoveRegister { operand_from: Register(5), operand_to: Register(0) }
  64: AddAssign { lhs: Register(0), rhs: Value(Int64(2)) }
  65: PushStack { operand: Derefed(0, 0) }
  66: Call { label: "fibonacci" }
  67: SubAssign { lhs: Register(6), rhs: Value(UInt64(1)) }
  68: PushStack { operand: Register(0) }
  69: MoveRegister { operand_from: Register(5), operand_to: Register(0) }
  70: AddAssign { lhs: Register(0), rhs: Value(Int64(2)) }
  71: PushStack { operand: Derefed(0, 0) }
  72: PushStack { operand: Value(UInt64(2)) }
  73: Print
  74: DefineLabel { label: ".__L2__" }
  75: MoveRegister { operand_from: Register(5), operand_to: Register(0) }
  76: AddAssign { lhs: Register(0), rhs: Value(Int64(2)) }
  77: MoveRegister { operand_from: Register(0), operand_to: Register(1) }
  78: MoveRegister { operand_from: Derefed(1, 0), operand_to: Register(0) }
  79: AddAssign { lhs: Derefed(1, 0), rhs: Value(UInt8(1)) }
  80: Jump { label: ".__L0__" }
  81: DefineLabel { label: ".__L1__" }
  82: MoveRegister { operand_from: Value(Int32(0)), operand_to: Register(0) }
  83: Return
  84: Panic { message: "Function main must return a Int32 value" }
  85: DefineLabel { label: "fibonacci" }
  86: PushStack { operand: Register(5) }
  87: MoveRegister { operand_from: Register(6), operand_to: Register(5) }
  88: MoveRegister { operand_from: Register(5), operand_to: Register(0) }
  89: AddAssign { lhs: Register(0), rhs: Value(Int64(-3)) }
  90: PushStack { operand: Derefed(0, 0) }
  91: MoveRegister { operand_from: Value(Int32(2)), operand_to: Register(0) }
  92: PopStack { operand: Register(1) }
  93: LessThan { lhs: Register(0), rhs: Register(1), to: Register(0) }
  94: LogicalNot { operand: Register(0) }
  95: JumpZero { label: ".__L3__", operand_cond: Register(0) }
  96: MoveRegister { operand_from: Value(Int32(1)), operand_to: Register(0) }
  97: Return
  98: Jump { label: ".__L4__" }
  99: DefineLabel { label: ".__L3__" }
 100: MoveRegister { operand_from: Value(Int32(2)), operand_to: Register(0) }
 101: PushStack { operand: Register(0) }
 102: MoveRegister { operand_from: Register(5), operand_to: Register(0) }
 103: AddAssign { lhs: Register(0), rhs: Value(Int64(-3)) }
 104: Assign { lhs_type: UInt32, lhs: Register(0), rhs: Derefed(0, 0) }
 105: PopStack { operand: Register(1) }
 106: SubAssign { lhs: Register(0), rhs: Register(1) }
 107: PushStack { operand: Register(0) }
 108: Call { label: "fibonacci" }
 109: SubAssign { lhs: Register(6), rhs: Value(UInt64(1)) }
 110: PushStack { operand: Register(0) }
 111: MoveRegister { operand_from: Value(Int32(1)), operand_to: Register(0) }
 112: PushStack { operand: Register(0) }
 113: MoveRegister { operand_from: Register(5), operand_to: Register(0) }
 114: AddAssign { lhs: Register(0), rhs: Value(Int64(-3)) }
 115: Assign { lhs_type: UInt32, lhs: Register(0), rhs: Derefed(0, 0) }
 116: PopStack { operand: Register(1) }
 117: SubAssign { lhs: Register(0), rhs: Register(1) }
 118: PushStack { operand: Register(0) }
 119: Call { label: "fibonacci" }
 120: SubAssign { lhs: Register(6), rhs: Value(UInt64(1)) }
 121: Assign { lhs_type: UInt32, lhs: Register(0), rhs: Register(0) }
 122: PopStack { operand: Register(1) }
 123: AddAssign { lhs: Register(0), rhs: Register(1) }
 124: Return
 125: DefineLabel { label: ".__L4__" }
 126: Panic { message: "Function fibonacci must return a Int32 value" }
  --------------------------------Start Address---------------------------------
 127: Call { label: "main" }
================================================================================
=============================Executing Instructions=============================
================================================================================
"Hello, World!"
Print: UInt64(36), Int32(100), Int32(100), 
Print: UInt32(30), 
"Fibonacci sequence:"
Print: Int32(1), Int32(1), 
Print: Int32(2), Int32(1), 
Print: Int32(3), UInt32(2), 
Print: Int32(4), UInt32(3), 
Print: Int32(5), UInt32(5), 
Print: Int32(6), UInt32(8), 
Print: Int32(7), UInt32(13), 
Print: Int32(8), UInt32(21), 
Print: Int32(9), UInt32(34), 
Print: Int32(10), UInt32(55), 
```

The visualized AST will be:

![AST](tree.png)

```
TranslationUnit
├── DeclarationStatement
│   └── Vars
│       └── (Some("fibonacci"), Function(Int32, [Int32]), None)
├── FunctionDefinitionStatement: main
│   ├── ReturnType: Int32
│   ├── Name: main
│   ├── Params: []
│   └── Body: CompoundStatement
│       ├── ExpressionStatement
│       │   └── PostParen
│       │       ├── Src: PrimaryIdentifier(print_str)
│       │       └── Args: [StringLiteral("Hello, World!")]
│       ├── DeclarationStatement
│       │   └── Vars
│       │       └── (Some("var"), Int32, ConstantInteger(10))
│       ├── DeclarationStatement
│       │   └── Vars
│       │       └── (Some("ptr"), Pointer(Int32), UnaryExpression(AddressOf, PrimaryIdentifier(var)))
│       ├── ExpressionStatement
│       │   └── AssignExpression
│       │       ├── Op: Assign
│       │       ├── Lhs: UnaryExpression(Dereference, PrimaryIdentifier(ptr))
│       │       └── Rhs: ConstantInteger(100)
│       ├── ExpressionStatement
│       │   └── PostParen
│       │       ├── Src: PrimaryIdentifier(print)
│       │       └── Args: [PrimaryIdentifier(ptr), UnaryExpression(Dereference, PrimaryIdentifier(ptr)), PrimaryIdentifier(var)]
│       ├── ExpressionStatement
│       │   └── PostParen
│       │       ├── Src: PrimaryIdentifier(print_str)
│       │       └── Args: [StringLiteral("Fibonacci sequence:")]
│       ├── DeclarationStatement
│       │   └── Vars
│       │       └── (Some("i"), Int32, None)
│       ├── ForStatement
│       │   ├── Init: AssignExpression
│       │   │   ├── Op: Assign
│       │   │   ├── Lhs: PrimaryIdentifier(i)
│       │   │   └── Rhs: ConstantInteger(1)
│       │   ├── Cond: ComparisonExpression
│       │   │   ├── Op: LessThanOrEqual
│       │   │   ├── Lhs: PrimaryIdentifier(i)
│       │   │   └── Rhs: ConstantInteger(10)
│       │   ├── Next: PostIncrement(PrimaryIdentifier(i))
│       │   └── Statement: CompoundStatement
│       │       └── ExpressionStatement
│       │           └── PostParen
│       │               ├── Src: PrimaryIdentifier(print)
│       │               └── Args: [PrimaryIdentifier(i), PostParen(PrimaryIdentifier(fibonacci), [PrimaryIdentifier(i)])]
│       └── ReturnStatement
│           └── Expr: ConstantInteger(0)
├── FunctionDefinitionStatement: fibonacci
│   ├── ReturnType: Int32
│   ├── Name: fibonacci
│   ├── Params: [(Some("n"), Int32)]
│   └── Body: CompoundStatement
│       └── IfStatement
│           ├── Cond: ComparisonExpression
│           │   ├── Op: LessThanOrEqual
│           │   ├── Lhs: PrimaryIdentifier(n)
│           │   └── Rhs: ConstantInteger(2)
│           ├── Then: ReturnStatement
│           │   └── Expr: ConstantInteger(1)
│           └── Else: ReturnStatement
│               └── Expr: AdditiveExpression
│                   ├── Op: Add
│                   ├── Lhs: PostParen(PrimaryIdentifier(fibonacci), [AdditiveExpression(Sub, PrimaryIdentifier(n), ConstantInteger(1))])
│                   └── Rhs: PostParen(PrimaryIdentifier(fibonacci), [AdditiveExpression(Sub, PrimaryIdentifier(n), ConstantInteger(2))])
```