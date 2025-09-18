# C language lexer & parser & virtual executer written in Rust

C language lexer & parser & virtual executer from scratch in Rust.

## Features
 - Tokenizer (Lexer)
 - Preprocessor
 - Parser ( AST Builder )
 - Virtual Machine ( LLVM via [inkwell](https://github.com/TheDan64/inkwell) )

## How it works
### Phase 1: Tokenizing
Tokenize the raw source code into a list of tokens.
This phase will remove c/cpp comments.
Sequence of whitespaces will be combined into one `Token::Whitespace`
The newline `\n` will be kept as a `Token::NewLine` for later phase
If the source is not end with `\n`, it will be added automatically

### Phase 2: Line Analysis
Analyze the tokens in each line and generate a list of `Line` which contains the result of the analysis. This phase will extract preprocessor directives and macro definitions.

### Phase 3: Preprocessing
Preprocess the source code by expanding macros and removing preprocessor directives.

### Phase 4: Building AbstractSyntaxTree
Build an Abstract Syntax Tree (AST) from the preprocessed token stream. The AST will be used to generate instructions.

### Phase 5: Semantic Analysis
Iterate through the AST and check for semantic errors, such as type mismatches and undeclared variables.

### Phase 6: LLVM linkage
Generate LLVM IR from the AST and execute it using the LLVM JIT compiler.

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

extern int printf(const char *fmt, ...);

// declaration of function fibonacci sequence
int fibonacci(int);

#define MY_MACRO_FUNC(x, y) y + x

#if MY_MACRO_FUNC(1, 2) == 3

// main function
int main() {
  int var = 10;
  int *ptr = &var;
  printf("Hello, World!\n");
  *ptr = MY_MACRO_FUNC(40, 60);
  printf("%p, %d, %d\n", (void *)ptr, *ptr, var);

  printf("%d\n", MY_MACRO_FUNC(10, 20));

  // print fibonacci sequence
  printf("%s\n", "Fibonacci sequence:");
  int i = 1;
  for (i = 1; i <= 10; i++) {
    printf("for i = %d, ", i);
    printf("%d\n", fibonacci(i));
  }

  return 0;
}

// definition of function fibonacci sequence using recursion
int fibonacci(int n) {
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
   0: Identifier("extern") Whitespace Identifier("int") Whitespace Identifier("printf") LeftParen Identifier("const") Whitespace Identifier("char") Whitespace Star Identifier("fmt") Comma Whitespace Ellipsis RightParen SemiColon 
   1: 
   2: 
   3: Identifier("int") Whitespace Identifier("fibonacci") LeftParen Identifier("int") RightParen SemiColon 
   4: 
   5: PreprocessorDefine Whitespace Identifier("MY_MACRO_FUNC") LeftParen Identifier("x") Comma Whitespace Identifier("y") RightParen Whitespace Identifier("y") Whitespace Plus Whitespace Identifier("x") 
   6: 
   7: PreprocessorIf Whitespace Identifier("MY_MACRO_FUNC") LeftParen ConstantInteger(1) Comma Whitespace ConstantInteger(2) RightParen Whitespace EqOp Whitespace ConstantInteger(3) 
   8: 
   9: 
  10: Identifier("int") Whitespace Identifier("main") LeftParen RightParen Whitespace LeftBrace 
  11: Whitespace Identifier("int") Whitespace Identifier("var") Whitespace Equal Whitespace ConstantInteger(10) SemiColon 
  12: Whitespace Identifier("int") Whitespace Star Identifier("ptr") Whitespace Equal Whitespace Ampersand Identifier("var") SemiColon 
  13: Whitespace Identifier("printf") LeftParen StringLiteral("Hello, World!\n") RightParen SemiColon 
  14: Whitespace Star Identifier("ptr") Whitespace Equal Whitespace Identifier("MY_MACRO_FUNC") LeftParen ConstantInteger(40) Comma Whitespace ConstantInteger(60) RightParen SemiColon 
  15: Whitespace Identifier("printf") LeftParen StringLiteral("%p, %d, %d\n") Comma Whitespace LeftParen Identifier("void") Whitespace Star RightParen Identifier("ptr") Comma Whitespace Star Identifier("ptr") Comma Whitespace Identifier("var") RightParen SemiColon 
  16: 
  17: Whitespace Identifier("printf") LeftParen StringLiteral("%d\n") Comma Whitespace Identifier("MY_MACRO_FUNC") LeftParen ConstantInteger(10) Comma Whitespace ConstantInteger(20) RightParen RightParen SemiColon 
  18: 
  19: Whitespace 
  20: Whitespace Identifier("printf") LeftParen StringLiteral("%s\n") Comma Whitespace StringLiteral("Fibonacci sequence:") RightParen SemiColon 
  21: Whitespace Identifier("int") Whitespace Identifier("i") Whitespace Equal Whitespace ConstantInteger(1) SemiColon 
  22: Whitespace Identifier("for") Whitespace LeftParen Identifier("i") Whitespace Equal Whitespace ConstantInteger(1) SemiColon Whitespace Identifier("i") Whitespace LeOp Whitespace ConstantInteger(10) SemiColon Whitespace Identifier("i") IncOp RightParen Whitespace LeftBrace 
  23: Whitespace Identifier("printf") LeftParen StringLiteral("for i = %d, ") Comma Whitespace Identifier("i") RightParen SemiColon 
  24: Whitespace Identifier("printf") LeftParen StringLiteral("%d\n") Comma Whitespace Identifier("fibonacci") LeftParen Identifier("i") RightParen RightParen SemiColon 
  25: Whitespace RightBrace 
  26: 
  27: Whitespace Identifier("return") Whitespace ConstantInteger(0) SemiColon 
  28: RightBrace 
  29: 
  30: 
  31: Identifier("int") Whitespace Identifier("fibonacci") LeftParen Identifier("int") Whitespace Identifier("n") RightParen Whitespace LeftBrace 
  32: Whitespace Identifier("if") Whitespace LeftParen Identifier("n") Whitespace LeOp Whitespace ConstantInteger(2) RightParen 
  33: Whitespace Identifier("return") Whitespace ConstantInteger(1) SemiColon 
  34: Whitespace Identifier("else") 
  35: Whitespace Identifier("return") Whitespace Identifier("fibonacci") LeftParen Identifier("n") Whitespace Minus Whitespace ConstantInteger(1) RightParen Whitespace Plus Whitespace Identifier("fibonacci") LeftParen Identifier("n") Whitespace Minus Whitespace ConstantInteger(2) RightParen SemiColon 
  36: RightBrace 
  37: 
  38: PreprocessorElse 
  39: 
  40: Identifier("THIS") Whitespace Identifier("WILL") Whitespace Identifier("BE") Whitespace Identifier("IGNORED") 
  41: 
  42: PreprocessorEndIf 
================================================================================
=============================Phase2: Line Analysis==============================
================================================================================
LINE | ---------------------------------Result----------------------------------
   0: RawTokens { tokens: [Identifier("extern"), Identifier("int"), Identifier("printf"), LeftParen, Identifier("const"), Identifier("char"), Star, Identifier("fmt"), Comma, Ellipsis, RightParen, SemiColon] }
   1: RawTokens { tokens: [Identifier("int"), Identifier("fibonacci"), LeftParen, Identifier("int"), RightParen, SemiColon] }
   2: DefineFunction { name: "MY_MACRO_FUNC", param_count: 2, replacement: [PreprocessorPlaceholder(1), Plus, PreprocessorPlaceholder(0)] }
   3: If { expression_tokens: [Identifier("MY_MACRO_FUNC"), LeftParen, ConstantInteger(1), Comma, ConstantInteger(2), RightParen, EqOp, ConstantInteger(3)] }
   4: RawTokens { tokens: [Identifier("int"), Identifier("main"), LeftParen, RightParen, LeftBrace] }
   5: RawTokens { tokens: [Identifier("int"), Identifier("var"), Equal, ConstantInteger(10), SemiColon] }
   6: RawTokens { tokens: [Identifier("int"), Star, Identifier("ptr"), Equal, Ampersand, Identifier("var"), SemiColon] }
   7: RawTokens { tokens: [Identifier("printf"), LeftParen, StringLiteral("Hello, World!\n"), RightParen, SemiColon] }
   8: RawTokens { tokens: [Star, Identifier("ptr"), Equal, Identifier("MY_MACRO_FUNC"), LeftParen, ConstantInteger(40), Comma, ConstantInteger(60), RightParen, SemiColon] }
   9: RawTokens { tokens: [Identifier("printf"), LeftParen, StringLiteral("%p, %d, %d\n"), Comma, LeftParen, Identifier("void"), Star, RightParen, Identifier("ptr"), Comma, Star, Identifier("ptr"), Comma, Identifier("var"), RightParen, SemiColon] }
  10: RawTokens { tokens: [Identifier("printf"), LeftParen, StringLiteral("%d\n"), Comma, Identifier("MY_MACRO_FUNC"), LeftParen, ConstantInteger(10), Comma, ConstantInteger(20), RightParen, RightParen, SemiColon] }
  11: RawTokens { tokens: [Identifier("printf"), LeftParen, StringLiteral("%s\n"), Comma, StringLiteral("Fibonacci sequence:"), RightParen, SemiColon] }
  12: RawTokens { tokens: [Identifier("int"), Identifier("i"), Equal, ConstantInteger(1), SemiColon] }
  13: RawTokens { tokens: [Identifier("for"), LeftParen, Identifier("i"), Equal, ConstantInteger(1), SemiColon, Identifier("i"), LeOp, ConstantInteger(10), SemiColon, Identifier("i"), IncOp, RightParen, LeftBrace] }
  14: RawTokens { tokens: [Identifier("printf"), LeftParen, StringLiteral("for i = %d, "), Comma, Identifier("i"), RightParen, SemiColon] }
  15: RawTokens { tokens: [Identifier("printf"), LeftParen, StringLiteral("%d\n"), Comma, Identifier("fibonacci"), LeftParen, Identifier("i"), RightParen, RightParen, SemiColon] }
  16: RawTokens { tokens: [RightBrace] }
  17: RawTokens { tokens: [Identifier("return"), ConstantInteger(0), SemiColon] }
  18: RawTokens { tokens: [RightBrace] }
  19: RawTokens { tokens: [Identifier("int"), Identifier("fibonacci"), LeftParen, Identifier("int"), Identifier("n"), RightParen, LeftBrace] }
  20: RawTokens { tokens: [Identifier("if"), LeftParen, Identifier("n"), LeOp, ConstantInteger(2), RightParen] }
  21: RawTokens { tokens: [Identifier("return"), ConstantInteger(1), SemiColon] }
  22: RawTokens { tokens: [Identifier("else")] }
  23: RawTokens { tokens: [Identifier("return"), Identifier("fibonacci"), LeftParen, Identifier("n"), Minus, ConstantInteger(1), RightParen, Plus, Identifier("fibonacci"), LeftParen, Identifier("n"), Minus, ConstantInteger(2), RightParen, SemiColon] }
  24: RawTokens { tokens: [RightBrace] }
  25: Else
  26: RawTokens { tokens: [Identifier("THIS"), Identifier("WILL"), Identifier("BE"), Identifier("IGNORED")] }
  27: EndIf
================================================================================
=============================Phase3: Preprocessing==============================
================================================================================
LINE | ---------------------------------Result----------------------------------
   0: [Extern, Int, Identifier("printf"), LeftParen, Const, Char, Star, Identifier("fmt"), Comma, Ellipsis, RightParen, SemiColon]
   1: [Int, Identifier("fibonacci"), LeftParen, Int, RightParen, SemiColon]
   2: [Int, Identifier("main"), LeftParen, RightParen, LeftBrace]
   3: [Int, Identifier("var"), Equal, ConstantInteger(10), SemiColon]
   4: [Int, Star, Identifier("ptr"), Equal, Ampersand, Identifier("var"), SemiColon]
   5: [Identifier("printf"), LeftParen, StringLiteral("Hello, World!\n"), RightParen, SemiColon]
   6: [Star, Identifier("ptr"), Equal, ConstantInteger(60), Plus, ConstantInteger(40), SemiColon]
   7: [Identifier("printf"), LeftParen, StringLiteral("%p, %d, %d\n"), Comma, LeftParen, Void, Star, RightParen, Identifier("ptr"), Comma, Star, Identifier("ptr"), Comma, Identifier("var"), RightParen, SemiColon]
   8: [Identifier("printf"), LeftParen, StringLiteral("%d\n"), Comma, ConstantInteger(20), Plus, ConstantInteger(10), RightParen, SemiColon]
   9: [Identifier("printf"), LeftParen, StringLiteral("%s\n"), Comma, StringLiteral("Fibonacci sequence:"), RightParen, SemiColon]
  10: [Int, Identifier("i"), Equal, ConstantInteger(1), SemiColon]
  11: [For, LeftParen, Identifier("i"), Equal, ConstantInteger(1), SemiColon, Identifier("i"), LeOp, ConstantInteger(10), SemiColon, Identifier("i"), IncOp, RightParen, LeftBrace]
  12: [Identifier("printf"), LeftParen, StringLiteral("for i = %d, "), Comma, Identifier("i"), RightParen, SemiColon]
  13: [Identifier("printf"), LeftParen, StringLiteral("%d\n"), Comma, Identifier("fibonacci"), LeftParen, Identifier("i"), RightParen, RightParen, SemiColon]
  14: [RightBrace]
  15: [Return, ConstantInteger(0), SemiColon]
  16: [RightBrace]
  17: [Int, Identifier("fibonacci"), LeftParen, Int, Identifier("n"), RightParen, LeftBrace]
  18: [If, LeftParen, Identifier("n"), LeOp, ConstantInteger(2), RightParen]
  19: [Return, ConstantInteger(1), SemiColon]
  20: [Else]
  21: [Return, Identifier("fibonacci"), LeftParen, Identifier("n"), Minus, ConstantInteger(1), RightParen, Plus, Identifier("fibonacci"), LeftParen, Identifier("n"), Minus, ConstantInteger(2), RightParen, SemiColon]
  22: [RightBrace]
================================================================================
======================Phase4: Building AbstractSyntaxTree=======================
================================================================================
ASTs: 
TranslationUnit {
    statements: [
        Declaration(
            StmtDeclaration {
                specs: [
                    StorageClassSpecifier(
                        Extern,
                    ),
                    TypeSpecifier(
                        Int,
                    ),
                ],
                inits: Some(
                    [
                        DeclInit {
                            declarator: Function(
                                ...
                                ...
                                ...
                                                            ),
                                                        },
                                                    ),
                                                ),
                                            },
                                        ),
                                    ),
                                },
                            ),
                        ],
                    },
                ),
            },
        ),
    ],
}
================================================================================
===========================Phase5: Semantic Analysis============================
================================================================================
TranslationUnit {
    statements: [],
    variables: {
        "printf": VariableInfo {
            name: "printf",
            uid: 1,
            cv_type: CVType {
                type_: Function(
                    FunctionType {
                        return_type: CVType {
                            type_: Integer(
                                Int32,
                            ),
                            const_: false,
                            volatile: false,
                        },
                        args: [
                            CombinedDeclarator {
                                name: Some(
                                    "fmt",
                                ),
                                cv_type: CVType {
                                    type_: Pointer(
                                        ...
                                        ...
                                        ...
                                                        ),
                                                    },
                                                ),
                                            ),
                                        },
                                    ),
                                ),
                            },
                        ),
                    ],
                },
            ),
            type_: FunctionType {
                return_type: CVType {
                    type_: Integer(
                        Int32,
                    ),
                    const_: false,
                    volatile: false,
                },
                args: [
                    CombinedDeclarator {
                        name: Some(
                            "n",
                        ),
                        cv_type: CVType {
                            type_: Integer(
                                Int32,
                            ),
                            const_: false,
                            volatile: false,
                        },
                    },
                ],
                variadic: false,
            },
            uid: 2,
            args: [
                VariableInfo {
                    name: "n",
                    uid: 7,
                    cv_type: CVType {
                        type_: Integer(
                            Int32,
                        ),
                        const_: false,
                        volatile: false,
                    },
                    storage: None,
                },
            ],
        },
    },
}
================================================================================
========================Phase6: Generating Instructions=========================
================================================================================
; ModuleID = 'main_module'
source_filename = "main_module"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"

@str = private unnamed_addr constant [15 x i8] c"Hello, World!\0A\00", align 1
@str.1 = private unnamed_addr constant [12 x i8] c"%p, %d, %d\0A\00", align 1
@str.2 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@str.3 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@str.4 = private unnamed_addr constant [20 x i8] c"Fibonacci sequence:\00", align 1
@str.5 = private unnamed_addr constant [13 x i8] c"for i = %d, \00", align 1
@str.6 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

declare i32 @printf(i8* %0, ...)

define i32 @fibonacci(i32 %0) {
func_block:
  %n = alloca i32, align 4
  store i32 %0, i32* %n, align 4
  %load = load i32, i32* %n, align 4
  %sle = icmp sle i32 %load, 2
  %zext = zext i1 %sle to i8
  %ifcond = icmp ne i8 %zext, 0
  br i1 %ifcond, label %then_block, label %else_block

then_block:                                       ; preds = %func_block
  ret i32 1

else_block:                                       ; preds = %func_block
  %load1 = load i32, i32* %n, align 4
  %sub = sub i32 %load1, 1
  %function_call = call i32 @fibonacci(i32 %sub)
  %load2 = load i32, i32* %n, align 4
  %sub3 = sub i32 %load2, 2
  %function_call4 = call i32 @fibonacci(i32 %sub3)
  %add = add i32 %function_call, %function_call4
  ret i32 %add

merge_block:                                      ; No predecessors!
}

define i32 @main() {
func_block:
  %var = alloca i32, align 4
  store i32 10, i32* %var, align 4
  %ptr = alloca i32*, align 8
  store i32* %var, i32** %ptr, align 8
  %function_call = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([15 x i8], [15 x i8]* @str, i32 0, i32 0))
  %load = load i32*, i32** %ptr, align 8
  store i32 100, i32* %load, align 4
  %load1 = load i32*, i32** %ptr, align 8
  %ptr_ptr = bitcast i32* %load1 to i8*
  %load2 = load i32*, i32** %ptr, align 8
  %load3 = load i32, i32* %load2, align 4
  %load4 = load i32, i32* %var, align 4
  %function_call5 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @str.1, i32 0, i32 0), i8* %ptr_ptr, i32 %load3, i32 %load4)
  %function_call6 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @str.2, i32 0, i32 0), i32 30)
  %function_call7 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @str.3, i32 0, i32 0), i8* getelementptr inbounds ([20 x i8], [20 x i8]* @str.4, i32 0, i32 0))
  %i = alloca i32, align 4
  store i32 1, i32* %i, align 4
  store i32 1, i32* %i, align 4
  br label %body_block

body_block:                                       ; preds = %continue_block, %func_block
  %load8 = load i32, i32* %i, align 4
  %function_call9 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([13 x i8], [13 x i8]* @str.5, i32 0, i32 0), i32 %load8)
  %load10 = load i32, i32* %i, align 4
  %function_call11 = call i32 @fibonacci(i32 %load10)
  %function_call12 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @str.6, i32 0, i32 0), i32 %function_call11)
  br label %continue_block

continue_block:                                   ; preds = %body_block
  %load13 = load i32, i32* %i, align 4
  %inc = add i32 %load13, 1
  store i32 %inc, i32* %i, align 4
  %load14 = load i32, i32* %i, align 4
  %sle = icmp sle i32 %load14, 10
  %zext = zext i1 %sle to i8
  %ifcond = icmp ne i8 %zext, 0
  br i1 %ifcond, label %body_block, label %break_block

break_block:                                      ; preds = %continue_block
  ret i32 0
}


Program returned: 0
Hello, World!
0x7ffc4f2c0a04, 100, 100
30
Fibonacci sequence:
for i = 1, 1
for i = 2, 1
for i = 3, 2
for i = 4, 3
for i = 5, 5
for i = 6, 8
for i = 7, 13
for i = 8, 21
for i = 9, 34
for i = 10, 55
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