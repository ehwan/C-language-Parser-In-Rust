# minimal C language lexer & parser & virtual executer written in Rust

C language lexer & parser & virtual executer from scratch in Rust.

## syntax not supported
 - Preprocessor
 - `union` `enum`
 - type qualifiers (`const`, `volatile`, `restrict` `static` `extern`) will be ignored
 - `typedef`

## Features
 - Tokenizer (Lexer)
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

// fibonacci sequence using recursion declaration
int fibonacci(int);

// main function
int main()
{
  print_str("Hello, World!"); // built in function 'print_str'
  int var = 10;
  int* ptr = &var;
  *ptr = 100;
  print(ptr, *ptr, var); // built in function 'print'

  // print fibonacci sequence
  print_str("Fibonacci sequence:");
  int i;
  for (i = 1; i <= 10; i++)
  {
    print(i, fibonacci(i));
  }

  return 0;
}

// fibonacci sequence using recursion definition
int fibonacci(int n)
{
  if (n <= 2)
    return 1;
  else
    return fibonacci(n - 1) + fibonacci(n - 2);
}
```

Pass c code to stdin
```sh
cat samples/sample.c | cargo run
```

The result will be:

```
Enter your code (and ^D for EOF):
============================ Tokenizing ============================
Tokens: 
   0: Int
   1: Identifier("fibonacci")
   2: LeftParen
   3: Int
   4: RightParen
   5: SemiColon
   6: Int
   7: Identifier("main")
   8: LeftParen
   9: RightParen
  10: LeftBrace
  11: Identifier("print_str")
  12: LeftParen
  13: StringLiteral("Hello, World!")
  14: RightParen
  15: SemiColon
  16: Int
  17: Identifier("var")
  18: Equal
  19: ConstantInteger(10)
  20: SemiColon
  21: Int
  22: Star
  23: Identifier("ptr")
  24: Equal
  25: Ampersand
  26: Identifier("var")
  27: SemiColon
  28: Star
  29: Identifier("ptr")
  30: Equal
  31: ConstantInteger(100)
  32: SemiColon
  33: Identifier("print")
  34: LeftParen
  35: Identifier("ptr")
  36: Comma
  37: Star
  38: Identifier("ptr")
  39: Comma
  40: Identifier("var")
  41: RightParen
  42: SemiColon
  43: Identifier("print_str")
  44: LeftParen
  45: StringLiteral("Fibonacci sequence:")
  46: RightParen
  47: SemiColon
  48: Int
  49: Identifier("i")
  50: SemiColon
  51: For
  52: LeftParen
  53: Identifier("i")
  54: Equal
  55: ConstantInteger(1)
  56: SemiColon
  57: Identifier("i")
  58: LeOp
  59: ConstantInteger(10)
  60: SemiColon
  61: Identifier("i")
  62: IncOp
  63: RightParen
  64: LeftBrace
  65: Identifier("print")
  66: LeftParen
  67: Identifier("i")
  68: Comma
  69: Identifier("fibonacci")
  70: LeftParen
  71: Identifier("i")
  72: RightParen
  73: RightParen
  74: SemiColon
  75: RightBrace
  76: Return
  77: ConstantInteger(0)
  78: SemiColon
  79: RightBrace
  80: Int
  81: Identifier("fibonacci")
  82: LeftParen
  83: Int
  84: Identifier("n")
  85: RightParen
  86: LeftBrace
  87: If
  88: LeftParen
  89: Identifier("n")
  90: LeOp
  91: ConstantInteger(2)
  92: RightParen
  93: Return
  94: ConstantInteger(1)
  95: SemiColon
  96: Else
  97: Return
  98: Identifier("fibonacci")
  99: LeftParen
 100: Identifier("n")
 101: Minus
 102: ConstantInteger(1)
 103: RightParen
 104: Plus
 105: Identifier("fibonacci")
 106: LeftParen
 107: Identifier("n")
 108: Minus
 109: ConstantInteger(2)
 110: RightParen
 111: SemiColon
 112: RightBrace
============================ Building AST ============================
ASTs: 
TranslationUnit { statements: [DeclarationStatement { vars: [(Some("fibonacci"), Function(Int32, [Int32]), None)] }, FunctionDefinitionStatement { return_type: Int32, name: "main", params: [], body: CompoundStatement { statements: [ExpressionStatement { expression: PostParen { src: PrimaryIdentifier { name: "print_str" }, args: [StringLiteral { value: "Hello, World!" }] } }, DeclarationStatement { vars: [(Some("var"), Int32, Some(ConstantInteger { value: 10 }))] }, DeclarationStatement { vars: [(Some("ptr"), Pointer(Int32), Some(UnaryExpression { op: AddressOf, src: PrimaryIdentifier { name: "var" } }))] }, ExpressionStatement { expression: AssignExpression { op: Assign, lhs: UnaryExpression { op: Dereference, src: PrimaryIdentifier { name: "ptr" } }, rhs: ConstantInteger { value: 100 } } }, ExpressionStatement { expression: PostParen { src: PrimaryIdentifier { name: "print" }, args: [PrimaryIdentifier { name: "ptr" }, UnaryExpression { op: Dereference, src: PrimaryIdentifier { name: "ptr" } }, PrimaryIdentifier { name: "var" }] } }, ExpressionStatement { expression: PostParen { src: PrimaryIdentifier { name: "print_str" }, args: [StringLiteral { value: "Fibonacci sequence:" }] } }, DeclarationStatement { vars: [(Some("i"), Int32, None)] }, ForStatement { init: AssignExpression { op: Assign, lhs: PrimaryIdentifier { name: "i" }, rhs: ConstantInteger { value: 1 } }, cond: ComparisonExpression { op: LessThanOrEqual, lhs: PrimaryIdentifier { name: "i" }, rhs: ConstantInteger { value: 10 } }, next: Some(PostIncrement { src: PrimaryIdentifier { name: "i" } }), statement: CompoundStatement { statements: [ExpressionStatement { expression: PostParen { src: PrimaryIdentifier { name: "print" }, args: [PrimaryIdentifier { name: "i" }, PostParen { src: PrimaryIdentifier { name: "fibonacci" }, args: [PrimaryIdentifier { name: "i" }] }] } }] } }, ReturnStatement { expr: Some(ConstantInteger { value: 0 }) }] } }, FunctionDefinitionStatement { return_type: Int32, name: "fibonacci", params: [(Some("n"), Int32)], body: CompoundStatement { statements: [IfStatement { cond: ComparisonExpression { op: LessThanOrEqual, lhs: PrimaryIdentifier { name: "n" }, rhs: ConstantInteger { value: 2 } }, then_statement: ReturnStatement { expr: Some(ConstantInteger { value: 1 }) }, else_statement: Some(ReturnStatement { expr: Some(AdditiveExpression { op: Add, lhs: PostParen { src: PrimaryIdentifier { name: "fibonacci" }, args: [AdditiveExpression { op: Sub, lhs: PrimaryIdentifier { name: "n" }, rhs: ConstantInteger { value: 1 } }] }, rhs: PostParen { src: PrimaryIdentifier { name: "fibonacci" }, args: [AdditiveExpression { op: Sub, lhs: PrimaryIdentifier { name: "n" }, rhs: ConstantInteger { value: 2 } }] } }) }) }] } }] }
============================ Generating Instructions ============================
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
  12: MoveRegister { operand_from: Value(Int32(100)), operand_to: Register(0) }
  13: PushStack { operand: Register(0) }
  14: MoveRegister { operand_from: Register(5), operand_to: Register(0) }
  15: AddAssign { lhs: Register(0), rhs: Value(Int64(1)) }
  16: MoveRegister { operand_from: Derefed(0, 0), operand_to: Register(0) }
  17: PopStack { operand: Register(1) }
  18: Assign { lhs_type: Int32, lhs: Derefed(0, 0), rhs: Register(1) }
  19: MoveRegister { operand_from: Register(5), operand_to: Register(0) }
  20: AddAssign { lhs: Register(0), rhs: Value(Int64(0)) }
  21: PushStack { operand: Derefed(0, 0) }
  22: MoveRegister { operand_from: Register(5), operand_to: Register(0) }
  23: AddAssign { lhs: Register(0), rhs: Value(Int64(1)) }
  24: MoveRegister { operand_from: Derefed(0, 0), operand_to: Register(0) }
  25: PushStack { operand: Derefed(0, 0) }
  26: MoveRegister { operand_from: Register(5), operand_to: Register(0) }
  27: AddAssign { lhs: Register(0), rhs: Value(Int64(1)) }
  28: PushStack { operand: Derefed(0, 0) }
  29: PushStack { operand: Value(UInt64(3)) }
  30: Print
  31: MoveRegister { operand_from: Value(UInt64(14)), operand_to: Register(0) }
  32: PrintStr { str: Register(0) }
  33: PushStack { operand: Value(Int32(0)) }
  34: MoveRegister { operand_from: Value(Int32(1)), operand_to: Register(0) }
  35: PushStack { operand: Register(0) }
  36: MoveRegister { operand_from: Register(5), operand_to: Register(0) }
  37: AddAssign { lhs: Register(0), rhs: Value(Int64(2)) }
  38: PopStack { operand: Register(1) }
  39: Assign { lhs_type: Int32, lhs: Derefed(0, 0), rhs: Register(1) }
  40: DefineLabel { label: ".__L0__" }
  41: MoveRegister { operand_from: Register(5), operand_to: Register(0) }
  42: AddAssign { lhs: Register(0), rhs: Value(Int64(2)) }
  43: PushStack { operand: Derefed(0, 0) }
  44: MoveRegister { operand_from: Value(Int32(10)), operand_to: Register(0) }
  45: PopStack { operand: Register(1) }
  46: LessThan { lhs: Register(0), rhs: Register(1), to: Register(0) }
  47: LogicalNot { operand: Register(0) }
  48: JumpZero { label: ".__L1__", operand_cond: Register(0) }
  49: MoveRegister { operand_from: Register(5), operand_to: Register(0) }
  50: AddAssign { lhs: Register(0), rhs: Value(Int64(2)) }
  51: PushStack { operand: Derefed(0, 0) }
  52: Call { label: "fibonacci" }
  53: SubAssign { lhs: Register(6), rhs: Value(UInt64(1)) }
  54: PushStack { operand: Register(0) }
  55: MoveRegister { operand_from: Register(5), operand_to: Register(0) }
  56: AddAssign { lhs: Register(0), rhs: Value(Int64(2)) }
  57: PushStack { operand: Derefed(0, 0) }
  58: PushStack { operand: Value(UInt64(2)) }
  59: Print
  60: DefineLabel { label: ".__L2__" }
  61: MoveRegister { operand_from: Register(5), operand_to: Register(0) }
  62: AddAssign { lhs: Register(0), rhs: Value(Int64(2)) }
  63: MoveRegister { operand_from: Register(0), operand_to: Register(1) }
  64: MoveRegister { operand_from: Derefed(1, 0), operand_to: Register(0) }
  65: AddAssign { lhs: Derefed(1, 0), rhs: Value(UInt8(1)) }
  66: Jump { label: ".__L0__" }
  67: DefineLabel { label: ".__L1__" }
  68: MoveRegister { operand_from: Value(Int32(0)), operand_to: Register(0) }
  69: Return
  70: Panic { message: "Function main must return a Int32 value" }
  71: DefineLabel { label: "fibonacci" }
  72: PushStack { operand: Register(5) }
  73: MoveRegister { operand_from: Register(6), operand_to: Register(5) }
  74: MoveRegister { operand_from: Register(5), operand_to: Register(0) }
  75: AddAssign { lhs: Register(0), rhs: Value(Int64(-3)) }
  76: PushStack { operand: Derefed(0, 0) }
  77: MoveRegister { operand_from: Value(Int32(2)), operand_to: Register(0) }
  78: PopStack { operand: Register(1) }
  79: LessThan { lhs: Register(0), rhs: Register(1), to: Register(0) }
  80: LogicalNot { operand: Register(0) }
  81: JumpZero { label: ".__L3__", operand_cond: Register(0) }
  82: MoveRegister { operand_from: Value(Int32(1)), operand_to: Register(0) }
  83: Return
  84: Jump { label: ".__L4__" }
  85: DefineLabel { label: ".__L3__" }
  86: MoveRegister { operand_from: Value(Int32(2)), operand_to: Register(0) }
  87: PushStack { operand: Register(0) }
  88: MoveRegister { operand_from: Register(5), operand_to: Register(0) }
  89: AddAssign { lhs: Register(0), rhs: Value(Int64(-3)) }
  90: Assign { lhs_type: UInt32, lhs: Register(0), rhs: Derefed(0, 0) }
  91: PopStack { operand: Register(1) }
  92: SubAssign { lhs: Register(0), rhs: Register(1) }
  93: PushStack { operand: Register(0) }
  94: Call { label: "fibonacci" }
  95: SubAssign { lhs: Register(6), rhs: Value(UInt64(1)) }
  96: PushStack { operand: Register(0) }
  97: MoveRegister { operand_from: Value(Int32(1)), operand_to: Register(0) }
  98: PushStack { operand: Register(0) }
  99: MoveRegister { operand_from: Register(5), operand_to: Register(0) }
 100: AddAssign { lhs: Register(0), rhs: Value(Int64(-3)) }
 101: Assign { lhs_type: UInt32, lhs: Register(0), rhs: Derefed(0, 0) }
 102: PopStack { operand: Register(1) }
 103: SubAssign { lhs: Register(0), rhs: Register(1) }
 104: PushStack { operand: Register(0) }
 105: Call { label: "fibonacci" }
 106: SubAssign { lhs: Register(6), rhs: Value(UInt64(1)) }
 107: Assign { lhs_type: UInt32, lhs: Register(0), rhs: Register(0) }
 108: PopStack { operand: Register(1) }
 109: AddAssign { lhs: Register(0), rhs: Register(1) }
 110: Return
 111: DefineLabel { label: ".__L4__" }
 112: Panic { message: "Function fibonacci must return a Int32 value" }
      -------------------- Start Address ---------------------
 113: Call { label: "main" }
============================ Executing Instructions ============================
"Hello, World!"
Print: UInt64(36), Int32(100), Int32(100), 
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