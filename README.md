# minimal C language lexer & parser & virtual executer written in Rust

minimal C language lexer & parser & virtual executer from scratch in Rust

A lot of language features are not implemented yet. 

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
// fibonacci sequence using recursion
int fibonacci(int);

int main()
{
  int var = 10; // Declare an integer variable and initialize it
  int* ptr; // Declare a pointer to an integer

  ptr = &var; // Store the address of var in the pointer ptr

  // Print the value of var
  print(var); // special built in function 'print'

  // Print the address of var using the pointer
  print(ptr); // this will print stack index (address of var)

  // Print the value stored at the address pointed to by ptr
  *ptr = 100;
  print(var); // this will print 100

  // print fibonacci sequence
  int i;
  for (i = 1; i <= 10; i++)
  {
    print(i, fibonacci(i));
  }

  return 0;
}

// fibonacci sequence using recursion
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
  11: Int
  12: Identifier("var")
  13: Equal
  14: ConstantInteger(10)
  15: SemiColon
  16: Int
  17: Star
  18: Identifier("ptr")
  19: SemiColon
  20: Identifier("ptr")
  21: Equal
  22: Ampersand
  23: Identifier("var")
  24: SemiColon
  25: Identifier("print")
  26: LeftParen
  27: Identifier("var")
  28: RightParen
  29: SemiColon
  30: Identifier("print")
  31: LeftParen
  32: Identifier("ptr")
  33: RightParen
  34: SemiColon
  35: Star
  36: Identifier("ptr")
  37: Equal
  38: ConstantInteger(100)
  39: SemiColon
  40: Identifier("print")
  41: LeftParen
  42: Identifier("var")
  43: RightParen
  44: SemiColon
  45: Int
  46: Identifier("i")
  47: SemiColon
  48: For
  49: LeftParen
  50: Identifier("i")
  51: Equal
  52: ConstantInteger(1)
  53: SemiColon
  54: Identifier("i")
  55: LeOp
  56: ConstantInteger(10)
  57: SemiColon
  58: Identifier("i")
  59: IncOp
  60: RightParen
  61: LeftBrace
  62: Identifier("print")
  63: LeftParen
  64: Identifier("i")
  65: Comma
  66: Identifier("fibonacci")
  67: LeftParen
  68: Identifier("i")
  69: RightParen
  70: RightParen
  71: SemiColon
  72: RightBrace
  73: Return
  74: ConstantInteger(0)
  75: SemiColon
  76: RightBrace
  77: Int
  78: Identifier("fibonacci")
  79: LeftParen
  80: Int
  81: Identifier("n")
  82: RightParen
  83: LeftBrace
  84: If
  85: LeftParen
  86: Identifier("n")
  87: LeOp
  88: ConstantInteger(2)
  89: RightParen
  90: Return
  91: ConstantInteger(1)
  92: SemiColon
  93: Else
  94: Return
  95: Identifier("fibonacci")
  96: LeftParen
  97: Identifier("n")
  98: Minus
  99: ConstantInteger(1)
 100: RightParen
 101: Plus
 102: Identifier("fibonacci")
 103: LeftParen
 104: Identifier("n")
 105: Minus
 106: ConstantInteger(2)
 107: RightParen
 108: SemiColon
 109: RightBrace
============================ Building AST ============================
ASTs:
TranslationUnit { statements: [DeclarationStatement { vars: [(Some("fibonacci"), Function(Int32, [Int32]), None)] }, FunctionDefinitionStatement { return_type: Int32, name: "main", params: [], body: CompoundStatement { statements: [DeclarationStatement { vars: [(Some("var"), Int32, Some(ConstantInteger { value: 10 }))] }, DeclarationStatement { vars: [(Some("ptr"), Pointer(Int32), None)] }, ExpressionStatement { expression: BinaryExpression { op: Assign, lhs: PrimaryIdentifier { name: "ptr" }, rhs: UnaryExpression { op: AddressOf, src: PrimaryIdentifier { name: "var" } } } }, ExpressionStatement { expression: PostParen { src: PrimaryIdentifier { name: "print" }, args: [PrimaryIdentifier { name: "var" }] } }, ExpressionStatement { expression: PostParen { src: PrimaryIdentifier { name: "print" }, args: [PrimaryIdentifier { name: "ptr" }] } }, ExpressionStatement { expression: BinaryExpression { op: Assign, lhs: UnaryExpression { op: Dereference, src: PrimaryIdentifier { name: "ptr" } }, rhs: ConstantInteger { value: 100 } } }, ExpressionStatement { expression: PostParen { src: PrimaryIdentifier { name: "print" }, args: [PrimaryIdentifier { name: "var" }] } }, DeclarationStatement { vars: [(Some("i"), Int32, None)] }, ForStatement { init: BinaryExpression { op: Assign, lhs: PrimaryIdentifier { name: "i" }, rhs: ConstantInteger { value: 1 } }, cond: ComparisonExpression { op: LessThanOrEqual, lhs: PrimaryIdentifier { name: "i" }, rhs: ConstantInteger { value: 10 } }, next: Some(PostIncrement { src: PrimaryIdentifier { name: "i" } }), statement: CompoundStatement { statements: [ExpressionStatement { expression: PostParen { src: PrimaryIdentifier { name: "print" }, args: [PrimaryIdentifier { name: "i" }, PostParen { src: PrimaryIdentifier { name: "fibonacci" }, args: [PrimaryIdentifier { name: "i" }] }] } }] } }, ReturnStatement { expr: Some(ConstantInteger { value: 0 }) }] } }, FunctionDefinitionStatement { return_type: Int32, name: "fibonacci", params: [(Some("n"), Int32)], body: CompoundStatement { statements: [IfStatement { cond: ComparisonExpression { op: LessThanOrEqual, lhs: PrimaryIdentifier { name: "n" }, rhs: ConstantInteger { value: 2 } }, then_statement: ReturnStatement { expr: Some(ConstantInteger { value: 1 }) }, else_statement: Some(ReturnStatement { expr: Some(BinaryExpression { op: Add, lhs: PostParen { src: PrimaryIdentifier { name: "fibonacci" }, args: [BinaryExpression { op: Sub, lhs: PrimaryIdentifier { name: "n" }, rhs: ConstantInteger { value: 1 } }] }, rhs: PostParen { src: PrimaryIdentifier { name: "fibonacci" }, args: [BinaryExpression { op: Sub, lhs: PrimaryIdentifier { name: "n" }, rhs: ConstantInteger { value: 2 } }] } }) }) }] } }] }
============================ Generating Instructions ============================
Instructions:
   0: DefineLabel { label: "main" }
   1: PushStack { operand: Register(4) }
   2: MoveRegister { operand_from: Register(5), operand_to: Register(4) }
   3: MoveRegister { operand_from: Value(Int32(10)), operand_to: Register(0) }
   4: Assign { lhs_type: Int32, lhs: Register(1), rhs: Register(0) }
   5: PushStack { operand: Register(1) }
   6: MoveRegister { operand_from: Value(Pointer(0)), operand_to: Register(1) }
   7: PushStack { operand: Register(1) }
   8: MoveRegister { operand_from: Register(4), operand_to: Register(0) }
   9: AddAssign { lhs: Register(0), rhs: Value(Int64(0)) }
  10: AddressOf { operand_from: Register(0), operand_to: Register(0) }
  11: PushStack { operand: Register(0) }
  12: MoveRegister { operand_from: Register(4), operand_to: Register(0) }
  13: AddAssign { lhs: Register(0), rhs: Value(Int64(1)) }
  14: PopStack { operand: Register(2) }
  15: Assign { lhs_type: Pointer(Int32), lhs: Derefed(0, 0), rhs: Register(2) }
  16: MoveRegister { operand_from: Register(4), operand_to: Register(0) }
  17: AddAssign { lhs: Register(0), rhs: Value(Int64(0)) }
  18: PushStack { operand: Derefed(0, 0) }
  19: PushStack { operand: Value(UInt64(1)) }
  20: Print
  21: MoveRegister { operand_from: Register(4), operand_to: Register(0) }
  22: AddAssign { lhs: Register(0), rhs: Value(Int64(1)) }
  23: PushStack { operand: Derefed(0, 0) }
  24: PushStack { operand: Value(UInt64(1)) }
  25: Print
  26: MoveRegister { operand_from: Value(Int32(100)), operand_to: Register(0) }
  27: PushStack { operand: Register(0) }
  28: MoveRegister { operand_from: Register(4), operand_to: Register(0) }
  29: AddAssign { lhs: Register(0), rhs: Value(Int64(1)) }
  30: Dereference { operand_from: Derefed(0, 0), operand_to: Register(0) }
  31: PopStack { operand: Register(2) }
  32: Assign { lhs_type: Int32, lhs: Derefed(0, 0), rhs: Register(2) }
  33: MoveRegister { operand_from: Register(4), operand_to: Register(0) }
  34: AddAssign { lhs: Register(0), rhs: Value(Int64(0)) }
  35: PushStack { operand: Derefed(0, 0) }
  36: PushStack { operand: Value(UInt64(1)) }
  37: Print
  38: MoveRegister { operand_from: Value(Int32(0)), operand_to: Register(1) }
  39: PushStack { operand: Register(1) }
  40: MoveRegister { operand_from: Value(Int32(1)), operand_to: Register(0) }
  41: PushStack { operand: Register(0) }
  42: MoveRegister { operand_from: Register(4), operand_to: Register(0) }
  43: AddAssign { lhs: Register(0), rhs: Value(Int64(2)) }
  44: PopStack { operand: Register(2) }
  45: Assign { lhs_type: Int32, lhs: Derefed(0, 0), rhs: Register(2) }
  46: DefineLabel { label: ".__L0__" }
  47: MoveRegister { operand_from: Register(4), operand_to: Register(0) }
  48: AddAssign { lhs: Register(0), rhs: Value(Int64(2)) }
  49: PushStack { operand: Derefed(0, 0) }
  50: MoveRegister { operand_from: Value(Int32(10)), operand_to: Register(0) }
  51: PopStack { operand: Register(1) }
  52: LessThan { lhs: Register(0), rhs: Register(1), to: Register(0) }
  53: LogicalNot { operand: Register(0) }
  54: JumpZero { label: ".__L1__", operand_cond: Register(0) }
  55: MoveRegister { operand_from: Register(4), operand_to: Register(0) }
  56: AddAssign { lhs: Register(0), rhs: Value(Int64(2)) }
  57: PushStack { operand: Derefed(0, 0) }
  58: Call { label: "fibonacci" }
  59: SubAssign { lhs: Register(5), rhs: Value(UInt64(1)) }
  60: PushStack { operand: Register(0) }
  61: MoveRegister { operand_from: Register(4), operand_to: Register(0) }
  62: AddAssign { lhs: Register(0), rhs: Value(Int64(2)) }
  63: PushStack { operand: Derefed(0, 0) }
  64: PushStack { operand: Value(UInt64(2)) }
  65: Print
  66: DefineLabel { label: ".__L2__" }
  67: MoveRegister { operand_from: Register(4), operand_to: Register(0) }
  68: AddAssign { lhs: Register(0), rhs: Value(Int64(2)) }
  69: MoveRegister { operand_from: Register(0), operand_to: Register(1) }
  70: MoveRegister { operand_from: Derefed(1, 0), operand_to: Register(0) }
  71: Increment { operand: Derefed(1, 0) }
  72: Jump { label: ".__L0__" }
  73: DefineLabel { label: ".__L1__" }
  74: MoveRegister { operand_from: Value(Int32(0)), operand_to: Register(0) }
  75: Return
  76: Panic { message: "Function main must return a Int32 value" }
  77: DefineLabel { label: "fibonacci" }
  78: PushStack { operand: Register(4) }
  79: MoveRegister { operand_from: Register(5), operand_to: Register(4) }
  80: MoveRegister { operand_from: Register(4), operand_to: Register(0) }
  81: AddAssign { lhs: Register(0), rhs: Value(Int64(-3)) }
  82: PushStack { operand: Derefed(0, 0) }
  83: MoveRegister { operand_from: Value(Int32(2)), operand_to: Register(0) }
  84: PopStack { operand: Register(1) }
  85: LessThan { lhs: Register(0), rhs: Register(1), to: Register(0) }
  86: LogicalNot { operand: Register(0) }
  87: JumpZero { label: ".__L3__", operand_cond: Register(0) }
  88: MoveRegister { operand_from: Value(Int32(1)), operand_to: Register(0) }
  89: Return
  90: Jump { label: ".__L4__" }
  91: DefineLabel { label: ".__L3__" }
  92: MoveRegister { operand_from: Value(Int32(2)), operand_to: Register(0) }
  93: PushStack { operand: Register(0) }
  94: MoveRegister { operand_from: Register(4), operand_to: Register(0) }
  95: AddAssign { lhs: Register(0), rhs: Value(Int64(-3)) }
  96: PopStack { operand: Register(2) }
  97: MoveRegister { operand_from: Derefed(0, 0), operand_to: Register(1) }
  98: SubAssign { lhs: Register(1), rhs: Register(2) }
  99: MoveRegister { operand_from: Register(1), operand_to: Register(0) }
 100: PushStack { operand: Register(0) }
 101: Call { label: "fibonacci" }
 102: SubAssign { lhs: Register(5), rhs: Value(UInt64(1)) }
 103: PushStack { operand: Register(0) }
 104: MoveRegister { operand_from: Value(Int32(1)), operand_to: Register(0) }
 105: PushStack { operand: Register(0) }
 106: MoveRegister { operand_from: Register(4), operand_to: Register(0) }
 107: AddAssign { lhs: Register(0), rhs: Value(Int64(-3)) }
 108: PopStack { operand: Register(2) }
 109: MoveRegister { operand_from: Derefed(0, 0), operand_to: Register(1) }
 110: SubAssign { lhs: Register(1), rhs: Register(2) }
 111: MoveRegister { operand_from: Register(1), operand_to: Register(0) }
 112: PushStack { operand: Register(0) }
 113: Call { label: "fibonacci" }
 114: SubAssign { lhs: Register(5), rhs: Value(UInt64(1)) }
 115: PopStack { operand: Register(2) }
 116: MoveRegister { operand_from: Register(0), operand_to: Register(1) }
 117: AddAssign { lhs: Register(1), rhs: Register(2) }
 118: MoveRegister { operand_from: Register(1), operand_to: Register(0) }
 119: Return
 120: DefineLabel { label: ".__L4__" }
 121: Panic { message: "Function fibonacci must return a Int32 value" }
      -------------------- Start Address ---------------------
 122: Call { label: "main" }
============================ Executing Instructions ============================
Print: Int32(10),
Print: Pointer(2),
Print: Int32(100),
Print: Int32(1), Int32(1),
Print: Int32(2), Int32(1),
Print: Int32(3), Int32(2),
Print: Int32(4), Int32(3),
Print: Int32(5), Int32(5),
Print: Int32(6), Int32(8),
Print: Int32(7), Int32(13),
Print: Int32(8), Int32(21),
Print: Int32(9), Int32(34),
Print: Int32(10), Int32(55),
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
│       ├── DeclarationStatement
│       │   └── Vars
│       │       └── (Some("var"), Int32, Some(ConstantInteger(10)))
│       ├── DeclarationStatement
│       │   └── Vars
│       │       └── (Some("ptr"), Pointer(Int32), None)
│       ├── ExpressionStatement
│       │   └── BinaryExpression
│       │       ├── Op: Assign
│       │       ├── Lhs: PrimaryIdentifier("ptr")
│       │       └── Rhs: UnaryExpression(AddressOf, PrimaryIdentifier("var"))
│       ├── ExpressionStatement
│       │   └── PostParen
│       │       ├── Src: PrimaryIdentifier("print")
│       │       └── Args: [PrimaryIdentifier("var")]
│       ├── ExpressionStatement
│       │   └── PostParen
│       │       ├── Src: PrimaryIdentifier("print")
│       │       └── Args: [PrimaryIdentifier("ptr")]
│       ├── ExpressionStatement
│       │   └── BinaryExpression
│       │       ├── Op: Assign
│       │       ├── Lhs: UnaryExpression(Dereference, PrimaryIdentifier("ptr"))
│       │       └── Rhs: ConstantInteger(100)
│       ├── ExpressionStatement
│       │   └── PostParen
│       │       ├── Src: PrimaryIdentifier("print")
│       │       └── Args: [PrimaryIdentifier("var")]
│       ├── DeclarationStatement
│       │   └── Vars
│       │       └── (Some("i"), Int32, None)
│       ├── ForStatement
│       │   ├── Init: BinaryExpression
│       │   │   ├── Op: Assign
│       │   │   ├── Lhs: PrimaryIdentifier("i")
│       │   │   └── Rhs: ConstantInteger(1)
│       │   ├── Cond: ComparisonExpression
│       │   │   ├── Op: LessThanOrEqual
│       │   │   ├── Lhs: PrimaryIdentifier("i")
│       │   │   └── Rhs: ConstantInteger(10)
│       │   ├── Next: PostIncrement(PrimaryIdentifier("i"))
│       │   └── Statement: CompoundStatement
│       │       └── ExpressionStatement
│       │           └── PostParen
│       │               ├── Src: PrimaryIdentifier("print")
│       │               └── Args: [PrimaryIdentifier("i"), PostParen(PrimaryIdentifier("fibonacci"), [PrimaryIdentifier("i")])]
│       └── ReturnStatement
│           └── Expr: Some(ConstantInteger(0))
├── FunctionDefinitionStatement: fibonacci
│   ├── ReturnType: Int32
│   ├── Name: fibonacci
│   ├── Params: [(Some("n"), Int32)]
│   └── Body: CompoundStatement
│       └── IfStatement
│           ├── Cond: ComparisonExpression
│           │   ├── Op: LessThanOrEqual
│           │   ├── Lhs: PrimaryIdentifier("n")
│           │   └── Rhs: ConstantInteger(2)
│           ├── Then: ReturnStatement
│           │   └── Expr: Some(ConstantInteger(1))
│           └── Else: ReturnStatement
│               └── Expr: Some(BinaryExpression)
│                   ├── Op: Add
│                   ├── Lhs: PostParen(PrimaryIdentifier("fibonacci"), [BinaryExpression(Sub, PrimaryIdentifier("n"), ConstantInteger(1))])
│                   └── Rhs: PostParen(PrimaryIdentifier("fibonacci"), [BinaryExpression(Sub, PrimaryIdentifier("n"), ConstantInteger(2))])
```