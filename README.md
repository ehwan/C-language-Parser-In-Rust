# minimal C language lexer & parser & virtual executer written in Rust

A minimal C language parser written in Rust

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
int main()
{
  unsigned int ma = (int)10;
  unsigned long long mb = 20;
  int mc = (unsigned long long)ma + mb; // WARNING: binary opeartion must be between same types
  print(ma, mb, mc);                    // SPECIAL built-in function for printing
  ++ma;
  print(ma, mb);
  int i = 0;

  for (i = 0; i < 10; ++i)
  {
    print(i);
  }
  return mc;
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
   1: Identifier("main")
   2: LeftParen
   3: RightParen
   4: LeftBrace
   5: Unsigned
   6: Int
   7: Identifier("ma")
   8: Equal
   9: LeftParen
  10: Int
  11: RightParen
  12: ConstantInteger(10)
  13: SemiColon
  14: Unsigned
  15: Long
  16: Long
  17: Identifier("mb")
  18: Equal
  19: ConstantInteger(20)
  20: SemiColon
  21: Int
  22: Identifier("mc")
  23: Equal
  24: LeftParen
  25: Unsigned
  26: Long
  27: Long
  28: RightParen
  29: Identifier("ma")
  30: Plus
  31: Identifier("mb")
  32: SemiColon
  33: Identifier("print")
  34: LeftParen
  35: Identifier("ma")
  36: Comma
  37: Identifier("mb")
  38: Comma
  39: Identifier("mc")
  40: RightParen
  41: SemiColon
  42: IncOp
  43: Identifier("ma")
  44: SemiColon
  45: Identifier("print")
  46: LeftParen
  47: Identifier("ma")
  48: Comma
  49: Identifier("mb")
  50: RightParen
  51: SemiColon
  52: Int
  53: Identifier("i")
  54: Equal
  55: ConstantInteger(0)
  56: SemiColon
  57: For
  58: LeftParen
  59: Identifier("i")
  60: Equal
  61: ConstantInteger(0)
  62: SemiColon
  63: Identifier("i")
  64: LessThan
  65: ConstantInteger(10)
  66: SemiColon
  67: IncOp
  68: Identifier("i")
  69: RightParen
  70: LeftBrace
  71: Identifier("print")
  72: LeftParen
  73: Identifier("i")
  74: RightParen
  75: SemiColon
  76: RightBrace
  77: Return
  78: Identifier("mc")
  79: SemiColon
  80: RightBrace
============================ Building AST ============================
ASTs: 
TranslationUnit { statements: [FunctionDefinitionStatement { return_type: Int32, name: "main", params: [], body: CompoundStatement { statements: [DeclarationStatement { vars: [(Some("ma"), UInt32, Some(CastExpression { src: ConstantInteger { value: 10 }, typeinfo: Int32 }))] }, DeclarationStatement { vars: [(Some("mb"), UInt64, Some(ConstantInteger { value: 20 }))] }, DeclarationStatement { vars: [(Some("mc"), Int32, Some(BinaryExpression { op: Add, lhs: CastExpression { src: PrimaryIdentifier { name: "ma" }, typeinfo: UInt64 }, rhs: PrimaryIdentifier { name: "mb" } }))] }, ExpressionStatement { expression: PostParen { src: PrimaryIdentifier { name: "print" }, args: [PrimaryIdentifier { name: "ma" }, PrimaryIdentifier { name: "mb" }, PrimaryIdentifier { name: "mc" }] } }, ExpressionStatement { expression: UnaryExpression { op: Increment, src: PrimaryIdentifier { name: "ma" } } }, ExpressionStatement { expression: PostParen { src: PrimaryIdentifier { name: "print" }, args: [PrimaryIdentifier { name: "ma" }, PrimaryIdentifier { name: "mb" }] } }, DeclarationStatement { vars: [(Some("i"), Int32, Some(ConstantInteger { value: 0 }))] }, ForStatement { init: BinaryExpression { op: Assign, lhs: PrimaryIdentifier { name: "i" }, rhs: ConstantInteger { value: 0 } }, cond: BinaryExpression { op: LessThan, lhs: PrimaryIdentifier { name: "i" }, rhs: ConstantInteger { value: 10 } }, next: Some(UnaryExpression { op: Increment, src: PrimaryIdentifier { name: "i" } }), statement: CompoundStatement { statements: [ExpressionStatement { expression: PostParen { src: PrimaryIdentifier { name: "print" }, args: [PrimaryIdentifier { name: "i" }] } }] } }, ReturnStatement { expr: Some(PrimaryIdentifier { name: "mc" }) }] } }] }
============================ Generating Instructions ============================
Instructions: 
   0: DefineLabel { label: "main" }
   1: PushStack { operand: Register(4) }
   2: MoveRegister { operand_from: Register(5), operand_to: Register(4) }
   3: MoveRegister { operand_from: Value(Int32(10)), operand_to: Register(0) }
   4: MoveRegister { operand_from: Register(0), operand_to: Register(1) }
   5: Cast { info: Int32, operand_from: Register(1), operand_to: Register(0) }
   6: Assign { lhs_type: UInt32, lhs: Register(1), rhs: Register(0) }
   7: PushStack { operand: Register(1) }
   8: MoveRegister { operand_from: Value(Int32(20)), operand_to: Register(0) }
   9: Assign { lhs_type: UInt64, lhs: Register(1), rhs: Register(0) }
  10: PushStack { operand: Register(1) }
  11: MoveRegister { operand_from: Register(4), operand_to: Register(0) }
  12: AddAssign { lhs: Register(0), rhs: Value(Int64(1)) }
  13: PushStack { operand: Derefed(0) }
  14: MoveRegister { operand_from: Register(4), operand_to: Register(0) }
  15: AddAssign { lhs: Register(0), rhs: Value(Int64(0)) }
  16: MoveRegister { operand_from: Register(0), operand_to: Register(1) }
  17: Cast { info: UInt64, operand_from: Derefed(1), operand_to: Register(0) }
  18: PopStack { operand: Register(2) }
  19: MoveRegister { operand_from: Register(0), operand_to: Register(1) }
  20: AddAssign { lhs: Register(1), rhs: Register(2) }
  21: MoveRegister { operand_from: Register(1), operand_to: Register(0) }
  22: Assign { lhs_type: Int32, lhs: Register(1), rhs: Register(0) }
  23: PushStack { operand: Register(1) }
  24: MoveRegister { operand_from: Register(4), operand_to: Register(0) }
  25: AddAssign { lhs: Register(0), rhs: Value(Int64(2)) }
  26: PushStack { operand: Derefed(0) }
  27: MoveRegister { operand_from: Register(4), operand_to: Register(0) }
  28: AddAssign { lhs: Register(0), rhs: Value(Int64(1)) }
  29: PushStack { operand: Derefed(0) }
  30: MoveRegister { operand_from: Register(4), operand_to: Register(0) }
  31: AddAssign { lhs: Register(0), rhs: Value(Int64(0)) }
  32: PushStack { operand: Derefed(0) }
  33: PushStack { operand: Value(UInt64(3)) }
  34: Print
  35: MoveRegister { operand_from: Register(4), operand_to: Register(0) }
  36: AddAssign { lhs: Register(0), rhs: Value(Int64(0)) }
  37: MoveRegister { operand_from: Register(0), operand_to: Register(1) }
  38: MoveRegister { operand_from: Derefed(1), operand_to: Register(0) }
  39: Increment { operand: Derefed(1) }
  40: MoveRegister { operand_from: Register(4), operand_to: Register(0) }
  41: AddAssign { lhs: Register(0), rhs: Value(Int64(1)) }
  42: PushStack { operand: Derefed(0) }
  43: MoveRegister { operand_from: Register(4), operand_to: Register(0) }
  44: AddAssign { lhs: Register(0), rhs: Value(Int64(0)) }
  45: PushStack { operand: Derefed(0) }
  46: PushStack { operand: Value(UInt64(2)) }
  47: Print
  48: MoveRegister { operand_from: Value(Int32(0)), operand_to: Register(0) }
  49: Assign { lhs_type: Int32, lhs: Register(1), rhs: Register(0) }
  50: PushStack { operand: Register(1) }
  51: MoveRegister { operand_from: Value(Int32(0)), operand_to: Register(0) }
  52: PushStack { operand: Register(0) }
  53: MoveRegister { operand_from: Register(4), operand_to: Register(0) }
  54: AddAssign { lhs: Register(0), rhs: Value(Int64(3)) }
  55: PopStack { operand: Register(2) }
  56: Assign { lhs_type: Int32, lhs: Derefed(0), rhs: Register(2) }
  57: DefineLabel { label: ".__L0__" }
  58: MoveRegister { operand_from: Value(Int32(10)), operand_to: Register(0) }
  59: PushStack { operand: Register(0) }
  60: MoveRegister { operand_from: Register(4), operand_to: Register(0) }
  61: AddAssign { lhs: Register(0), rhs: Value(Int64(3)) }
  62: PopStack { operand: Register(2) }
  63: MoveRegister { operand_from: Derefed(0), operand_to: Register(1) }
  64: LessThan { lhs: Register(1), rhs: Register(2), to: Register(0) }
  65: JumpZero { label: ".__L1__", operand_cond: Register(0) }
  66: MoveRegister { operand_from: Register(4), operand_to: Register(0) }
  67: AddAssign { lhs: Register(0), rhs: Value(Int64(3)) }
  68: PushStack { operand: Derefed(0) }
  69: PushStack { operand: Value(UInt64(1)) }
  70: Print
  71: DefineLabel { label: ".__L2__" }
  72: MoveRegister { operand_from: Register(4), operand_to: Register(0) }
  73: AddAssign { lhs: Register(0), rhs: Value(Int64(3)) }
  74: MoveRegister { operand_from: Register(0), operand_to: Register(1) }
  75: MoveRegister { operand_from: Derefed(1), operand_to: Register(0) }
  76: Increment { operand: Derefed(1) }
  77: Jump { label: ".__L0__" }
  78: DefineLabel { label: ".__L1__" }
  79: MoveRegister { operand_from: Register(4), operand_to: Register(0) }
  80: AddAssign { lhs: Register(0), rhs: Value(Int64(2)) }
  81: MoveRegister { operand_from: Derefed(0), operand_to: Register(1) }
  82: MoveRegister { operand_from: Register(1), operand_to: Register(0) }
  83: Return
  84: Panic { message: "Function main must return a Int32 value" }
      -------------------- Start Address ---------------------
  85: MoveRegister { operand_from: Value(UInt64(0)), operand_to: Register(0) }
  86: Call { address: Register(0) }
============================ Executing Instructions ============================
Print: UInt32(10), UInt64(20), Int32(30), 
Print: UInt32(11), UInt64(20), 
Print: Int32(0), 
Print: Int32(1), 
Print: Int32(2), 
Print: Int32(3), 
Print: Int32(4), 
Print: Int32(5), 
Print: Int32(6), 
Print: Int32(7), 
Print: Int32(8), 
Print: Int32(9),
```

The visualized AST will be:

![AST](tree.png)

```
TranslationUnit
└── FunctionDefinitionStatement
    ├── ReturnType: Int32
    ├── Name: main
    ├── Params: []
    └── Body: CompoundStatement
        ├── DeclarationStatement
        │   └── Vars
        │       └── (Some("ma"), UInt32, Some(CastExpression { src: ConstantInteger { value: 10 }, typeinfo: Int32 }))
        ├── DeclarationStatement
        │   └── Vars
        │       └── (Some("mb"), UInt64, Some(ConstantInteger { value: 20 }))
        ├── DeclarationStatement
        │   └── Vars
        │       └── (Some("mc"), Int32, Some(BinaryExpression { op: Add, lhs: CastExpression { src: PrimaryIdentifier { name: "ma" }, typeinfo: UInt64 }, rhs: PrimaryIdentifier { name: "mb" } }))
        ├── ExpressionStatement
        │   └── Expression: PostParen
        │       └── Src: PrimaryIdentifier { name: "print" }
        │           └── Args: [PrimaryIdentifier { name: "ma" }, PrimaryIdentifier { name: "mb" }, PrimaryIdentifier { name: "mc" }]
        ├── ExpressionStatement
        │   └── Expression: UnaryExpression { op: Increment, src: PrimaryIdentifier { name: "ma" } }
        ├── ExpressionStatement
        │   └── Expression: PostParen
        │       └── Src: PrimaryIdentifier { name: "print" }
        │           └── Args: [PrimaryIdentifier { name: "ma" }, PrimaryIdentifier { name: "mb" }]
        ├── DeclarationStatement
        │   └── Vars
        │       └── (Some("i"), Int32, Some(ConstantInteger { value: 0 }))
        ├── ForStatement
        │   ├── Init: BinaryExpression { op: Assign, lhs: PrimaryIdentifier { name: "i" }, rhs: ConstantInteger { value: 0 } }
        │   ├── Cond: BinaryExpression { op: LessThan, lhs: PrimaryIdentifier { name: "i" }, rhs: ConstantInteger { value: 10 } }
        │   ├── Next: Some(UnaryExpression { op: Increment, src: PrimaryIdentifier { name: "i" } })
        │   └── Statement: CompoundStatement
        │       └── ExpressionStatement
        │           └── Expression: PostParen
        │               └── Src: PrimaryIdentifier { name: "print" }
        │                   └── Args: [PrimaryIdentifier { name: "i" }]
        └── ReturnStatement
            └── Expr: Some(PrimaryIdentifier { name: "mc" })
```