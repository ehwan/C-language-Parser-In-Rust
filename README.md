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


 // ~~~~~~~~ Result of Tokenizing ~~~~~~~~


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


// ~~~~~~~~ Result of Generating Instructions ~~~~~~~~


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