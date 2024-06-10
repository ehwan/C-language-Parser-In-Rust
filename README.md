# minimal C language lexer & parser & executer written in Rust

A minimal C language parser written in Rust

A lot of features are not implemented yet. 
This is just a toy project for testing [RustyParser](https://github.com/ehwan/RustyParser)

## How to run
```sh
cargo run
```
Will execute the program, and you can pass the C code to stdin. Once you are done, press `^D` to finish the input. The program will tokenize, parse, generate instructions, and execute the code.

Sample C codes (only with implemented features) can be found in `samples/` directory. Try them with `cat samples/sample.c | cargo run`

## Running Workflows
 1) SourceFile -> Tokeninze -> `Token Stream`
 2) `TokenStream` -> Parse -> `Abstract Syntax Tree`
 3) `Abstract Syntax Tree` -> Generate Code -> `Sequence of Instructions` <-- Currently Here
 4) Execution

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
Int
Identifier("main")
LeftParen
RightParen
LeftBrace
Unsigned
Int
Identifier("ma")
Equal
LeftParen
Int
RightParen
ConstantInteger(10)
SemiColon
Unsigned
Long
Long
Identifier("mb")
Equal
ConstantInteger(20)
SemiColon
Int
Identifier("mc")
Equal
LeftParen
Unsigned
Long
Long
RightParen
Identifier("ma")
Plus
Identifier("mb")
SemiColon
Identifier("print")
LeftParen
Identifier("ma")
Comma
Identifier("mb")
Comma
Identifier("mc")
RightParen
SemiColon
IncOp
Identifier("ma")
SemiColon
Identifier("print")
LeftParen
Identifier("ma")
Comma
Identifier("mb")
RightParen
SemiColon
Int
Identifier("i")
Equal
ConstantInteger(0)
SemiColon
For
LeftParen
Identifier("i")
Equal
ConstantInteger(0)
SemiColon
Identifier("i")
LessThan
ConstantInteger(10)
SemiColon
IncOp
Identifier("i")
RightParen
LeftBrace
Identifier("print")
LeftParen
Identifier("i")
RightParen
SemiColon
RightBrace
Return
Identifier("mc")
SemiColon
RightBrace
============================ Building AST ============================
ASTs: 
TranslationUnit { statements: [FunctionDefinitionStatement { return_type: Int32, name: "main", params: [], body: CompoundStatement { statements: [DeclarationStatement { vars: [(Some("ma"), UInt32, Some(CastExpression { src: ConstantInteger { value: 10 }, typeinfo: Int32 }))] }, DeclarationStatement { vars: [(Some("mb"), UInt64, Some(ConstantInteger { value: 20 }))] }, DeclarationStatement { vars: [(Some("mc"), Int32, Some(BinaryExpression { op: Add, lhs: CastExpression { src: PrimaryIdentifier { name: "ma" }, typeinfo: UInt64 }, rhs: PrimaryIdentifier { name: "mb" } }))] }, ExpressionStatement { expression: PostParen { src: PrimaryIdentifier { name: "print" }, args: [PrimaryIdentifier { name: "ma" }, PrimaryIdentifier { name: "mb" }, PrimaryIdentifier { name: "mc" }] } }, ExpressionStatement { expression: UnaryExpression { op: Increment, src: PrimaryIdentifier { name: "ma" } } }, ExpressionStatement { expression: PostParen { src: PrimaryIdentifier { name: "print" }, args: [PrimaryIdentifier { name: "ma" }, PrimaryIdentifier { name: "mb" }] } }, DeclarationStatement { vars: [(Some("i"), Int32, Some(ConstantInteger { value: 0 }))] }, ForStatement { init: BinaryExpression { op: Assign, lhs: PrimaryIdentifier { name: "i" }, rhs: ConstantInteger { value: 0 } }, cond: BinaryExpression { op: LessThan, lhs: PrimaryIdentifier { name: "i" }, rhs: ConstantInteger { value: 10 } }, next: Some(UnaryExpression { op: Increment, src: PrimaryIdentifier { name: "i" } }), statement: CompoundStatement { statements: [ExpressionStatement { expression: PostParen { src: PrimaryIdentifier { name: "print" }, args: [PrimaryIdentifier { name: "i" }] } }] } }, ReturnStatement { expr: Some(PrimaryIdentifier { name: "mc" }) }] } }] }
============================ Generating Instructions ============================
Function Definition: main
Instructions: 
Null
NewScope
Constant { value: Int32(10), info: Int32 }
Cast { info: Int32 }
NewVariable { name: "ma", info: UInt32 }
GetVariable { name: "ma" }
Assign
Constant { value: Int32(20), info: Int32 }
NewVariable { name: "mb", info: UInt64 }
GetVariable { name: "mb" }
Assign
GetVariable { name: "ma" }
Cast { info: UInt64 }
PushRegister
GetVariable { name: "mb" }
PopStackTo
Add
NewVariable { name: "mc", info: Int32 }
GetVariable { name: "mc" }
Assign
GetVariable { name: "mc" }
PushRegister
GetVariable { name: "mb" }
PushRegister
GetVariable { name: "ma" }
PushRegister
Constant { value: UInt64(3), info: UInt64 }
PushRegister
Print
GetVariable { name: "ma" }
MoveRegister
DeepCopyRegister
Increment
GetVariable { name: "mb" }
PushRegister
GetVariable { name: "ma" }
PushRegister
Constant { value: UInt64(2), info: UInt64 }
PushRegister
Print
Constant { value: Int32(0), info: Int32 }
NewVariable { name: "i", info: Int32 }
GetVariable { name: "i" }
Assign
GetVariable { name: "i" }
PushRegister
Constant { value: Int32(0), info: Int32 }
PopStackTo
Assign
Null
GetVariable { name: "i" }
PushRegister
Constant { value: Int32(10), info: Int32 }
PopStackTo
LessThan
GetLabelAddress { label: ".__L1__" }
JumpZero
NewScope
GetVariable { name: "i" }
PushRegister
Constant { value: UInt64(1), info: UInt64 }
PushRegister
Print
PopScope
GetVariable { name: "i" }
MoveRegister
DeepCopyRegister
Increment
GetLabelAddress { label: ".__L0__" }
Jump
Null
GetVariable { name: "mc" }
PopStackTo
Jump
PopScope
PopStackTo
Jump
============================ Executing Instructions ============================
UInt32(10)
UInt64(20)
Int32(30)
UInt32(11)
UInt64(20)
Int32(0)
Int32(1)
Int32(2)
Int32(3)
Int32(4)
Int32(5)
Int32(6)
Int32(7)
Int32(8)
Int32(9)
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
