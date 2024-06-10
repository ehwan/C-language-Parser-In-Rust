# C-Parser-In-Rust
A minimal C language parser written in Rust

A lot of features are not implemented yet. This is just a toy project for testing [RustyParser](https://github.com/ehwan/RustyParser)

## Running Structures
 1) SourceFile -> Tokeninze -> `Token Stream`
 2) TokenStream -> Parse -> `Abstract Syntax Tree`
 3) AST -> Generate Code -> `Sequence of Instructions` <-- Currently Here
 4) Execution

## Example
```c
struct MyStruct
{
  int struct_a;
  int struct_b;
};
union MyUnion
{
  int union_a;
  int union_b;
};
enum MyEnum
{
  enum_a,
  enum_b,
  enum_c = 10,
  enum_d,
  enum_e = 20
};
int take(float**, int* b, struct MyStruct s);
int main()
{
  unsigned int ma = (int)10;
  unsigned long long mb = 20;
  int mc = ma + mb;
  return mc;
}
```

Pass c code to stdin
```sh
cat sample.c | ./target/debug/clang-parser
```

The result will be:

```
Enter your code:
============================ Tokenizing ============================
Tokens:
Struct
Identifier("MyStruct")
LeftBrace
Int
Identifier("struct_a")
SemiColon
Int
Identifier("struct_b")
SemiColon
RightBrace
SemiColon
Union
Identifier("MyUnion")
LeftBrace
Int
Identifier("union_a")
SemiColon
Int
Identifier("union_b")
SemiColon
RightBrace
SemiColon
Enum
Identifier("MyEnum")
LeftBrace
Identifier("enum_a")
Comma
Identifier("enum_b")
Comma
Identifier("enum_c")
Equal
ConstantInteger(10)
Comma
Identifier("enum_d")
Comma
Identifier("enum_e")
Equal
ConstantInteger(20)
RightBrace
SemiColon
Int
Identifier("take")
LeftParen
Float
Star
Star
Comma
Int
Star
Identifier("b")
Comma
Struct
Identifier("MyStruct")
Identifier("s")
RightParen
SemiColon
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
Identifier("ma")
Plus
Identifier("mb")
SemiColon
Return
Identifier("mc")
SemiColon
RightBrace
============================ Building AST ============================
ASTs:
TranslationUnit { statements: [DeclarationStatement { vars: [(None, Struct(StructInfo { name: Some("MyStruct"), fields: Some({"struct_a": Int32, "struct_b": Int32}) }), None)] }, DeclarationStatement { vars: [(None, Union(UnionInfo { name: Some("MyUnion"), fields: Some({"union_a": Int32, "union_b": Int32}) }), None)] }, DeclarationStatement { vars: [(None, Enum(EnumInfo { name: Some("MyEnum"), fields: Some({"enum_a": 0, "enum_d": 11, "enum_c": 10, "enum_e": 20, "enum_b": 1}) }), None)] }, DeclarationStatement { vars: [(Some("take"), Function(Int32, [Pointer(Pointer(Float32)), Pointer(Int32), Struct(StructInfo { name: Some("MyStruct"), fields: None })]), None)] }, FunctionDefinitionStatement { return_type: Int32, name: "main", params: [], body: CompoundStatement { statements: [DeclarationStatement { vars: [(Some("ma"), UInt32, Some(CastExpression { src: ConstantInteger { value: 10 }, typeinfo: Int32 }))] }, DeclarationStatement { vars: [(Some("mb"), UInt64, Some(ConstantInteger { value: 20 }))] }, DeclarationStatement { vars: [(Some("mc"), Int32, Some(BinaryExpression { op: Add, lhs: PrimaryIdentifier { name: "ma" }, rhs: PrimaryIdentifier { name: "mb" } }))] }, ReturnStatement { expr: Some(PrimaryIdentifier { name: "mc" }) }] } }] }
============================ Generating Instructions ============================
Function Definition: main
Instructions:
DeclareStructure { info: StructInfo { name: Some("MyStruct"), fields: Some({"struct_a": Int32, "struct_b": Int32}) } }
DeclareUnion { info: UnionInfo { name: Some("MyUnion"), fields: Some({"union_a": Int32, "union_b": Int32}) } }
DeclareEnum { info: EnumInfo { name: Some("MyEnum"), fields: Some({"enum_a": 0, "enum_d": 11, "enum_c": 10, "enum_e": 20, "enum_b": 1}) } }
NewVariable { name: "take", info: Function(Int32, [Pointer(Pointer(Float32)), Pointer(Int32), Struct(StructInfo { name: Some("MyStruct"), fields: None })]) }
Null
NewScope
Constant { value: UInt32(10), info: UInt32 }
Cast { info: Int32 }
NewVariable { name: "ma", info: UInt32 }
GetVariable { name: "ma" }
Assign
Constant { value: UInt32(20), info: UInt32 }
NewVariable { name: "mb", info: UInt64 }
GetVariable { name: "mb" }
Assign
GetVariable { name: "ma" }
PushRegister
GetVariable { name: "mb" }
PopStackTo
Add
NewVariable { name: "mc", info: Int32 }
GetVariable { name: "mc" }
Assign
GetVariable { name: "mc" }
PopStackTo
Jump
PopScope
PopStackTo
Jump
```

The visualized AST will be:

![AST](tree.png)

```
TranslationUnitAST
├── DeclarationStatementAST 1
│   └── StructInfo MyStruct
│       ├── Int32 struct_a
│       └── Int32 struct_b
├── DeclarationStatementAST 2
│   └── UnionInfo MyUnion
│       ├── Int32 union_a
│       └── Int32 union_b
├── DeclarationStatementAST 3
│   └── EnumInfo MyEnum
│       ├── enum_e = 20
│       ├── enum_a = 0
│       ├── enum_c = 10
│       ├── enum_b = 1
│       └── enum_d = 11
├── DeclarationVarsStatementAST
│   └── DirectFunctionDeclaratorAST take
│       ├── Parameters
│       │   ├── (Float32, AbstractPointerDeclaratorAST)
│       │   │   └── AbstractPointerDeclaratorAST
│       │   ├── (Int32, PointerDeclaratorAST)
│       │   │   └── IdentifierDeclaratorAST b
│       │   └── (StructInfo MyStruct (no fields), IdentifierDeclaratorAST s)
├── FunctionDefinitionStatementAST main
│   └── CompoundStatementAST
│       ├── DeclarationVarsStatementAST ma
│       │   └── UInt32 ma = CastExpressionAST (10 to Int32)
│       ├── DeclarationVarsStatementAST mb
│       │   └── UInt64 mb = 20
│       ├── DeclarationVarsStatementAST mc
│       │   └── Int32 mc = BinaryExpressionAST (ma + mb)
│       └── ReturnStatementAST
│           └── mc
```
