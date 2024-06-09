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
Tokenizing...
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
-----------------------------------
Parsing...
ASTs: 
TranslationUnitAST { statements: [DeclarationStatementAST { typeinfo: Struct(StructInfo { name: Some("MyStruct"), fields: Some({"struct_a": Int32, "struct_b": Int32}) }) }, DeclarationStatementAST { typeinfo: Union(UnionInfo { name: Some("MyUnion"), fields: Some({"union_a": Int32, "union_b": Int32}) }) }, DeclarationStatementAST { typeinfo: Enum(EnumInfo { name: Some("MyEnum"), fields: Some({"enum_e": 20, "enum_a": 0, "enum_c": 10, "enum_b": 1, "enum_d": 11}) }) }, DeclarationVarsStatementAST { typeinfo: Int32, declarators: [DirectFunctionDeclaratorAST { declarator: IdentifierDeclaratorAST { name: "take" }, params: [(Float32, Some(AbstractPointerDeclaratorAST { declarator: Some(AbstractPointerDeclaratorAST { declarator: None }) })), (Int32, Some(PointerDeclaratorAST { declarator: IdentifierDeclaratorAST { name: "b" } })), (Struct(StructInfo { name: Some("MyStruct"), fields: None }), Some(IdentifierDeclaratorAST { name: "s" }))] }] }, FunctionDefinitionStatementAST { return_type: Int32, declarator: DirectFunctionDeclaratorAST { declarator: IdentifierDeclaratorAST { name: "main" }, params: [] }, body: CompoundStatementAST { statements: [DeclarationVarsStatementAST { typeinfo: UInt32, declarators: [InitDeclaratorAST { declarator: IdentifierDeclaratorAST { name: "ma" }, initializer: CastExpressionAST { src: ConstantIntegerAST { value: 10 }, typeinfo: Int32 } }] }, DeclarationVarsStatementAST { typeinfo: UInt64, declarators: [InitDeclaratorAST { declarator: IdentifierDeclaratorAST { name: "mb" }, initializer: ConstantIntegerAST { value: 20 } }] }, DeclarationVarsStatementAST { typeinfo: Int32, declarators: [InitDeclaratorAST { declarator: IdentifierDeclaratorAST { name: "mc" }, initializer: BinaryExpressionAST { op: Add, lhs: PrimaryIdentifierAST { name: "ma" }, rhs: PrimaryIdentifierAST { name: "mb" } } }] }, ReturnStatementAST { expr: Some(PrimaryIdentifierAST { name: "mc" }) }] } }] }
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
