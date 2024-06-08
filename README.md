# C-Parser-In-Rust
A minimal C language parser written in Rust

A lot of features are not implemented yet. This is just a toy project for testing [RustyParser](https://github.com/ehwan/RustyParser)

## Running Structures
 1) SourceFile -> Tokeninze -> `Token Stream`
 2) TokenStream -> Parse -> `Abstract Syntax Tree` <-- Currently Here
 3) AST -> Generate Code -> `Sequence of Instructions`
 4) Execution

## Example
```c
/// sample.c
struct MyStruct
{
  int a;
  int b;
};
int take(float**, int* b, struct MyStruct s);
int main()
{
  int a = (int)10;
  int b = 20;
  int c = a + b;
  return c;
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
Identifier("a")
SemiColon
Int
Identifier("b")
SemiColon
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
Int
Identifier("a")
Equal
LeftParen
Int
RightParen
ConstantInteger(10)
SemiColon
Int
Identifier("b")
Equal
ConstantInteger(20)
SemiColon
Int
Identifier("c")
Equal
Identifier("a")
Plus
Identifier("b")
SemiColon
Return
Identifier("c")
SemiColon
RightBrace
-----------------------------------
Parsing...
ASTs: 
TranslationUnitAST { statements: [DeclarationStatementAST { specifier: Struct(StructDeclarationTypenameAST { name: Some("MyStruct"), declarations: [StructMemberDeclarationStatementAST { specifier: I32, declarators: [IdentifierDeclaratorAST { name: "a" }] }, StructMemberDeclarationStatementAST { specifier: I32, declarators: [IdentifierDeclaratorAST { name: "b" }] }] }) }, DeclarationVarsStatementAST { specifier: I32, declarators: [DirectFunctionDeclaratorAST { decl: IdentifierDeclaratorAST { name: "take" }, params: [ParameterDeclarationStatementAST { specifier: F32, declarator: Some(AbstractPointerDeclaratorAST { decl: Some(AbstractPointerDeclaratorAST { decl: None }) }) }, ParameterDeclarationStatementAST { specifier: I32, declarator: Some(PointerDeclaratorAST { decl: IdentifierDeclaratorAST { name: "b" } }) }, ParameterDeclarationStatementAST { specifier: Struct(StructTypenameAST { name: "MyStruct" }), declarator: Some(IdentifierDeclaratorAST { name: "s" }) }] }] }, FunctionDefinitionStatementAST { specifier: I32, declarator: DirectFunctionDeclaratorAST { decl: IdentifierDeclaratorAST { name: "main" }, params: [] }, body: CompoundStatementAST { statements: [DeclarationVarsStatementAST { specifier: I32, declarators: [InitDeclaratorAST { declarator: IdentifierDeclaratorAST { name: "a" }, initializer: CastExpressionAST { src: ConstantIntegerAST { value: 10 }, typename: TypeSpecifierAST { specifier: I32 } } }] }, DeclarationVarsStatementAST { specifier: I32, declarators: [InitDeclaratorAST { declarator: IdentifierDeclaratorAST { name: "b" }, initializer: ConstantIntegerAST { value: 20 } }] }, DeclarationVarsStatementAST { specifier: I32, declarators: [InitDeclaratorAST { declarator: IdentifierDeclaratorAST { name: "c" }, initializer: BinaryExpressionAST { op: Add, lhs: PrimaryIdentifierAST { name: "a" }, rhs: PrimaryIdentifierAST { name: "b" } } }] }, ReturnStatementAST { expr: Some(PrimaryIdentifierAST { name: "c" }) }] } }] }
```

The AST will be:
![AST](tree.png)

```
TranslationUnitAST
├── DeclarationStatementAST
│   └── StructDeclarationTypenameAST
│       ├── MyStruct
│       ├── I32 a
│       └── I32 b
├── DeclarationVarsStatementAST
│   └── DirectFunctionDeclaratorAST
│       ├── I32 take
│       └── Parameters
│           ├── ParameterDeclarationStatementAST
│           │   ├── F32
│           │   └── AbstractPointerDeclaratorAST
│           │       └── AbstractPointerDeclaratorAST
│           ├── ParameterDeclarationStatementAST
│           │   ├── I32
│           │   └── PointerDeclaratorAST
│           │       └── IdentifierDeclaratorAST b
│           └── ParameterDeclarationStatementAST
│               ├── Struct MyStruct
│               └── IdentifierDeclaratorAST s
├── FunctionDefinitionStatementAST
│   ├── I32 main
│   └── CompoundStatementAST
│       ├── DeclarationVarsStatementAST
│       │   └── I32 a = CastExpressionAST (10 to I32)
│       ├── DeclarationVarsStatementAST
│       │   └── I32 b = 20
│       ├── DeclarationVarsStatementAST
│       │   └── I32 c = BinaryExpressionAST (a + b)
│       └── ReturnStatementAST
│           └── c
```
