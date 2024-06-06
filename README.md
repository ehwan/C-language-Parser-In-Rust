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
int main()
{
  int a = 10;
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
Identifier("main")
LeftParen
RightParen
LeftBrace
Int
Identifier("a")
Equal
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
TranslationUnitAST { declarations: [DeclarationAST { specifier: Struct(StructDeclAndSpecifierAST { name: Some("MyStruct"), declarations: [StructMemberDeclarationAST { type_specifier: Int, declarators: [DeclaratorIdentifierAST { name: "a" }] }, StructMemberDeclarationAST { type_specifier: Int, declarators: [DeclaratorIdentifierAST { name: "b" }] }] }), init_declarators: [] }, FunctionDefinitionAST { return_type: Int, funcdecl: DeclaratorFunctionAST { decl: DeclaratorIdentifierAST { name: "main" }, args: [] }, body: CompoundStatementAST { statements: [DeclarationAST { specifier: Int, init_declarators: [InitDeclaratorAST { declarator: DeclaratorIdentifierAST { name: "a" }, initializer: ConstantIntegerAST { value: 10 } }] }, DeclarationAST { specifier: Int, init_declarators: [InitDeclaratorAST { declarator: DeclaratorIdentifierAST { name: "b" }, initializer: ConstantIntegerAST { value: 20 } }] }, DeclarationAST { specifier: Int, init_declarators: [InitDeclaratorAST { declarator: DeclaratorIdentifierAST { name: "c" }, initializer: BinaryExpressionAST { op: Add, lhs: PrimaryIdentifierAST { name: "a" }, rhs: PrimaryIdentifierAST { name: "b" } } }] }, ReturnStatementAST { expr: Some(PrimaryIdentifierAST { name: "c" }) }] } }] }
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
├── FunctionDefinitionStatementAST
│   ├── I32 main
│   └── CompoundStatementAST
│       ├── I32 a = 10
│       ├── I32 b = 20
│       ├── I32 c
│       │   └── BinaryExpressionAST
│       │       └── a + b
│       └── Return c
```
