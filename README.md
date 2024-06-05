# C-Parser-In-Rust
A minimal C language parser written in Rust

This is test project for [RustyParser](https://github.com/ehwan/RustyParser)

## Sample
```c
/// sample.c
int main() {
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


```
Enter your code:
Tokenizing...
Tokens:
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
TranslationUnitAST { declarations: [FunctionDefinitionAST { return_type: Int, funcdecl: DeclaratorFunctionAST { decl: DeclaratorIdentifierAST { name: "main" }, args: [] }, body: CompoundStatementAST { statements: [DeclarationAST { specifier: Int, init_declarators: [InitDeclaratorAST { declarator: DeclaratorIdentifierAST { name: "a" }, initializer: ConstantIntegerAST { value: 10 } }] }, DeclarationAST { specifier: Int, init_declarators: [InitDeclaratorAST { declarator: DeclaratorIdentifierAST { name: "b" }, initializer: ConstantIntegerAST { value: 20 } }] }, DeclarationAST { specifier: Int, init_declarators: [InitDeclaratorAST { declarator: DeclaratorIdentifierAST { name: "c" }, initializer: BinaryExpressionAST { op: Add, lhs: PrimaryIdentifierAST { name: "a" }, rhs: PrimaryIdentifierAST { name: "b" } } }] }, ReturnStatementAST { expr: Some(PrimaryIdentifierAST { name: "c" }) }] } }] }
```

## Running Structures
 1) SourceFile -> Tokeninze -> `Token Stream`
 2) TokenStream -> Parse -> `Abstract Syntax Tree` <-- Currently Here
 3) AST -> Generate Code -> `Sequence of Instructions`
 4) Execution