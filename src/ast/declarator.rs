use super::expression::Expression;

use std::vec::Vec;

#[derive(Debug, Clone)]
pub enum StorageClassSpecifier {
    Static,
    Typedef,
    Extern,
    Auto,
    Register,
}
#[derive(Debug, Clone)]
pub enum TypeQualifier {
    Const,
    Volatile,
}
#[derive(Debug, Clone)]
pub enum TypeSpecifier {
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Signed,
    Unsigned,
    Typename(String),
    StructOrUnion(StructOrUnionSpecifier),
    Enum(EnumSpecifier),
}

#[derive(Debug, Clone)]
pub struct StructOrUnionSpecifier {
    pub is_struct: bool,
    pub name: Option<String>,
    pub decls: Option<Vec<StructDeclaration>>,
}

#[derive(Debug, Clone)]
pub struct StructDeclaration {
    pub specs: Vec<SpecifierQualifier>,
    pub declarators: Vec<Declarator>,
}

#[derive(Debug, Clone)]
pub struct EnumSpecifier {
    pub name: Option<String>,
    pub enumerators: Option<Vec<Enumerator>>,
}

#[derive(Debug, Clone)]
pub struct Enumerator {
    pub name: String,
    pub value: Option<Expression>,
}

#[derive(Debug, Clone)]
pub enum DeclarationSpecifier {
    StorageClassSpecifier(StorageClassSpecifier),
    TypeQualifier(TypeQualifier),
    TypeSpecifier(TypeSpecifier),
}

#[derive(Debug, Clone)]
pub enum SpecifierQualifier {
    TypeQualifier(TypeQualifier),
    TypeSpecifier(TypeSpecifier),
}

#[derive(Debug, Clone)]
pub struct Typename {
    pub specs: Vec<SpecifierQualifier>,
    pub declarator: Option<Box<Declarator>>,
}

#[derive(Debug, Clone)]
pub struct ParameterDeclaration {
    pub specs: Vec<DeclarationSpecifier>,
    pub declarator: Option<Box<Declarator>>,
}
#[derive(Debug, Clone)]
pub struct ParameterList {
    pub params: Vec<ParameterDeclaration>,
    pub variadic: bool,
}

#[derive(Debug, Clone)]
pub enum Declarator {
    Identifier(DeclIdentifier),
    Const(DeclConst),
    Pointer(DeclPointer),
    Volatile(DeclVolatile),
    ArrayFixed(DeclArrayFixed),
    ArrayUnbounded(DeclArrayUnbounded),
    Function(DeclFunction),
}

#[derive(Debug, Clone)]
pub struct DeclIdentifier {
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct DeclConst {
    pub declarator: Option<Box<Declarator>>,
}

#[derive(Debug, Clone)]
pub struct DeclPointer {
    pub declarator: Option<Box<Declarator>>,
}
#[derive(Debug, Clone)]
pub struct DeclVolatile {
    pub declarator: Option<Box<Declarator>>,
}
#[derive(Debug, Clone)]
pub struct DeclInit {
    pub declarator: Box<Declarator>,
    pub initializer: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct DeclArrayFixed {
    pub declarator: Option<Box<Declarator>>,
    pub size: Expression,
}

#[derive(Debug, Clone)]
pub struct DeclArrayUnbounded {
    pub declarator: Option<Box<Declarator>>,
}

#[derive(Debug, Clone)]
pub struct DeclFunction {
    pub declarator: Option<Box<Declarator>>,
    pub params: ParameterList,
}
