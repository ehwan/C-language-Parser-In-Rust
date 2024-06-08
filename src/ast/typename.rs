use super::{
    ast::AST,
    declarator::DeclaratorTrait,
    expression::ExpressionTrait,
    statement::{StatementTrait, StructMemberDeclarationStatementAST},
};
use crate::program::Program;

use std::any::Any;

pub enum TypenameType {
    Specifier,
    StructDeclaration,
    Struct,
    EnumDeclaration,
    Enum,
    UnionDeclaration,
    Union,
    WithDeclaration,
}
pub trait TypenameTrait: AST {
    fn get_type(&self) -> TypenameType;
}

#[derive(Debug)]
pub enum TypeSpecifier {
    Void,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Struct(Box<dyn TypenameTrait>),
    Union(Box<dyn TypenameTrait>),
    Enum(Box<dyn TypenameTrait>),
    Identifier(String),
}

#[derive(Debug)]
pub struct TypeSpecifierAST {
    pub specifier: TypeSpecifier,
}
impl AST for TypeSpecifierAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl TypenameTrait for TypeSpecifierAST {
    fn get_type(&self) -> TypenameType {
        TypenameType::Specifier
    }
}

#[derive(Debug)]
pub struct StructDeclarationTypenameAST {
    pub name: Option<String>,
    pub declarations: Vec<Box<StructMemberDeclarationStatementAST>>,
}
impl AST for StructDeclarationTypenameAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl TypenameTrait for StructDeclarationTypenameAST {
    fn get_type(&self) -> TypenameType {
        TypenameType::StructDeclaration
    }
}

#[derive(Debug)]
pub struct StructTypenameAST {
    pub name: String,
}
impl AST for StructTypenameAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl TypenameTrait for StructTypenameAST {
    fn get_type(&self) -> TypenameType {
        TypenameType::Struct
    }
}

#[derive(Debug)]
pub struct TypenameWithDeclarationAST {
    pub typename: Box<dyn TypenameTrait>,
    pub declarator: Box<dyn DeclaratorTrait>,
}
impl AST for TypenameWithDeclarationAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl TypenameTrait for TypenameWithDeclarationAST {
    fn get_type(&self) -> TypenameType {
        TypenameType::WithDeclaration
    }
}

#[derive(Debug)]
pub struct Enumerator {
    pub name: String,
    pub value: Option<Box<dyn ExpressionTrait>>,
}

#[derive(Debug)]
pub struct EnumDeclarationTypenameAST {
    pub name: Option<String>,
    pub declarations: Vec<Enumerator>,
}
impl AST for EnumDeclarationTypenameAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl TypenameTrait for EnumDeclarationTypenameAST {
    fn get_type(&self) -> TypenameType {
        TypenameType::EnumDeclaration
    }
}

#[derive(Debug)]
pub struct EnumTypenameAST {
    pub name: String,
}
impl AST for EnumTypenameAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl TypenameTrait for EnumTypenameAST {
    fn get_type(&self) -> TypenameType {
        TypenameType::Enum
    }
}

#[derive(Debug)]
pub struct UnionDeclarationTypenameAST {
    pub name: Option<String>,
    pub declarations: Vec<Box<StructMemberDeclarationStatementAST>>,
}
impl AST for UnionDeclarationTypenameAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl TypenameTrait for UnionDeclarationTypenameAST {
    fn get_type(&self) -> TypenameType {
        TypenameType::UnionDeclaration
    }
}

#[derive(Debug)]
pub struct UnionTypenameAST {
    pub name: String,
}
impl AST for UnionTypenameAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl TypenameTrait for UnionTypenameAST {
    fn get_type(&self) -> TypenameType {
        TypenameType::Union
    }
}
