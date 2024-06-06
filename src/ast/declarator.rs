use super::{ast::AST, expression::ExpressionTrait, statement::StatementTrait};
use crate::program::Program;

use std::any::Any;

pub enum DeclaratorType {
    Identifier,
    DirectArrayFixed,
    DirectArrayUnbounded,
    DirectFunction,

    Pointer,
    Init,

    AbstractArrayFixed,
    AbstractArrayUnbounded,
    AbstractFunction,
}

pub trait DeclaratorTrait: AST {
    fn get_type(&self) -> DeclaratorType;
}

#[derive(Debug)]
pub struct IdentifierDeclaratorAST {
    pub name: String,
}
impl AST for IdentifierDeclaratorAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl DeclaratorTrait for IdentifierDeclaratorAST {
    fn get_type(&self) -> DeclaratorType {
        DeclaratorType::Identifier
    }
}

#[derive(Debug)]
pub struct DirectArrayFixedDeclaratorAST {
    pub decl: Box<dyn DeclaratorTrait>,
    pub size: Box<dyn ExpressionTrait>,
}
impl AST for DirectArrayFixedDeclaratorAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl DeclaratorTrait for DirectArrayFixedDeclaratorAST {
    fn get_type(&self) -> DeclaratorType {
        DeclaratorType::DirectArrayFixed
    }
}

#[derive(Debug)]
pub struct DirectArrayUnboundedDeclaratorAST {
    pub decl: Box<dyn DeclaratorTrait>,
}
impl AST for DirectArrayUnboundedDeclaratorAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl DeclaratorTrait for DirectArrayUnboundedDeclaratorAST {
    fn get_type(&self) -> DeclaratorType {
        DeclaratorType::DirectArrayUnbounded
    }
}

#[derive(Debug)]
pub struct DirectFunctionDeclaratorAST {
    pub decl: Box<dyn DeclaratorTrait>,
    pub params: Vec<Box<dyn StatementTrait>>,
}
impl AST for DirectFunctionDeclaratorAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl DeclaratorTrait for DirectFunctionDeclaratorAST {
    fn get_type(&self) -> DeclaratorType {
        DeclaratorType::DirectFunction
    }
}

#[derive(Debug)]
pub struct PointerDeclaratorAST {
    pub decl: Box<dyn DeclaratorTrait>,
}
impl AST for PointerDeclaratorAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl DeclaratorTrait for PointerDeclaratorAST {
    fn get_type(&self) -> DeclaratorType {
        DeclaratorType::Pointer
    }
}

#[derive(Debug)]
pub struct InitDeclaratorAST {
    pub declarator: Box<dyn DeclaratorTrait>,
    pub initializer: Box<dyn ExpressionTrait>,
}
impl AST for InitDeclaratorAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl DeclaratorTrait for InitDeclaratorAST {
    fn get_type(&self) -> DeclaratorType {
        DeclaratorType::Init
    }
}
