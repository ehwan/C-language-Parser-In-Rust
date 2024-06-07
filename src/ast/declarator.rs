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
    AbstractPointer,
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

#[derive(Debug)]
pub struct AbstractArrayFixedDeclaratorAST {
    pub decl: Box<dyn DeclaratorTrait>,
    pub size: Box<dyn ExpressionTrait>,
}
impl AST for AbstractArrayFixedDeclaratorAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl DeclaratorTrait for AbstractArrayFixedDeclaratorAST {
    fn get_type(&self) -> DeclaratorType {
        DeclaratorType::AbstractArrayFixed
    }
}

#[derive(Debug)]
pub struct AbstractArrayUnboundedDeclaratorAST {
    pub decl: Box<dyn DeclaratorTrait>,
}
impl AST for AbstractArrayUnboundedDeclaratorAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl DeclaratorTrait for AbstractArrayUnboundedDeclaratorAST {
    fn get_type(&self) -> DeclaratorType {
        DeclaratorType::AbstractArrayUnbounded
    }
}

#[derive(Debug)]
pub struct AbstractFunctionDeclaratorAST {
    pub decl: Box<dyn DeclaratorTrait>,
    pub params: Vec<Box<dyn StatementTrait>>,
}
impl AST for AbstractFunctionDeclaratorAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl DeclaratorTrait for AbstractFunctionDeclaratorAST {
    fn get_type(&self) -> DeclaratorType {
        DeclaratorType::AbstractFunction
    }
}

#[derive(Debug)]
pub struct AbstractPointerDeclaratorAST {
    pub decl: Box<dyn DeclaratorTrait>,
}
impl AST for AbstractPointerDeclaratorAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
impl DeclaratorTrait for AbstractPointerDeclaratorAST {
    fn get_type(&self) -> DeclaratorType {
        DeclaratorType::AbstractPointer
    }
}
