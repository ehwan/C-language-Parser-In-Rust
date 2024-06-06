use crate::program::Program;
use crate::token::Token;
use std::any::Any;
use std::cell::RefCell;
use std::rc::Rc;
use std::vec::Vec;

use crate::program::*;

pub trait AST: core::fmt::Debug + Any {
    fn emit(&self, program: &mut Program);
    fn as_any(&self) -> &dyn Any;
}

#[derive(Debug, Clone)]
pub struct NullAST;

impl AST for NullAST {
    fn emit(&self, program: &mut Program) {
        // do nothing
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

/*

#[derive(Debug)]
pub struct StructSpecifierAST {
    pub name: String,
}
impl AST for StructSpecifierAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
#[derive(Debug)]
pub struct StructDeclAndSpecifierAST {
    pub name: Option<String>,
    pub declarations: Vec<Box<dyn AST>>,
}
impl AST for StructDeclAndSpecifierAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct InitializerListAST {
    pub initializers: Vec<Box<dyn AST>>,
}
impl AST for InitializerListAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct DeclarationListAST {
    pub declarations: Vec<Box<dyn AST>>,
}
impl AST for DeclarationListAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct StructMemberDeclarationAST {
    pub type_specifier: TypeSpecifier,
    pub declarators: Vec<Box<dyn AST>>,
}
impl AST for StructMemberDeclarationAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct ParameterDeclarationAST {
    pub specifiers: TypeSpecifier,
    pub declarator: Option<Box<dyn AST>>,
}
impl AST for ParameterDeclarationAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct InitDeclaratorAST {
    pub declarator: Box<dyn DeclaratorTrait>,
    pub initializer: Box<dyn AST>,
}
impl AST for InitDeclaratorAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct FunctionDefinitionAST {
    pub return_type: TypeSpecifier,
    pub funcdecl: Box<dyn AST>,
    pub body: Box<dyn AST>,
}
impl AST for FunctionDefinitionAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct DeclarationAST {
    pub specifier: TypeSpecifier,
    pub init_declarators: Vec<Box<dyn AST>>,
}
impl AST for DeclarationAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct TranslationUnitAST {
    pub declarations: Vec<Box<dyn AST>>,
}
impl AST for TranslationUnitAST {
    fn emit(&self, program: &mut Program) {
        for decl in &self.declarations {
            decl.emit(program);
        }
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

*/
