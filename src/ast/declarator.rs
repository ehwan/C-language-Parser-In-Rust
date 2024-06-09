use super::{
    ast::{ASTType, AST},
    typename::TypeInfo,
};
use crate::program::Program;
use std::vec::Vec;

use std::any::Any;

#[derive(Debug)]
pub struct IdentifierDeclaratorAST {
    pub name: String,
}
impl AST for IdentifierDeclaratorAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::DeclaratorIdentifier
    }
    fn get_typeinfo_from_direct_declarator(&self, info: TypeInfo) -> (String, TypeInfo) {
        (self.name.clone(), info)
    }
}

#[derive(Debug)]
pub struct DirectArrayFixedDeclaratorAST {
    pub declarator: Box<dyn AST>,
    pub size: Box<dyn AST>,
}
impl AST for DirectArrayFixedDeclaratorAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::DeclaratorDirectArrayFixed
    }
    fn get_typeinfo_from_direct_declarator(&self, info: TypeInfo) -> (String, TypeInfo) {
        let (name, info) = self.declarator.get_typeinfo_from_direct_declarator(info);
        let size = self
            .size
            .get_constant_i64()
            .expect("Array size must be constant") as usize;
        (name, TypeInfo::Array(Box::new(info), Some(size)))
    }
}

#[derive(Debug)]
pub struct DirectArrayUnboundedDeclaratorAST {
    pub declarator: Box<dyn AST>,
}
impl AST for DirectArrayUnboundedDeclaratorAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::DeclaratorDirectArrayUnbounded
    }
    fn get_typeinfo_from_direct_declarator(&self, info: TypeInfo) -> (String, TypeInfo) {
        let (name, info) = self.declarator.get_typeinfo_from_direct_declarator(info);
        (name, TypeInfo::Array(Box::new(info), None))
    }
}

#[derive(Debug)]
pub struct DirectFunctionDeclaratorAST {
    pub declarator: Box<dyn AST>,
    pub params: Vec<(TypeInfo, Option<Box<dyn AST>>)>,
}
impl AST for DirectFunctionDeclaratorAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::DeclaratorDirectFunction
    }
    fn get_typeinfo_from_declarator(&self, info: TypeInfo) -> TypeInfo {
        let return_type = self.declarator.get_typeinfo_from_declarator(info);
        let mut params = Vec::new();
        for (param_type, param_declarator) in &self.params {
            let param_type = if let Some(param_declarator) = param_declarator {
                param_declarator.get_typeinfo_from_declarator(param_type.clone())
            } else {
                param_type.clone()
            };
            params.push(param_type);
        }
        TypeInfo::Function(Box::new(return_type), params)
    }
    fn get_typeinfo_from_direct_declarator(&self, info: TypeInfo) -> (String, TypeInfo) {
        let (name, return_type) = self.declarator.get_typeinfo_from_direct_declarator(info);
        let mut params = Vec::new();
        for (param_type, param_declarator) in &self.params {
            let param_type = if let Some(param_declarator) = param_declarator {
                param_declarator.get_typeinfo_from_declarator(param_type.clone())
            } else {
                param_type.clone()
            };
            params.push(param_type);
        }
        (name, TypeInfo::Function(Box::new(return_type), params))
    }
}

#[derive(Debug)]
pub struct PointerDeclaratorAST {
    pub declarator: Box<dyn AST>,
}
impl AST for PointerDeclaratorAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::DeclaratorPointer
    }

    fn get_typeinfo_from_direct_declarator(&self, info: TypeInfo) -> (String, TypeInfo) {
        let (name, info) = self.declarator.get_typeinfo_from_direct_declarator(info);
        (name, TypeInfo::Pointer(Box::new(info)))
    }
    fn get_typeinfo_from_declarator(&self, info: TypeInfo) -> TypeInfo {
        let info = self.declarator.get_typeinfo_from_declarator(info);
        TypeInfo::Pointer(Box::new(info))
    }
}

#[derive(Debug)]
pub struct InitDeclaratorAST {
    pub declarator: Box<dyn AST>,
    pub initializer: Box<dyn AST>,
}
impl AST for InitDeclaratorAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::DeclaratorInit
    }
    fn get_typeinfo_from_direct_declarator(&self, info: TypeInfo) -> (String, TypeInfo) {
        self.declarator.get_typeinfo_from_direct_declarator(info)
    }
    fn get_typeinfo_from_declarator(&self, info: TypeInfo) -> TypeInfo {
        self.declarator.get_typeinfo_from_declarator(info)
    }
}

#[derive(Debug)]
pub struct AbstractArrayFixedDeclaratorAST {
    pub declarator: Option<Box<dyn AST>>,
    pub size: Box<dyn AST>,
}
impl AST for AbstractArrayFixedDeclaratorAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::DeclaratorAbstractArrayFixed
    }

    fn get_typeinfo_from_declarator(&self, info: TypeInfo) -> TypeInfo {
        let info = if let Some(decl) = &self.declarator {
            decl.get_typeinfo_from_declarator(info)
        } else {
            info
        };
        let size = self
            .size
            .get_constant_i64()
            .expect("Array size must be constant") as usize;
        TypeInfo::Array(Box::new(info), Some(size))
    }
}

#[derive(Debug)]
pub struct AbstractArrayUnboundedDeclaratorAST {
    pub declarator: Option<Box<dyn AST>>,
}
impl AST for AbstractArrayUnboundedDeclaratorAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::DeclaratorAbstractArrayUnbounded
    }

    fn get_typeinfo_from_declarator(&self, info: TypeInfo) -> TypeInfo {
        let info = if let Some(decl) = &self.declarator {
            decl.get_typeinfo_from_declarator(info)
        } else {
            info
        };
        TypeInfo::Array(Box::new(info), None)
    }
}

#[derive(Debug)]
pub struct AbstractFunctionDeclaratorAST {
    pub declarator: Option<Box<dyn AST>>,
    pub params: Vec<(TypeInfo, Option<Box<dyn AST>>)>,
}
impl AST for AbstractFunctionDeclaratorAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::DeclaratorAbstractFunction
    }

    fn get_typeinfo_from_declarator(&self, info: TypeInfo) -> TypeInfo {
        let return_type = if let Some(decl) = &self.declarator {
            decl.get_typeinfo_from_declarator(info)
        } else {
            info
        };
        let mut params = Vec::new();
        for (param_type, param_declarator) in &self.params {
            let param_type = if let Some(param_declarator) = param_declarator {
                param_declarator.get_typeinfo_from_declarator(param_type.clone())
            } else {
                param_type.clone()
            };
            params.push(param_type);
        }
        TypeInfo::Function(Box::new(return_type), params)
    }
}

#[derive(Debug)]
pub struct AbstractPointerDeclaratorAST {
    pub declarator: Option<Box<dyn AST>>,
}
impl AST for AbstractPointerDeclaratorAST {
    fn emit(&self, program: &mut Program) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn get_type(&self) -> ASTType {
        ASTType::DeclaratorAbstractPointer
    }
    fn get_typeinfo_from_declarator(&self, info: TypeInfo) -> TypeInfo {
        let info = if let Some(decl) = &self.declarator {
            decl.get_typeinfo_from_declarator(info)
        } else {
            info
        };
        TypeInfo::Pointer(Box::new(info))
    }
}
