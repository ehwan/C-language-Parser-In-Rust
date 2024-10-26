use super::expression::Expression;
use super::typename::TypeInfo;

use std::vec::Vec;

#[derive(Debug, Clone)]
pub enum Declarator {
    Identifier(DeclIdentifier),
    DirectArrayFixed(DeclDirectArrayFixed),
    DirectArrayUnbounded(DeclDirectArrayUnbounded),
    DirectFunction(DeclDirectFunction),
    Const(DeclConst),
    Pointer(DeclPointer),
    Init(DeclInit),
    AbstractArrayFixed(DeclAbstractArrayFixed),
    AbstractArrayUnbounded(DeclAbstractArrayUnbounded),
    AbstractFunction(DeclAbstractFunction),
    AbstractPointer(DeclAbstractPointer),
    AbstractConst(DeclAbstractConst),
}

impl Declarator {
    pub fn resolve_typeinfo(&self, info: TypeInfo) -> (Option<String>, TypeInfo) {
        match self {
            Declarator::Identifier(decl) => decl.resolve_typeinfo(info),
            Declarator::DirectArrayFixed(decl) => decl.resolve_typeinfo(info),
            Declarator::DirectArrayUnbounded(decl) => decl.resolve_typeinfo(info),
            Declarator::DirectFunction(decl) => decl.resolve_typeinfo(info),
            Declarator::Const(decl) => decl.resolve_typeinfo(info),
            Declarator::Pointer(decl) => decl.resolve_typeinfo(info),
            Declarator::Init(decl) => decl.resolve_typeinfo(info),
            Declarator::AbstractArrayFixed(decl) => decl.resolve_typeinfo(info),
            Declarator::AbstractArrayUnbounded(decl) => decl.resolve_typeinfo(info),
            Declarator::AbstractFunction(decl) => decl.resolve_typeinfo(info),
            Declarator::AbstractPointer(decl) => decl.resolve_typeinfo(info),
            Declarator::AbstractConst(decl) => decl.resolve_typeinfo(info),
        }
    }
}

#[derive(Debug, Clone)]
pub struct DeclIdentifier {
    pub name: String,
}
impl DeclIdentifier {
    pub fn resolve_typeinfo(&self, info: TypeInfo) -> (Option<String>, TypeInfo) {
        (Some(self.name.clone()), info)
    }
}

#[derive(Debug, Clone)]
pub struct DeclDirectArrayFixed {
    pub declarator: Box<Declarator>,
    pub size: Expression,
}
impl DeclDirectArrayFixed {
    pub fn resolve_typeinfo(&self, info: TypeInfo) -> (Option<String>, TypeInfo) {
        let (name, info) = self.declarator.resolve_typeinfo(info);
        let size = self
            .size
            .get_constant_i64()
            .expect("Array size must be constant") as usize;
        (name, TypeInfo::Array(Box::new(info), Some(size)))
    }
}

#[derive(Debug, Clone)]
pub struct DeclDirectArrayUnbounded {
    pub declarator: Box<Declarator>,
}
impl DeclDirectArrayUnbounded {
    pub fn resolve_typeinfo(&self, info: TypeInfo) -> (Option<String>, TypeInfo) {
        let (name, info) = self.declarator.resolve_typeinfo(info);
        (name, TypeInfo::Array(Box::new(info), None))
    }
}

#[derive(Debug, Clone)]
pub struct DeclDirectFunction {
    pub declarator: Box<Declarator>,
    pub params: Vec<(Option<String>, TypeInfo)>,
}
impl DeclDirectFunction {
    pub fn resolve_typeinfo(&self, info: TypeInfo) -> (Option<String>, TypeInfo) {
        let (name, return_type) = self.declarator.resolve_typeinfo(info);
        let mut params = Vec::new();
        for (_, param_type) in &self.params {
            params.push(param_type.clone());
        }
        (name, TypeInfo::Function(Box::new(return_type), params))
    }
}
#[derive(Debug, Clone)]
pub struct DeclConst {
    pub declarator: Box<Declarator>,
}
impl DeclConst {
    pub fn resolve_typeinfo(&self, info: TypeInfo) -> (Option<String>, TypeInfo) {
        let (name, info) = self.declarator.resolve_typeinfo(info);
        (name, TypeInfo::Const(Box::new(info)))
    }
}

#[derive(Debug, Clone)]
pub struct DeclPointer {
    pub declarator: Box<Declarator>,
}
impl DeclPointer {
    pub fn resolve_typeinfo(&self, info: TypeInfo) -> (Option<String>, TypeInfo) {
        let (name, info) = self.declarator.resolve_typeinfo(info);
        (name, TypeInfo::Pointer(Box::new(info)))
    }
}

#[derive(Debug, Clone)]
pub struct DeclInit {
    pub declarator: Box<Declarator>,
    pub initializer: Option<Expression>,
}
impl DeclInit {
    pub fn resolve_typeinfo(&self, info: TypeInfo) -> (Option<String>, TypeInfo) {
        self.declarator.resolve_typeinfo(info)
    }
}

#[derive(Debug, Clone)]
pub struct DeclAbstractArrayFixed {
    pub declarator: Option<Box<Declarator>>,
    pub size: Expression,
}
impl DeclAbstractArrayFixed {
    pub fn resolve_typeinfo(&self, info: TypeInfo) -> (Option<String>, TypeInfo) {
        let (name, info_) = if let Some(decl) = &self.declarator {
            decl.resolve_typeinfo(info)
        } else {
            (None, info)
        };
        let size = self
            .size
            .get_constant_i64()
            .expect("Array size must be constant") as usize;
        (name, TypeInfo::Array(Box::new(info_), Some(size)))
    }
}

#[derive(Debug, Clone)]
pub struct DeclAbstractArrayUnbounded {
    pub declarator: Option<Box<Declarator>>,
}
impl DeclAbstractArrayUnbounded {
    pub fn resolve_typeinfo(&self, info: TypeInfo) -> (Option<String>, TypeInfo) {
        let (name, info_) = if let Some(decl) = &self.declarator {
            decl.resolve_typeinfo(info)
        } else {
            (None, info)
        };
        (name, TypeInfo::Array(Box::new(info_), None))
    }
}

#[derive(Debug, Clone)]
pub struct DeclAbstractFunction {
    pub declarator: Option<Box<Declarator>>,
    pub params: Vec<(Option<String>, TypeInfo)>,
}
impl DeclAbstractFunction {
    pub fn resolve_typeinfo(&self, info: TypeInfo) -> (Option<String>, TypeInfo) {
        let (name, return_type) = if let Some(decl) = &self.declarator {
            decl.resolve_typeinfo(info)
        } else {
            (None, info)
        };
        (
            name,
            TypeInfo::Function(
                Box::new(return_type),
                self.params.iter().map(|(_, t)| t.clone()).collect(),
            ),
        )
    }
}

#[derive(Debug, Clone)]
pub struct DeclAbstractPointer {
    pub declarator: Option<Box<Declarator>>,
}
impl DeclAbstractPointer {
    pub fn resolve_typeinfo(&self, info: TypeInfo) -> (Option<String>, TypeInfo) {
        let (name, info_) = if let Some(decl) = &self.declarator {
            decl.resolve_typeinfo(info)
        } else {
            (None, info)
        };
        (name, TypeInfo::Pointer(Box::new(info_)))
    }
}

#[derive(Debug, Clone)]
pub struct DeclAbstractConst {
    pub declarator: Option<Box<Declarator>>,
}
impl DeclAbstractConst {
    pub fn resolve_typeinfo(&self, info: TypeInfo) -> (Option<String>, TypeInfo) {
        let (name, info_) = if let Some(decl) = &self.declarator {
            decl.resolve_typeinfo(info)
        } else {
            (None, info)
        };
        (name, TypeInfo::Const(Box::new(info_)))
    }
}
