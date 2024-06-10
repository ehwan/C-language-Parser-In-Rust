use super::expression::Expression;
use super::typename::TypeInfo;

use std::any::Any;
use std::vec::Vec;

pub trait Declarator: std::fmt::Debug + Any {
    fn as_any(&self) -> &dyn Any;

    // get (variable_name, real_type) from direct declarator and type info
    fn resolve_typeinfo(&self, _info: TypeInfo) -> (Option<String>, TypeInfo) {
        panic!(
            "get_direct_typeinfo_from_declarator not implemented for {:?}",
            self
        );
    }
}

#[derive(Debug)]
pub struct IdentifierDeclarator {
    pub name: String,
}
impl Declarator for IdentifierDeclarator {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn resolve_typeinfo(&self, info: TypeInfo) -> (Option<String>, TypeInfo) {
        (Some(self.name.clone()), info)
    }
}

#[derive(Debug)]
pub struct DirectArrayFixedDeclarator {
    pub declarator: Box<dyn Declarator>,
    pub size: Box<dyn Expression>,
}
impl Declarator for DirectArrayFixedDeclarator {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn resolve_typeinfo(&self, info: TypeInfo) -> (Option<String>, TypeInfo) {
        let (name, info) = self.declarator.resolve_typeinfo(info);
        let size = self
            .size
            .get_constant_i64()
            .expect("Array size must be constant") as usize;
        (name, TypeInfo::Array(Box::new(info), Some(size)))
    }
}

#[derive(Debug)]
pub struct DirectArrayUnboundedDeclarator {
    pub declarator: Box<dyn Declarator>,
}
impl Declarator for DirectArrayUnboundedDeclarator {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn resolve_typeinfo(&self, info: TypeInfo) -> (Option<String>, TypeInfo) {
        let (name, info) = self.declarator.resolve_typeinfo(info);
        (name, TypeInfo::Array(Box::new(info), None))
    }
}

#[derive(Debug)]
pub struct DirectFunctionDeclarator {
    pub declarator: Box<dyn Declarator>,
    pub params: Vec<(Option<String>, TypeInfo)>,
}
impl Declarator for DirectFunctionDeclarator {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn resolve_typeinfo(&self, info: TypeInfo) -> (Option<String>, TypeInfo) {
        let (name, return_type) = self.declarator.resolve_typeinfo(info);
        let mut params = Vec::new();
        for (_, param_type) in &self.params {
            params.push(param_type.clone());
        }
        (name, TypeInfo::Function(Box::new(return_type), params))
    }
}

#[derive(Debug)]
pub struct PointerDeclarator {
    pub declarator: Box<dyn Declarator>,
}
impl Declarator for PointerDeclarator {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn resolve_typeinfo(&self, info: TypeInfo) -> (Option<String>, TypeInfo) {
        let (name, info) = self.declarator.resolve_typeinfo(info);
        (name, TypeInfo::Pointer(Box::new(info)))
    }
}

#[derive(Debug)]
pub struct InitDeclarator {
    pub declarator: Box<dyn Declarator>,
    pub initializer: Option<Box<dyn Expression>>,
}
impl Declarator for InitDeclarator {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn resolve_typeinfo(&self, info: TypeInfo) -> (Option<String>, TypeInfo) {
        self.declarator.resolve_typeinfo(info)
    }
}

#[derive(Debug)]
pub struct AbstractArrayFixedDeclarator {
    pub declarator: Option<Box<dyn Declarator>>,
    pub size: Box<dyn Expression>,
}
impl Declarator for AbstractArrayFixedDeclarator {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn resolve_typeinfo(&self, info: TypeInfo) -> (Option<String>, TypeInfo) {
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

#[derive(Debug)]
pub struct AbstractArrayUnboundedDeclarator {
    pub declarator: Option<Box<dyn Declarator>>,
}
impl Declarator for AbstractArrayUnboundedDeclarator {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn resolve_typeinfo(&self, info: TypeInfo) -> (Option<String>, TypeInfo) {
        let (name, info_) = if let Some(decl) = &self.declarator {
            decl.resolve_typeinfo(info)
        } else {
            (None, info)
        };
        (name, TypeInfo::Array(Box::new(info_), None))
    }
}

#[derive(Debug)]
pub struct AbstractFunctionDeclarator {
    pub declarator: Option<Box<dyn Declarator>>,
    pub params: Vec<(Option<String>, TypeInfo)>,
}
impl Declarator for AbstractFunctionDeclarator {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn resolve_typeinfo(&self, info: TypeInfo) -> (Option<String>, TypeInfo) {
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

#[derive(Debug)]
pub struct AbstractPointerDeclarator {
    pub declarator: Option<Box<dyn Declarator>>,
}
impl Declarator for AbstractPointerDeclarator {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn resolve_typeinfo(&self, info: TypeInfo) -> (Option<String>, TypeInfo) {
        let (name, info_) = if let Some(decl) = &self.declarator {
            decl.resolve_typeinfo(info)
        } else {
            (None, info)
        };
        (name, TypeInfo::Pointer(Box::new(info_)))
    }
}
