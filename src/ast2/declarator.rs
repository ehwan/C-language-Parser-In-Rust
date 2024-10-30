use super::CVType;
use super::CompileError;

#[derive(Debug, Clone)]
pub struct CombinedDeclarator {
    /// variable name, for direct declarator
    pub name: Option<String>,
    pub cv_type: CVType,
}

pub(crate) struct SpecifierQualifierCollector {}

impl SpecifierQualifierCollector {
    pub fn new() -> Self {
        Self {}
    }
    pub fn set_const(&mut self) -> Result<(), CompileError> {
        unimplemented!()
    }
    pub fn set_volatile(&mut self) -> Result<(), CompileError> {
        unimplemented!()
    }
    pub fn set_int(&mut self) -> Result<(), CompileError> {
        unimplemented!()
    }
    pub fn set_char(&mut self) -> Result<(), CompileError> {
        unimplemented!()
    }
    pub fn set_short(&mut self) -> Result<(), CompileError> {
        unimplemented!()
    }
    pub fn set_long(&mut self) -> Result<(), CompileError> {
        unimplemented!()
    }
    pub fn set_signed(&mut self) -> Result<(), CompileError> {
        unimplemented!()
    }
    pub fn set_unsigned(&mut self) -> Result<(), CompileError> {
        unimplemented!()
    }
    pub fn set_float(&mut self) -> Result<(), CompileError> {
        unimplemented!()
    }
    pub fn set_double(&mut self) -> Result<(), CompileError> {
        unimplemented!()
    }
    pub fn set_void(&mut self) -> Result<(), CompileError> {
        unimplemented!()
    }
    pub fn set_struct(&mut self) -> Result<(), CompileError> {
        unimplemented!()
    }
    pub fn set_union(&mut self) -> Result<(), CompileError> {
        unimplemented!()
    }
    pub fn set_enum(&mut self) -> Result<(), CompileError> {
        unimplemented!()
    }
    pub fn set_typename(&mut self) -> Result<(), CompileError> {
        unimplemented!()
    }

    pub fn into_type(self) -> Result<CVType, CompileError> {
        unimplemented!()
    }
}
