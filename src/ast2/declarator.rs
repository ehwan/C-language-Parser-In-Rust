use super::CVType;
use super::CompileError;
use super::PrimitiveType;

#[derive(Debug, Clone)]
pub struct CombinedDeclarator {
    /// variable name, for direct declarator
    pub name: Option<String>,
    pub cv_type: CVType,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntSizeSpecifier {
    Char,
    Short,
    Int,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SignedSpecifier {
    Signed,
    Unsigned,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FloatSizeSpecifier {
    Float,
    Double,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct SpecifierQualifierCollector {
    pub long: bool,
    pub signed: Option<SignedSpecifier>,
    pub int: Option<IntSizeSpecifier>,
    pub float: Option<FloatSizeSpecifier>,
    pub const_: bool,
    pub volatile: bool,
    pub void: bool,
    // pub struct_: Option<bool>,
}

impl SpecifierQualifierCollector {
    pub fn new() -> Self {
        Self {
            long: false,
            signed: None,
            int: None,
            float: None,
            const_: false,
            volatile: false,
            void: false,
            // struct_: None,
        }
    }
    pub fn set_const(&mut self) -> Result<(), CompileError> {
        self.const_ = true;
        Ok(())
    }
    pub fn set_volatile(&mut self) -> Result<(), CompileError> {
        self.volatile = true;
        Ok(())
    }
    pub fn set_int(&mut self) -> Result<(), CompileError> {
        if let Some(old) = self.int {
            if old != IntSizeSpecifier::Int {
                return Err(CompileError::InvalidTypeSpecifier);
            }
        }
        self.int = Some(IntSizeSpecifier::Int);
        Ok(())
    }
    pub fn set_char(&mut self) -> Result<(), CompileError> {
        if let Some(old) = self.int {
            if old != IntSizeSpecifier::Char {
                return Err(CompileError::InvalidTypeSpecifier);
            }
        }
        self.int = Some(IntSizeSpecifier::Char);
        Ok(())
    }
    pub fn set_short(&mut self) -> Result<(), CompileError> {
        if let Some(old) = self.int {
            if old != IntSizeSpecifier::Short {
                return Err(CompileError::InvalidTypeSpecifier);
            }
        }
        self.int = Some(IntSizeSpecifier::Short);
        Ok(())
    }
    pub fn set_long(&mut self) -> Result<(), CompileError> {
        self.long = true;
        Ok(())
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
    pub fn set_typename(&mut self, cv_type: &CVType) -> Result<(), CompileError> {
        unimplemented!()
    }

    pub fn into_type(self) -> Result<CVType, CompileError> {
        unimplemented!("SpecifierQualifierCollector::into_type")
        /*
        let base_type = if self.long {
            match self.int {
                Some(IntSizeSpecifier::Char) | Some(IntSizeSpecifier::Short) => {
                    return Err(CompileError::InvalidTypeSpecifier);
                }
                Some(IntSizeSpecifier::Int) | None => match self.signed {
                    Some(SignedSpecifier::Unsigned) => PrimitiveType::UInt64,
                    Some(SignedSpecifier::Signed) | None => PrimitiveType::Int64,
                },
                _ => unreachable!(),
            }
        } else {
        };

        Ok(CVType {
            const_: self.const_,
            volatile: self.volatile,
            type_: base_type,
        })
        */
    }
}
